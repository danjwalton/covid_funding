required.packages <- c("data.table", "jsonlite", "httr", "ggplot2", "WDI", "readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/covid_funding/")

get.query.data <- function(filename=NULL, query_id=NULL, path="project_data", force.update = F, remove.old = F){
  if(is.null(filename) & is.null(query_id)) stop("Please specify a filename and/or query id.", call.=F)
  query <- paste0("https://ddw.devinit.org/api/export/", query_id, "/")
  old.files <- list.files(path, paste0("^", filename, ".{11}[.]csv"))
  new.file <- old.files[which.max(as.Date(gsub(paste0(filename, "_"), "", old.files)))]
  if(force.update | length(old.files) == 0){
    if(is.null(query_id)) stop(paste0("Local file for ", filename, " not found. Please specify a query id to download."), call.=F)
    message("Downloading new dataset...")
    if(is.null(filename)) filename <- paste0("query_", query_id)
    new.file <- paste0(filename, "_", Sys.Date(), ".csv")
    fwrite(fread(query), paste0(path, "/", new.file))
  }
  query.data <- fread(paste0(path, "/", new.file))
  old.files <- old.files[old.files != new.file]
  if(remove.old & length(old.files) != 0) file.remove(paste0(path, "/", old.files))
  return(query.data)
}

get_fts <- function(year = NULL, planid = NULL, emergencyid = NULL, globalclusterid = NULL, destinationlocationid = NULL){
  lapply(c("data.table", "jsonlite"), require, character.only=T)
  if(!is.null(year)){
    year <- paste0("year=", paste0(year, collapse=","))
  }
  if(!is.null(planid)){
    planid <- paste0("planid=", paste0(planid, collapse=","))
  }
  if(!is.null(emergencyid)){
    emergencyid <- paste0("emergencyid=", paste0(emergencyid, collapse=","))
  }
  if(!is.null(globalclusterid)){
    globalclusterid <- paste0("globalclusterid=", paste0(globalclusterid, collapse=","))
  }
  if(!is.null(destinationlocationid)){
    destinationlocationid <- paste0("destinationlocationid:", paste0(destinationlocation, collapse=","))
  }
  
  call.filter <- NULL
  if(!is.null(destinationlocationid)){
    call.filter <- paste0("&filterby=", destinationlocationid)
  }
  
  hpc <- "https://api.hpc.tools/v1/public/fts/flow?"
  call.param <- paste(year, planid, emergencyid, globalclusterid, call.filter, "format=json&limit=1000", sep="&")
  call <- paste0(hpc, call.param)
  fts <- fromJSON(call, flatten=T)
  
  flowslist <- list()
  flowslist[[1]] <- fts$data$flows
  i <- 2
  while (!is.null(fts$meta$nextLink)){
    nextLink <- fts$meta$nextLink
    fts <- fromJSON(nextLink, flatten=T)
    flowslist[[i]] <- fts$data$flows
    i <- i + 1
  }
  
  flows <- rbindlist(flowslist, fill=T, use.names = T)
  return(flows)
}

unnest_fts <- function(fts, cols = c("sourceObjects", "destinationObjects"), splits = "type", remove.nested = T, group.same = T){
  require(data.table)
  if(length(cols) != length(splits) & length(splits) != 1) stop("There must be one split for each nested col, or a single common split for all nested cols.", call.=F)
  fts <- as.data.table(fts)
  expand.splits <- data.table(cols = cols, splits = splits)
  message("Unnesting...")
  for(i in 1:nrow(expand.splits)){
    col <- expand.splits[i]$cols
    split <- expand.splits[i]$splits
    if(group.same){
      expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(t(unlist(split(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")), as.data.table(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")))[, ..split]))))), fill=T)
    } else {
      expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(unlist(split(x, as.data.table(x)[, ..split])))), fill=T)
    }
    names(expanded) <- paste(col, names(expanded), sep="_")
    split.cols <- unique(names(expanded)[grepl(paste0("[.]", split, "\\d*$"), names(expanded))])
    expanded[, (split.cols) := NULL]
    expanded[, (split.cols) := NULL]
    expanded <- expanded[,which(unlist(lapply(expanded, function(x)!(all(is.na(x))|all(is.null(x)))))),with=F]
    fts <- cbind(fts, expanded)
    if(remove.nested) fts[, (col) := NULL][]
  }
  return(fts)
}

split_rows <- function(data, value.cols = "amountUSD", split.cols = "destinationObjects_Location.name", split.pattern = ";", remove.unsplit = T){
  split.pattern <- trimws(split.pattern)
  temp <- data[, .(trimws(unlist(strsplit(get(split.cols), split.pattern))), get(value.cols)/(1+nchar(get(split.cols))-nchar(gsub(split.pattern, "", get(split.cols))))), by=rownames(data)]
  names(temp) <- c("rownames", paste0(split.cols, ".split"), paste0(value.cols, ".split"))
  data <- merge(data[, rownames := rownames(data)], temp)
  if(remove.unsplit){
    data[, (split.cols) := NULL]
    data[, (value.cols) := NULL]
  }
  data$rownames <- NULL
  return(data)
}

split_iati <- function(iati, cols = c("sectors", "countries_regions")){
  require(data.table)
  for(col in cols){
    message("Splitting ",col, "...")
    temp <- iati[, .(trimws(unlist(strsplit(get(col), "[}], [{]")))), by=rownames(iati)]
    temp <- temp[, tstrsplit(V1, ", "), by = rownames]
    colsplitnames <- c("rownames", paste0(col, "_", gsub(":.*|[[{]|'", "", temp[1, -1])))
    temp <- data.table(temp[, apply(.SD, 2, function(x) trimws(gsub(".*:|[}]]|'", "", x)))])
    setnames(temp, colsplitnames)
    iati <- merge(iati[, rownames := rownames(iati)], temp, by = "rownames")
    iati$rownames <- NULL
  }
  percentagenames <- grep("_percentage$", names(iati), value = T)
  iati[, split_value_USD := as.numeric(value_USD) * apply(apply(.SD, 2, function(x) as.numeric(x))/100, 1, prod), .SDcols = (percentagenames)]
}

temp <- tempfile(fileext = ".xlsx")
download.file("https://github.com/markbrough/covid19-data/raw/gh-pages/traceability/transactions.xlsx", "project_data/transactions.xlsx", mode = 'wb')

iati <- data.table(read_xlsx("project_data/transactions.xlsx", col_types = "text"))
iati <- split_iati(iati, cols = c("sectors", "countries_regions"))
iati.names <- fread("project_data/iatidonors.csv", header = T)

iati <- merge(iati, iati.names, by.x = "provider_text", by.y = "IATI_provider", all.x = T)

fts.2020 <- unnest_fts(get_fts(year = 2020))
fts.codes <- get.query.data("FTS_ISO", 651)
fts.codes$`Country code` <- as.character(fts.codes$`Country code`)

fts.2020[
    grepl(911, sourceObjects_Emergency.id)|grepl(911, destinationObjects_Emergency.id)
  | 
    grepl(26513, sourceObjects_GlobalCluster.id)|grepl(26513, destinationObjects_GlobalCluster.id)
  | 
    grepl(952, sourceObjects_Plan.id)|grepl(952, destinationObjects_Plan.id)
         , covid := TRUE]

fts.2020[grepl(952, sourceObjects_Plan.id)|grepl(952, destinationObjects_Plan.id), ghrp := TRUE]

fts.2020.split <- split_rows(fts.2020, value.cols = "amountUSD", split.cols = "destinationObjects_Location.id", split.pattern = ";", remove.unsplit = T)

fts.2020 <- merge(fts.2020, fts.codes, by.x = "destinationObjects_Location.id", by.y = "Country code", all.x=T)
isos <- as.data.table(WDI(indicator = "NY.GDP.PCAP.KD", start=2019, end=2019, extra=T)[, c("iso2c", "iso3c")])
isos[iso2c == "KP"] <- "PRK"
isos[iso2c == "ZG"] <- "ZGX"
isos[iso2c == "ZJ"] <- "ZJX"

fts.2020$FTS_name <- gsub(", Government of", "", fts.2020$sourceObjects_Organization.name)
fts.2020 <- merge(fts.2020, isos, by.x = "ISO Alpha 3", by.y = "iso3c", all.x = T)

fts.pairs <- unique(paste0(fts.2020$FTS_name, fts.2020$iso2c))

iati$transaction_type_fts <- ifelse(iati$transaction_type_code %in% c(3), "paid", ifelse(iati$transaction_type_code == 2, "commitment", "other"))
iati[, humanitarian_fixed := ifelse(humanitarian %in% c("1", "true") | grepl("720", sectors_code), ifelse(paste0(FTS_name, countries_regions_code) %in% fts.pairs, "humanitarian_in_FTS", "humanitarian_in_IATI"), "development"), by = `...1`]

iati.donor.cast <- dcast(iati[transaction_type_fts != "other" & finance_type == 110 & covid_relevant %in% c("MAYBE", "YES") & as.Date(transaction_date) > as.Date("2020-01-01")], FTS_name ~ humanitarian_fixed, value.var = "split_value_USD", fun.aggregate = function(x) sum(x, na.rm = T))

fts.donor.cast <- dcast(fts.2020[covid == T], FTS_name ~ status, value.var = "amountUSD", fun.aggregate = function(x) sum(x, na.rm = T)) #outgoing flows by organisation
fts.donor.cast.inc <- dcast(fts.2020[covid == T], destinationObjects_Organization.name ~ status, value.var = "amountUSD", fun.aggregate = function(x) -sum(x, na.rm = T)) #incoming flows by organisations

fts.donor.cast <- merge(fts.donor.cast, fts.donor.cast.inc, by.x = "FTS_name", by.y = "destinationObjects_Organization.name", all.x = T)

fts.donor.cast <- fts.donor.cast[, .(commitment = sum(commitment.x, commitment.y, na.rm = T), paid = sum(paid.x, paid.y, na.rm = T), pledge = sum(pledge.x, pledge.y, na.rm = T)), by = FTS_name] #estimate first-level flows only
fwrite(fts.donor.cast, "output/fts_donors.csv")
#setnames(iati.donor.cast, c("FTS_name", paste0("development", names(iati.donor.cast)[-1])))
setnames(fts.donor.cast, c("FTS_name", "humanitarian"))

all.donor.cast <- merge(iati.donor.cast, fts.donor.cast, by = "FTS_name", all = T)
fwrite(all.donor.cast, "output/all_donors.csv")

iati.recipient.cast <- dcast(iati[transaction_type_fts != "other" & finance_type == 110 & covid_relevant %in% c("MAYBE", "YES") & as.Date(transaction_date) > as.Date("2020-01-01")], countries_regions_code ~ humanitarian_fixed, value.var = "split_value_USD", fun.aggregate = function(x) sum(x, na.rm = T))
fts.recipient.cast <- dcast(fts.2020[covid == T], iso2c + `Country name` ~ ., value.var = "amountUSD", fun.aggregate = function(x) sum(as.numeric(x), na.rm = T))

fwrite(fts.recipient.cast, "output/fts_recipients.csv")
#setnames(iati.recipient.cast, c("iso2c", paste0("development", names(iati.recipient.cast)[-1])))
setnames(fts.recipient.cast, c("iso2c", "Country_name", "humanitarian"))

all.recipient.cast <- merge(iati.recipient.cast, fts.recipient.cast, by.x = "countries_regions_code", by.y = "iso2c", all = T)
all.recipient.cast <- all.recipient.cast[,c("countries_regions_code", "Country_name", "development", "humanitarian_in_FTS", "humanitarian_in_IATI", "humanitarian")]
fwrite(all.recipient.cast, "output/all_recipients.csv")

iati.date.cast <- dcast(iati[transaction_type_fts != "other" & finance_type == 110 & covid_relevant %in% c("MAYBE", "YES") & as.Date(transaction_date) > as.Date("2020-01-01")], as.Date(transaction_date) ~ humanitarian_fixed, value.var = "split_value_USD", fun.aggregate = function(x) sum(x, na.rm = T))
fts.date.cast <- dcast(fts.2020[covid == T], as.Date(date) ~ ., value.var = "amountUSD", fun.aggregate = function(x) sum(x, na.rm = T))

setnames(fts.date.cast, c("transaction_date", "humanitarian"))

all.date.cast <- merge(iati.date.cast, fts.date.cast, all = T)
all.date.cast[is.na(all.date.cast)] <- 0

all.date.cast[,c(2:5)] <- data.table(sapply(all.date.cast[,c(2:5)], cumsum))

fwrite(all.date.cast, "output/all_dates.csv")

iati$org_type <- "Other"
iati[receiver_type >= 10]$org_type <- "Government"
iati[receiver_type >= 20]$org_type <- "NGO"
iati[receiver_type >= 30]$org_type <- "PPP"
iati[receiver_type >= 40]$org_type <- "Multilateral"
iati[receiver_type >= 60]$org_type <- "Private organization/foundation"
iati[receiver_type >= 80]$org_type <- "Other"

iati.types.cast <- dcast(iati[transaction_type_fts != "other" & finance_type == 110 & covid_relevant %in% c("MAYBE", "YES") & as.Date(transaction_date) > as.Date("2020-01-01")], org_type ~ humanitarian_fixed, value.var = "split_value_USD", fun.aggregate = function(x) sum(x, na.rm = T))

fts.2020$org_type <- fts.2020$destinationObjects_Organization.organizationTypes
fts.2020[!(destinationObjects_Organization.organizationTypes %in% iati.types.cast$org_type)]$org_type <- "Multilateral"
fts.2020[is.na(destinationObjects_Organization.organizationTypes)]$org_type <- "Other"
fts.2020[destinationObjects_Organization.organizationTypes == "Inter-governmental"]$org_type <- "Government"

fts.types.cast <- dcast(fts.2020[covid == T], org_type ~ ., value.var = "amountUSD", fun.aggregate = function(x) sum(x, na.rm = T))

setnames(fts.types.cast, ".", "humanitarian")

all.types.cast <- merge(iati.types.cast, fts.types.cast)
fwrite(all.types.cast, "output/all_types.csv")

iati.recipients.ngos.cast <- dcast(iati[org_type == "NGO" & transaction_type_fts != "other" & finance_type == 110 & covid_relevant %in% c("MAYBE", "YES") & as.Date(transaction_date) > as.Date("2020-01-01")], countries_regions_code ~ humanitarian_fixed, value.var = "split_value_USD", fun.aggregate = function(x) sum(x, na.rm = T))
fts.recipients.ngos.cast <- dcast(fts.2020[org_type == "NGO" & covid == T], iso2c + `Country name` ~ ., value.var = "amountUSD", fun.aggregate = function(x) sum(x, na.rm = T))

setnames(fts.recipients.ngos.cast, c("iso2c", "Country_name", "humanitarian"))

all.recipients.ngos.cast <- merge(iati.recipients.ngos.cast, fts.recipients.ngos.cast, by.x = "countries_regions_code", by.y = "iso2c", all = T)
all.recipients.ngos.cast <- all.recipients.ngos.cast[,c("countries_regions_code", "Country_name", "development", "humanitarian_in_FTS", "humanitarian_in_IATI", "humanitarian")]
fwrite(all.recipients.ngos.cast, "output/all_recipients_ngos.csv")

iati.donors.ngos.cast <- dcast(iati[org_type == "NGO" & transaction_type_fts != "other" & finance_type == 110 & covid_relevant %in% c("MAYBE", "YES") & as.Date(transaction_date) > as.Date("2020-01-01")], FTS_name ~ humanitarian_fixed, value.var = "split_value_USD", fun.aggregate = function(x) sum(x, na.rm = T))
fts.donors.ngos.cast <- dcast(fts.2020[org_type == "NGO" & covid == T], FTS_name ~ ., value.var = "amountUSD", fun.aggregate = function(x) sum(x, na.rm = T))

setnames(fts.donors.ngos.cast, c("FTS_name", "humanitarian"))

all.donors.ngos.cast <- merge(iati.donors.ngos.cast, fts.donors.ngos.cast, all = T)
fwrite(all.donors.ngos.cast, "output/all_donors_ngos.csv")

