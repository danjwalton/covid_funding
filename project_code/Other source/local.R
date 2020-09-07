required.packages <- c("data.table", "jsonlite", "httr", "ggplot2", "WDI")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/covid_funding/")

#Functions
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

# iati.2016 <- get.query.data("IATI_2016", 652, force.update = F, remove.old = T)
# iati.2017 <- get.query.data("IATI_2017", 653, force.update = F, remove.old = T)
# iati.2018 <- get.query.data("IATI_2018", 654, force.update = F, remove.old = T)
# iati.2019 <- get.query.data("IATI_2019", 655, force.update = F, remove.old = T)
# iati.2020 <- get.query.data("IATI_2020", 656, force.update = F, remove.old = T)

iati.2020 <- get.query.data("IATI_2020_commits", 667, force.update = T, remove.old = T)

iatis <- ls()[grepl("iati.20", ls())]
iati.all <- rbindlist(lapply(iatis, get))
rm(list = iatis)
gc()

iati.ts <- dcast(iati.all, `YYYYMM year and month` ~ ., value.var = "Transaction Value USD - Calculated", fun.aggregate = function(x) sum(x, na.rm=T))
iati.ts[, seasonal := decompose(ts(., frequency = 12), "multiplicative")$seasonal]

iati.publishers <- get.query.data("IATI_pubs", 650)

iati.publishers[`Organisation Headquarters` == "United States", `Organisation Headquarters` := "United States of America"]
iati.all <- merge(iati.all, iati.publishers, by.x="Reporting Organsation Reference", by.y="Reporting org ref", all.x=T)
iati.all$provider.fts.name <- ifelse(iati.all$`Reporting Organisation Type Name - Calculated` == "Government", paste0(iati.all$`Organisation Headquarters`, ", Government of"), iati.all$`Reporting Organisation Narrative`)
iati.all$receiver.fts.name <- ifelse(iati.all$`Transaction Receiver Organisation Type` == "Government" | iati.all$`Transaction Receiver Organisation Type` == "10" | grepl("^Ministry of", iati.all$`Transaction Receiver Organisation Narrative`), paste0(iati.all$`Country Name - Calculated`, ", Government of"), iati.all$`Transaction Receiver Organisation Narrative`)

iati.all$covid_corrected <- ifelse(as.Date(iati.all$`Transaction Date - Calculated`) < as.Date("2020-03-01"), FALSE, iati.all$`Covid flag - Calculated`)
iati.all$humanitarian_corrected <- ifelse(iati.all$Humanitarian == "true" | iati.all$Humanitarian == "1" | iati.all$`Transaction Humanitarian` == 1 | iati.all$`DI Sector Name` == "Humanitarian", "humanitarian", "development")

fts.2020 <- unnest_fts(get_fts(year = 2020))
fts.codes <- get.query.data("FTS_ISO", 651)
fts.codes$`Country code` <- as.character(fts.codes$`Country code`)

fts.2020[grepl(911, sourceObjects_Emergency.id)|grepl(911, destinationObjects_Emergency.id)|
           grepl(26513, sourceObjects_GlobalCluster.id)|grepl(26513, destinationObjects_GlobalCluster.id)|
           grepl(952, sourceObjects_Plan.id)|grepl(952, destinationObjects_Plan.id), covid := TRUE]

fts.2020 <- split_rows(fts.2020, value.cols = "amountUSD", split.cols = "destinationObjects_Location.id", split.pattern = ";", remove.unsplit = T)

fts.2020 <- merge(fts.2020, fts.codes, by.x = "destinationObjects_Location.id.split", by.y = "Country code", all.x=T)
isos <- as.data.table(WDI(indicator = "NY.GDP.PCAP.KD", start=2019, end=2019, extra=T)[, c("iso2c", "iso3c")])
isos[iso2c == "KP"] <- "PRK"
isos[iso2c == "ZG"] <- "ZGX"
isos[iso2c == "ZJ"] <- "ZJX"

fts.2020 <- merge(fts.2020, isos, by.x = "ISO Alpha 3", by.y = "iso3c", all.x = T)

fts.agg <- dcast(fts.2020[covid == TRUE], sourceObjects_Organization.name + iso2c + destinationObjects_Organization.name ~ ., value.var = "amountUSD.split", fun.aggregate = function(x) sum(x, na.rm=T))

iati.agg <- dcast(iati.all[covid_corrected == TRUE], provider.fts.name + `Country Code - Calculated` + receiver.fts.name ~ humanitarian_corrected, value.var = "Transaction Value USD - Calculated", fun.aggregate = function(x) sum(x, na.rm=T))

all.agg <- merge(fts.agg, iati.agg, by.x = c("sourceObjects_Organization.name", "iso2c", "destinationObjects_Organization.name"), by.y = c("provider.fts.name", "Country Code - Calculated", "receiver.fts.name"), all=T)
all.agg[, max.humanitarian := apply(.SD, 1, function(x) max(x, na.rm = T)), .SDcols = c(".", "humanitarian")]
