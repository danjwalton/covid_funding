required.packages <- c("data.table","jsonlite","httr","XML")
lapply(required.packages, require, character.only=T)

wd <- "G:/My Drive/Work/GitHub/covid_funding/"
setwd(wd)

base.url <- "https://fts.unocha.org/appeals/overview/2020"

data <- htmlParse(GET(base.url))

plans <- data.table(Plan = xpathSApply(data, "//td/a", xmlValue), base.link = xpathSApply(data, "//td/a", xmlAttrs))
plans <- plans[grepl("appeals", base.link)]
plans[, base.link := gsub("summary", "", base.link)]

out <- list()
for(i in 1:nrow(plans)){
  plan.name <- plans$Plan[i]
  message(plan.name)
  planlink <- paste0('https://fts.unocha.org/', plans$base.link[i], "summary")
  data <- htmlParse(GET(planlink))
  tables <- readHTMLTable(xpathSApply(data, "//div[@class='funding-progress-bar']", xmlGetAttr, "data-content"))
  names.tables <- xpathSApply(data, "//div[@class='funding-info']", xmlValue)
  
  if(any(grepl("COVID-19", names.tables))){
    covid <- data.table(transpose(tables[grepl(" COVID-19",names.tables)][[1]]))
    non.covid <- data.table(transpose(tables[grepl("-COVID-19",names.tables)][[1]]))
    names(covid) <- paste0("COVID.",unlist(covid[1]))
    names(non.covid) <- paste0("Non-COVID.",unlist(non.covid[1]))
  } else {
    if(grepl("COVID", plan.name)){
      covid <- data.table(transpose(tables[[1]]))
      names(covid) <- paste0("COVID.",unlist(covid[1]))
      non.covid <- NULL
    } else {
      non.covid <- data.table(transpose(tables[[1]]))
      names(non.covid) <- paste0("Non-COVID.",unlist(non.covid[1]))
      covid <- NULL
    }
  }
  
  covid <- covid[-1]
  non.covid <- non.covid[-1]
  
  out[[i]] <- cbind(plan.name = plan.name, covid, non.covid)
}

agg <- rbindlist(out, fill = T)

agg <- agg[,lapply(.SD, function(x) gsub("US[$]| COVID?.*|,| Flash?.*| Intersectoral?.*| 2019?.*","", x))]
agg <- agg[, lapply(.SD, function(x) sum(as.numeric(x),na.rm=T)), .SDcols = names(agg)[-1], by = plan.name]
fwrite(agg, "output/HRP requirements_RRP_sep.csv")

#WIP beyond this point#
rrps <- fread("project_data/rrp_data.csv")
rrps <- rrps[Year == 2020 & (Country != "Regional" | Country != "region")]
rrps <- rrps[, lapply(.SD, function(x) as.numeric(gsub("[$]|,|[%]","",x))), .SDcols = 4:7, by = .(RRP, Year, Country)]
rrps[is.na(rrps)] <- 0

rrps[Country == "Congo, The Democratic Republic of the" | Country == "Dem Rep of the Congo"]$Country <- "DRC"
rrps[Country == "Tanzania, United Republic of"]$Country <- "Tanzania"
rrps[Country == "Central African Republic"]$Country <- "CAR"
rrps[Country == "Syrian Arab Republic"]$Country <- "Syria"

rrps.non.covid <- rrps[,c("Country", "Funds Received", "Funds Requested")]
names(rrps.non.covid) <- c("plan.name", "Non-COVID.Funded through this plan", "Non-COVID.Total requirements")
rrps.non.covid[, `Non-COVID.Unmet requirements` := `Non-COVID.Total requirements`- `Non-COVID.Funded through this plan`]

agg <- rbind(agg, rrps.non.covid, fill = T)
agg[plan.name == "DPR Korea"]$plan.name <- "DPRK"

agg <- agg[, lapply(.SD, function(x) sum(as.numeric(x),na.rm=T)), .SDcols = names(agg)[-1], by = plan.name]
agg[, `:=` (COVID_funding_level = `COVID.Funded through this plan`/`COVID.Total requirements`, `Non-COVID_funding_level` = `Non-COVID.Funded through this plan`/`Non-COVID.Total requirements`)]

fwrite(agg, "output/HRP requirements_RRP_inc.csv")
