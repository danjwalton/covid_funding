required.packages <- c("data.table", "httr", "ggplot2", "WDI", "XML")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/covid_funding/")

iati.1 <- xmlParse("http://d-portal.org/q.xml?from=act&limit=-1&distincton=aid&%2Fhumanitarian-scope@code=EP-2020-000012-001")
iati.2 <- xmlParse("http://d-portal.org/q.xml?from=act&limit=-1&distincton=aid&text_search=COVID-19")
iati.3 <- xmlParse("http://d-portal.org/q.xml?from=act&limit=-1&distincton=aid&text_search=COVID%2019")
iati.4 <- xmlParse("http://d-portal.org/q.xml?from=act&limit=-1&distincton=aid&%2Ftag@code=COVID-19")
iati.5 <- xmlParse("http://d-portal.org/q.xml?from=act&limit=-1&distincton=aid&%2Fhumanitarian-scope@code=HCOVD20")

# iatis <- ls()[grepl("iati.", ls())]
# iati.covid <- rbindlist(get(iatis))

transactions <- getNodeSet(iati.1, "//transaction")

test <- transactions[[1]]

transaction.data <- function(transaction, data.name, is.attr = F, in.parent = F){
  if(in.parent) x <- xmlParent(transaction) else x <- transaction
  if(is.attr) v <- (xmlAttrs(x[[data.name]])) else v <- (xmlValue(x[[data.name]]))
  return(v)
}

iati_id <- transaction.data(transactions[[i]], 'iati-identifier', in.parent = T)
reporting_org_text <- transaction.data(transactions[[i]], 'reporting-org', in.parent = T)
reporting_org_ref <- transaction.data(test, 'reporting-org', is.attr = T, in.parent = T)['ref']
reporting_org_type <- transaction.data(test, 'reporting-org', is.attr = T, in.parent = T)['type']
value_currency <- if(!is.na(transaction.data(test, 'value', is.attr = T, in.parent = F)["currency"])){
  transaction.data(test, 'value', is.attr = T, in.parent = F)["currency"] } else {
  if(!is.na(transaction.data(test, 'default-currency', is.attr = F, in.parent = T))){
    transaction.data(test, 'default-currency', is.attr = F, in.parent = T)
  } else {
  'USD'
  }
  }
transaction_date <- transaction.data(test, 'value', is.attr = T, in.parent = F)['value-date']
transaction_type_code <- transaction.data(test, 'transaction-type', is.attr = T)['code']
finance_type <- if(!is.na(transaction.data(test, 'finance-type', is.attr = F, in.parent = F))){
  transaction.data(test, 'finance-type', is.attr = F, in.parent = F) } else {
    if(!is.na(transaction.data(test, 'default-finance-type', is.attr = F, in.parent = T))){
      transaction.data(test, 'default-finance-type', is.attr = F, in.parent = T)
    } else {
      ""
    }
  }
