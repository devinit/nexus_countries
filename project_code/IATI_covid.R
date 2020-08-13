required.packages <- c("data.table", "jsonlite", "httr", "ggplot2")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/nexus_countries/")

get.query.data <- function(filename=NULL, query_id=NULL, path="project_data", force.update = F, remove.old = T){
  if(is.null(filename) & is.null(query_id)) stop("Please specify a filename and/or query id.", call.=F)
  query <- paste0("https://ddw.devinit.org/api/export/", query_id, "/")
  old.files <- list.files(path, paste0(filename, ".*csv"))
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

iati.raw <- get.query.data("Nexus_IATI", 638, force.update = F)

iati <- iati.raw[x_default_vocabulary != ""]
iati <- iati[x_transaction_year >= 2016]

iati$x_transaction_month_year <- format(as.Date(iati$x_transaction_date), "%Y-%m")
iati$x_covid_corrected <- ifelse(iati$x_transaction_year < 2020, FALSE, iati$x_covid)

iati <- iati[as.Date(x_transaction_date) < as.Date("2020-7-1")]

nexus.covid <- dcast(as.data.table(iati), x_country + x_covid_corrected ~ x_transaction_month_year, value.var="x_transaction_value_usd", fun.aggregate = function(x) sum(x, na.rm=T))
nexus.covid <- dcast(melt(nexus.covid, id.vars = c("x_country", "x_covid_corrected")), x_country + variable ~ x_covid_corrected, value.var="value")

nexus.covid[, c("FALSE", "seasonal", "trend", "random") := decompose(ts(`FALSE`, frequency = 12), "multiplicative"), by=x_country]
nexus.covid[, FALSE_adjusted := `FALSE`/seasonal, by=x_country]

ggplot(nexus.covid, aes(y = FALSE_adjusted, x = as.numeric(variable)))+
  geom_line(aes(color=x_country))

list2env(split(nexus.covid, nexus.covid$x_country), envir = .GlobalEnv)

fwrite(nexus.covid, "output/covid_trend.csv")
