required.packages <- c("data.table", "jsonlite", "httr", "ggplot2")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/nexus_countries/")

get.query.data <- function(filename=NULL, query_id=NULL, path="project_data", force.update = F, remove.old = F){
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

iati.raw <- get.query.data("Nexus_IATI", 638, force.update = T, remove.old = T)

iati <- iati.raw[`Calculated Transaction Year` >= 2016]

iati$x_transaction_month_year <- format(as.Date(iati$`Calculated Transaction Date`), "%Y-%m")
iati$x_covid_corrected <- ifelse(as.Date(iati$`Calculated Transaction Date`) < as.Date("2020-03-01"), FALSE, iati$`X Covid flag`)

iati$humanitarian_corrected <- ifelse(iati$Humanitarian == "true" | iati$Humanitarian == "1" | iati$`Transaction Humanitarian` == 1 | iati$`DI Sector Name` == "Humanitarian", "humanitarian", "development")

iati <- iati[as.Date(`Calculated Transaction Date`) < as.Date("2020-8-1")]

nexus.covid <- dcast(as.data.table(iati), Country + x_covid_corrected + humanitarian_corrected ~ x_transaction_month_year, value.var="Calulated Transaction Value USD", fun.aggregate = function(x) sum(x, na.rm=T))
nexus.covid <- dcast(melt(nexus.covid, id.vars = c("Country", "x_covid_corrected", "humanitarian_corrected")), Country + variable ~ x_covid_corrected + humanitarian_corrected, value.var="value")

nexus.covid[, `:=` (FALSE_total = FALSE_humanitarian + FALSE_development, TRUE_total = TRUE_humanitarian + TRUE_development)]

nexus.covid[, c("FALSE_total", "seasonal", "trend", "random") := decompose(ts(FALSE_total, frequency = 12), "multiplicative"), by=Country]
nexus.covid[, FALSE_total_adjusted := FALSE_total/seasonal, by=Country]

ggplot(nexus.covid, aes(y = FALSE_total_adjusted, x = as.numeric(variable)))+
  geom_line(aes(color=Country))

list2env(split(nexus.covid, nexus.covid$Country), envir = .GlobalEnv)

nexus.covid$year <- substr(nexus.covid$variable, 0, 4)
nexus.covid$month <- month.abb[as.numeric(substr(nexus.covid$variable, 6, 8))]

fwrite(nexus.covid, "output/covid_trend.csv")
