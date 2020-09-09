required.packages <- c("WDI","data.table", "readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/nexus_countries/")

aims <- data.table(read_excel("project_data/AIMSProjects.xlsx"))

disb.cols <- names(aims)[grepl("disbursements", names(aims))]

loc.cols <- c("BRA", "FGS", "Galmudug", "Hiirshabelle", "Jubaland", "Puntland", "Somaliland", "South West", "UNATTRIBUTED")
cur.cols <- c("Currency", "Exchange rate")
flag.cols <- c("CAPACITY DEVELOPMENT MARKER", "COVID-19", "DURABLE SOLUTIONS", "GENDER MARKER", "HUMANITARIAN", "PWG CONSULTATION", "YOUTH MARKER", "STABILIZATION/CRESTA")
year.cols <- as.character(2014:2025)

aims.melt <- melt(aims, id.vars = c(cur.cols, disb.cols, flag.cols), measure.vars = loc.cols)

aims.melt[, (disb.cols) := (as.numeric(value)/as.numeric(`Exchange rate`)*data.table(sapply(.SD, as.numeric)))/100, .SDcols=(disb.cols)][, (cur.cols) := NULL][, `:=` (region = as.character(variable), value = NULL, variable = NULL)]
aims.melt <- melt(aims.melt, id.vars = c("region", flag.cols))
aims.melt <- aims.melt[value > 0, .(value = sum(value)), by = c("region", "variable", flag.cols)][, c("type", "year") := tstrsplit(gsub(" disbursements", "", variable), " FY ")][, variable := NULL]

aims.cast <- dcast(aims.melt[, .(value = sum(value)), by = .(region, year, HUMANITARIAN)], region + HUMANITARIAN ~ year)
fwrite(aims.cast, "output/region_oda.csv")
aims.cast.covid <- dcast(aims.melt[`COVID-19` %in% c("Targeted", "Relevant"), .(value = sum(value)), by = .(region, year)], region ~ year)
