required.packages <- c("WDI","data.table", "readxl", "zoo")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/nexus_countries/project_data/Somalia budget/")

regions <- c("federal", "galmudug", "jubbaland", "puntland", "somaliland", "southwest")

#Federal
federal <- fread("Federal/Federal government All Expenditure (2013-2020).txt")
federal$Type <- "Revenue (domestic)"

federal.totals <- setnames(dcast(federal, Type ~ Year, value.var = "Value", fun.aggregate = function(x) sum(x, na.rm = T)), c("Type", "Total 2013", "Total 2014", "Total 2015", "Total 2016", "Total 2017", "Total 2018", "Total 2019", "Total 2020"))[]

#Galmudug
galmudug <- as.data.table(read_excel("Galmudug/Budget 2016-2019.xlsx"))

galmudug$...1 <- NULL

galmudug[, Type := ifelse(grepl("grant", tolower(`(US$)`)), "Revenue (external)", "Revenue (domestic)")]
galmudug <- galmudug[!grepl("total", tolower(`(US$)`))] #remove total

galmudug.totals <- setnames(dcast(melt(galmudug, id.vars = c("(US$)", "Type")), Type ~ variable, value.var = "value", fun.aggregate = function(x) sum(x, na.rm = T)), c("Type", "Total 2016", "Total 2017", "Total 2018", "Total 2019"))[]

#Jubbaland
files <- list.files("Jubbaland", full.names = T, pattern = ".xlsx")
jubbalandlist <- list()
for(i in 1:length(files)){
  j <- 1
  datalist <- list()
  go <- T
  while(go) {
    print(paste0(files[i], ", Sheet ", j))
    tryCatch({
      datalist[[j]] <- read_excel(files[i], sheet = j)
    }, error = function(e) assign("go", F, envir = .GlobalEnv)
    )
    j <- j + 1
  }
  data <- tail(rbindlist(datalist), -1)
  rm(datalist)
  jubbalandlist[[i]] <- cbind(data, file = files[i])
  rm(data)
}
jubbaland <- rbindlist(jubbalandlist, fill = T)
rm(jubbalandlist)

jubbaland <- jubbaland[!is.na(Name)]
jubbaland[, Type := na.locf(ifelse(Name == "Revenue", "Revenue (domestic)", ifelse(Name == "Expense", "Expense", NA)))]
jubbaland[, Type := ifelse(Account %in% c("132101", "133101"), "Revenue (external)", Type)]
jubbaland[, c(1:3)] <- sapply(seq(1,3), function(x) na.locf(jubbaland[,x, with =F], na.rm = F))

jubbaland.totals <- setnames(dcast(jubbaland[!is.na(Account)], Type ~ file, value.var = "Budgeted", fun.aggregate = function(x) sum(x, na.rm = T)), c("Type", "Total 2016", "Total 2017", "Total 2018"))[]

#South West
files <- list.files("South West", full.names = T, pattern = ".xlsx")
southwestlist <- list()
for(i in 1:length(files)){
  j <- 1
  datalist <- list()
  go <- T
  while(go) {
    print(paste0(files[i], ", Sheet ", j))
    tryCatch({
      datalist[[j]] <- read_excel(files[i], sheet = j)
    }, error = function(e) assign("go", F, envir = .GlobalEnv)
    )
    j <- j + 1
  }
  data <- tail(rbindlist(datalist), -1)
  rm(datalist)
  southwestlist[[i]] <- cbind(data, file = files[i])
  rm(data)
}

setnames(southwestlist[[4]], "Budget Year", "Budgeted")
southwest <- rbindlist(southwestlist, fill = T)
rm(southwestlist)

southwest <- southwest[!is.na(Name)]
southwest[, Type := na.locf(ifelse(Name == "Revenue", "Revenue (domestic)", ifelse(Name == "Expense", "Expense", NA)))]
southwest[, Type := ifelse(Account %in% c("132101", "133101"), "Revenue (external)", Type)]
southwest[, c(1:3)] <- sapply(seq(1,3), function(x) na.locf(southwest[,x, with =F], na.rm = F))

southwest.totals <- setnames(dcast(southwest[!is.na(Account)], Type ~ file, value.var = "Budgeted", fun.aggregate = function(x) sum(x, na.rm = T)), c("Type", "Total 2016", "Total 2017", "Total 2018"))[]

#Puntland
puntland <- as.data.table(read_excel("Puntland/Puntland summary budget 2016-2018.xlsx"))
puntland[, Type := ifelse(grepl("project", tolower(Description)), "Revenue (external)", "Revenue (domestic)")]
puntland <- head(puntland[!grepl("total", tolower(Description))],-1) #remove total

puntland.totals <- setnames(dcast(melt(puntland, id.vars = c("Description", "Type")), Type ~ variable, value.var = "value", fun.aggregate = function(x) sum(x, na.rm = T)), c("Type", "Total 2015", "Total 2016", "Total 2017", "Total 2018"))[]

#Somaliland
somaliland <- as.data.table(read_excel("Somaliland/somaliland summary budget 2017-2018.xlsx"))
somaliland[, `:=` (`2017` = as.numeric(`2017`)/9328, `2018` = as.numeric(`2018`)/10189)] #convert to USD

somaliland$P.NO <- NULL
somaliland$Madax <- NULL
somaliland$`Magaca Wasaaradda/ Hay'ada` <- NULL

somaliland[, Type := ifelse(grepl("grant", tolower(`English: Name of Ministry / Agency`)), "Revenue (external)", "Revenue (domestic)")]
somaliland <- somaliland[!grepl("total", tolower(`English: Name of Ministry / Agency`))] #remove total

somaliland.totals <- setnames(dcast(melt(somaliland, id.vars = c("English: Name of Ministry / Agency", "Type")), Type ~ variable, value.var = "value", fun.aggregate = function(x) sum(x, na.rm = T)), c("Type", "Total 2017", "Total 2018"))[]

#Join all
all.data <- list()
for(i in 1:length(regions)){
  data <- get(paste0(regions[i], ".totals"))
  data$region <- regions[i]
  all.data[[i]] <- data
}

all.totals <- rbindlist(all.data, fill = T)
all.totals <- all.totals[Type == "Revenue (domestic)"]

  fwrite(all.totals, "Somalia domestic budgets.csv")
  