required.packages <- c("WDI","data.table", "readxl", "tabulizer", "readr", "pdftools")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/nexus_countries/project_data/Somalia budget/")

#Jubbaland
files <- list.files("Jubbaland", full.names = T)

#for(file in files){
  tables <- extract_tables(files[1], output="data.frame")
  tables <- lapply(tables[-1], function(x) setDT(tail(x, -2))) #remove first table and nested column names
  tables <- lapply(tables, function(x) x[, which(unlist(lapply(x, function(y) !all(is.na(y))))), with=F]) #remove columns which are all NA
  tables <- lapply(tables, function(x) if(ncol(x) == 12) x[, X.1 := NULL] else x)
  for(i in 1:length(tables)){
    table <- tables[[i]]
    widths <- apply(table, 2, function(x) max(nchar(x)-nchar(gsub(" ", "", x))))
    for(j in 1:nrow(table)){
      row <- table[j]
      for(k in 4:ncol(row)){
        row[, k] <- paste0(row[,..k], paste0(rep(" ", widths[k]-(nchar(row[,..k])-nchar(gsub(" ", "", row[,..k])))), collapse = ""))
      }
      table[j] <- row
    }
    tables[[i]] <- table
  }
  tables <- lapply(tables, function(x) x[, `:=` (temp = apply(.SD, 1, function(x) trimws(paste(x, collapse = " ")))), .SDcols = 4:ncol(x)]) #paste together all financial data separated with a space
  tables <- lapply(tables, function(x) x[, 4:(ncol(x)-1) := NULL]) #remove old data cols
  #new.cols <- c("Budget share", "Budgeted", "Allocation", "Commited", "Liability", "Paid", "Available", "Balance", "Paid share")
  tables <- lapply(tables, function(x) x[, tstrsplit(temp, " ")]) #split out new temp data
  data <- rbindlist(tables)
  names(data) <- c("Fund", "Code", "Name", )
#}