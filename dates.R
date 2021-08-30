rm(list=ls())
setwd("C:/Users/gaoan/Desktop/step8")
info = read.csv("games_basic_info.csv")

str(info)

info$date_string = paste(as.character(info$release_year), as.character(info$release_month), as.character(info$release_day), sep="-")

info$release_date = as.Date(info$date_string)

info$release_date[1]-365
