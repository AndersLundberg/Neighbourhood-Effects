setwd("C:\\Users\\ander\\Dropbox\\Speciale")


install.packages("devtools")

library(devtools)
devtools::install_github("GIScience/openrouteservice-r")
install_github("August-Emil/fmkort")




library(fmkort)
library(readxl)



#my_data <- sapply(my_data, as.character)
#my_data[is.na(my_data)] <- "0"

my_data1 <- read_excel("perc_new.xlsx")

fmkommunekort(data = my_data1, id = "id", value = "mean", legend = T, legendtitle = "Income rank", scale="numeric", background = "white")


my_data <- read_excel("Dis_indkomst.xlsx")
colls<-  c(rgb(17/255,30/255,108/255),rgb(29/255,41/255,81/255),rgb(0/255,49/255,82/255),rgb(16/255,52/255,166/255),rgb(15/255,82/255,186/255),rgb(14/255,77/255,146/255),rgb(0/255,128/255,255/255),rgb(70/255,130/255,180/255),rgb(0/255,142/255,204/255))

fmkommunekort(data = my_data, id = "komnavn", value = "perc|par_perc<=25", legend = T, legendtitle = "Income rank", scale="numeric", background = "white")
fmkommunekort(data = my_data, id = "komnavn", value = "perc|par_perc>=75", legend = T, legendtitle = "Income rank", scale="numeric", background = "white")
fmkommunekort(data = my_data, id = "komnavn", value = "Parpec<20,per>80", legend = T, legendtitle = "Share", scale="numeric", background = "white")

#UDD
my_data <- read_excel("Dis_udd.xlsx")

#Dårlige kort?
fmkommunekort(data = my_data, id = "Kom_navn", value = "fagpar25", legend = T, legendtitle = "Share", scale="numeric", background = "white")
fmkommunekort(data = my_data, id = "Kom_navn", value = "fagpar75", legend = T, legendtitle = "Share", scale="numeric", background = "white")

fmkommunekort(data = my_data, id = "Kom_navn", value = "ufag", legend = T, legendtitle = "Share", scale="numeric", background = "white")

fmkommunekort(data = my_data, id = "Kom_navn", value = "vidpar25", legend = T, legendtitle = "Share", scale="numeric", background = "white")
fmkommunekort(data = my_data, id = "Kom_navn", value = "vidpar75", legend = T, legendtitle = "Share", scale="numeric", background = "white")

fmkommunekort(data = my_data, id = "Kom_navn", value = "kortlang", legend = T, legendtitle = "Share", scale="numeric", background = "white")


my_data <- my_data <- read_excel("Vidergaaende kort.xlsx")
#my_data <- sapply(my_data, as.character)
#my_data[is.na(my_data)] <- "0"

fmkommunekort(data = my_data, id = "Kom_navn", value = "mean", legend = T, legendtitle = "Share", scale="numeric", background = "white")


devtools::install_github("sebastianbarfort/mapDK")

library(mapDK)

my_data <- read_excel("Perc_sogne.xlsx")
my_data[3] <- (my_data[3]-100)*(-1)
mapDK(values= "mean", id="id",data=my_data, detail="parish", show_missing = TRUE, guide.label = "Mean rank")


my_data <- read_excel("videregaaende_sogn.xlsx")
my_data[3] <- (my_data[3]-1)*(-1)*100
mapDK(values= "mean", id="id",data=my_data, detail="parish", show_missing = TRUE, guide.label = "Percent")
