##### DOWNLOAD DATA FROM OSF ######
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#Download all data from OSF
get_file(node = "gw39z",
         file = "Data_plant_pollinator_Finse_2016_2017.zip")


zipFile <- "Data_plant_pollinator_Finse_2016_2017.zip"
unzip(zipFile)
