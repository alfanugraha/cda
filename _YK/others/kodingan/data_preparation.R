fileSys <- read.table("data/sistem.csv", header = TRUE, sep=",")
fileOrg <- read.table("data/organisasi.csv", header = TRUE, sep=",")
fileInd <- read.table("data/individu.csv", header = TRUE, sep=",")

saveRDS(fileSys, "fileSys")
saveRDS(fileOrg, "fileOrg")
saveRDS(fileInd, "fileInd")


#Mengunduh data secara online dari KoBo
dataSistem<-kobo_data_downloader("327419", "cdna2019:Icraf2019!")
dataOrganisasi<-kobo_data_downloader("327585", "cdna2019:Icraf2019!")
dataIndividu<-kobo_data_downloader("327418", "cdna2019:Icraf2019!")

saveRDS(dataSistem, "data/dataSistem")
saveRDS(dataOrganisasi, "data/dataOrganisasi")
saveRDS(dataIndividu, "data/dataIndividu")