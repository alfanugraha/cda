library(httr)
library(jsonlite)
library(readr)

kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

form_org <- 327585 #Organisasi

## Organisasi ##
url_org <- paste0(kc_server_url,"api/v1/data/",form_org,".csv")
rawdata_org <- GET(url_org,authenticate("cdna2019","Icraf2019!"),progress())
dataOrganisasi <- read_csv(content(rawdata_org,"raw",encoding = "UTF-8"))

saveRDS(dataOrganisasi, "data/dataOrganisasi")

summInputOrg<-readRDS("data/dataOrganisasi")

summInputOrg$`profil/jabatan`<-NULL; summInputOrg$`meta/instanceID`<-NULL; summInputOrg$`__version__`<-NULL
summInputOrg$`_uuid`<-NULL; summInputOrg$`_submission_time`<-NULL; summInputOrg$`_tags`<-NULL; summInputOrg$`_notes`<-NULL

summInputOrg$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan`<-NULL
summInputOrg$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan_001`<-NULL

for (i in 2:4){
  eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat2/alasan_00",i,"`","<-NULL")))
}

for (i in 5:6){
  eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat3/alasan_00",i,"`","<-NULL")))
}
for (i in 7:9){
  eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat4/alasan_00",i,"`","<-NULL")))
}
summInputOrg$`perangkat1/perangkat4/alasan_010`<-NULL

for (i in 11:13){
  eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat5/alasan_0",i,"`","<-NULL")))
}

for (i in 14:15){
  eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat6/alasan_0",i,"`","<-NULL")))
}

for (i in 16:22){
  eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat7/alasan_0",i,"`","<-NULL")))
}

for (i in 23:29){
  eval(parse(text=paste0("summInputOrg$`sdm1/sdm2/alasan_0",i,"`","<-NULL")))
}

summInputOrg$`sdm1/sdm3/alasan_030`<-NULL
summInputOrg$`sdm1/sdm4/alasan_031`<-NULL

for (i in 32:33){
  eval(parse(text=paste0("summInputOrg$`sdm1/sdm5/alasan_0",i,"`","<-NULL")))
}

for (i in 34:35){
  eval(parse(text=paste0("summInputOrg$`sdm1/sdm6/alasan_0",i,"`","<-NULL")))
}

for (i in 36:39){
  eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi2/alasan_0",i,"`","<-NULL")))
}

for (i in 40:42){
  eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi3/alasan_0",i,"`","<-NULL")))
}

for (i in 43:44){
  eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi4/alasan_0",i,"`","<-NULL")))
}

summInputOrg$`teknologi1/teknologi3/q8.2.3` <- NULL

summInputOrg$`perangkat1/perangkat4/q4.4.3`[summInputOrg$`perangkat1/perangkat4/q4.4.3` == "n/a"]  <- 3
summInputOrg[summInputOrg == "n/a"]<-NA
summInputOrg<-na.omit(summInputOrg)
summInputOrg$year <- format(as.Date(summInputOrg$`profil/tanggal`), format = "%Y")
# summInputOrg<-filter(summInputOrg,summInputOrg$year==input$selectedYear)
summInputOrg<-filter(summInputOrg,summInputOrg$year==2019)
year <- 2019
# year <- summInputOrg$year
summInputOrg<-as.data.frame(summInputOrg)

summOrg<-as.data.frame(lapply(summInputOrg[,5:length(summInputOrg)], as.numeric))

q4.1<-rowSums(summOrg[,1:2]); q4.1<-as.data.frame(q4.1)/2
q4.2<-rowSums(summOrg[,3:5]); q4.2<-as.data.frame(q4.2)/3
q4.3<-rowSums(summOrg[,6:7]); q4.3<-as.data.frame(q4.3)/2
q4.4<-rowSums(summOrg[,8:11]); q4.4<-as.data.frame(q4.4)/4
q4.5<-rowSums(summOrg[,12:14]); q4.5<-as.data.frame(q4.5)/3
q4.6<-rowSums(summOrg[,15:16]); q4.6<-as.data.frame(q4.6)/2
q4.7<-rowSums(summOrg[,17:23]); q4.7<-as.data.frame(q4.7)/7
q5.1<-rowSums(summOrg[,24:30]); q5.1<-as.data.frame(q5.1)/7
q5.2<-summOrg$sdm1.sdm3.q5.2; q5.3<-summOrg$sdm1.sdm4.q5.3
q5.4<-rowSums(summOrg[,33:34]); q5.4<-as.data.frame(q5.4)/2
q5.5<-rowSums(summOrg[,35:36]); q5.5<-as.data.frame(q5.5)/2
q8.1<-rowSums(summOrg[,37:40]); q8.1<-as.data.frame(q8.1)/4
q8.2<-rowSums(summOrg[,41:42]); q8.2<-as.data.frame(q8.2)/2
q8.3<-rowSums(summOrg[,43:44]); q8.3<-as.data.frame(q8.3)/2
valOrganisasi <-cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
colnames(valOrganisasi) <-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
summTempOrganisasi <-as.data.frame(cbind(valOrganisasi,year))

indikatorOrg <-read.table("init/organisasi.csv", header=TRUE, sep=";")
summIndikatorOrg <-as.data.frame(indikatorOrg$Perbaikan)
colnames(summIndikatorOrg) <-"Indikator"

## Menampilkan hasil satu provinsi untuk tingkat organisasi ##
summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==categoryProvince$provinsi)
# summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Aceh")

## Membuat tabel Level setiap aspek ##
Level4 <-rowSums(summTempOrganisasi[,4:10])/length(summTempOrganisasi[,4:10])
LevelOrg <-mean(Level4)
Level5 <-rowSums(summTempOrganisasi[,11:15])/length(summTempOrganisasi[,11:15])
LevelSDM <-mean(Level5)
Level8 <-rowSums(summTempOrganisasi[,16:18])/length(summTempOrganisasi[,16:18])
LevelTek <-mean(Level8)
LevelOrg_gabungan <-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
LevelOrg_gabungan <- round(LevelOrg_gabungan, digits = 2)
gapOrg_gabungan <-5-LevelOrg_gabungan
Aspek_Penilaian <-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
summOrganisasi <-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
colnames(summOrganisasi) <- c("Aspek Penilaian", "Level", "GAP")
summOrganisasi<-datatable(summOrganisasi,escape = FALSE, rownames = FALSE)

## Menampilkan level per indikator ##
Ind4.1 <-mean(summTempOrganisasi$q4.1); Ind4.2<-mean(summTempOrganisasi$q4.2); Ind4.3<-mean(summTempOrganisasi$q4.3); Ind4.4<-mean(summTempOrganisasi$q4.4); Ind4.5<-mean(summTempOrganisasi$q4.5); Ind4.6<-mean(summTempOrganisasi$q4.6); Ind4.7<-mean(summTempOrganisasi$q4.7)
Ind5.1 <-mean(summTempOrganisasi$q5.1); Ind5.2<-mean(summTempOrganisasi$q5.2); Ind5.3<-mean(summTempOrganisasi$q5.3); Ind5.4<-mean(summTempOrganisasi$q5.4); Ind5.5<-mean(summTempOrganisasi$q5.5)
Ind8.1 <-mean(summTempOrganisasi$q8.1);Ind8.2<-mean(summTempOrganisasi$q8.2);Ind8.3<-mean(summTempOrganisasi$q8.3)
levelProvOrg <-as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
levelProvOrg <-round(levelProvOrg,digits = 2)
gapProvOrg <-5-levelProvOrg
provOrg <-as.data.frame(cbind(summIndikatorOrg,levelProvOrg,gapProvOrg))
colnames(provOrg) <-c("Indikator", "Level", "GAP")
# tablesCDA$summaryProvOrg <-provOrg

summOrganisasi
#datatable(summOrganisasi,escape = FALSE, rownames = FALSE)

plot_ly(provOrg, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
  add_trace(x=~GAP, name= 'GAP') %>%
  layout(yaxis=list(title='Indikator'), barmode='stack') %>%
  layout(legend = list(orientation = 'h')) %>%
  layout(yaxis = list(tickfont = list(size = 8), tickangle = 45, title = ""),
         xaxis = list(title = ""))

