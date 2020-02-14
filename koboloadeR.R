library(koboloadeR)
library (dplyr)

#Shiny app untuk menampilkan dan memgunduh data dari KoBo
kobo_apps("data_viewer")

#Mengunduh data secara langsung
Sistem<-kobo_data_downloader("327419", "cdna2019:Icraf2019!")
Organisasi<-kobo_data_downloader("327585", "cdna2019:Icraf2019!")
Individu<-kobo_data_downloader("327418", "cdna2019:Icraf2019!")

saveRDS(Sistem, "fileSistem")
saveRDS(Organisasi, "fileOrganisasi")
saveRDS(Individu, "fileIndividu")

### TINGKAT INDIVIDU ####
inputIndividu<-readRDS("data/fileIndividu")
inputIndividu$`profil/gender`<-NULL; inputIndividu$`profil/jabatan`<-NULL; inputIndividu$`profil/akun`<-NULL; inputIndividu$`profil/noHP`<-NULL; inputIndividu$`profil/email`<-NULL
inputIndividu$`meta/instanceID`<-NULL; inputIndividu$`__version__`<-NULL; inputIndividu$`_uuid`<-NULL; inputIndividu$`_submission_time`<-NULL; inputIndividu$`_tags`<-NULL; inputIndividu$`_notes`<-NULL

inputIndividu$`sdm_i1/sdm_i2/alasan`<-NULL
inputIndividu$`sdm_i1/sdm_i2/alasan_001`<-NULL

for (i in 2:9){
  eval(parse(text=paste0("inputIndividu$`sdm_i1/sdm_i3/alasan_00",i,"`","<-NULL")))
}
inputIndividu$`sdm_i1/sdm_i3/alasan_010`<-NULL

for (i in 11:19){
  eval(parse(text=paste0("inputIndividu$`sdm_i1/sdm_i4/alasan_0",i,"`","<-NULL")))
}

for (i in 20:22){
  eval(parse(text=paste0("inputIndividu$`sdm_i1/sdm_i5/alasan_0",i,"`","<-NULL")))
}

#Menghilangkan n/a pada data frame#
inputIndividu[inputIndividu == "n/a"]  <- NA
inputIndividu <- na.omit(inputIndividu)

individu<- as.data.frame(lapply(inputIndividu[,5:length(inputIndividu)], as.numeric))

q6.1<-rowSums(individu[,1:2]); q6.1<-as.data.frame(q6.1)/2
q6.2<-rowSums(individu[,3:11]); q6.2<-as.data.frame(q6.2)/9
q6.3<-rowSums(individu[,12:20]); q6.3<-as.data.frame(q6.3)/9
q6.4<-rowSums(individu[,21:23]); q6.4<-as.data.frame(q6.4)/3
valInd<-cbind(inputIndividu$`profil/provinsi`,inputIndividu$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
tempIndividu<-as.data.frame(valInd)

#Mendefinisikan nama setiap indikator
indikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
indikatorInd  <- as.data.frame(indikatorInd)

##Menampilkan hasil satu induvidu per provinsi##
#tempIndividu<-filter(valInd,valInd$Provinsi==input$categoryProvince & valInd$Nama==input$selectizeName)
tempIndividu<-filter(valInd,valInd$Provinsi=="Aceh" & valInd$Nama=="Yumna")

##Membuat tabel Level setiap aspek##
aspekInd<-"6. Sumber Daya Manusia - Individu"
Level6<-rowMeans(tempIndividu[3:length(tempIndividu)])
Level6<-round(Level6, digits = 2)
gap6<-5-Level6
gap6<-round(gap6, digits = 2)
tingkatInd<-as.data.frame(cbind(aspekInd, Level6, gap6))
colnames(tingkatInd)<-c("Aspek Penilaian","Level","GAP")

##Membuat bar chart untuk tingkat Individu####
Ind6.1<-mean(tempIndividu$q6.1); Ind6.2<-mean(tempIndividu$q6.2); Ind6.3<-mean(tempIndividu$q6.3); Ind6.4<-mean(tempIndividu$q6.4)
tempLevelInd <- as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
tempLevelInd<-round(tempLevelInd, digits = 2)
tempGapInd<-5-tempLevelInd
tempGapInd<-round(tempGapInd,digits = 2)
graphInd<-cbind(indikatorInd,tempLevelInd,tempGapInd)
colnames(graphInd)<-c("Indikator","Level","GAP")
  
plot_ly(graphInd, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
    add_trace(x=~GAP, name= 'GAP') %>%
    layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Individu")


### TINGKAT ORGANISASI ####
inputOrganisasi<-readRDS("data/fileOrganisasi")
inputOrganisasi$`profil/jabatan`<-NULL; inputOrganisasi$`meta/instanceID`<-NULL; inputOrganisasi$`__version__`<-NULL
inputOrganisasi$`_uuid`<-NULL; inputOrganisasi$`_submission_time`<-NULL; inputOrganisasi$`_tags`<-NULL; inputOrganisasi$`_notes`<-NULL

inputOrganisasi$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan`<-NULL
inputOrganisasi$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan_001`<-NULL

for (i in 2:4){
  eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat2/alasan_00",i,"`","<-NULL")))
}

for (i in 5:6){
  eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat3/alasan_00",i,"`","<-NULL")))
}
for (i in 7:9){
  eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat4/alasan_00",i,"`","<-NULL")))
}
inputOrganisasi$`perangkat1/perangkat4/alasan_010`<-NULL

for (i in 11:13){
  eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat5/alasan_0",i,"`","<-NULL")))
}

for (i in 14:15){
  eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat6/alasan_0",i,"`","<-NULL")))
}

for (i in 16:22){
  eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat7/alasan_0",i,"`","<-NULL")))
}

for (i in 23:29){
  eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm2/alasan_0",i,"`","<-NULL")))
}

inputOrganisasi$`sdm1/sdm3/alasan_030`<-NULL
inputOrganisasi$`sdm1/sdm4/alasan_031`<-NULL

for (i in 32:33){
  eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm5/alasan_0",i,"`","<-NULL")))
}

for (i in 34:35){
  eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm6/alasan_0",i,"`","<-NULL")))
}

for (i in 36:39){
  eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi2/alasan_0",i,"`","<-NULL")))
}

for (i in 40:42){
  eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi3/alasan_0",i,"`","<-NULL")))
}

for (i in 43:44){
  eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi4/alasan_0",i,"`","<-NULL")))
}


### TINGKAT SISTEM ###
inputSistem<-readRDS("data/fileSistem")

inputSistem$`meta/instanceID`<-NULL; inputSistem$`__version__`<-NULL; inputSistem$`_uuid`<-NULL; inputSistem$`_submission_time`<-NULL; inputSistem$`_tags`<-NULL; inputSistem$`_notes`<-NULL
inputSistem$`regulasi/regulasi1/alasan`<-NULL
inputSistem$`regulasi/regulasi2/alasan_001`<-NULL
inputSistem$`integrasi1/integrasi2/alasan_002`<-NULL
inputSistem$`integrasi1/integrasi3/alasan_003`<-NULL
inputSistem$`integrasi1/integrasi4/alasan_004`<-NULL
inputSistem$`integrasi1/integrasi5/alasan_005`<-NULL
inputSistem$`integrasi1/integrasi6/alasan_006`<-NULL
inputSistem$`integrasi1/integrasi6/alasan_007`<-NULL
inputSistem$`proses1/proses2/alasan_008`<-NULL
inputSistem$`proses1/proses2_001/alasan_009`<-NULL
inputSistem$`proses1/proses3/alasan_010`<-NULL
inputSistem$`proses1/proses4/alasan_011`<-NULL
inputSistem$`proses1/proses4_001/alasan_012`<-NULL

for (i in 13:31){
  eval(parse(text=paste0("inputSistem$`datainfo1/datainfo2/alasan_0",i,"`","<-NULL")))
}

for (i in 32:50){
  eval(parse(text=paste0("inputSistem$`datainfo1/datainfo3/alasan_0",i,"`","<-NULL")))
}

for (i in 51:52){
  eval(parse(text=paste0("inputSistem$`datainfo1/datainfo4/alasan_0",i,"`","<-NULL")))
}

for (i in 53:57){
  eval(parse(text=paste0("inputSistem$`pemantauan1/pemantauan2/alasan_0",i,"`","<-NULL")))
}

for (i in 58:63){
  eval(parse(text=paste0("inputSistem$`pemantauan1/pemantauan3/alasan_0",i,"`","<-NULL")))
}

for (i in 64:66){
  eval(parse(text=paste0("inputSistem$`pemantauan1/pemantauan4/alasan_0",i,"`","<-NULL")))
}

for (i in 67:68){
  eval(parse(text=paste0("inputSistem$`pemantauan1/pemantauan5/alasan_0",i,"`","<-NULL")))
}
