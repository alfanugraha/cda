library(koboloadeR)
library(dplyr)
library(stringr)

###Year Filter####
dataIndividu<-kobo_data_downloader("327418", "cdna2019:Icraf2019!")
saveRDS(dataIndividu, "data/dataIndividu")
inputIndividu<-readRDS("data/dataIndividu")

df <- data.frame(inputIndividu)
df$year <- format(as.Date(df$profil.tanggal), format = "%Y")

selectedYear <- "2020"
data<-filter(df,df$year==selectedYear)

###Show Priority####
dataSistem<-kobo_data_downloader("327419", "cdna2019:Icraf2019!")
dataOrganisasi<-kobo_data_downloader("327585", "cdna2019:Icraf2019!")
dataIndividu<-kobo_data_downloader("327418", "cdna2019:Icraf2019!")

saveRDS(dataSistem, "data/dataSistem")
saveRDS(dataOrganisasi, "data/dataOrganisasi")
saveRDS(dataIndividu, "data/dataIndividu")

prov <- "Sulawesi Selatan"

#### Tabel Prioritas Tingkat Sistem ####
summInputSys<-readRDS("data/dataSistem")

summInputSys$`meta/instanceID`<-NULL; summInputSys$`__version__`<-NULL; summInputSys$`_uuid`<-NULL; summInputSys$`_submission_time`<-NULL; summInputSys$`_tags`<-NULL; summInputSys$`_notes`<-NULL
summInputSys$`regulasi/regulasi1/alasan`<-NULL
summInputSys$`regulasi/regulasi2/alasan_001`<-NULL
summInputSys$`integrasi1/integrasi2/alasan_002`<-NULL
summInputSys$`integrasi1/integrasi3/alasan_003`<-NULL
summInputSys$`integrasi1/integrasi4/alasan_004`<-NULL
summInputSys$`integrasi1/integrasi5/alasan_005`<-NULL
summInputSys$`integrasi1/integrasi6/alasan_006`<-NULL
summInputSys$`integrasi1/integrasi6/alasan_007`<-NULL
summInputSys$`proses1/proses2/alasan_008`<-NULL
summInputSys$`proses1/proses2_001/alasan_009`<-NULL
summInputSys$`proses1/proses3/alasan_010`<-NULL
summInputSys$`proses1/proses4/alasan_011`<-NULL
summInputSys$`proses1/proses4_001/alasan_012`<-NULL

for (i in 13:31){
  eval(parse(text=paste0("summInputSys$`datainfo1/datainfo2/alasan_0",i,"`","<-NULL")))
}

for (i in 32:50){
  eval(parse(text=paste0("summInputSys$`datainfo1/datainfo3/alasan_0",i,"`","<-NULL")))
}

for (i in 51:52){
  eval(parse(text=paste0("summInputSys$`datainfo1/datainfo4/alasan_0",i,"`","<-NULL")))
}

for (i in 53:57){
  eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan2/alasan_0",i,"`","<-NULL")))
}

for (i in 58:63){
  eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan3/alasan_0",i,"`","<-NULL")))
}

for (i in 64:66){
  eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan4/alasan_0",i,"`","<-NULL")))
}

for (i in 67:68){
  eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan5/alasan_0",i,"`","<-NULL")))
}


summInputSys<-as.data.frame(summInputSys)
summInputSys$`pemantauan1/pemantauan3/q9.2.6`[summInputSys$`pemantauan1/pemantauan3/q9.2.6` == "n/a"]  <- NA
summInputSys$`pemantauan1/pemantauan5/q9.4.1`[summInputSys$`pemantauan1/pemantauan5/q9.4.1` == "n/a"]  <- NA
summInputSys$`pemantauan1/pemantauan5/q9.4.2`[summInputSys$`pemantauan1/pemantauan5/q9.4.2` == "n/a"]  <- NA
summInputSys[is.na(summInputSys)]<-3
summSys<- as.data.frame(lapply(summInputSys[,3:length(summInputSys)], as.numeric))

q2.5<-rowSums(summSys[,9:10]); q2.5<- as.data.frame(q2.5)/2
q7.1 <- rowSums(summSys[,14:32]); q7.1<- as.data.frame(q7.1)/19
q7.2 <- rowSums(summSys[,33:51]); q7.2<- as.data.frame(q7.2)/19
q7.3<-rowSums(summSys[,52:53]); q7.3<-as.data.frame(q7.3)/2
q9.1<-rowSums(summSys[,54:58]); q9.1<-as.data.frame(q9.1)/5
q9.2<-rowSums(summSys[,59:64]); q9.2<-as.data.frame(q9.2)/6
q9.3<-rowSums(summSys[,65:67]); q9.3<-as.data.frame(q9.3)/3
q9.4<-rowSums(summSys[,68:69]); q9.4<-as.data.frame(q9.4)/2

summLevelSistem<-cbind(summInputSys$`provinsi/provinsi_001`,summSys$regulasi.regulasi1.q1.1,summSys$regulasi.regulasi2.q1.2,summSys$integrasi1.integrasi2.q2.1,summSys$integrasi1.integrasi3.q2.2,summSys$integrasi1.integrasi4.q2.3, summSys$integrasi1.integrasi5.q2.4, q2.5, summSys$proses1.proses2.q3.1, summSys$proses1.proses2_001.q3.2, summSys$proses1.proses3.q3.3, summSys$proses1.proses4.q3.4, summSys$proses1.proses4_001.q3.5, q7.1, q7.2, q7.3, q9.1, q9.2, q9.3, q9.4)
colnames(summLevelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")
summTempSistem<-as.data.frame((summLevelSistem))

indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
summIndikatorSys <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))

## Menampilkan hasil satu provinsi untuk tingkat sistem ##
# summTempSistem<-filter(summTempSistem,summInputSys$`provinsi/provinsi_001`==input$categoryProvince)
summTempSistem<-filter(summTempSistem,Provinsi==prov)

## Membuat tabel Level setiap aspek ##   
aspekSys<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
LevelReg<-mean(as.matrix(summTempSistem[,2:3])); LevelInt<-mean(as.matrix(summTempSistem[4:8])); LevelProses<-mean(as.matrix(summTempSistem[9:13])); LevelData<-mean(as.matrix(summTempSistem[14:16])); LevelPEP<-mean(as.matrix(summTempSistem[17:20]))
summ_allLevelSys<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
#gapReg<-mean(as.matrix(tempSistem[21:22])); gapInt<-mean(as.matrix(tempSistem[23:27])); gapProses<-mean(as.matrix(tempSistem[28:32])); gapData<-mean(as.matrix(tempSistem[33:35])); gapPEP<-mean(as.matrix(tempSistem[36:39]))
gapReg<-5-LevelReg; gapInt<-5-LevelInt; gapProses<-5-LevelProses; gapData<-5-LevelData; gapPEP<-5-LevelPEP
summ_allGapSys<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
summSistem<-as.data.frame(cbind(aspekSys, summ_allLevelSys, summ_allGapSys))
colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")

## Menampilkan level per indikator & prioritas ##
t_summTempSistem<-t(summTempSistem[2:length(summTempSistem)])
provSys<-rowMeans(t_summTempSistem)
provSys<-round(provSys, digits = 2)

tabelSys<-cbind(summIndikatorSys,provSys)
tabelSys$prioritasSys <- "Tidak prioritas" 
tabelSys<-within(tabelSys, {prioritasSys<-ifelse(provSys<=3, "Prioritas rendah", prioritasSys)})
tabelSys<-within(tabelSys, {prioritasSys<-ifelse(provSys<=2, "Prioritas tinggi", prioritasSys)})
tabelSys<-within(tabelSys, {prioritasSys<-ifelse(provSys<=1, "Prioritas sangat tinggi", prioritasSys)})

colnames(tabelSys)<-c("Indikator","Level","Prioritas")

#### Tabel Prioritas Tingkat Organisasi ####
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

summInputOrg$`perangkat1/perangkat4/q4.4.3`[summInputOrg$`perangkat1/perangkat4/q4.4.3` == "n/a"]  <- 3
summInputOrg[summInputOrg == "n/a"]  <- NA
summInputOrg <- na.omit(summInputOrg)
summInputOrg<-as.data.frame(summInputOrg)

summOrg<- as.data.frame(lapply(summInputOrg[,5:length(summInputOrg)], as.numeric))

q4.1<-rowSums(summOrg[,1:2]); q4.1<- as.data.frame(q4.1)/2
q4.2<-rowSums(summOrg[,3:5]); q4.2<- as.data.frame(q4.2)/3
q4.3<-rowSums(summOrg[,6:7]); q4.3<- as.data.frame(q4.3)/2
q4.4<-rowSums(summOrg[,8:11]); q4.4<- as.data.frame(q4.4)/4
q4.5<-rowSums(summOrg[,12:14]); q4.5<- as.data.frame(q4.5)/3
q4.6<-rowSums(summOrg[,15:16]); q4.6<- as.data.frame(q4.6)/2
q4.7<-rowSums(summOrg[,17:23]); q4.7<- as.data.frame(q4.7)/7
q5.1<-rowSums(summOrg[,24:30]); q5.1<- as.data.frame(q5.1)/7
q5.2<-summOrg$sdm1.sdm3.q5.2; q5.3<-summOrg$sdm1.sdm4.q5.3
q5.4<-rowSums(summOrg[,33:34]); q5.4<- as.data.frame(q5.4)/2
q5.5<-rowSums(summOrg[,35:36]); q5.5<- as.data.frame(q5.5)/2
q8.1<-rowSums(summOrg[,37:40]); q8.1<- as.data.frame(q8.1)/4
q8.2<-rowSums(summOrg[,41:43]); q8.2<- as.data.frame(q8.2)/3
q8.3<-rowSums(summOrg[,44:45]); q8.3<- as.data.frame(q8.3)/2
valOrganisasi <- cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
colnames(valOrganisasi)<-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
summTempOrganisasi<-as.data.frame(valOrganisasi)

indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
summIndikatorOrg <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
colnames(summIndikatorOrg)<-"Indikator"

##Menampilkan hasil satu provinsi untuk tingkat organisasi##
# summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==input$categoryProvince)
summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==prov)

##Membuat tabel Level setiap aspek##   
Level4<-rowSums(summTempOrganisasi[,4:10])/length(summTempOrganisasi[,4:10])
LevelOrg<-mean(Level4)
Level5 <- rowSums(summTempOrganisasi[,11:15])/length(summTempOrganisasi[,11:15])
LevelSDM<-mean(Level5)
Level8 <- rowSums(summTempOrganisasi[,16:18])/length(summTempOrganisasi[,16:18])
LevelTek<-mean(Level8)
LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
gapOrg_gabungan<-5-LevelOrg_gabungan
Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
summOrganisasi<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
colnames(summOrganisasi)<- c("Aspek Penilaian", "Level", "GAP")

## Menampilkan level per indikator & prioritas ##
Ind4.1<-mean(summTempOrganisasi$q4.1); Ind4.2<-mean(summTempOrganisasi$q4.2); Ind4.3<-mean(summTempOrganisasi$q4.3); Ind4.4<-mean(summTempOrganisasi$q4.4); Ind4.5<-mean(summTempOrganisasi$q4.5); Ind4.6<-mean(summTempOrganisasi$q4.6); Ind4.7<-mean(summTempOrganisasi$q4.7)
Ind5.1<-mean(summTempOrganisasi$q5.1); Ind5.2<-mean(summTempOrganisasi$q5.2); Ind5.3<-mean(summTempOrganisasi$q5.3); Ind5.4<-mean(summTempOrganisasi$q5.4); Ind5.5<-mean(summTempOrganisasi$q5.5)
Ind8.1<-mean(summTempOrganisasi$q8.1);Ind8.2<-mean(summTempOrganisasi$q8.2);Ind8.3<-mean(summTempOrganisasi$q8.3)
provOrg<-as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
provOrg<-round(provOrg,digits = 2)

tabelOrg<-cbind(summIndikatorOrg,provOrg)
tabelOrg$prioritasOrg<-"Tidak prioritas" 
tabelOrg<-within(tabelOrg, {prioritasOrg<-ifelse(provOrg<=3, "Prioritas rendah", prioritasOrg)})
tabelOrg<-within(tabelOrg, {prioritasOrg<-ifelse(provOrg<=2, "Prioritas tinggi", prioritasOrg)})
tabelOrg<-within(tabelOrg, {prioritasOrg<-ifelse(provOrg<=1, "Prioritas sangat tinggi", prioritasOrg)})

colnames(tabelOrg)<-c("Indikator","Level","Prioritas")

### Tabel Prioritas Tingkat Individu ####
summInputInd<-readRDS("data/dataIndividu")
summInputInd$`profil/gender`<-NULL; summInputInd$`profil/jabatan`<-NULL; summInputInd$`profil/akun`<-NULL; summInputInd$`profil/noHP`<-NULL; summInputInd$`profil/email`<-NULL
summInputInd$`meta/instanceID`<-NULL; summInputInd$`__version__`<-NULL; summInputInd$`_uuid`<-NULL; summInputInd$`_submission_time`<-NULL; summInputInd$`_tags`<-NULL; summInputInd$`_notes`<-NULL

summInputInd$`sdm_i1/sdm_i2/alasan`<-NULL
summInputInd$`sdm_i1/sdm_i2/alasan_001`<-NULL

for (i in 2:9){
  eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i3/alasan_00",i,"`","<-NULL")))
}
summInputInd$`sdm_i1/sdm_i3/alasan_010`<-NULL

for (i in 11:19){
  eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i4/alasan_0",i,"`","<-NULL")))
}

for (i in 20:22){
  eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i5/alasan_0",i,"`","<-NULL")))
}

## Menghilangkan n/a pada data frame ##
summInputInd[summInputInd == "n/a"]  <- NA
summInputInd <- na.omit(summInputInd)

summInd<- as.data.frame(lapply(summInputInd[,5:length(summInputInd)], as.numeric))

q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
q6.3<-rowSums(summInd[,12:20]); q6.3<-as.data.frame(q6.3)/9
q6.4<-rowSums(summInd[,21:23]); q6.4<-as.data.frame(q6.4)/3
valInd<-cbind(summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
summTempIndividu<-as.data.frame(valInd)

summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
summIndikatorInd  <- as.data.frame(summIndikatorInd)

# summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==input$categoryProvince)
summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==prov)

## Membuat tabel Level setiap aspek ##
Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
Level6<-mean(as.matrix(summTempIndividu[3:length(summTempIndividu)]))
gap6<-5-Level6
summIndividu<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
colnames(summIndividu)<-c("Aspek Penilaian","Level","GAP")

## Menampilkan level per indikator & prioritas ##
Ind6.1<-mean(valInd$q6.1); Ind6.2<-mean(valInd$q6.2); Ind6.3<-mean(valInd$q6.3); Ind6.4<-mean(valInd$q6.4)
provInd<-as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
provInd<-round(provInd, digits=2)

tabelInd<-cbind(summIndikatorInd,provInd)
tabelInd$prioritasInd <- "Tidak prioritas" 
tabelInd<-within(tabelInd, {prioritasInd<-ifelse(provInd<=3, "Prioritas rendah", prioritasInd)})
tabelInd<-within(tabelInd, {prioritasInd<-ifelse(provInd<=2, "Prioritas tinggi", prioritasInd)})
tabelInd<-within(tabelInd, {prioritasInd<-ifelse(provInd<=1, "Prioritas sangat tinggi", prioritasInd)})

colnames(tabelInd)<-c("Indikator","Level","Prioritas")

### Tabel Prioritas Gabungan ####
allprioritas <- rbind(tabelSys,tabelOrg,tabelInd)
prioritas <- allprioritas[order(allprioritas$Level),]

### Tabel Level Per Aspek Semua Tingkat ###
summary<-as.data.frame(rbind(summSistem, summOrganisasi, summIndividu))
summary$`Aspek Penilaian`<-NULL
aspek<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan","4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi", "6. Sumber Daya Manusia - Individu")
summary<-cbind(aspek,summary)
finalLevel<-as.numeric(summary$Level)
finalLevel<-round(finalLevel,digits = 2)
finalGAP<-as.data.frame(5-finalLevel)
finalGAP<-round(finalGAP, digits = 2)
# summary$Level<-as.numeric(summary$Level)
summary<-as.data.frame(cbind(summary$aspek,finalLevel, finalGAP))
# summary$GAP<-NULL
colnames(summary)<-c("Aspek", "Level", "GAP")
rownames(summary)<-1:9

# datatable(prioritas,escape = FALSE, rownames = FALSE)

###Show Recommendation####
initial_indikator <- substring(prioritas$Indikator,1, 1)
tabel <- data.frame(cbind(initial_indikator, prioritas$Prioritas))
colnames(tabel)<-c("Aspek", "Prioritas")
tabel$kondisi <- "Cluster 5"
tabel <- within(tabel, {kondisi<-ifelse(tabel$Aspek=="2" & tabel$Prioritas=="Prioritas tinggi" | tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 1", kondisi)
})
tabel <- within(tabel, {kondisi<-ifelse(tabel$Aspek=="8" & tabel$Prioritas=="Prioritas tinggi" | tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 2", kondisi)
})
tabel <- within(tabel, {kondisi<-ifelse(tabel$Aspek=="1" & tabel$Prioritas=="Prioritas tinggi" | tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 3", kondisi)
})
tabel <- within(tabel, {kondisi<-ifelse(tabel$Aspek=="9" & tabel$Prioritas=="Prioritas tinggi"| tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 4", kondisi)
})
tabel <- within(tabel, {kondisi<-ifelse(tabel$Aspek=="3" & tabel$Prioritas=="Prioritas tinggi"| tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 4", kondisi)
})

totalCluster <- sum(str_count(tabel$kondisi, "Cluster 5"))
ifelse(totalCluster >= 20, "Cluster 5", "Belum terdefinisi")

ifelse(tabel$Aspek=="2" || tabel$Aspek=="5" || tabel$Aspek=="6" && tabel$Prioritas=="Prioritas tinggi" || tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 1", "Cluster 5")
ifelse(tabel$Aspek=="8" || tabel$Aspek=="5" || tabel$Aspek=="6" && tabel$Prioritas=="Prioritas tinggi" || tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 2", "Cluster 5")
ifelse(tabel$Aspek=="1" || tabel$Aspek=="5" || tabel$Aspek=="6" && tabel$Prioritas=="Prioritas tinggi" || tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 3", "Cluster 5")
ifelse(tabel$Aspek=="9" || tabel$Aspek=="2" || tabel$Aspek=="3" || tabel$Aspek=="5" || tabel$Aspek=="6" && tabel$Prioritas=="Prioritas tinggi"|| tabel$Prioritas=="Prioritas sangat tinggi", "Cluster 4", "Cluster 5")


initial_indikator <- substring(prioritas$Indikator,1, 3)
tabel <- data.frame(cbind(initial_indikator, prioritas$Level, prioritas$Prioritas))
colnames(tabel)<-c("Indikator", "Level", "Prioritas")
tabel$Level <- as.numeric(levels(tabel$Level))[tabel$Level]
tabel$Rekomendasi <- "Tidak ada rekomendasi"

###Define the recommendation of each indicator"####
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="1.1" & Level<=2 , "Penyusunan Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="1.2" & Level<=2 , "Sosialisasi Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD dan Pembuatan petunjuk operasional dalam regulasi yang mengatur PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.1" & Level<=2 , "Pengarusutamaan konsep pembangunan rendah karbon dalam visi misi daerah", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.2" & Level<=2 , "Pengarusutamaan isu strategis pembangunan rendah karbon dalam perencanaan pembangunan daerah ", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.3" & Level<=2 , "Pengarusutamaan prioritas pembangunan rendah karbon dalam perencanaan pembangunan daerah ", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.4" & Level<=2 , "Pengarusutamaan indikator pembangunan rendah karbon dalam perencanaan pembangunan daerah ", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.5" & Level<=2 , "Pengarusutamaan program pembangunan rendah karbon/aksi mitigasi sebagai program/kegiatan pembangunan daerah yang tertuang dalam RPJMD serta Renstra K/L", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.1" & Level<=2 , "Penyelesaian Kaji Ulang RAD GRK dalam rangka penyusunan Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.2" & Level<=2 , "Tanpa rekomendasi", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.3" & Level<=2 , "Penyelesaian Kaji Ulang RAD GRK dalam rangka penyusunan Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.4" & Level<=2 , "Tanpa rekomendasi", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.5" & Level<=2 , "Pengarusutamaan program pembangunan rendah karbon/aksi mitigasi sebagai program/kegiatan pembangunan daerah yang tertuang dalam RPJMD serta Renstra K/L", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.1" & Level<=2 , "Revitalisasi organisasi kelompok kerja", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.2" & Level<=2 , "Adaptasi struktur organisasi yang disesuaikan dengan kebutuhan implementasi pembangunan rendah karbon", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.3" & Level<=2 , "Penyusunan mekanisme pengambilan keputusan yang inklusif dan berbasiskan data yang shahih", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.4" & Level<=2 , "Penyusunan prosedur/proses kerja serta pengelolaan kelembagaan yang mencakup seluruh aktivitas Pokja", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.5" & Level<=2 , "Fasilitasi penyediaan anggaran bagi operasional Pokja termasuk proses penganggaran di dalam APBD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.6" & Level<=2 , "Fasilitasi terbentuknya harmonisasi kerja dalam Pokja PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.7" & Level<=2 , "Penyusunan mekanisme kerja sama antara Pokja dengan pihak eksternal", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.1" & Level<=2 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.2" & Level<=2 , "Penguatan kapasitas anggota Pokja dalam proses pengarusutamaan PPRKD ke dalam perencanaan pembangunan daerah", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.3" & Level<=2 , "Penguatan kapasitas anggota Pokja dalam proses penulisan/pelaporan pembangunan rendah karbon", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.4" & Level<=2 , "Penyusunan mekanisme kerja sama antara Pokja dengan pihak eksternal", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.5" & Level<=2 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.1" & Level<=2 , "Fasilitasi terbentuknya harmonisasi kerja dalam Pokja PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.2" & Level<=2 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.3" & Level<=2 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.4" & Level<=2 , "Fasilitasi terbentuknya harmonisasi kerja dalam Pokja PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="7.1" & Level<=2 , "Penyediaan data yang berkualitas dalam pelaksanaan PPRKD", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="7.2" & Level<=2 , "Pengelolaan data  yang berkualitas dalam pelaksanaan pembangunan rendah karbon", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="7.3" & Level<=2 , "Pengelolaan data  yang berkualitas dalam pelaksanaan pembangunan rendah karbon", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="8.1" & Level<=2 , "Peningkatan penggunaan perangkat lunak dalam pengelolaan data dan analisis teknis dalam rangka perencanaan dan pemantauan, evaluasi, serta pelaporan pembangunan rendah karbon", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="8.2" & Level<=2 , "Pengadaan perangkat keras penunjang aktivitas perencanaan, pemantauan, evaluasi, dan pelaporan pembangunan rendah karbon", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="8.3" & Level<=2 , "Penyediaan kapasitas jaringan yang mendukung proses perencanaan, pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.1" & Level<=2 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.2" & Level<=2 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.3" & Level<=2 , "Penyusunan prosedur pelibatan berbagai pihak termasuk kabupaten/kota dan swasta dalam proses pemantauan, evaluasi, dan pelaporan program pembangunan rendah karbon", Rekomendasi)})
tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.4" & Level<=2 , "Penyusunan prosedur pemanfaatan data pemantauan, evaluasi, dan pelaporan bagi kepentingan berbagai pihak", Rekomendasi)})