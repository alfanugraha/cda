### TINGKAT SISTEM ####
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
summInputSys$year <- format(as.Date(summInputSys$`provinsi/tanggal`), format = "%Y")
# yearSys <- summInputSys$year
# summInputSys<-filter(summInputSys,summInputSys$year==input$selectedYear)
summInputSys<-filter(summInputSys,summInputSys$year==2019)

summSys<- as.data.frame(lapply(summInputSys[,3:(length(summInputSys)-1)], as.numeric))

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

## Membuat tabel Level setiap aspek ##
aspekSys<-c("Provinsi","Regulasi/peraturan daerah","Integrasi dalam Perencanaan Pembangunan Daerah", "Proses", "Data dan Informasi", "Pemantauan, Evaluasi, dan Pelaporan")
LevelReg<-rowMeans(as.matrix(summTempSistem[,2:3])); LevelInt<-rowMeans(as.matrix(summTempSistem[4:8])); LevelProses<-rowMeans(as.matrix(summTempSistem[9:13])); LevelData<-rowMeans(as.matrix(summTempSistem[14:16])); LevelPEP<-rowMeans(as.matrix(summTempSistem[17:20]))
summ_allLevelSys<-as.data.frame((cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
finalSistem<-cbind(summTempSistem$Provinsi, summ_allLevelSys)
colnames(finalSistem)[names(finalSistem)=="summTempSistem$Provinsi"] <- "Provinsi"

table_nasionalSistem <- ddply(finalSistem, 'Provinsi', summarize, a1 = mean(LevelReg), a2 = mean(LevelInt), a3 = mean(LevelProses), a4 = mean(LevelData), a5 = mean(LevelPEP))
colnames(table_nasionalSistem)<-aspekSys

### TINGKAT ORGANISASI ####
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
summInputOrg$year <- format(as.Date(summInputOrg$`profil/tanggal`), format = "%Y")
# yearOrg <- summInputOrg$year
# summInputOrg<-filter(summInputOrg,summInputOrg$year==input$selectedYear)
summInputOrg<-filter(summInputOrg,summInputOrg$year==2019)
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

##Membuat tabel Level setiap aspek##
LevelOrg <- rowMeans(summTempOrganisasi[,4:10])
LevelSDM <- rowMeans(summTempOrganisasi[,11:15])
LevelTek <- rowMeans(summTempOrganisasi[,16:18])
LevelOrg_gabungan<-as.data.frame((cbind(LevelOrg,LevelSDM,LevelTek)))
finalOrganisasi<-as.data.frame(cbind(summTempOrganisasi$Provinsi, LevelOrg_gabungan))
colnames(finalOrganisasi)[names(finalOrganisasi)=="summTempOrganisasi$Provinsi"] <- "Provinsi"

table_nasionalOrganisasi <- ddply(finalOrganisasi, 'Provinsi', summarize, a1 = mean(LevelOrg), a2 = mean(LevelSDM), a3 = mean(LevelTek))
aspekOrg<-c("Provinsi","Organisasi","Sumber Daya Manusia", "Teknologi")
colnames(table_nasionalOrganisasi)<-aspekOrg

### TINGKAT INDIVIDU ####
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
summInputInd$year <- format(as.Date(summInputInd$`profil/tanggal`), format = "%Y")
# yearInd <- summInputInd$year
# summInputInd<-filter(summInputInd,summInputInd$year==input$selectedYear)
summInputInd<-filter(summInputInd,summInputInd$year==2019)

summInd<- as.data.frame(lapply(summInputInd[,5:length(summInputInd)], as.numeric))

q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
q6.3<-rowSums(summInd[,12:20]); q6.3<-as.data.frame(q6.3)/9
q6.4<-rowSums(summInd[,21:23]); q6.4<-as.data.frame(q6.4)/3
valInd<-cbind(summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
summTempIndividu<-as.data.frame(valInd)

##Membuat tabel Level setiap aspek##
LevelSDM_ind <- rowMeans(summTempIndividu[,3:6])
finalIndividu<-as.data.frame(cbind(data.frame(summTempIndividu$Provinsi), LevelSDM_ind))
colnames(finalIndividu)[names(finalIndividu)=="summTempIndividu.Provinsi"] <- "Provinsi"

table_nasionalIndividu <- ddply(finalIndividu, 'Provinsi', summarize, a1 = mean(LevelSDM_ind))
aspekInd<-c("Provinsi","Sumber Daya Manusia - Individu")
colnames(table_nasionalIndividu)<-aspekInd

### ALL STAGES ####
tableSDM <- merge(table_nasionalOrganisasi, table_nasionalIndividu)
meanSDM <- (tableSDM$`Sumber Daya Manusia` + tableSDM$`Sumber Daya Manusia - Individu`)/2
tableSDM$`Sumber Daya Manusia` <- round(meanSDM, digits = 2)
tableSDM$Organisasi <- round(tableSDM$Organisasi, digits = 2)
tableSDM$Teknologi <- round(tableSDM$Teknologi, digits = 2)
tableSDM$`Sumber Daya Manusia - Individu` <- NULL
tableSDM <- as.data.frame(tableSDM)

dataNasional <- merge(table_nasionalSistem, tableSDM)

### Membuat Grafik Nasional ###
chartNasional <- gather(dataNasional, variable, value, -Provinsi)
colnames(chartNasional)<-c("Provinsi","Aspek","Level")
chartNasional$Level <- round(chartNasional$Level, digits = 2)

graph <- ggplot(chartNasional, aes(fill=Aspek, y=Level, x=Provinsi)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_blank())
ggplotly(graph)
