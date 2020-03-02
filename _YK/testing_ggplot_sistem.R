library(koboloadeR)
library (dplyr)
library(plotly)

#Mengunduh data secara langsung
dataSistem<-kobo_data_downloader("327419", "cdna2019:Icraf2019!")
saveRDS(dataSistem, "data/dataSistem")

inputSistem<-readRDS("data/dataSistem")

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


inputSistem<-as.data.frame(inputSistem)
inputSistem$`pemantauan1/pemantauan3/q9.2.6`[inputSistem$`pemantauan1/pemantauan3/q9.2.6` == "n/a"]  <- NA
inputSistem$`pemantauan1/pemantauan5/q9.4.1`[inputSistem$`pemantauan1/pemantauan5/q9.4.1` == "n/a"]  <- NA
inputSistem$`pemantauan1/pemantauan5/q9.4.2`[inputSistem$`pemantauan1/pemantauan5/q9.4.2` == "n/a"]  <- NA
inputSistem[is.na(inputSistem)]<-3
sistem<- as.data.frame(lapply(inputSistem[,3:length(inputSistem)], as.numeric))

q2.5<-rowSums(sistem[,9:10]); q2.5<- as.data.frame(q2.5)/2
q7.1 <- rowSums(sistem[,14:32]); q7.1<- as.data.frame(q7.1)/19
q7.2 <- rowSums(sistem[,33:51]); q7.2<- as.data.frame(q7.2)/19
q7.3<-rowSums(sistem[,52:53]); q7.3<-as.data.frame(q7.3)/2
q9.1<-rowSums(sistem[,54:58]); q9.1<-as.data.frame(q9.1)/5
q9.2<-rowSums(sistem[,59:64]); q9.2<-as.data.frame(q9.2)/6
q9.3<-rowSums(sistem[,65:67]); q9.3<-as.data.frame(q9.3)/3
q9.4<-rowSums(sistem[,68:69]); q9.4<-as.data.frame(q9.4)/2

levelSistem<-cbind(inputSistem$`provinsi/provinsi_001`,sistem$regulasi.regulasi1.q1.1,sistem$regulasi.regulasi2.q1.2,sistem$integrasi1.integrasi2.q2.1,sistem$integrasi1.integrasi3.q2.2, sistem$integrasi1.integrasi4.q2.3, sistem$integrasi1.integrasi5.q2.4, q2.5,sistem$proses1.proses2.q3.1, sistem$proses1.proses2_001.q3.2, sistem$proses1.proses3.q3.3 ,sistem$proses1.proses4.q3.4, sistem$proses1.proses4_001.q3.5, q7.1,q7.2,q7.3,q9.1,q9.2,q9.3,q9.4)
colnames(levelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")

gap_1.1<-5-levelSistem$q1.1; gap_1.2<-5-levelSistem$q1.2; gap_2.1<-5-levelSistem$q2.1; gap_2.2<-5-levelSistem$q2.2; gap_2.3<-5-levelSistem$q2.3; gap_2.4<-5-levelSistem$q2.4; gap_2.5<-5-levelSistem$q2.5
gap_3.1<-5-levelSistem$q3.1; gap_3.2<-5-levelSistem$q3.2; gap_3.3<-5-levelSistem$q3.3; gap_3.4<-5-levelSistem$q3.4; gap_3.5<-5-levelSistem$q3.5
gap_7.1<-5-levelSistem$q7.1; gap_7.2<-5-levelSistem$q7.2; gap_7.3<-5-levelSistem$q7.3; gap_9.1<-5-levelSistem$q9.1; gap_9.2<-5-levelSistem$q9.2; gap_9.3<-5-levelSistem$q9.3; gap_9.4<-5-levelSistem$q9.4
valGAP<-cbind(gap_1.1,gap_1.2,gap_2.1,gap_2.2,gap_2.3,gap_2.4,gap_2.5,gap_3.1,gap_3.2,gap_3.3,gap_3.4,gap_3.5,gap_7.1,gap_7.2,gap_7.3,gap_9.1,gap_9.2,gap_9.3,gap_9.4)
valSistem<-cbind(levelSistem,valGAP)
tempSistem<-as.data.frame((valSistem))

file_indSys<- read.table("init/system.csv", header=TRUE, sep=",")
indikatorSys <- as.data.frame(unique(file_indSys$Kapasitas_Fungsional))

##Menampilkan hasil satu provinsi###
#tempSistem<-filter(tempSistem,Provinsi==input$categoryProvince)
tempSistem<-filter(tempSistem,Provinsi=="Aceh")

##Membuat tabel Level setiap aspek###
aspekSys<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
LevelReg<-mean(as.matrix(tempSistem[,2:3])); LevelInt<-mean(as.matrix(tempSistem[4:8])); LevelProses<-mean(as.matrix(tempSistem[9:13])); LevelData<-mean(as.matrix(tempSistem[14:16])); LevelPEP<-mean(as.matrix(tempSistem[17:20]))
allLevelSys<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
allLevelSys<-round(allLevelSys,digits = 2)
gapReg<-5-LevelReg; gapInt<-5-LevelInt; gapProses<-5-LevelProses; gapData<-5-LevelData; gapPEP<-5-LevelPEP
allGapSys<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
allGapSys<-round(allGapSys, digits=2)
tingkatSys<-as.data.frame(cbind(aspekSys, allLevelSys, allGapSys))
colnames(tingkatSys)<-c("Aspek Penilaian","Level","GAP")

##Membuat bar chart untuk tingkat Sistem###
t_tempSistem <- t(tempSistem[2:length(tempSistem)])
provSys <- rowMeans(t_tempSistem)
provSys<-round(provSys, digits = 2)
graphSistem<-cbind(indikatorSys,provSys[1:19],provSys[20:38])
colnames(graphSistem)<-c("Indikator","Level","GAP")

plot_ly(graphSistem, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
  add_trace(x=~GAP, name= 'GAP') %>%
  layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Sistem") 

## ggplot untuk unduh hasil anlisis ####
nilai1 <- t(graphSistem$Level)
nilai2 <- t(graphSistem$GAP)
nilai <- t(cbind(nilai1,nilai2))
jenis1 <- t(rep("Level", length(graphSistem$Level)))
jenis2 <- t(rep("Gap", length(graphSistem$GAP)))
jenis <- t(cbind(jenis1,jenis2))
indikator <- data.frame(graphSistem$Indikator)
dataGraphSys <- data.frame(cbind(jenis,nilai,indikator))
colnames(dataGraphSys) <- c("jenis", "nilai", "indikator")

dataGraphSys <- ddply(dataGraphSys, .(indikator),
                      transform, pos = cumsum(nilai)-nilai)
ggplot() + geom_bar(data=dataGraphSys, aes(x=indikator, y=nilai, fill=jenis), stat="identity") + 
  geom_text(data=dataGraphSys, aes(x =indikator, y =nilai, label =paste0(nilai)), size=4)

## Cara 3 (coord flip)
ggplot(data=dataGraphSys, aes(x=indikator, y=nilai, fill=jenis)) +
  geom_bar(stat="identity") +
  coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  geom_text(data=dataGraphSys, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)
