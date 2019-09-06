library(readxl)
library(dplyr)
library(plotly)

###SISTEM###
#i=;j=;k=
#Baca file excel dari KoBo
inputResp<-read_excel("data/cdna_sistem.xlsx")

inputResp$logo<-NULL
inputResp$intro0<-NULL
inputResp$intro0a<-NULL
inputResp$url_widget2<-NULL
inputResp$url_widget2_001<-NULL
inputResp$intro1<-NULL
inputResp$introSistem<-NULL
inputResp$introregulasi<-NULL
inputResp$introintegrasi1<-NULL
inputResp$introproses1<-NULL
inputResp$introdatainfo1<-NULL
inputResp$intropemantauan1<-NULL

inputResp$alasan<-NULL
for (i in 1:9){
  eval(parse(text=paste0("inputResp$alasan_00",i,"<-NULL")))
}

for (i in 9:65){
  eval(parse(text=paste0("inputResp$alasan_0",i,"<-NULL")))
}

inputResp<-as.data.frame(inputResp)
sistem<- as.data.frame(lapply(inputResp[,2:length(inputResp)], as.numeric))

q2.5<-rowSums(sistem[,7:8])
q2.5<- as.data.frame(q2.5)/2

q7.1 <- rowSums(sistem[,14:32])
q7.1<- as.data.frame(q7.1)/19

q7.2 <- rowSums(sistem[,33:51])
q7.2<- as.data.frame(q7.2)/19

q7.3<-rowSums(sistem[,52:53])
q7.3<-as.data.frame(q7.3)/2

q9.1<-rowSums(sistem[,54:58])
q9.1<-as.data.frame(q9.1)/5

q9.2<-rowSums(sistem[,59:63])
q9.2<-as.data.frame(q9.2)/5

q9.3<-rowSums(sistem[,64:66])
q9.3<-as.data.frame(q9.3)/3

levelSistem<-cbind(sistem$q1.1,sistem$q1.2,sistem$q2.1,sistem$q2.2,sistem$q2.3, sistem$q2.4, q2.5,sistem$q3.1,sistem$q3.2,sistem$q3.3,sistem$q3.4,sistem$q3.5,q7.1,q7.2,q7.3,q9.1,q9.2,q9.3)
colnames(levelSistem)<-c("q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3")

gap_1.1<-5-levelSistem$q1.1
gap_1.2<-5-levelSistem$q1.2
gap_2.1<-5-levelSistem$q2.1
gap_2.2<-5-levelSistem$q2.2
gap_2.3<-5-levelSistem$q2.3
gap_2.4<-5-levelSistem$q2.4
gap_2.5<-5-levelSistem$q2.5
gap_3.1<-5-levelSistem$q3.1
gap_3.2<-5-levelSistem$q3.2
gap_3.3<-5-levelSistem$q3.3
gap_3.4<-5-levelSistem$q3.4
gap_3.5<-5-levelSistem$q3.5
gap_7.1<-5-levelSistem$q7.1
gap_7.2<-5-levelSistem$q7.2
gap_7.3<-5-levelSistem$q7.3
gap_9.1<-5-levelSistem$q9.1
gap_9.2<-5-levelSistem$q9.2
gap_9.3<-5-levelSistem$q9.3
valGAP<-cbind(gap_1.1,gap_1.2,gap_2.1,gap_2.2,gap_2.3,gap_2.4,gap_2.5,gap_3.1,gap_3.2,gap_3.3,gap_3.4,gap_3.5,gap_7.1,gap_7.2,gap_7.3,gap_9.1,gap_9.2,gap_9.3)
val_Sistem<-cbind(levelSistem,valGAP)
tempSistem<-as.data.frame(t(val_Sistem))

indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
tes <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))
result_Sistem <-cbind(tes,tempSistem)
colnames(result_Sistem)<- c("Indikator","Lampung","Bengkulu","Bangka Belitung", "Kepulauan Riau", "Sumatera Barat", "Sumatera Utara", "Sumatera Selatan", "Jambi", "Aceh", "Riau")
#Menampilkan hasil satu responden
k=1
#Hasil per Kapasistas Fungsional
result_Sistem[[k]]<-cbind(result_Sistem$Indikator,result_Sistem[k+1])

#Hasil per BAB
Indikator_Penilaian<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
LevelReg<-mean(tempSistem[1:2,k])
LevelInt<-mean(tempSistem[3:7,k])
LevelProses<-mean(tempSistem[8:12,k])
LevelData<-mean(tempSistem[13:15,k])
LevelPEP<-mean(tempSistem[16:18,k])
LevelSistem<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
gapReg<-mean(tempSistem[19:20,k])
gapInt<-mean(tempSistem[21:25,k])
gapProses<-mean(tempSistem[26:31,k])
gapData<-mean(tempSistem[32:34,k])
gapPEP<-mean(tempSistem[35:36,k])
GAPSistem<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
summSistem<-as.data.frame(cbind(Indikator_Penilaian, LevelSistem, GAPSistem))
colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")
rownames(summSistem)<-c("1","2","3","4","5")

write.csv(summSistem, file = "sistem_Lampung.csv")

dataSistem<-as.data.frame(val_Sistem)
graphSistem<-cbind(tes,t((val_Sistem[i,1:18])),t(val_Sistem[i,19:36]))
colnames(graphSistem)<-c("Indikator","Level","GAP")
write.csv(graphSistem, file = "graphsistem_Lampung.csv")

###Organisasi
inputOrg <- read_excel("data/cdna_org.xlsx")
inputOrg$introOrganisasi<-NULL
inputOrg$introperangkat1<-NULL
inputOrg$introsdm1<-NULL
inputOrg$introteknologi1<-NULL
inputOrg$alasan<-NULL
for (i in 1:9){
  eval(parse(text=paste0("inputOrg$alasan_00",i,"<-NULL")))
}

for (i in 9:44){
  eval(parse(text=paste0("inputOrg$alasan_0",i,"<-NULL")))
}
organisasi<- as.data.frame(lapply(inputOrg[,5:length(inputOrg)], as.numeric))

q4.1<-rowSums(organisasi[,1:2])
q4.1<- as.data.frame(q4.1)/2

q4.2<-rowSums(organisasi[,3:5])
q4.2<- as.data.frame(q4.2)/3

q4.3<-rowSums(organisasi[,6:7])
q4.3<- as.data.frame(q4.3)/2

q4.4<-rowSums(organisasi[,8:11])
q4.4<- as.data.frame(q4.4)/4

q4.5<-rowSums(organisasi[,12:14])
q4.5<- as.data.frame(q4.5)/3

q4.6<-rowSums(organisasi[,15:16])
q4.6<- as.data.frame(q4.6)/2

q4.7<-rowSums(organisasi[,17:23])
q4.7<- as.data.frame(q4.7)/7

q5.1<-rowSums(organisasi[,24:30])
q5.1<- as.data.frame(q5.1)/7

q5.2<-organisasi$q5.2
q5.3<-organisasi$q5.3

q5.4<-rowSums(organisasi[,33:34])
q5.4<- as.data.frame(q5.4)/2

q5.5<-rowSums(organisasi[,35:36])
q5.5<- as.data.frame(q5.5)/2

q8.1<-rowSums(organisasi[,37:40])
q8.1<- as.data.frame(q8.1)/4

q8.2<-rowSums(organisasi[,41:43])
q8.2<- as.data.frame(q8.2)/3

q8.3<-rowSums(organisasi[,44:45])
q8.3<- as.data.frame(q8.3)/2

valOrganisasi <- cbind(q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)

gap4.1<-5-q4.1
gap4.2<-5-q4.2
gap4.3<-5-q4.3
gap4.4<-5-q4.4
gap4.5<-5-q4.5
gap4.6<-5-q4.6
gap4.7<-5-q4.7
gap5.1<-5-q5.1
gap5.2<-5-q5.2
gap5.3<-5-q5.3
gap5.4<-5-q5.4
gap5.5<-5-q5.5
gap8.1<-5-q8.1
gap8.2<-5-q8.2
gap8.3<-5-q8.3
valGAPorg<- cbind(gap4.1,gap4.2,gap4.3,gap4.4,gap4.5,gap4.6,gap4.7,gap5.1,gap5.2,gap5.3,gap5.4,gap5.5,gap8.1,gap8.2,gap8.3)
colnames(valGAPorg)<-c("gap4.1","gap4.2","gap4.3","gap4.4","gap4.5","gap4.6","gap4.7","gap5.1","gap5.2","gap5.3","gap5.4","gap5.5","gap8.1","gap8.2","gap8.3")
val_Organisasi<-cbind(valOrganisasi,valGAPorg)
tempOrganisasi<-as.data.frame(t(val_Organisasi))

indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
tes2 <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
colnames(tes2)<-"Indikator"
result_Organisasi <-cbind(tes2,tempOrganisasi)

#Menampilkan hasil satu responden
k=22
#Hasil per Kapasistas Fungsional
result_Organisasi[[k]]<-cbind(result_Organisasi$Indikator,result_Organisasi[k+1])

#Hasil per BAB
#Hanya satu responden
LevelOrg<-mean(tempOrganisasi[1:7,k])
LevelSDM<-mean(tempOrganisasi[8:12,k])
LevelTek<-mean(tempOrganisasi[13:15,k])
LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
gapOrg<-mean(tempOrganisasi[16:22,k])
gapSDM<-mean(tempOrganisasi[23:27,k])
gapTek<-mean(tempOrganisasi[28:30,k])
gapOrg_gabungan<-as.data.frame(t(cbind(gapOrg,gapSDM,gapTek)))
summOrg<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
colnames(summOrg)<- c("Aspek Penilaian", "Level", "GAP")
rownames(summOrg)<- c("1","2","3")
write.csv(summOrg, "organisasi_lampung.csv")

graphOrg<-cbind(tes2,t((val_Organisasi[i,1:15])),t(val_Organisasi[i,16:30]))
colnames(graphOrg)<-c("Indikator","Level","GAP")
write.csv(graphOrg, "graphorg_lampung.csv")

###INDIVIDU
#Baca file excel dari KoBo
inputRespInd<-read_excel("data/cdna_ind.xlsx")

inputRespInd$introIndividu<-NULL
inputRespInd$introSDM2<-NULL
inputRespInd$alasan<-NULL
for (i in 1:9){
  eval(parse(text=paste0("inputRespInd$alasan_00",i,"<-NULL")))
}

for (i in 10:22){
  eval(parse(text=paste0("inputRespInd$alasan_0",i,"<-NULL")))
}

inputRespInd<-as.data.frame(inputRespInd)
# valResp<-unlist(inputRespInd[,15:37])
valResp<- as.data.frame(lapply(inputRespInd[,3:length(inputRespInd)], as.numeric))

Level6.1<-rowSums(valResp[,1:2])
Level6.1<-as.data.frame(Level6.1)/2

Level6.2<-rowSums(valResp[,3:11])
Level6.2<-as.data.frame(Level6.2)/9

Level6.3<-rowSums(valResp[,12:20])
Level6.3<-as.data.frame(Level6.3)/9

Level6.4<-rowSums(valResp[,21:23])
Level6.4<-as.data.frame(Level6.4)/3

valInd<-cbind(Level6.1,Level6.2,Level6.3,Level6.4)

gap6.1<-5-Level6.1
gap6.2<-5-Level6.2
gap6.3<-5-Level6.3
gap6.4<-5-Level6.4
valGAPind<-cbind(gap6.1,gap6.2,gap6.3,gap6.4)
colnames(valGAPind)<-c("gap6.1","gap6.2","gap6.3","gap6.4")
val_Individu <- cbind(valInd,valGAPind)
individu<-as.data.frame(t(val_Individu))

Indikator <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
Indikator  <- as.data.frame(Indikator)
# colnames(Indikator)<-"Indikator Penilaian"
result_Individu<-cbind(Indikator,individu)

#Menampilkan hasil satu responden
k=21
#Hasil per Kapasistas Fungsional
result_Individu[[k]]<-cbind(result_Individu$Indikator,result_Individu[k+1])

#Hasil per BAB
Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
LevelSDM_Ind<-mean(individu[1:4,k])
GAP_Ind<-mean(individu[5:8,k])
summInd<-as.data.frame(cbind(Indikator_Penilaian_Ind, LevelSDM_Ind, GAP_Ind))
colnames(summInd)<-c("Aspek Penilaian","Level","GAP")
write.csv(summInd, "individu_lampung.csv")

##BAR Chart
graphInd<-cbind(Indikator,t((val_Individu[k,1:4])),t(val_Individu[k,5:8]))
colnames(graphInd)<-c("Indikator","Level","GAP")
write.csv(graphInd, "graphind_lampung.csv")
