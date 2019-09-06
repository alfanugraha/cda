library(readxl)
library(dplyr)
library(plotly)

###SISTEM###
#j=;k=
#m=i+5
#n=j+5

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

###ORGANISASI###
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

Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
org4.1<-mean(as.numeric(tempOrganisasi[1,i:j]))
org4.2<-mean(as.numeric(tempOrganisasi[2,i:j]))
org4.3<-mean(as.numeric(tempOrganisasi[3,i:j]))
org4.4<-mean(as.numeric(tempOrganisasi[4,i:j]))
org4.5<-mean(as.numeric(tempOrganisasi[5,i:j]))
org4.6<-mean(as.numeric(tempOrganisasi[6,i:j]))
org4.7<-mean(as.numeric(tempOrganisasi[7,i:j]))
org4<-cbind(org4.1,org4.2,org4.3,org4.4,org4.5,org4.6,org4.7)
LevelOrg2<-mean(org4)

sdm5.1<-mean(as.numeric(tempOrganisasi[8,i:j]))
sdm5.2<-mean(as.numeric(tempOrganisasi[9,i:j]))
sdm5.3<-mean(as.numeric(tempOrganisasi[10,i:j]))
sdm5.4<-mean(as.numeric(tempOrganisasi[11,i:j]))
sdm5.5<-mean(as.numeric(tempOrganisasi[12,i:j]))
sdm5<-cbind(sdm5.1,sdm5.2,sdm5.3,sdm5.4,sdm5.5)
LevelSDM2<-mean(sdm5)

tek8.1<-mean(as.numeric(tempOrganisasi[13,i:j]))
tek8.2<-mean(as.numeric(tempOrganisasi[14,i:j]))
tek8.3<-mean(as.numeric(tempOrganisasi[15,i:j]))
tek8<-cbind(tek8.1,tek8.2,tek8.3)
LevelTek2<-mean(tek8)
LevelAllOrg<-as.data.frame(t(cbind(LevelOrg2,LevelSDM2,LevelTek2)))

gapOrg4.1<-mean(as.numeric(tempOrganisasi[1,i:j]))
gapOrg4.2<-mean(as.numeric(tempOrganisasi[2,i:j]))
gapOrg4.3<-mean(as.numeric(tempOrganisasi[3,i:j]))
gapOrg4.4<-mean(as.numeric(tempOrganisasi[4,i:j]))
gapOrg4.5<-mean(as.numeric(tempOrganisasi[5,i:j]))
gapOrg4.6<-mean(as.numeric(tempOrganisasi[6,i:j]))
gapOrg4.7<-mean(as.numeric(tempOrganisasi[7,i:j]))
gapOrg4<-cbind(gapOrg4.1,gapOrg4.2,gapOrg4.3,gapOrg4.4,gapOrg4.5,gapOrg4.6,gapOrg4.7)
GAPOrg2<-mean(gapOrg4)

gapSDM5.1<-mean(as.numeric(tempOrganisasi[8,i:j]))
gapSDM5.2<-mean(as.numeric(tempOrganisasi[9,i:j]))
gapSDM5.3<-mean(as.numeric(tempOrganisasi[10,i:j]))
gapSDM5.4<-mean(as.numeric(tempOrganisasi[11,i:j]))
gapSDM5.5<-mean(as.numeric(tempOrganisasi[12,i:j]))
gapSDM5<-cbind(gapSDM5.1,gapSDM5.2,gapSDM5.3,gapSDM5.4,gapSDM5.5)
GAPSDM2<-mean(gapSDM5)

gapTek8.1<-mean(as.numeric(tempOrganisasi[13,i:j]))
gapTek8.2<-mean(as.numeric(tempOrganisasi[14,i:j]))
gapTek8.3<-mean(as.numeric(tempOrganisasi[15,i:j]))
gapTek8<-cbind(gapTek8.1,gapTek8.2,gapTek8.3)
GAPTek2<-mean(gapTek8)
GAPAllOrg <- as.data.frame(t(cbind(GAPOrg2,GAPSDM2,GAPTek2)))
summOrg2<-as.data.frame(cbind(Aspek_Penilaian,LevelAllOrg,GAPAllOrg))
colnames(summOrg2)<- c("Aspek Penilaian", "Level", "GAP")
rownames(summOrg2)<- c("1","2","3")
write.csv(summOrg2, "organisasi_Aceh.csv")

tempgraphOrg2<-cbind(t((val_Organisasi[i:j,1:15])),t(val_Organisasi[i:j,16:30]))
tempLevel4.1<-mean(as.numeric(tempgraphOrg2[1,i:j]))
tempLevel4.2<-mean(as.numeric(tempgraphOrg2[2,i:j]))
tempLevel4.3<-mean(as.numeric(tempgraphOrg2[3,i:j]))
tempLevel4.4<-mean(as.numeric(tempgraphOrg2[4,i:j]))
tempLevel4.5<-mean(as.numeric(tempgraphOrg2[5,i:j]))
tempLevel4.6<-mean(as.numeric(tempgraphOrg2[6,i:j]))
tempLevel4.7<-mean(as.numeric(tempgraphOrg2[7,i:j]))
tempLevel4<-as.data.frame((cbind(tempLevel4.1,tempLevel4.2,tempLevel4.3,tempLevel4.4,tempLevel4.5,tempLevel4.6,tempLevel4.7)))

tempLevel5.1<-mean(as.numeric(tempgraphOrg2[8,i:j]))
tempLevel5.2<-mean(as.numeric(tempgraphOrg2[9,i:j]))
tempLevel5.3<-mean(as.numeric(tempgraphOrg2[10,i:j]))
tempLevel5.4<-mean(as.numeric(tempgraphOrg2[11,i:j]))
tempLevel5.5<-mean(as.numeric(tempgraphOrg2[12,i:j]))
tempLevel5<-as.data.frame((cbind(tempLevel5.1,tempLevel5.2,tempLevel5.3,tempLevel5.4,tempLevel5.5)))
                            
tempLevel8.1<-mean(as.numeric(tempgraphOrg2[13,i:j]))
tempLevel8.2<-mean(as.numeric(tempgraphOrg2[14,i:j]))
tempLevel8.3<-mean(as.numeric(tempgraphOrg2[15,i:j]))                            
tempLevel8<-as.data.frame((cbind(tempLevel8.1,tempLevel8.2,tempLevel8.3)))

tempLevel<-as.data.frame(t(cbind(tempLevel4,tempLevel5,tempLevel8)))

tempGAP4.1<-mean(as.numeric(tempgraphOrg2[1,m:n]))
tempGAP4.2<-mean(as.numeric(tempgraphOrg2[2,m:n]))
tempGAP4.3<-mean(as.numeric(tempgraphOrg2[3,m:n]))
tempGAP4.4<-mean(as.numeric(tempgraphOrg2[4,m:n]))
tempGAP4.5<-mean(as.numeric(tempgraphOrg2[5,m:n]))
tempGAP4.6<-mean(as.numeric(tempgraphOrg2[6,m:n]))
tempGAP4.7<-mean(as.numeric(tempgraphOrg2[7,m:n]))
tempGAP4<-as.data.frame((cbind(tempGAP4.1,tempGAP4.2,tempGAP4.3,tempGAP4.4,tempGAP4.5,tempGAP4.6,tempGAP4.7)))

tempGAP5.1<-mean(as.numeric(tempgraphOrg2[8,m:n]))
tempGAP5.2<-mean(as.numeric(tempgraphOrg2[9,m:n]))
tempGAP5.3<-mean(as.numeric(tempgraphOrg2[10,m:n]))
tempGAP5.4<-mean(as.numeric(tempgraphOrg2[11,m:n]))
tempGAP5.5<-mean(as.numeric(tempgraphOrg2[12,m:n]))
tempGAP5<-as.data.frame((cbind(tempGAP5.1,tempGAP5.2,tempGAP5.3,tempGAP5.4,tempGAP5.5)))

tempGAP8.1<-mean(as.numeric(tempgraphOrg2[13,m:n]))
tempGAP8.2<-mean(as.numeric(tempgraphOrg2[14,m:n]))
tempGAP8.3<-mean(as.numeric(tempgraphOrg2[15,m:n]))                            
tempGAP8<-as.data.frame((cbind(tempGAP8.1,tempGAP8.2,tempGAP8.3)))

tempGAP<-as.data.frame(t(cbind(tempGAP4,tempGAP5,tempGAP8)))
graphOrg2<-cbind(tes2,tempLevel,tempGAP)
colnames(graphOrg2)<-c("Indikator","Level","GAP")
write.csv(graphOrg2, "graphorg_Aceh.csv")


###INDIVIDU###
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
ind6.1<-mean(as.numeric(individu[1,i:j]))
ind6.2<-mean(as.numeric(individu[2,i:j]))
ind6.3<-mean(as.numeric(individu[3,i:j]))
ind6.4<-mean(as.numeric(individu[4,i:j]))
tempind<-cbind(ind6.1,ind6.2,ind6.3,ind6.4)
LevelInd<-mean(tempind)

tempgap6.1<-mean(as.numeric(individu[5,i:j]))
tempgap6.2<-mean(as.numeric(individu[6,i:j]))
tempgap6.3<-mean(as.numeric(individu[7,i:j]))
tempgap6.4<-mean(as.numeric(individu[8,i:j]))
tempgap<-cbind(tempgap6.1,tempgap6.2,tempgap6.3,tempgap6.4)
GAP_Ind<-mean(tempgap)
summInd2<-as.data.frame(cbind(Indikator_Penilaian_Ind, LevelInd, GAP_Ind))
colnames(summInd2)<-c("Aspek Penilaian","Level","GAP")
write.csv(summInd2, "individu_Aceh.csv")

##BAR Chart
graphInd2<-cbind(Indikator,t(tempind),t(tempgap))
colnames(graphInd2)<-c("Indikator","Level","GAP")
write.csv(graphInd2, "graphind_lampung.csv")
