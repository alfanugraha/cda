###Analisis Setiap Region###

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

indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
tes <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))
result_Sistem2 <-cbind(tes,t(levelSistem))
colnames(result_Sistem2)<-cbind("Indikator","Aceh","Bangka Belitung","Bengkulu","Jambi","Kepulauan Riau", "Lampung","Riau","Sumatera Barat","Sumatera Selatan","Sumatera Utara")

write.csv(result_Sistem2,"hasilregionalbarat.csv")


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

indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
tes2 <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
colnames(tes2)<-"Indikator"
result_Organisasi2 <-cbind(inputOrg$provinsi,valOrganisasi)
hasilOrg<-as.data.frame(t(result_Organisasi2))
write.csv(hasilOrg,"hasilorgbarat.csv")

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

Indikator <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
Indikator  <- as.data.frame(Indikator)
# colnames(Indikator)<-"Indikator Penilaian"
result_Individu2<-cbind(Indikator,t(valInd))
write.csv(result_Individu2,"hasilindbarat.csv")

