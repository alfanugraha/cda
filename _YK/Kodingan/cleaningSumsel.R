library(readxl)
library(dplyr)

###TINGKAT SISTEM (PEP)###

inputResp<-read_excel("data/cdna_pep.xlsx")

inputResp$logo<-NULL; inputResp$intro0<-NULL; inputResp$intro0a<-NULL; inputResp$url_widget2<-NULL; inputResp$intro1a<-NULL
inputResp$tanggal<-NULL; inputResp$`_index`<-NULL;inputResp$`_validation_status`<-NULL; inputResp$`_submission_time`<-NULL; inputResp$`_uuid`<-NULL; inputResp$`_id`<-NULL
inputResp$intropenutup<-NULL; inputResp$intropenutup2<-NULL; inputResp$introSistem<-NULL; inputResp$intropemantauan1<-NULL
inputResp$alasan<-NULL
for (i in 1:9){
  eval(parse(text=paste0("inputResp$alasan_00",i,"<-NULL")))
}

for (i in 10:15){
  eval(parse(text=paste0("inputResp$alasan_0",i,"<-NULL")))
}

inputResp<-as.data.frame(inputResp)
sistem<- as.data.frame(lapply(inputResp[,5:length(inputResp)], as.numeric))

q9.1<-rowSums(sistem[,1:5]); q9.1<-as.data.frame(q9.1)/5
q9.2<-rowSums(sistem[,6:11]); q9.2<-as.data.frame(q9.2)/6
q9.3<-rowSums(sistem[,12:14]); q9.3<-as.data.frame(q9.3)/3
q9.4<-rowSums(sistem[,15:16]); q9.4<-as.data.frame(q9.4)/2

levelSistem<-cbind(q9.1,q9.2,q9.3,q9.4)
colnames(levelSistem)<-c("q9.1","q9.2","q9.3","q9.4")
write.csv(levelSistem,"Hasil sistem.csv")

# gap_9.1<-5-levelSistem$q9.1; gap_9.2<-5-levelSistem$q9.2; gap_9.3<-5-levelSistem$q9.3; gap_9.4<-5-levelSistem$q9.4
# valGAP<-cbind(gap_9.1,gap_9.2,gap_9.3,gap_9.4)
# val_Sistem<-cbind(levelSistem,valGAP)
# tempSistem<-as.data.frame((val_Sistem))


# tes <- c("9.1 Muatan/Subtansi", "9.2 Pelaksanaan", "9.3 Pelaksana", "9.4 Pemanfaatan")
# 
# #Menampilkan hasil satu responden
# #tempSistem<-filter(tempSistem,Provinsi==input$categoryProvince)
# 
# #Hasil per Aspek
# Indikator_Penilaian<-c("9. Pemantauan, Evaluasi, dan Pelaporan")
# LevelPEP<-mean(as.numeric(tempSistem[1:4]))
# LevelSistem<-as.data.frame(t(LevelPEP))
# gapPEP<-mean(as.numeric(tempSistem[5:8]))
# GAPSistem<-as.data.frame(t(gapPEP))
# summSistem<-as.data.frame(cbind(Indikator_Penilaian, LevelSistem, GAPSistem))
# colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")
# 
# #Hasil per Kapasitas Fungsional
# tabelKapasitasSistem<-as.data.frame(cbind(tes,t((tempSistem[1:4])),t(tempSistem[5:8])))
# colnames(tabelKapasitasSistem)<-c("Kapasitas Fungsional","Level","GAP")

###TINGKAT INDIVIDU###
inputRespInd<-read_excel("data/cdna_ind2.xlsx")
#inputRespInd<-read_excel("data/cdna_individu_sumsel.xlsx")

inputRespInd$logo<-NULL; inputRespInd$intro0<-NULL; inputRespInd$intro0a<-NULL; inputRespInd$intro1a<-NULL; inputRespInd$callid<-NULL
inputRespInd$gender<-NULL; inputRespInd$jabatan<-NULL; inputRespInd$akun <- NULL; inputRespInd$tanggal<-NULL; inputRespInd$callresp<-NULL
inputRespInd$introIndividu<-NULL; inputRespInd$introSDM2<-NULL; inputRespInd$`_index`<-NULL; inputRespInd$`_validation_status`<-NULL
inputRespInd$`_submission_time`<-NULL; inputRespInd$`_uuid`<-NULL; inputRespInd$`_id`<-NULL; inputRespInd$intropenutup<-NULL
inputRespInd$alasan<-NULL
for (i in 1:9){
  eval(parse(text=paste0("inputRespInd$alasan_00",i,"<-NULL")))
}

for (i in 10:22){
  eval(parse(text=paste0("inputRespInd$alasan_0",i,"<-NULL")))
}

inputRespInd<-as.data.frame(inputRespInd)
valResp<- as.data.frame(lapply(inputRespInd[,6:length(inputRespInd)], as.numeric))

Level6.1<-rowSums(valResp[,1:2]); Level6.1<-as.data.frame(Level6.1)/2
Level6.2<-rowSums(valResp[,3:11]); Level6.2<-as.data.frame(Level6.2)/9
Level6.3<-rowSums(valResp[,12:20]); Level6.3<-as.data.frame(Level6.3)/9
Level6.4<-rowSums(valResp[,21:23]); Level6.4<-as.data.frame(Level6.4)/3
valInd<-cbind(inputRespInd$provinsi,Level6.1,Level6.2,Level6.3,Level6.4)
individu<-as.data.frame(valInd)
write.csv(individu,"hasilindividu_sumsel.csv")

# Indikator <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
# Indikator  <- as.data.frame(Indikator)
# 
# #individu<-filter(individu,inputRespInd$provinsi==input$categoryProvince)
# 
# #Hasil per Aspek
# Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
# Level6<-rowSums(individu[,2:5])/length(individu[,2:5])
# Level6<-sum(Level6)/length(individu$`inputRespInd$provinsi`)
# gap6<-5-Level6
# summInd2<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
# colnames(summInd2)<-c("Aspek Penilaian","Level","GAP")
# 
# ##Hasil per Kapasitas Fungsional
# Ind6.1<-mean(individu$Level6.1); Ind6.2<-mean(individu$Level6.2); Ind6.3<-mean(individu$Level6.3); Ind6.4<-mean(individu$Level6.4)
# tempLevelInd <- as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
# tempGapInd <- 5 - tempLevelInd
# graphInd2<-cbind(Indikator,tempLevelInd,tempGapInd)
# colnames(graphInd2)<-c("Indikator","Level","GAP")
# graphInd2
