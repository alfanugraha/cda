###SUMMARY###

#Hasil per BAB Sistem
tempIndikator_Penilaian<-c("Regulasi/peraturan daerah","Integrasi dalam Perencanaan Pembangunan Daerah","Proses","Data dan Informasi","Pemantauan, Evaluasi, dan Pelaporan")
tempdataSistem<-as.data.frame(cbind(tempIndikator_Penilaian, LevelSistem, GAPSistem))
colnames(tempdataSistem)<-c("Aspek","Level","GAP")

#Hasil per BAB Organisasi
orgLevel_gabungan<-mean(LevelOrg,LevelSDM,LevelTek)
orgGAP_gabungan<-mean(gapOrg,gapSDM,gapTek)
Aspek<-"Organisasi"
Level<-orgLevel_gabungan
GAP<-orgGAP_gabungan
tempdataOrg<-cbind(Aspek, Level, GAP)

#Hasil per BAB Individu
tempIndikator_Penilaian_Ind<-"Sumber Daya Manusia"
tempdataInd<-as.data.frame(cbind(tempIndikator_Penilaian_Ind, LevelSDM_Ind, GAP_Ind))
colnames(tempdataInd)<-c("Aspek","Level","GAP")

##Hasil Gabungan sementara
tempsummary<-data.frame(rbind(tempdataSistem,tempdataOrg,tempdataInd))
rownames(tempsummary)<-1:7

##Hasil Gabungan akhir
a<-as.factor(tempsummary$Aspek)
Aspek<-as.data.frame(a)
b<-as.numeric(tempsummary$Level)
Level<-as.data.frame(b)
c<-as.numeric(tempsummary$GAP)
GAP<-as.data.frame(c)
summary<-cbind(Aspek,Level,GAP)
colnames(summary)<-c("Aspek","Level","GAP")

###BAR CHART Summary
plot_ly(summary, x=~Aspek, y=~Level, type='bar', name='Level') %>%
  add_trace(y=~GAP, name='GAP') %>%
  layout(yaxis = list(title='Nilai'), barmode='stack', title="Level dan Gap Penilaian Kapasitas Semua Tingkat")