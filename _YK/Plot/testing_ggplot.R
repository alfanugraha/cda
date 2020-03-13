library(koboloadeR)
library (dplyr)
library(plotly)
library(ggplot2)
library(ggthemes)

#Mengunduh data secara langsung
dataIndividu<-kobo_data_downloader("327418", "cdna2019:Icraf2019!")
saveRDS(dataIndividu, "data/dataIndividu")

### TINGKAT INDIVIDU ####
inputIndividu<-readRDS("data/dataIndividu")
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

##Membuat bar chart untuk tingkat Individu###
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

## ggplot untuk unduh hasil anlisis ####
nilai1 <- t(graphInd$Level)
nilai2 <- t(graphInd$GAP)
nilai <- t(cbind(nilai1,nilai2))
jenis1 <- t(rep("Level", length(graphInd$Level)))
jenis2 <- t(rep("Gap", length(graphInd$GAP)))
jenis <- t(cbind(jenis1,jenis2))
indikator <- data.frame(graphInd$Indikator)
dataGraphInd <- data.frame(cbind(jenis,nilai,indikator))
colnames(dataGraphInd) <- c("jenis", "nilai", "indikator")
 
## Cara 1
dataGraphInd <- ddply(dataGraphInd, .(indikator),
                     transform, pos = cumsum(nilai)-nilai)
ggplot() + geom_bar(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis), stat="identity") + 
  geom_text(data=dataGraphInd, aes(x =indikator, y =nilai, label =paste0(nilai)), size=4)

## Cara 2
ggplot(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis)) +
  geom_bar(stat="identity") + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  geom_text(data=dataGraphInd, aes(x =indikator, y =nilai, label =paste0(nilai)), size=4)

## Cara 3 (coord flip)
ggplot(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis)) +
  geom_bar(stat="identity") +
  coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  geom_text(data=dataGraphInd, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)




#### Kodingan ####
output$downloadResults <- downloadHandler(
  filename = paste0(input$categoryProvince, "_hasil.doc"),
  content = function(file){
    tingkatSys<-tablesCDA$summarySystem
    summOrganisasi<-tablesCDA$summaryProvOrg
    summIndividu<-tablesCDA$summaryProvInd
    prioritas<-tablesCDA$priorityTable
    # chartSys<-ggplot(data=dataGraphSys, aes(x=indikator, y=nilai, fill=jenis)) +
    #   geom_bar(stat="identity") +
    #   coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
    #   theme(legend.position="bottom", legend.direction="horizontal",
    #         legend.title = element_blank()) +
    #   geom_text(data=dataGraphOrg, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)
    # chartOrg<-ggplot(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis)) +
    #   geom_bar(stat="identity") +
    #   coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
    #   theme(legend.position="bottom", legend.direction="horizontal",
    #         legend.title = element_blank()) +
    #   geom_text(data=dataGraphInd, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)
    chartInd<-ggplot(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis)) +
      geom_bar(stat="identity") +
      coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      geom_text(data=dataGraphSumm, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)
    # chartSumm<-ggplot(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis)) +
    #   geom_bar(stat="identity") +
    #   coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
    #   theme(legend.position="bottom", legend.direction="horizontal",
    #         legend.title = element_blank()) +
    #   geom_text(data=dataGraphInd, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)
    title <- "\\b\\fs32 Hasil Analisis Penilaian Kapasistas Mandiri\\b0\\fs20"
    fileresult = file.path(tempdir(), paste0(input$categoryProvince, "_hasil.doc"))
    rtffile <- RTF(fileresult, font.size = 9)
    addParagraph(rtffile, title)
    addNewLine(rtffile)
    addParagraph(rtffile, "\\b\\fs14 Tabel 1 Aspek Penilaian Tingkat Sistem per Provinsi\\b0\\fs14")
    addTable(rtffile, tingkatSys, font.size = 8)
    #addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartSys)
    addNewLine(rtffile)
    addParagraph(rtffile, "\\b\\fs14 Tabel 2 Aspek Penilaian Tingkat Organiasi per Provinsi\\b0\\fs14")
    addTable(rtffile, summOrganisasi, font.size = 8)
    #addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartOrg)
    addNewLine(rtffile)
    addParagraph(rtffile, "\\b\\fs14 Tabel 3 Aspek Penilaian Tingkat Individu per Provinsi\\b0\\fs14")
    addTable(rtffile, summIndividu, font.size = 8)
    addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartInd)
    addNewLine(rtffile)
    addParagraph(rtffile, "\\b\\fs14 Tabel 4 Rangkuman Hasil dan Tingkat Prioritas per Provinsi\\b0\\fs14")
    addTable(rtffile, prioritas, font.size = 8)
    #addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartSumm)
    done(rtffile)
