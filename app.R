###*initiate library##
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
library(plotly)

library(leaflet)
library(readxl)
library(dplyr)

###*setup dashboard page####
ui <- source('interface.R')

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  tablesCDA <- reactiveValues(summarySystem=data.frame(),summaryOrg=data.frame(), summaryInd=data.frame(), allSummary=data.frame())
  
  output$resTblSys <- renderDataTable({
    inputResp<-read_excel("data/cdna_sistem2.xlsx")
    
    inputResp$logo<-NULL; inputResp$intro0<-NULL; inputResp$intro0a<-NULL; inputResp$url_widget2<-NULL; inputResp$url_widget2_001<-NULL; inputResp$intro1<-NULL; inputResp$respgeopoint<-NULL
    inputResp$tanggal<-NULL; inputResp$`_index`<-NULL;inputResp$`_validation_status`<-NULL; inputResp$`_submission_time`<-NULL; inputResp$`_uuid`<-NULL; inputResp$`_id`<-NULL
    inputResp$`_respgeopoint_precision`<-NULL; inputResp$`_respgeopoint_altitude`<-NULL; inputResp$intropenutup<-NULL; inputResp$intropenutup2<-NULL; inputResp$introSistem<-NULL
    inputResp$introregulasi<-NULL; inputResp$introintegrasi1<-NULL; inputResp$introproses1<-NULL; inputResp$introdatainfo1<-NULL;inputResp$intropemantauan1<-NULL
    inputResp$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputResp$alasan_00",i,"<-NULL")))
    }
    
    for (i in 9:65){
      eval(parse(text=paste0("inputResp$alasan_0",i,"<-NULL")))
    }
    
    inputResp<-as.data.frame(inputResp)
    sistem<- as.data.frame(lapply(inputResp[,2:length(inputResp)], as.numeric))
    
    q2.5<-rowSums(sistem[,7:8]); q2.5<- as.data.frame(q2.5)/2
    q7.1 <- rowSums(sistem[,14:32]); q7.1<- as.data.frame(q7.1)/19
    q7.2 <- rowSums(sistem[,33:51]); q7.2<- as.data.frame(q7.2)/19
    q7.3<-rowSums(sistem[,52:53]); q7.3<-as.data.frame(q7.3)/2
    q9.1<-rowSums(sistem[,54:58]); q9.1<-as.data.frame(q9.1)/5
    q9.2<-rowSums(sistem[,59:63]); q9.2<-as.data.frame(q9.2)/5
    q9.3<-rowSums(sistem[,64:66]); q9.3<-as.data.frame(q9.3)/3
    
    levelSistem<-cbind(inputResp$provinsi_001,sistem$q1.1,sistem$q1.2,sistem$q2.1,sistem$q2.2,sistem$q2.3, sistem$q2.4, q2.5,sistem$q3.1,sistem$q3.2,sistem$q3.3,sistem$q3.4,sistem$q3.5,q7.1,q7.2,q7.3,q9.1,q9.2,q9.3)
    colnames(levelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3")
    
    gap_1.1<-5-levelSistem$q1.1; gap_1.2<-5-levelSistem$q1.2; gap_2.1<-5-levelSistem$q2.1; gap_2.2<-5-levelSistem$q2.2; gap_2.3<-5-levelSistem$q2.3; gap_2.4<-5-levelSistem$q2.4; gap_2.5<-5-levelSistem$q2.5
    gap_3.1<-5-levelSistem$q3.1; gap_3.2<-5-levelSistem$q3.2; gap_3.3<-5-levelSistem$q3.3; gap_3.4<-5-levelSistem$q3.4; gap_3.5<-5-levelSistem$q3.5
    gap_7.1<-5-levelSistem$q7.1; gap_7.2<-5-levelSistem$q7.2; gap_7.3<-5-levelSistem$q7.3; gap_9.1<-5-levelSistem$q9.1; gap_9.2<-5-levelSistem$q9.2; gap_9.3<-5-levelSistem$q9.3
    valGAP<-cbind(gap_1.1,gap_1.2,gap_2.1,gap_2.2,gap_2.3,gap_2.4,gap_2.5,gap_3.1,gap_3.2,gap_3.3,gap_3.4,gap_3.5,gap_7.1,gap_7.2,gap_7.3,gap_9.1,gap_9.2,gap_9.3)
    val_Sistem<-cbind(levelSistem,valGAP)
    tempSistem<-as.data.frame((val_Sistem))
    
    indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
    tes <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))
    
    #Menampilkan hasil satu responden
    tempSistem<-filter(tempSistem,Provinsi==input$categoryProvince)
    
    #Hasil per BAB
    Indikator_Penilaian<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-mean(as.numeric(tempSistem[2:3])); LevelInt<-mean(as.numeric(tempSistem[4:8])); LevelProses<-mean(as.numeric(tempSistem[9:13])); LevelData<-mean(as.numeric(tempSistem[14:16])); LevelPEP<-mean(as.numeric(tempSistem[17:19]))
    LevelSistem<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    gapReg<-mean(as.numeric(tempSistem[20:21])); gapInt<-mean(as.numeric(tempSistem[22:26])); gapProses<-mean(as.numeric(tempSistem[27:32])); gapData<-mean(as.numeric(tempSistem[33:35])); gapPEP<-mean(as.numeric(tempSistem[36:37]))
    GAPSistem<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
    summSistem<-as.data.frame(cbind(Indikator_Penilaian, LevelSistem, GAPSistem))
    colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")
    
    ##BAR CHART
    graphSistem<-cbind(tes,t((tempSistem[2:19])),t(tempSistem[20:37]))
    colnames(graphSistem)<-c("Indikator","Level","GAP")
    tablesCDA$summarySystem <- graphSistem
    summSistem
  })
  
  output$resChartSys <- renderPlotly({
    graphSistem <- tablesCDA$summarySystem  
    plot_ly(graphSistem, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Sistem") 
  })
  
  output$resTblOrg <- renderDataTable({
    inputResp2<-read_excel("data/cdna_org2.xlsx")
    
    inputResp2$logo<-NULL; inputResp2$intro0<-NULL; inputResp2$intro0a<-NULL; inputResp2$url_widget2<-NULL; inputResp2$intro1a<-NULL; inputResp2$nama<-NULL; inputResp2$institusi<-NULL
    inputResp2$jabatan<-NULL; inputResp2$tanggal<-NULL; inputResp2$introOrganisasi<-NULL; inputResp2$introperangkat1<-NULL; inputResp2$introsdm1<-NULL; inputResp2$introteknologi1<-NULL
    inputResp2$`_index`<-NULL; inputResp2$`_validation_status`<-NULL; inputResp2$`_submission_time`<-NULL; inputResp2$`_uuid`<-NULL; inputResp2$`_id`<-NULL; inputResp2$intropenutup1<-NULL; inputResp2$intropenutup<-NULL
    inputResp2$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputResp2$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:44){
      eval(parse(text=paste0("inputResp2$alasan_0",i,"<-NULL")))
    }
    
    inputOrg<-as.data.frame(inputResp2)
    organisasi<- as.data.frame(lapply(inputOrg[,2:length(inputOrg)], as.numeric))
    
    q4.1<-rowSums(organisasi[,1:2]); q4.1<- as.data.frame(q4.1)/2
    q4.2<-rowSums(organisasi[,3:5]); q4.2<- as.data.frame(q4.2)/3
    q4.3<-rowSums(organisasi[,6:7]); q4.3<- as.data.frame(q4.3)/2
    q4.4<-rowSums(organisasi[,8:11]); q4.4<- as.data.frame(q4.4)/4
    q4.5<-rowSums(organisasi[,12:14]); q4.5<- as.data.frame(q4.5)/3
    q4.6<-rowSums(organisasi[,15:16]); q4.6<- as.data.frame(q4.6)/2
    q4.7<-rowSums(organisasi[,17:23]); q4.7<- as.data.frame(q4.7)/7
    q5.1<-rowSums(organisasi[,24:30]); q5.1<- as.data.frame(q5.1)/7
    q5.2<-organisasi$q5.2; q5.3<-organisasi$q5.3
    q5.4<-rowSums(organisasi[,33:34]); q5.4<- as.data.frame(q5.4)/2
    q5.5<-rowSums(organisasi[,35:36]); q5.5<- as.data.frame(q5.5)/2
    q8.1<-rowSums(organisasi[,37:40]); q8.1<- as.data.frame(q8.1)/4
    q8.2<-rowSums(organisasi[,41:43]); q8.2<- as.data.frame(q8.2)/3
    q8.3<-rowSums(organisasi[,44:45]); q8.3<- as.data.frame(q8.3)/2
    valOrganisasi <- cbind(inputOrg$provinsi,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    tempOrganisasi<-as.data.frame(valOrganisasi)
    
    indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
    tes2 <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
    colnames(tes2)<-"Indikator"
    
    #Menampilkan hasil satu provinsi
    tempOrganisasi<-filter(tempOrganisasi,inputOrg$provinsi==input$categoryProvince)
    
    Level4<-rowSums(tempOrganisasi[,2:8])/length(tempOrganisasi[,2:8])
    LevelOrg<-mean(Level4)
    Level5 <- rowSums(tempOrganisasi[,9:13])/length(tempOrganisasi[,9:13])
    LevelSDM<-mean(Level5)
    Level8 <- rowSums(tempOrganisasi[,14:16])/length(tempOrganisasi[,14:16])
    LevelTek<-mean(Level8)
    LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
    gapOrg_gabungan<-5-LevelOrg_gabungan
    Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
    summOrg<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
    colnames(summOrg)<- c("Aspek Penilaian", "Level", "GAP")
    
    Ind4.1<-mean(tempOrganisasi$q4.1); Ind4.2<-mean(tempOrganisasi$q4.2); Ind4.3<-mean(tempOrganisasi$q4.3); Ind4.4<-mean(tempOrganisasi$q4.4); Ind4.5<-mean(tempOrganisasi$q4.5); Ind4.6<-mean(tempOrganisasi$q4.6); Ind4.7<-mean(tempOrganisasi$q4.7)
    Ind5.1<-mean(tempOrganisasi$q5.1); Ind5.2<-mean(tempOrganisasi$q5.2); Ind5.3<-mean(tempOrganisasi$q5.3); Ind5.4<-mean(tempOrganisasi$q5.4); Ind5.5<-mean(tempOrganisasi$q5.5)
    Ind8.1<-mean(tempOrganisasi$q8.1);Ind8.2<-mean(tempOrganisasi$q8.2);Ind8.3<-mean(tempOrganisasi$q8.3)
    tempLevelOrg <- as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
    tempGapOrg<- 5-tempLevelOrg
    graphOrg<-cbind(tes2,tempLevelOrg,tempGapOrg)
    colnames(graphOrg)<-c("Indikator","Level","GAP")
    tablesCDA$summaryOrg <- graphOrg
    summOrg
  })
  
  output$resChartOrg <- renderPlotly({
    graphOrg <- tablesCDA$summaryOrg 
    plot_ly(graphOrg, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Organisasi") 
  })
  
  output$resTblInd <- renderDataTable({
    inputRespInd<-read_excel("data/cdna_ind2.xlsx")
    
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
    
    Indikator <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    Indikator  <- as.data.frame(Indikator)
    
    individu<-filter(individu,inputRespInd$provinsi==input$categoryProvince)
    
    #Hasil per BAB
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-rowSums(individu[,2:5])/length(individu[,2:5])
    Level6<-sum(Level6)/length(individu$`inputRespInd$provinsi`)
    gap6<-5-Level6
    summInd2<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(summInd2)<-c("Aspek Penilaian","Level","GAP")

    ##BAR Chart
    Ind6.1<-mean(individu$Level6.1); Ind6.2<-mean(individu$Level6.2); Ind6.3<-mean(individu$Level6.3); Ind6.4<-mean(individu$Level6.4)
    tempLevelInd <- as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
    tempGapInd <- 5 - tempLevelInd
    graphInd2<-cbind(Indikator,tempLevelInd,tempGapInd)
    colnames(graphInd2)<-c("Indikator","Level","GAP")
    tablesCDA$summaryInd <- graphInd2
    summInd2
  })
  
  output$resChartInd <- renderPlotly({
    graphInd2 <- tablesCDA$summaryInd
    plot_ly(graphInd2, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Individu")
  })
  
  output$resTblSumm <- renderDataTable({
    ##Sistem
    inputResp<-read_excel("data/cdna_sistem2.xlsx")
    
    inputResp$logo<-NULL; inputResp$intro0<-NULL; inputResp$intro0a<-NULL; inputResp$url_widget2<-NULL; inputResp$url_widget2_001<-NULL; inputResp$intro1<-NULL; inputResp$respgeopoint<-NULL
    inputResp$tanggal<-NULL; inputResp$`_index`<-NULL;inputResp$`_validation_status`<-NULL; inputResp$`_submission_time`<-NULL; inputResp$`_uuid`<-NULL; inputResp$`_id`<-NULL
    inputResp$`_respgeopoint_precision`<-NULL; inputResp$`_respgeopoint_altitude`<-NULL; inputResp$intropenutup<-NULL; inputResp$intropenutup2<-NULL; inputResp$introSistem<-NULL
    inputResp$introregulasi<-NULL; inputResp$introintegrasi1<-NULL; inputResp$introproses1<-NULL; inputResp$introdatainfo1<-NULL;inputResp$intropemantauan1<-NULL
    inputResp$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputResp$alasan_00",i,"<-NULL")))
    }
    
    for (i in 9:65){
      eval(parse(text=paste0("inputResp$alasan_0",i,"<-NULL")))
    }
    
    inputResp<-as.data.frame(inputResp)
    sistem<- as.data.frame(lapply(inputResp[,2:length(inputResp)], as.numeric))
    
    q2.5<-rowSums(sistem[,7:8]); q2.5<- as.data.frame(q2.5)/2
    q7.1 <- rowSums(sistem[,14:32]); q7.1<- as.data.frame(q7.1)/19
    q7.2 <- rowSums(sistem[,33:51]); q7.2<- as.data.frame(q7.2)/19
    q7.3<-rowSums(sistem[,52:53]); q7.3<-as.data.frame(q7.3)/2
    q9.1<-rowSums(sistem[,54:58]); q9.1<-as.data.frame(q9.1)/5
    q9.2<-rowSums(sistem[,59:63]); q9.2<-as.data.frame(q9.2)/5
    q9.3<-rowSums(sistem[,64:66]); q9.3<-as.data.frame(q9.3)/3
    
    levelSistem<-cbind(inputResp$provinsi_001,sistem$q1.1,sistem$q1.2,sistem$q2.1,sistem$q2.2,sistem$q2.3, sistem$q2.4, q2.5,sistem$q3.1,sistem$q3.2,sistem$q3.3,sistem$q3.4,sistem$q3.5,q7.1,q7.2,q7.3,q9.1,q9.2,q9.3)
    colnames(levelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3")
    
    # gap_1.1<-5-levelSistem$q1.1; gap_1.2<-5-levelSistem$q1.2; gap_2.1<-5-levelSistem$q2.1; gap_2.2<-5-levelSistem$q2.2; gap_2.3<-5-levelSistem$q2.3; gap_2.4<-5-levelSistem$q2.4; gap_2.5<-5-levelSistem$q2.5
    # gap_3.1<-5-levelSistem$q3.1; gap_3.2<-5-levelSistem$q3.2; gap_3.3<-5-levelSistem$q3.3; gap_3.4<-5-levelSistem$q3.4; gap_3.5<-5-levelSistem$q3.5
    # gap_7.1<-5-levelSistem$q7.1; gap_7.2<-5-levelSistem$q7.2; gap_7.3<-5-levelSistem$q7.3; gap_9.1<-5-levelSistem$q9.1; gap_9.2<-5-levelSistem$q9.2; gap_9.3<-5-levelSistem$q9.3
    # valGAP<-cbind(gap_1.1,gap_1.2,gap_2.1,gap_2.2,gap_2.3,gap_2.4,gap_2.5,gap_3.1,gap_3.2,gap_3.3,gap_3.4,gap_3.5,gap_7.1,gap_7.2,gap_7.3,gap_9.1,gap_9.2,gap_9.3)
    # val_Sistem<-cbind(levelSistem,valGAP)
    tempSistem<-as.data.frame((val_Sistem))
    
    indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
    tes <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))
    
    #Menampilkan hasil satu responden
    tempSistem<-filter(tempSistem,Provinsi==input$categoryProvince)
    
    #Hasil per BAB
    Indikator_Penilaian<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-mean(as.numeric(tempSistem[2:3])); LevelInt<-mean(as.numeric(tempSistem[4:8])); LevelProses<-mean(as.numeric(tempSistem[9:13])); LevelData<-mean(as.numeric(tempSistem[14:16])); LevelPEP<-mean(as.numeric(tempSistem[17:19]))
    LevelSistem<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    # gapReg<-mean(as.numeric(tempSistem[20:21])); gapInt<-mean(as.numeric(tempSistem[22:26])); gapProses<-mean(as.numeric(tempSistem[27:32])); gapData<-mean(as.numeric(tempSistem[33:35])); gapPEP<-mean(as.numeric(tempSistem[36:37]))
    # GAPSistem2<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
    GAPSistem<-5-LevelSistem
    summSistem<-as.data.frame(cbind(Indikator_Penilaian, LevelSistem, GAPSistem))
    colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")
    
    ##Organisasi
    inputResp2<-read_excel("data/cdna_org2.xlsx")
    
    inputResp2$logo<-NULL; inputResp2$intro0<-NULL; inputResp2$intro0a<-NULL; inputResp2$url_widget2<-NULL; inputResp2$intro1a<-NULL; inputResp2$nama<-NULL; inputResp2$institusi<-NULL
    inputResp2$jabatan<-NULL; inputResp2$tanggal<-NULL; inputResp2$introOrganisasi<-NULL; inputResp2$introperangkat1<-NULL; inputResp2$introsdm1<-NULL; inputResp2$introteknologi1<-NULL
    inputResp2$`_index`<-NULL; inputResp2$`_validation_status`<-NULL; inputResp2$`_submission_time`<-NULL; inputResp2$`_uuid`<-NULL; inputResp2$`_id`<-NULL; inputResp2$intropenutup1<-NULL; inputResp2$intropenutup<-NULL
    inputResp2$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputResp2$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:44){
      eval(parse(text=paste0("inputResp2$alasan_0",i,"<-NULL")))
    }
    
    inputOrg<-as.data.frame(inputResp2)
    organisasi<- as.data.frame(lapply(inputOrg[,2:length(inputOrg)], as.numeric))
    
    q4.1<-rowSums(organisasi[,1:2]); q4.1<- as.data.frame(q4.1)/2
    q4.2<-rowSums(organisasi[,3:5]); q4.2<- as.data.frame(q4.2)/3
    q4.3<-rowSums(organisasi[,6:7]); q4.3<- as.data.frame(q4.3)/2
    q4.4<-rowSums(organisasi[,8:11]); q4.4<- as.data.frame(q4.4)/4
    q4.5<-rowSums(organisasi[,12:14]); q4.5<- as.data.frame(q4.5)/3
    q4.6<-rowSums(organisasi[,15:16]); q4.6<- as.data.frame(q4.6)/2
    q4.7<-rowSums(organisasi[,17:23]); q4.7<- as.data.frame(q4.7)/7
    q5.1<-rowSums(organisasi[,24:30]); q5.1<- as.data.frame(q5.1)/7
    q5.2<-organisasi$q5.2; q5.3<-organisasi$q5.3
    q5.4<-rowSums(organisasi[,33:34]); q5.4<- as.data.frame(q5.4)/2
    q5.5<-rowSums(organisasi[,35:36]); q5.5<- as.data.frame(q5.5)/2
    q8.1<-rowSums(organisasi[,37:40]); q8.1<- as.data.frame(q8.1)/4
    q8.2<-rowSums(organisasi[,41:43]); q8.2<- as.data.frame(q8.2)/3
    q8.3<-rowSums(organisasi[,44:45]); q8.3<- as.data.frame(q8.3)/2
    valOrganisasi <- cbind(inputOrg$provinsi,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    tempOrganisasi<-as.data.frame(valOrganisasi)
    
    indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
    tes2 <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
    colnames(tes2)<-"Indikator"
    
    #Menampilkan hasil satu provinsi
    tempOrganisasi<-filter(tempOrganisasi,inputOrg$provinsi==input$categoryProvince)
    
    Level4<-rowSums(tempOrganisasi[,2:8])/length(tempOrganisasi[,2:8])
    LevelOrg<-mean(Level4)
    Level5 <- rowSums(tempOrganisasi[,9:13])/length(tempOrganisasi[,9:13])
    LevelSDM<-mean(Level5)
    Level8 <- rowSums(tempOrganisasi[,14:16])/length(tempOrganisasi[,14:16])
    LevelTek<-mean(Level8)
    LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
    gapOrg_gabungan<-5-LevelOrg_gabungan
    Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
    summOrg<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
    colnames(summOrg)<- c("Aspek Penilaian", "Level", "GAP")
    
    ##Individu
    inputRespInd<-read_excel("data/cdna_ind2.xlsx")
    
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
    
    Indikator <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    Indikator  <- as.data.frame(Indikator)
    
    individu<-filter(individu,inputRespInd$provinsi==input$categoryProvince)
    
    #Hasil per BAB
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-rowSums(individu[,2:5])/length(individu[,2:5])
    Level6<-sum(Level6)/length(individu$`inputRespInd$provinsi`)
    gap6<-5-Level6
    summInd2<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(summInd2)<-c("Aspek Penilaian","Level","GAP")
    
    ##Gabungan
    summary<-as.data.frame(rbind(summSistem, summOrg, summInd2))
    summary$`Aspek Penilaian`<-NULL
    aspek<-c("Regulasi/peraturan daerah","Integrasi dalam Perencanaan Pembangunan Daerah", "Proses", "Data dan Informasi", "Pemantauan, Evaluasi, dan Pelaporan","Organisasi","Sumber Daya Manusia - Organisasi", "Teknologi", "Sumber Daya Manusia - Individu")
    summary<-cbind(aspek,summary)
    finalGAP<-as.data.frame(5-as.numeric(summary$Level))
    summary$Level<-as.numeric(summary$Level)
    summary<-as.data.frame(cbind(summary,finalGAP))
    summary$GAP<-NULL
    colnames(summary)<-c("Aspek Penilaian", "Level", "GAP")
    rownames(summary)<-1:9
    tablesCDA$allSummary <- summary
    summary
  })
  
  output$resChartSumm <- renderPlotly({
    ###BAR CHART Summary
    summary<-tablesCDA$allSummary
    plot_ly(summary, x=~`Aspek Penilaian`, y=~Level, type='bar', name='Level') %>%
      add_trace(y=~GAP, name='GAP') %>%
      layout(yaxis = list(title='Nilai'), barmode='stack', title="Level dan Gap Penilaian Kapasitas Semua Tingkat")
  })
  
  output$koboMap <- renderLeaflet({
    long_lat_data<-read_excel("data/CDNA_SistemOrganisasi.xlsx")
    long_lat_data$`_respgeopoint_latitude` <- as.numeric(long_lat_data$`_respgeopoint_latitude`)
    long_lat_data$`_respgeopoint_longitude` <- as.numeric(long_lat_data$`_respgeopoint_longitude`)
    kobo_data <- subset(long_lat_data, select=c(`_respgeopoint_latitude`, `_respgeopoint_longitude`, provinsi_001))
    colnames(kobo_data) = c("lat", "long", "prov")
    # leaflet(data = kobo_data) %>% addTiles() %>%
    #   addMarkers(~`_respgeopoint_longitude`, ~`_respgeopoint_latitude`, popup = ~as.character(provinsi_001), label = ~as.character(provinsi_001)) 
    # 
    leaflet(data = kobo_data) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions()
    )
    
  })
  
  # # Export data
  # observeEvent(input$exportInd, {
  #   saveData(tablesCDA$tableIndividu)
  # })

  
}

###*run the apps#### 
shinyApp(ui = ui, server = server)

