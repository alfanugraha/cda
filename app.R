###*initiate library##
library(shiny)
#library(shinythemes)
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
    inputResp<-read_excel("data/CDNA_SistemOrganisasi.xlsx")
    
    inputResp$intro1<-NULL
    inputResp$acknowledge1<-NULL
    inputResp$introSistem<-NULL
    inputResp$inroRegulasi<-NULL
    inputResp$introIntergrasi<-NULL
    inputResp$introProses<-NULL
    inputResp$introDataInfo<-NULL
    inputResp$introPemantauan<-NULL
    inputResp$introOrganisasi<-NULL
    inputResp$introPerangkat<-NULL
    inputResp$introSDM<-NULL
    inputResp$introTeknologi<-NULL
    inputResp$`_Terima_kasih_ata_nekan_tombol_berikut`<-NULL
    
    inputResp$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputResp$alasan_00",i,"<-NULL")))
    }
    
    for (i in 9:99){
      eval(parse(text=paste0("inputResp$alasan_0",i,"<-NULL")))
    }
    
    for (i in 100:110){
      eval(parse(text=paste0("inputResp$alasan_",i,"<-NULL")))
    }
    
    inputResp<-as.data.frame(inputResp)
    
    ###SISTEM###
    sistem<- as.data.frame(lapply(inputResp[,11:76], as.numeric))
    
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
    colnames(tes)<-"Indikator"
    result_Sistem <-cbind(tes,tempSistem)
    
    #Menampilkan hasil satu responden
    i=1
    #Hasil per Kapasistas Fungsional
    result_Sistem[[i]]<-cbind(result_Sistem$Indikator,result_Sistem[i+1])
    
    #Hasil per BAB
    Indikator_Penilaian<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-mean(tempSistem[1:2,i])
    LevelInt<-mean(tempSistem[3:7,i])
    LevelProses<-mean(tempSistem[8:12,i])
    LevelData<-mean(tempSistem[13:15,i])
    LevelPEP<-mean(tempSistem[16:18,i])
    LevelSistem<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    gapReg<-mean(tempSistem[19:20,i])
    gapInt<-mean(tempSistem[21:25,i])
    gapProses<-mean(tempSistem[26:30,i])
    gapData<-mean(tempSistem[31:33,i])
    gapPEP<-mean(tempSistem[34:35,i])
    GAPSistem<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
    summSistem<-as.data.frame(cbind(Indikator_Penilaian, LevelSistem, GAPSistem))
    colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")
    rownames(summSistem)<-c("1","2","3","4","5")
    
    ##BAR CHART
    dataSistem<-as.data.frame(val_Sistem)
    graphSistem<-cbind(tes,t((val_Sistem[i,1:18])),t(val_Sistem[i,19:36]))
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
    organisasi<- as.data.frame(lapply(inputResp[,77:121], as.numeric))
    
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
    i=1
    #Hasil per Kapasistas Fungsional
    result_Organisasi[[i]]<-cbind(result_Organisasi$Indikator,result_Organisasi[i+1])
    
    #Hasil per BAB
    Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
    LevelOrg<-mean(tempOrganisasi[1:7,i])
    LevelSDM<-mean(tempOrganisasi[8:12,i])
    LevelTek<-mean(tempOrganisasi[13:15,i])
    LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
    gapOrg<-mean(tempOrganisasi[16:22,i])
    gapSDM<-mean(tempOrganisasi[23:27,i])
    gapTek<-mean(tempOrganisasi[28:30,i])
    gapOrg_gabungan<-as.data.frame(t(cbind(gapOrg,gapSDM,gapTek)))
    summOrg<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
    colnames(summOrg)<- c("Aspek Penilaian", "Level", "GAP")
    rownames(summOrg)<- c("1","2","3")
    
    ##BAR CHART
    dataOrg<-as.data.frame(val_Organisasi)
    graphOrg<-cbind(tes2,t((val_Organisasi[i,1:15])),t(val_Organisasi[i,16:30]))
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
    #Baca file excel dari KoBo
    inputRespInd<-read_excel("data/CDNA_Individu.xlsx")
    
    inputRespInd$intro1<-NULL
    inputRespInd$acknowledge1<-NULL
    inputRespInd$introIndividu<-NULL
    inputRespInd$introSDM2<-NULL
    inputRespInd$`_Terimakasih_callr_nekan_tombol_berikut`<-NULL
    inputRespInd$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputRespInd$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:22){
      eval(parse(text=paste0("inputRespInd$alasan_0",i,"<-NULL")))
    }
    
    inputRespInd<-as.data.frame(inputRespInd)
    # valResp<-unlist(inputRespInd[,15:37])
    valResp<- as.data.frame(lapply(inputRespInd[,15:37], as.numeric))
    
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
    i=1
    #Hasil per Kapasistas Fungsional
    result_Individu[[i]]<-cbind(result_Individu$Indikator,result_Individu[i+1])
    
    #Hasil per BAB
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    LevelSDM_Ind<-mean(individu[1:4,i])
    GAP_Ind<-mean(individu[5:8,i])
    summInd<-as.data.frame(cbind(Indikator_Penilaian_Ind, LevelSDM_Ind, GAP_Ind))
    colnames(summInd)<-c("Aspek Penilaian","Level","GAP")
    
    ##BAR Chart
    dataInd<-as.data.frame(val_Individu)
    graphInd<-cbind(Indikator,t((val_Individu[i,1:4])),t(val_Individu[i,5:8]))
    colnames(graphInd)<-c("Indikator","Level","GAP")
    tablesCDA$summaryInd <- graphInd
    summInd
  })
  
  output$resChartInd <- renderPlotly({
    graphInd <- tablesCDA$summaryInd
    plot_ly(graphInd, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Individu")
  })
  
  output$resTblSumm <- renderDataTable({
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
    graphSumm <- summary
    tablesCDA$allSummary <- graphSumm
    summary
 
  })
  
  output$resChartSumm <- renderPlotly({
    ###BAR CHART Summary
    graphSumm<-tablesCDA$allSummary
    plot_ly(summary, x=~Aspek, y=~Level, type='bar', name='Level') %>%
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

