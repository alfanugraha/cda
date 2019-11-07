###*initiate library##
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
library(ggplot2)
library(plotly)

library(leaflet)
library(readxl)
library(magrittr)
library(rlang)
library(dplyr)
library(DT)

###*setup dashboard page####
ui <- source('interfaceNew.R')

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  tablesCDA <- reactiveValues(summarySystem=data.frame(),summaryOrg=data.frame(), summaryInd=data.frame(), allSummary=data.frame())
  
  observeEvent(input$inputSetting,
               showModal(ui=modalDialog("Data berhasil tersimpan", footer = modalButton("Close")), session=session))
  ###SISTEM###
  output$resTblSys <- renderDataTable({
    
    inputSys<-readRDS("data/fileSys")
    
    inputSys$intro00<-NULL;inputSys$intro0a2<-NULL; inputSys$logo0<-NULL; inputSys$logo<-NULL; inputSys$intro0<-NULL; inputSys$intro0a<-NULL; inputSys$url_widget2<-NULL; inputSys$intro1<-NULL;
    inputSys$X_index<-NULL;inputSys$X_validation_status<-NULL; inputSys$X_submission_time<-NULL; inputSys$X_uuid<-NULL; inputSys$X_id<-NULL
    inputSys$intropenutup<-NULL; inputSys$intropenutup2<-NULL; inputSys$introSistem<-NULL; inputSys$introregulasi<-NULL; inputSys$introintegrasi1<-NULL; 
    inputSys$introproses1<-NULL; inputSys$introdatainfo1<-NULL;inputSys$intropemantauan1<-NULL
    inputSys$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputSys$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:68){
      eval(parse(text=paste0("inputSys$alasan_0",i,"<-NULL")))
    }
    
    inputSys<-as.data.frame(inputSys)
    inputSys[is.na(inputSys)]<-3
    sistem<- as.data.frame(lapply(inputSys[,3:length(inputSys)], as.numeric))
    
    q2.5<-rowSums(sistem[,9:10]); q2.5<- as.data.frame(q2.5)/2
    q7.1 <- rowSums(sistem[,14:32]); q7.1<- as.data.frame(q7.1)/19
    q7.2 <- rowSums(sistem[,33:51]); q7.2<- as.data.frame(q7.2)/19
    q7.3<-rowSums(sistem[,52:53]); q7.3<-as.data.frame(q7.3)/2
    q9.1<-rowSums(sistem[,54:58]); q9.1<-as.data.frame(q9.1)/5
    q9.2<-rowSums(sistem[,59:64]); q9.2<-as.data.frame(q9.2)/6
    q9.3<-rowSums(sistem[,65:67]); q9.3<-as.data.frame(q9.3)/3
    q9.4<-rowSums(sistem[,68:69]); q9.4<-as.data.frame(q9.4)/2
    
    levelSistem<-cbind(inputSys$provinsi_001,sistem$q1.1,sistem$q1.2,sistem$q2.1,sistem$q2.2,sistem$q2.3, sistem$q2.4, q2.5,sistem$q3.1,sistem$q3.2,sistem$q3.3,sistem$q3.4,sistem$q3.5,q7.1,q7.2,q7.3,q9.1,q9.2,q9.3,q9.4)
    colnames(levelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")
    
    gap_1.1<-5-levelSistem$q1.1; gap_1.2<-5-levelSistem$q1.2; gap_2.1<-5-levelSistem$q2.1; gap_2.2<-5-levelSistem$q2.2; gap_2.3<-5-levelSistem$q2.3; gap_2.4<-5-levelSistem$q2.4; gap_2.5<-5-levelSistem$q2.5
    gap_3.1<-5-levelSistem$q3.1; gap_3.2<-5-levelSistem$q3.2; gap_3.3<-5-levelSistem$q3.3; gap_3.4<-5-levelSistem$q3.4; gap_3.5<-5-levelSistem$q3.5
    gap_7.1<-5-levelSistem$q7.1; gap_7.2<-5-levelSistem$q7.2; gap_7.3<-5-levelSistem$q7.3; gap_9.1<-5-levelSistem$q9.1; gap_9.2<-5-levelSistem$q9.2; gap_9.3<-5-levelSistem$q9.3; gap_9.4<-5-levelSistem$q9.4
    valGAP<-cbind(gap_1.1,gap_1.2,gap_2.1,gap_2.2,gap_2.3,gap_2.4,gap_2.5,gap_3.1,gap_3.2,gap_3.3,gap_3.4,gap_3.5,gap_7.1,gap_7.2,gap_7.3,gap_9.1,gap_9.2,gap_9.3,gap_9.4)
    valSistem<-cbind(levelSistem,valGAP)
    tempSistem<-as.data.frame((valSistem))
    
    file_indSys<- read.table("init/system.csv", header=TRUE, sep=",")
    indikatorSys <- as.data.frame(unique(file_indSys$Kapasitas_Fungsional))
    
    #Menampilkan hasil satu responden
    tempSistem<-filter(tempSistem,Provinsi==input$categoryProvince)
    # tempSistem<-filter(tempSistem,Provinsi=="Nusa Tenggara Barat")
    
    #Hasil per Aspek
    Indikator_Penilaian<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-mean(as.matrix(tempSistem[,2:3])); LevelInt<-mean(as.matrix(tempSistem[4:8])); LevelProses<-mean(as.matrix(tempSistem[9:13])); LevelData<-mean(as.matrix(tempSistem[14:16])); LevelPEP<-mean(as.matrix(tempSistem[17:20]))
    allLevelSys<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    # gapReg<-mean(as.matrix(tempSistem[21:22])); gapInt<-mean(as.matrix(tempSistem[23:27])); gapProses<-mean(as.matrix(tempSistem[28:32])); gapData<-mean(as.matrix(tempSistem[33:35])); gapPEP<-mean(as.matrix(tempSistem[36:39]))
    gapReg<-5-LevelReg; gapInt<-5-LevelInt; gapProses<-5-LevelProses; gapData<-5-LevelData; gapPEP<-5-LevelPEP
    allGapSys<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
    tingkatSys<-as.data.frame(cbind(Indikator_Penilaian, allLevelSys, allGapSys))
    colnames(tingkatSys)<-c("Aspek Penilaian","Level","GAP")
    
    ##BAR CHART
    t_tempSistem <- t(tempSistem[2:length(tempSistem)])
    provSys <- rowMeans(t_tempSistem)
    graphSistem<-cbind(indikatorSys,provSys[1:19],provSys[20:38])
    colnames(graphSistem)<-c("Indikator","Level","GAP")
    tablesCDA$summarySystem <- graphSistem
    
    datatable(tingkatSys,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartSys <- renderPlotly({
    graphSistem <- tablesCDA$summarySystem  
    plot_ly(graphSistem, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Sistem") 
  })
  
  ###ORGANISASI###
  output$resTblOrg <- renderDataTable({
    inputOrg<-readRDS("data/fileOrg")
    
    inputOrg$intro00<-NULL; inputOrg$intro0a2<-NULL; inputOrg$logo0<-NULL; inputOrg$logo<-NULL; inputOrg$intro0<-NULL; inputOrg$intro0a<-NULL
    inputOrg$url_widget2<-NULL; inputOrg$intro1a<-NULL; inputOrg$jabatan<-NULL
    #inputOrg$nama<-NULL; inputOrg$institusi<-NULL; inputOrg$tanggal<-NULL; 
    inputOrg$introOrganisasi<-NULL; inputOrg$introperangkat1<-NULL; inputOrg$introsdm1<-NULL; inputOrg$introteknologi1<-NULL
    inputOrg$X_index<-NULL; inputOrg$X_validation_status<-NULL; inputOrg$X_submission_time<-NULL; inputOrg$X_uuid<-NULL; inputOrg$X_id<-NULL; inputOrg$intropenutup1<-NULL; inputOrg$intropenutup<-NULL
    inputOrg$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputOrg$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:44){
      eval(parse(text=paste0("inputOrg$alasan_0",i,"<-NULL")))
    }
    
    inputOrg<-as.data.frame(inputOrg)
    organisasi<- as.data.frame(lapply(inputOrg[,5:length(inputOrg)], as.numeric))
    
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
    valOrganisasi <- cbind(inputOrg$provinsi,inputOrg$institusi,inputOrg$nama,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    tempOrganisasi<-as.data.frame(valOrganisasi)
    
    file_indOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
    indikatorOrg <- as.data.frame(unique(file_indOrg$Kapasitas_Fungsional))
    colnames(indikatorOrg)<-"Indikator"
    
    #Menampilkan hasil OPD
    # tempOrganisasi<-filter(tempOrganisasi,inputOrg$provinsi==input$categoryProvince & inputOrg$institusi==input$selectizeInstitution) #buat field insitution
    tempOrganisasi<-filter(tempOrganisasi,inputOrg$provinsi=="Bengkulu" & inputOrg$institusi=="Bappeda provinsi Bengkulu")
    
    #Hasi per Aspek
    Level4<-rowSums(tempOrganisasi[,4:10])/length(tempOrganisasi[,4:10])
    LevelOrg<-mean(Level4)
    Level5 <- rowSums(tempOrganisasi[,11:15])/length(tempOrganisasi[,11:15])
    LevelSDM<-mean(Level5)
    Level8 <- rowSums(tempOrganisasi[,16:18])/length(tempOrganisasi[,16:18])
    LevelTek<-mean(Level8)
    LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
    gapOrg_gabungan<-5-LevelOrg_gabungan
    Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
    tingkatOrg<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
    colnames(tingkatOrg)<- c("Aspek Penilaian", "Level", "GAP")
    
    #Bar Chart
    Ind4.1<-mean(tempOrganisasi$q4.1); Ind4.2<-mean(tempOrganisasi$q4.2); Ind4.3<-mean(tempOrganisasi$q4.3); Ind4.4<-mean(tempOrganisasi$q4.4); Ind4.5<-mean(tempOrganisasi$q4.5); Ind4.6<-mean(tempOrganisasi$q4.6); Ind4.7<-mean(tempOrganisasi$q4.7)
    Ind5.1<-mean(tempOrganisasi$q5.1); Ind5.2<-mean(tempOrganisasi$q5.2); Ind5.3<-mean(tempOrganisasi$q5.3); Ind5.4<-mean(tempOrganisasi$q5.4); Ind5.5<-mean(tempOrganisasi$q5.5)
    Ind8.1<-mean(tempOrganisasi$q8.1);Ind8.2<-mean(tempOrganisasi$q8.2);Ind8.3<-mean(tempOrganisasi$q8.3)
    tempLevelOrg <- as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
    tempGapOrg<- 5-tempLevelOrg
    graphOrg<-cbind(indikatorOrg,tempLevelOrg,tempGapOrg)
    colnames(graphOrg)<-c("Indikator","Level","GAP")
    tablesCDA$summaryOrg <- graphOrg
    
    datatable(tingkatOrg,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartOrg <- renderPlotly({
    graphOrg <- tablesCDA$summaryOrg 
    plot_ly(graphOrg, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Organisasi") 
  })
  
  output$selectizeInstitution <- renderUI({
    inputOrg<-readRDS("data/fileOrg")
    
    inputOrg$intro00<-NULL; inputOrg$intro0a2<-NULL; inputOrg$logo0<-NULL; inputOrg$logo<-NULL; inputOrg$intro0<-NULL; inputOrg$intro0a<-NULL
    inputOrg$url_widget2<-NULL; inputOrg$intro1a<-NULL; inputOrg$jabatan<-NULL
    #inputOrg$nama<-NULL; inputOrg$institusi<-NULL; inputOrg$tanggal<-NULL; 
    inputOrg$introOrganisasi<-NULL; inputOrg$introperangkat1<-NULL; inputOrg$introsdm1<-NULL; inputOrg$introteknologi1<-NULL
    inputOrg$X_index<-NULL; inputOrg$X_validation_status<-NULL; inputOrg$X_submission_time<-NULL; inputOrg$X_uuid<-NULL; inputOrg$X_id<-NULL; inputOrg$intropenutup1<-NULL; inputOrg$intropenutup<-NULL
    inputOrg$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputOrg$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:44){
      eval(parse(text=paste0("inputOrg$alasan_0",i,"<-NULL")))
    }
    inputOrg<-as.data.frame(inputOrg)
    
    selectizeInput('selectizeInstitution', 'Pilih OPD Anda', choices=list(
      OPD=as.character(sort(inputOrg$institusi))
    ), multiple=FALSE)
  })
  
  ###INDIVIDU###
  output$resTblInd <- renderDataTable({
    inputInd<-readRDS("data/fileInd")
    
    inputInd$intro00<-NULL; inputInd$intro0a2<-NULL; inputInd$logo0<-NULL; inputInd$logo<-NULL; inputInd$intro0<-NULL; inputInd$intro0a<-NULL; inputInd$intro1a<-NULL
    inputInd$gender<-NULL; inputInd$jabatan<-NULL; inputInd$akun <- NULL; #inputInd$tanggal<-NULL
    inputInd$introIndividu<-NULL; inputInd$introSDM2<-NULL; inputInd$X_index<-NULL; inputInd$X_validation_status<-NULL
    inputInd$X_submission_time<-NULL; inputInd$X_uuid<-NULL; inputInd$X_id<-NULL; inputInd$intropenutup<-NULL
    inputInd$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputInd$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:22){
      eval(parse(text=paste0("inputInd$alasan_0",i,"<-NULL")))
    }
    
    inputInd<-as.data.frame(inputInd)
    inputInd<-na.omit(inputInd)
    individu<- as.data.frame(lapply(inputInd[,7:length(inputInd)], as.numeric))
    
    q6.1<-rowSums(individu[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(individu[,3:11]); q6.2<-as.data.frame(q6.2)/9
    q6.3<-rowSums(individu[,12:20]); q6.3<-as.data.frame(q6.3)/9
    q6.4<-rowSums(individu[,21:23]); q6.4<-as.data.frame(q6.4)/3
    valInd<-cbind(inputInd$provinsi,inputInd$nama, q6.1,q6.2,q6.3,q6.4)
    tempIndividu<-as.data.frame(valInd)
    
    indikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    indikatorInd  <- as.data.frame(indikatorInd)
    
    tempIndividu<-filter(inputInd,inputInd$provinsi==input$categoryProvince & inputInd$nama==input$selectizeName)
    tempIndividu<-filter(inputInd,inputInd$provinsi=="Bengkulu" & inputInd$nama=="Yudan")
    
    #Hasil per Aspek
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-rowMeans(tempIndividu[7:length(tempIndividu)])
    gap6<-5-Level6
    tingkatInd<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(tingkatInd)<-c("Aspek Penilaian","Level","GAP")
    
    ##BAR Chart
    valInd <- filter(valInd,inputInd$provinsi=="Bengkulu" & inputInd$nama=="Yudan")
    Ind6.1<-mean(valInd$q6.1); Ind6.2<-mean(valInd$q6.2); Ind6.3<-mean(valInd$q6.3); Ind6.4<-mean(valInd$q6.4)
    tempLevelInd <- as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
    tempGapInd <- 5 - tempLevelInd
    graphInd<-cbind(indikatorInd,tempLevelInd,tempGapInd)
    colnames(graphInd)<-c("Indikator","Level","GAP")
    tablesCDA$summaryInd <- graphInd
    
    datatable(tingkatInd,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartInd <- renderPlotly({
    graphInd <- tablesCDA$summaryInd
    plot_ly(graphInd, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Individu")
  })
  
  output$selectizeName <- renderUI({
    inputInd<-readRDS("data/fileInd")
    
    inputInd$intro00<-NULL; inputInd$intro0a2<-NULL; inputInd$logo0<-NULL; inputInd$logo<-NULL; inputInd$intro0<-NULL; inputInd$intro0a<-NULL; inputInd$intro1a<-NULL
    inputInd$gender<-NULL; inputInd$jabatan<-NULL; inputInd$akun <- NULL; #inputInd$tanggal<-NULL
    inputInd$introIndividu<-NULL; inputInd$introSDM2<-NULL; inputInd$X_index<-NULL; inputInd$X_validation_status<-NULL
    inputInd$X_submission_time<-NULL; inputInd$X_uuid<-NULL; inputInd$X_id<-NULL; inputInd$intropenutup<-NULL
    inputInd$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("inputInd$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:22){
      eval(parse(text=paste0("inputInd$alasan_0",i,"<-NULL")))
    }
    inputInd<-as.data.frame(inputInd)
    inputInd<-na.omit(inputInd)
    
    selectizeInput('selectizeName', 'Pilih nama Anda', choices=list(
      Nama=as.character(sort(inputInd$nama))
    ), multiple=FALSE)
  })
  
  ###RANGKUMAN###
  summInputSys<-readRDS("data/fileSys")
  summInputOrg<-readRDS("data/fileOrg")
  summInputInd<-readRDS("data/fileInd")
  
  output$resTblSumm <- renderDataTable({
    ##Sistem
    # summInputSys<-readRDS("data/fileSys")
    
    summInputSys$intro00<-NULL;summInputSys$intro0a2<-NULL; summInputSys$logo0<-NULL; summInputSys$logo<-NULL; summInputSys$intro0<-NULL; summInputSys$intro0a<-NULL; summInputSys$url_widget2<-NULL; summInputSys$intro1<-NULL;
    summInputSys$X_index<-NULL;summInputSys$X_validation_status<-NULL; summInputSys$X_submission_time<-NULL; summInputSys$X_uuid<-NULL; summInputSys$X_id<-NULL
    summInputSys$intropenutup<-NULL; summInputSys$intropenutup2<-NULL; summInputSys$introSistem<-NULL; summInputSys$introregulasi<-NULL; summInputSys$introintegrasi1<-NULL; 
    summInputSys$introproses1<-NULL; summInputSys$introdatainfo1<-NULL;summInputSys$intropemantauan1<-NULL
    summInputSys$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("summInputSys$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:68){
      eval(parse(text=paste0("summInputSys$alasan_0",i,"<-NULL")))
    }
    
    summInputSys<-as.data.frame(summInputSys)
    summInputSys[is.na(summInputSys)]<-3
    summSys<- as.data.frame(lapply(summInputSys[,3:length(summInputSys)], as.numeric))
    
    q2.5<-rowSums(summSys[,9:10]); q2.5<-as.data.frame(q2.5)/2
    q7.1<-rowSums(summSys[,14:32]); q7.1<-as.data.frame(q7.1)/19
    q7.2<-rowSums(summSys[,33:51]); q7.2<-as.data.frame(q7.2)/19
    q7.3<-rowSums(summSys[,52:53]); q7.3<-as.data.frame(q7.3)/2
    q9.1<-rowSums(summSys[,54:58]); q9.1<-as.data.frame(q9.1)/5
    q9.2<-rowSums(summSys[,59:64]); q9.2<-as.data.frame(q9.2)/6
    q9.3<-rowSums(summSys[,65:67]); q9.3<-as.data.frame(q9.3)/3
    q9.4<-rowSums(summSys[,68:69]); q9.4<-as.data.frame(q9.4)/2
    
    summLevelSistem<-cbind(summInputSys$provinsi_001,summSys$q1.1,summSys$q1.2,summSys$q2.1,summSys$q2.2,summSys$q2.3, summSys$q2.4, q2.5,summSys$q3.1,summSys$q3.2,summSys$q3.3,summSys$q3.4,summSys$q3.5,q7.1,q7.2,q7.3,q9.1,q9.2,q9.3,q9.4)
    colnames(summLevelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")
    
    # gap_1.1<-5-levelSistem$q1.1; gap_1.2<-5-levelSistem$q1.2; gap_2.1<-5-levelSistem$q2.1; gap_2.2<-5-levelSistem$q2.2; gap_2.3<-5-levelSistem$q2.3; gap_2.4<-5-levelSistem$q2.4; gap_2.5<-5-levelSistem$q2.5
    # gap_3.1<-5-levelSistem$q3.1; gap_3.2<-5-levelSistem$q3.2; gap_3.3<-5-levelSistem$q3.3; gap_3.4<-5-levelSistem$q3.4; gap_3.5<-5-levelSistem$q3.5
    # gap_7.1<-5-levelSistem$q7.1; gap_7.2<-5-levelSistem$q7.2; gap_7.3<-5-levelSistem$q7.3; gap_9.1<-5-levelSistem$q9.1; gap_9.2<-5-levelSistem$q9.2; gap_9.3<-5-levelSistem$q9.3
    # valGAP<-cbind(gap_1.1,gap_1.2,gap_2.1,gap_2.2,gap_2.3,gap_2.4,gap_2.5,gap_3.1,gap_3.2,gap_3.3,gap_3.4,gap_3.5,gap_7.1,gap_7.2,gap_7.3,gap_9.1,gap_9.2,gap_9.3)
    # val_Sistem<-cbind(levelSistem,valGAP)
    summTempSistem<-as.data.frame((summLevelSistem))
    
    indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
    summIndikatorSys <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))
    
    #Menampilkan hasil satu provinsi
    summTempSistem<-filter(summTempSistem,Provinsi==input$categoryProvince)
    # summTempSistem<-filter(summTempSistem,Provinsi=="Aceh")
    
    #Hasil per Aspek
    Indikator_Penilaian<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-mean(as.matrix(summTempSistem[,2:3])); LevelInt<-mean(as.matrix(summTempSistem[4:8])); LevelProses<-mean(as.matrix(summTempSistem[9:13])); LevelData<-mean(as.matrix(summTempSistem[14:16])); LevelPEP<-mean(as.matrix(summTempSistem[17:20]))
    summ_allLevelSys<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    #gapReg<-mean(as.matrix(tempSistem[21:22])); gapInt<-mean(as.matrix(tempSistem[23:27])); gapProses<-mean(as.matrix(tempSistem[28:32])); gapData<-mean(as.matrix(tempSistem[33:35])); gapPEP<-mean(as.matrix(tempSistem[36:39]))
    gapReg<-5-LevelReg; gapInt<-5-LevelInt; gapProses<-5-LevelProses; gapData<-5-LevelData; gapPEP<-5-LevelPEP
    summ_allGapSys<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
    summSistem<-as.data.frame(cbind(Indikator_Penilaian, summ_allLevelSys, summ_allGapSys))
    colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")
    
    #Hasil per indikator & prioritas
    t_summTempSistem <- t(summTempSistem[2:length(summTempSistem)])
    provSys <- rowMeans(t_summTempSistem)
    prioritasSys<-NULL
    if (provSys<=1) {
      prioritasSys = "Prioritas sangat tinggi"
    } else if (provSys<=2) {
      prioritasSys = "Prioritas tinggi"
    } else if (provSys<=3) {
      prioritasSys = "Prioritas rendah"
    } else {
      prioritasSys = "Tidak prioritas"
    }
    tabelSys<-cbind(summIndikatorSys,provSys, prioritasSys)
    colnames(tabelSys)<-c("Indikator","Level","Prioritas")
    
    ##Organisasi
    # summInputOrg<-readRDS("data/fileOrg")
    
    summInputOrg$intro00<-NULL; summInputOrg$intro0a2<-NULL; summInputOrg$logo0<-NULL; summInputOrg$logo<-NULL; summInputOrg$intro0<-NULL; summInputOrg$intro0a<-NULL
    summInputOrg$url_widget2<-NULL; summInputOrg$intro1a<-NULL; summInputOrg$jabatan<-NULL
    #summInputOrg$nama<-NULL; summInputOrg$institusi<-NULL; summInputOrg$tanggal<-NULL; 
    summInputOrg$introOrganisasi<-NULL; summInputOrg$introperangkat1<-NULL; summInputOrg$introsdm1<-NULL; summInputOrg$introteknologi1<-NULL
    summInputOrg$X_index<-NULL; summInputOrg$X_validation_status<-NULL; summInputOrg$X_submission_time<-NULL; summInputOrg$X_uuid<-NULL; summInputOrg$X_id<-NULL; summInputOrg$intropenutup1<-NULL; summInputOrg$intropenutup<-NULL
    summInputOrg$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("summInputOrg$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:44){
      eval(parse(text=paste0("summInputOrg$alasan_0",i,"<-NULL")))
    }
    
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
    q5.2<-summOrg$q5.2; q5.3<-summOrg$q5.3
    q5.4<-rowSums(summOrg[,33:34]); q5.4<- as.data.frame(q5.4)/2
    q5.5<-rowSums(summOrg[,35:36]); q5.5<- as.data.frame(q5.5)/2
    q8.1<-rowSums(summOrg[,37:40]); q8.1<- as.data.frame(q8.1)/4
    q8.2<-rowSums(summOrg[,41:43]); q8.2<- as.data.frame(q8.2)/3
    q8.3<-rowSums(summOrg[,44:45]); q8.3<- as.data.frame(q8.3)/2
    valOrganisasi <- cbind(summInputOrg$provinsi,summInputOrg$institusi,summInputOrg$nama,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    summTempOrganisasi<-as.data.frame(valOrganisasi)
    
    indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
    summIndikatorOrg <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
    colnames(summIndikatorOrg)<-"Indikator"
    
    #Menampilkan hasil satu provinsi
    summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$provinsi==input$categoryProvince)
    # summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$provinsi=="Aceh")
    
    #Hasil per aspek
    Level4<-rowSums(summTempOrganisasi[,4:10])/length(summTempOrganisasi[,4:10])
    LevelOrg<-mean(Level4)
    Level5 <- rowSums(summTempOrganisasi[,11:15])/length(summTempOrganisasi[,11:15])
    LevelSDM<-mean(Level5)
    Level8 <- rowSums(summTempOrganisasi[,16:18])/length(summTempOrganisasi[,16:18])
    LevelTek<-mean(Level8)
    LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
    gapOrg_gabungan<-5-LevelOrg_gabungan
    Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
    summOrganisasi<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
    colnames(summOrganisasi)<- c("Aspek Penilaian", "Level", "GAP")
    
    #Hasil per indikator dan prioritas
    Ind4.1<-mean(summTempOrganisasi$q4.1); Ind4.2<-mean(summTempOrganisasi$q4.2); Ind4.3<-mean(summTempOrganisasi$q4.3); Ind4.4<-mean(summTempOrganisasi$q4.4); Ind4.5<-mean(summTempOrganisasi$q4.5); Ind4.6<-mean(summTempOrganisasi$q4.6); Ind4.7<-mean(summTempOrganisasi$q4.7)
    Ind5.1<-mean(summTempOrganisasi$q5.1); Ind5.2<-mean(summTempOrganisasi$q5.2); Ind5.3<-mean(summTempOrganisasi$q5.3); Ind5.4<-mean(summTempOrganisasi$q5.4); Ind5.5<-mean(summTempOrganisasi$q5.5)
    Ind8.1<-mean(summTempOrganisasi$q8.1);Ind8.2<-mean(summTempOrganisasi$q8.2);Ind8.3<-mean(summTempOrganisasi$q8.3)
    provOrg <- as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
    prioritasOrg<-NULL
    if (provOrg<=1) {
      prioritasOrg = "Prioritas sangat tinggi"
    } else if (provOrg<=2) {
      prioritasOrg = "Prioritas tinggi"
    } else if (provOrg<=3) {
      prioritasOrg = "Prioritas rendah"
    } else {
      prioritasOrg = "Tidak prioritas"
    }
    tabelOrg<-cbind(summIndikatorOrg,provOrg,prioritasOrg)
    colnames(tabelOrg)<-c("Indikator","Level","Prioritas")
    
    
    ##Individu
    # summInputInd<-readRDS("data/fileInd")
    
    summInputInd$intro00<-NULL; summInputInd$intro0a2<-NULL; summInputInd$logo0<-NULL; summInputInd$logo<-NULL; summInputInd$intro0<-NULL; summInputInd$intro0a<-NULL; summInputInd$intro1a<-NULL
    summInputInd$gender<-NULL; summInputInd$jabatan<-NULL; summInputInd$akun <- NULL; #summInputInd$tanggal<-NULL
    summInputInd$introIndividu<-NULL; summInputInd$introSDM2<-NULL; summInputInd$X_index<-NULL; summInputInd$X_validation_status<-NULL
    summInputInd$X_submission_time<-NULL; summInputInd$X_uuid<-NULL; summInputInd$X_id<-NULL; summInputInd$intropenutup<-NULL
    summInputInd$alasan<-NULL
    for (i in 1:9){
      eval(parse(text=paste0("summInputInd$alasan_00",i,"<-NULL")))
    }
    
    for (i in 10:22){
      eval(parse(text=paste0("summInputInd$alasan_0",i,"<-NULL")))
    }
    
    summInputInd<-as.data.frame(summInputInd)
    summInputInd<-na.omit(summInputInd)
    summInd<- as.data.frame(lapply(summInputInd[,7:length(summInputInd)], as.numeric))
    
    q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
    q6.3<-rowSums(summInd[,12:20]); q6.3<-as.data.frame(q6.3)/9
    q6.4<-rowSums(summInd[,21:23]); q6.4<-as.data.frame(q6.4)/3
    valInd<-cbind(summInputInd$provinsi,q6.1,q6.2,q6.3,q6.4)
    summTempIndividu<-as.data.frame(valInd)
    
    summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    summIndikatorInd  <- as.data.frame(summIndikatorInd)
    
    summTempIndividu<-filter(summTempIndividu,summInputInd$provinsi==input$categoryProvince)
    # summTempIndividu<-filter(summTempIndividu,summInputInd$provinsi=="Aceh")
    
    #Hasil per Aspek
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-mean(as.matrix(summTempIndividu[2:length(summTempIndividu)]))
    gap6<-5-Level6
    summIndividu<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(summIndividu)<-c("Aspek Penilaian","Level","GAP")
    
    #Hasil per indikator dan prioritas
    Ind6.1<-mean(valInd$q6.1); Ind6.2<-mean(valInd$q6.2); Ind6.3<-mean(valInd$q6.3); Ind6.4<-mean(valInd$q6.4)
    provInd <- as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
    prioritasInd <- NULL
    if (provInd<=1) {
      prioritasInd = "Prioritas sangat tinggi"
    } else if (provInd<=2) {
      prioritasInd = "Prioritas tinggi"
    } else if (provInd<=3) {
      prioritasInd = "Prioritas rendah"
    } else {
      prioritasInd = "Tidak prioritas"
    }
    tabelInd<-cbind(summIndikatorInd,provInd,prioritasInd)
    colnames(tabelInd)<-c("Indikator","Level","Prioritas")
    
    ##Tabel Prioritas
    allprioritas <- rbind(tabelSys,tabelOrg,tabelInd)
    prioritas <- allprioritas[order(allprioritas$Level),]
    
    ##Gabungan
    summary<-as.data.frame(rbind(summSistem, summOrganisasi, summIndividu))
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
    
    datatable(prioritas,escape = FALSE, rownames = FALSE)
    
  })
  
  output$resChartSumm <- renderPlotly({
    ###BAR CHART Summary
    summary<-tablesCDA$allSummary
    plot_ly(summary, x=~`Aspek Penilaian`, y=~Level, type='bar', name='Level') %>%
      add_trace(y=~GAP, name='GAP') %>%
      layout(yaxis = list(title='Nilai'), barmode='stack', title="Level dan Gap Penilaian Kapasitas Provinsi")
  })
  
  # output$koboMap <- renderLeaflet({
  #   long_lat_data<-read_excel("data/CDNA_SistemOrganisasi.xlsx")
  #   long_lat_data$`_respgeopoint_latitude` <- as.numeric(long_lat_data$`_respgeopoint_latitude`)
  #   long_lat_data$`_respgeopoint_longitude` <- as.numeric(long_lat_data$`_respgeopoint_longitude`)
  #   kobo_data <- subset(long_lat_data, select=c(`_respgeopoint_latitude`, `_respgeopoint_longitude`, provinsi_001))
  #   colnames(kobo_data) = c("lat", "long", "prov")
  #   # leaflet(data = kobo_data) %>% addTiles() %>%
  #   #   addMarkers(~`_respgeopoint_longitude`, ~`_respgeopoint_latitude`, popup = ~as.character(provinsi_001), label = ~as.character(provinsi_001)) 
  #   # 
  #   leaflet(data = kobo_data) %>% addTiles() %>% addMarkers(
  #     clusterOptions = markerClusterOptions()
  #   )
  #   
  # })
  
  # # Export data
  # observeEvent(input$exportInd, {
  #   saveData(tablesCDA$tableIndividu)
  # })
  
}

###*run the apps#### 
shinyApp(ui = ui, server = server)

