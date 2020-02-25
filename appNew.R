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
library(koboloadeR)

###*setup dashboard page####
ui <- source('interfaceNew.R')

#Mengunduh data secara langsung
dataSistem<-kobo_data_downloader("327419", "cdna2019:Icraf2019!")
dataOrganisasi<-kobo_data_downloader("327585", "cdna2019:Icraf2019!")
dataIndividu<-kobo_data_downloader("327418", "cdna2019:Icraf2019!")

saveRDS(dataSistem, "data/dataSistem")
saveRDS(dataOrganisasi, "data/dataOrganisasi")
saveRDS(dataIndividu, "data/dataIndividu")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  tablesCDA <- reactiveValues(summarySystem=data.frame(),summaryOrg=data.frame(), summaryInd=data.frame(), allSummary=data.frame(), summaryProvInd=data.frame(), summaryProvOrg=data.frame())
  
  observeEvent(input$inputSetting, {
    showModal(ui=modalDialog("Anda berhasil masuk", easyClose = TRUE), session=session)
  })
  ####MENU SISTEM####
  output$resTblSys <- renderDataTable({
    
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
    tempSistem<-filter(tempSistem,Provinsi==input$categoryProvince)
    # tempSistem<-filter(tempSistem,Provinsi=="Aceh")
    
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
    
    ##Membuat bar chart untuk tingkat Sistem####
    t_tempSistem <- t(tempSistem[2:length(tempSistem)])
    provSys <- rowMeans(t_tempSistem)
    provSys<-round(provSys, digits = 2)
    graphSistem<-cbind(indikatorSys,provSys[1:19],provSys[20:38])
    colnames(graphSistem)<-c("Indikator","Level","GAP")
    tablesCDA$summarySystem <- graphSistem
    
    datatable(tingkatSys,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartSys <- renderPlotly({
    graphSistem <- tablesCDA$summarySystem  
    plot_ly(graphSistem, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack') 
    
  })
  
  ####MENU ORGANISASI####
  ## Hasil Analisis setiap OPD ####
  output$resTblOrg <- renderDataTable({
    inputOrganisasi<-readRDS("data/dataOrganisasi")
    inputOrganisasi$`profil/jabatan`<-NULL; inputOrganisasi$`meta/instanceID`<-NULL; inputOrganisasi$`__version__`<-NULL
    inputOrganisasi$`_uuid`<-NULL; inputOrganisasi$`_submission_time`<-NULL; inputOrganisasi$`_tags`<-NULL; inputOrganisasi$`_notes`<-NULL
    
    inputOrganisasi$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan`<-NULL
    inputOrganisasi$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan_001`<-NULL
    
    for (i in 2:4){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat2/alasan_00",i,"`","<-NULL")))
    }
    
    for (i in 5:6){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat3/alasan_00",i,"`","<-NULL")))
    }
    for (i in 7:9){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat4/alasan_00",i,"`","<-NULL")))
    }
    inputOrganisasi$`perangkat1/perangkat4/alasan_010`<-NULL
    
    for (i in 11:13){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 14:15){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 16:22){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat7/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 23:29){
      eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm2/alasan_0",i,"`","<-NULL")))
    }
    
    inputOrganisasi$`sdm1/sdm3/alasan_030`<-NULL
    inputOrganisasi$`sdm1/sdm4/alasan_031`<-NULL
    
    for (i in 32:33){
      eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 34:35){
      eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 36:39){
      eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi2/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 40:42){
      eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi3/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 43:44){
      eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi4/alasan_0",i,"`","<-NULL")))
    }
    
    inputOrganisasi[inputOrganisasi == "n/a"]  <- NA
    inputOrganisasi <- na.omit(inputOrganisasi)
    # inputOrganisasi<-as.data.frame(inputOrganisasi)
    
    organisasi<- as.data.frame(lapply(inputOrganisasi[,5:length(inputOrganisasi)], as.numeric))
    
    q4.1<-rowSums(organisasi[,1:2]); q4.1<- as.data.frame(q4.1)/2
    q4.2<-rowSums(organisasi[,3:5]); q4.2<- as.data.frame(q4.2)/3
    q4.3<-rowSums(organisasi[,6:7]); q4.3<- as.data.frame(q4.3)/2
    q4.4<-rowSums(organisasi[,8:11]); q4.4<- as.data.frame(q4.4)/4
    q4.5<-rowSums(organisasi[,12:14]); q4.5<- as.data.frame(q4.5)/3
    q4.6<-rowSums(organisasi[,15:16]); q4.6<- as.data.frame(q4.6)/2
    q4.7<-rowSums(organisasi[,17:23]); q4.7<- as.data.frame(q4.7)/7
    q5.1<-rowSums(organisasi[,24:30]); q5.1<- as.data.frame(q5.1)/7
    q5.2<-organisasi$sdm1.sdm3.q5.2; q5.3<-organisasi$sdm1.sdm4.q5.3
    q5.4<-rowSums(organisasi[,33:34]); q5.4<- as.data.frame(q5.4)/2
    q5.5<-rowSums(organisasi[,35:36]); q5.5<- as.data.frame(q5.5)/2
    q8.1<-rowSums(organisasi[,37:40]); q8.1<- as.data.frame(q8.1)/4
    q8.2<-rowSums(organisasi[,41:43]); q8.2<- as.data.frame(q8.2)/3
    q8.3<-rowSums(organisasi[,44:45]); q8.3<- as.data.frame(q8.3)/2
    valOrganisasi <- cbind(inputOrganisasi$`profil/provinsi`,inputOrganisasi$`profil/institusi`,inputOrganisasi$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    colnames(valOrganisasi)<-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
    tempOrganisasi<-as.data.frame(valOrganisasi)
    
    file_indOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
    indikatorOrg <- as.data.frame(unique(file_indOrg$Kapasitas_Fungsional))
    colnames(indikatorOrg)<-"Indikator"
    
    ##Menampilkan hasil OPD per provinsi###
    tempOrganisasi<-filter(valOrganisasi,valOrganisasi$Provinsi==input$categoryProvince & valOrganisasi$Institusi==input$selectizeInstitution) #buat field insitution
    #tempOrganisasi<-filter(valOrganisasi,valOrganisasi$Provinsi=="Sulawesi Selatan" & valOrganisasi$Institusi=="bappeda")
    
    ##Membuat tabel Level setiap aspek###
    Level4<-rowSums(tempOrganisasi[,4:10])/length(tempOrganisasi[,4:10])
    LevelOrg<-mean(Level4)
    Level5 <- rowSums(tempOrganisasi[,11:15])/length(tempOrganisasi[,11:15])
    LevelSDM<-mean(Level5)
    Level8 <- rowSums(tempOrganisasi[,16:18])/length(tempOrganisasi[,16:18])
    LevelTek<-mean(Level8)
    LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
    LevelOrg_gabungan<-round(LevelOrg_gabungan,digits = 2)
    gapOrg_gabungan<-5-LevelOrg_gabungan
    gapOrg_gabungan<-round(gapOrg_gabungan,digits = 2)
    aspekOrg<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
    tingkatOrg<-as.data.frame(cbind(aspekOrg, LevelOrg_gabungan, gapOrg_gabungan))
    colnames(tingkatOrg)<- c("Aspek Penilaian", "Level", "GAP")
    
    ##Membuat bar chart untuk tingkat Organisasi###
    Ind4.1<-mean(tempOrganisasi$q4.1); Ind4.2<-mean(tempOrganisasi$q4.2); Ind4.3<-mean(tempOrganisasi$q4.3); Ind4.4<-mean(tempOrganisasi$q4.4); Ind4.5<-mean(tempOrganisasi$q4.5); Ind4.6<-mean(tempOrganisasi$q4.6); Ind4.7<-mean(tempOrganisasi$q4.7)
    Ind5.1<-mean(tempOrganisasi$q5.1); Ind5.2<-mean(tempOrganisasi$q5.2); Ind5.3<-mean(tempOrganisasi$q5.3); Ind5.4<-mean(tempOrganisasi$q5.4); Ind5.5<-mean(tempOrganisasi$q5.5)
    Ind8.1<-mean(tempOrganisasi$q8.1);Ind8.2<-mean(tempOrganisasi$q8.2);Ind8.3<-mean(tempOrganisasi$q8.3)
    tempLevelOrg <- as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
    tempLevelOrg<-round(tempLevelOrg,digits = 2)
    tempGapOrg<- 5-tempLevelOrg
    tempGapOrg<-round(tempGapOrg, digits = 2)
    graphOrg<-cbind(indikatorOrg,tempLevelOrg,tempGapOrg)
    colnames(graphOrg)<-c("Indikator","Level","GAP")
    tablesCDA$summaryOrg <- graphOrg
    
    datatable(tingkatOrg,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartOrg <- renderPlotly({
    graphOrg <- tablesCDA$summaryOrg 
    plot_ly(graphOrg, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack') 
    
  })
  
  output$selectizeInstitution <- renderUI({
    inputOrganisasi<-readRDS("data/dataOrganisasi")
    inputOrganisasi$`profil/jabatan`<-NULL; inputOrganisasi$`meta/instanceID`<-NULL; inputOrganisasi$`__version__`<-NULL
    inputOrganisasi$`_uuid`<-NULL; inputOrganisasi$`_submission_time`<-NULL; inputOrganisasi$`_tags`<-NULL; inputOrganisasi$`_notes`<-NULL
    
    inputOrganisasi$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan`<-NULL
    inputOrganisasi$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan_001`<-NULL
    
    for (i in 2:4){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat2/alasan_00",i,"`","<-NULL")))
    }
    
    for (i in 5:6){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat3/alasan_00",i,"`","<-NULL")))
    }
    for (i in 7:9){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat4/alasan_00",i,"`","<-NULL")))
    }
    inputOrganisasi$`perangkat1/perangkat4/alasan_010`<-NULL
    
    for (i in 11:13){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 14:15){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 16:22){
      eval(parse(text=paste0("inputOrganisasi$`perangkat1/perangkat7/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 23:29){
      eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm2/alasan_0",i,"`","<-NULL")))
    }
    
    inputOrganisasi$`sdm1/sdm3/alasan_030`<-NULL
    inputOrganisasi$`sdm1/sdm4/alasan_031`<-NULL
    
    for (i in 32:33){
      eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 34:35){
      eval(parse(text=paste0("inputOrganisasi$`sdm1/sdm6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 36:39){
      eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi2/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 40:42){
      eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi3/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 43:44){
      eval(parse(text=paste0("inputOrganisasi$`teknologi1/teknologi4/alasan_0",i,"`","<-NULL")))
    }
    
    inputOrganisasi[inputOrganisasi == "n/a"]  <- NA
    inputOrganisasi <- na.omit(inputOrganisasi)
    inputOrg<-as.data.frame(inputOrganisasi)
    
    institution<-filter(inputOrg,inputOrg$`profil/provinsi`==input$categoryProvince)
    #institution<-filter(inputOrg,inputOrg$`profil/provinsi`=="Aceh")
    
    selectizeInput('selectizeInstitution', 'Pilih OPD Anda', choices=list(
      OPD=as.character(sort(institution$`profil/institusi`))
    ), multiple=FALSE)
  })
  
  ### Ringkasan Hasil Organisasi ####
  output$resTblOrgAll <- renderDataTable({
    summInputOrg<-readRDS("data/dataOrganisasi")
    summInputOrg$`profil/jabatan`<-NULL; summInputOrg$`meta/instanceID`<-NULL; summInputOrg$`__version__`<-NULL
    summInputOrg$`_uuid`<-NULL; summInputOrg$`_submission_time`<-NULL; summInputOrg$`_tags`<-NULL; summInputOrg$`_notes`<-NULL
    
    summInputOrg$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan`<-NULL
    summInputOrg$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan_001`<-NULL
    
    for (i in 2:4){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat2/alasan_00",i,"`","<-NULL")))
    }
    
    for (i in 5:6){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat3/alasan_00",i,"`","<-NULL")))
    }
    for (i in 7:9){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat4/alasan_00",i,"`","<-NULL")))
    }
    summInputOrg$`perangkat1/perangkat4/alasan_010`<-NULL
    
    for (i in 11:13){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 14:15){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 16:22){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat7/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 23:29){
      eval(parse(text=paste0("summInputOrg$`sdm1/sdm2/alasan_0",i,"`","<-NULL")))
    }
    
    summInputOrg$`sdm1/sdm3/alasan_030`<-NULL
    summInputOrg$`sdm1/sdm4/alasan_031`<-NULL
    
    for (i in 32:33){
      eval(parse(text=paste0("summInputOrg$`sdm1/sdm5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 34:35){
      eval(parse(text=paste0("summInputOrg$`sdm1/sdm6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 36:39){
      eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi2/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 40:42){
      eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi3/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 43:44){
      eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi4/alasan_0",i,"`","<-NULL")))
    }
    
    summInputOrg[summInputOrg == "n/a"]<-NA
    summInputOrg<-na.omit(summInputOrg)
    summInputOrg<-as.data.frame(summInputOrg)
    
    summOrg<-as.data.frame(lapply(summInputOrg[,5:length(summInputOrg)], as.numeric))
    
    q4.1<-rowSums(summOrg[,1:2]); q4.1<-as.data.frame(q4.1)/2
    q4.2<-rowSums(summOrg[,3:5]); q4.2<-as.data.frame(q4.2)/3
    q4.3<-rowSums(summOrg[,6:7]); q4.3<-as.data.frame(q4.3)/2
    q4.4<-rowSums(summOrg[,8:11]); q4.4<-as.data.frame(q4.4)/4
    q4.5<-rowSums(summOrg[,12:14]); q4.5<-as.data.frame(q4.5)/3
    q4.6<-rowSums(summOrg[,15:16]); q4.6<-as.data.frame(q4.6)/2
    q4.7<-rowSums(summOrg[,17:23]); q4.7<-as.data.frame(q4.7)/7
    q5.1<-rowSums(summOrg[,24:30]); q5.1<-as.data.frame(q5.1)/7
    q5.2<-summOrg$sdm1.sdm3.q5.2; q5.3<-summOrg$sdm1.sdm4.q5.3
    q5.4<-rowSums(summOrg[,33:34]); q5.4<-as.data.frame(q5.4)/2
    q5.5<-rowSums(summOrg[,35:36]); q5.5<-as.data.frame(q5.5)/2
    q8.1<-rowSums(summOrg[,37:40]); q8.1<-as.data.frame(q8.1)/4
    q8.2<-rowSums(summOrg[,41:43]); q8.2<-as.data.frame(q8.2)/3
    q8.3<-rowSums(summOrg[,44:45]); q8.3<-as.data.frame(q8.3)/2
    valOrganisasi <-cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    colnames(valOrganisasi) <-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
    summTempOrganisasi <-as.data.frame(valOrganisasi)
    
    indikatorOrg <-read.table("init/organisation.csv", header=TRUE, sep=",")
    summIndikatorOrg <-as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
    colnames(summIndikatorOrg) <-"Indikator"
    
    ##Menampilkan hasil satu provinsi untuk tingkat organisasi##
    summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==input$categoryProvince)
    #summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Aceh")
    
    ##Membuat tabel Level setiap aspek##   
    Level4 <-rowSums(summTempOrganisasi[,4:10])/length(summTempOrganisasi[,4:10])
    LevelOrg <-mean(Level4)
    Level5 <-rowSums(summTempOrganisasi[,11:15])/length(summTempOrganisasi[,11:15])
    LevelSDM <-mean(Level5)
    Level8 <-rowSums(summTempOrganisasi[,16:18])/length(summTempOrganisasi[,16:18])
    LevelTek <-mean(Level8)
    LevelOrg_gabungan <-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
    LevelOrg_gabungan <- round(LevelOrg_gabungan, digits = 2)
    gapOrg_gabungan <-5-LevelOrg_gabungan
    Aspek_Penilaian <-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
    summOrganisasi <-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan, gapOrg_gabungan))
    colnames(summOrganisasi) <- c("Aspek Penilaian", "Level", "GAP")
    
    ## Menampilkan level per indikator ##
    Ind4.1 <-mean(summTempOrganisasi$q4.1); Ind4.2<-mean(summTempOrganisasi$q4.2); Ind4.3<-mean(summTempOrganisasi$q4.3); Ind4.4<-mean(summTempOrganisasi$q4.4); Ind4.5<-mean(summTempOrganisasi$q4.5); Ind4.6<-mean(summTempOrganisasi$q4.6); Ind4.7<-mean(summTempOrganisasi$q4.7)
    Ind5.1 <-mean(summTempOrganisasi$q5.1); Ind5.2<-mean(summTempOrganisasi$q5.2); Ind5.3<-mean(summTempOrganisasi$q5.3); Ind5.4<-mean(summTempOrganisasi$q5.4); Ind5.5<-mean(summTempOrganisasi$q5.5)
    Ind8.1 <-mean(summTempOrganisasi$q8.1);Ind8.2<-mean(summTempOrganisasi$q8.2);Ind8.3<-mean(summTempOrganisasi$q8.3)
    levelProvOrg <-as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
    levelProvOrg <-round(levelProvOrg,digits = 2)
    gapProvOrg <-5-levelProvOrg
    provOrg <-cbind(summIndikatorOrg,levelProvOrg,gapProvOrg)
    colnames(provOrg) <-c("Indikator", "Level", "GAP")
    tablesCDA$summaryProvOrg <-provOrg
    
    datatable(summOrganisasi,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartOrgAll <- renderPlotly({
    provOrg <-tablesCDA$summaryProvOrg
    plot_ly(provOrg, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack')
  })
  
  ####MENU INDIVIDU####
  
  ### Hasil Analisis setiap Individu ####
  output$resTblInd <- renderDataTable({
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
    tempIndividu<-filter(valInd,valInd$Provinsi==input$categoryProvince & valInd$Nama==input$selectizeName)
    #tempIndividu<-filter(valInd,valInd$Provinsi=="Aceh" & valInd$Nama=="Yumna")
    
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
    tablesCDA$summaryInd <- graphInd
    
    datatable(tingkatInd,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartInd <- renderPlotly({
    graphInd <- tablesCDA$summaryInd
    plot_ly(graphInd, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack')
  })
  
  output$selectizeName <- renderUI({
    inputInd<-readRDS("data/dataIndividu")
    inputInd$`profil/gender`<-NULL; inputInd$`profil/jabatan`<-NULL; inputInd$`profil/akun`<-NULL; inputInd$`profil/noHP`<-NULL; inputInd$`profil/email`<-NULL
    inputInd$`meta/instanceID`<-NULL; inputInd$`__version__`<-NULL; inputInd$`_uuid`<-NULL; inputInd$`_submission_time`<-NULL; inputInd$`_tags`<-NULL; inputInd$`_notes`<-NULL
    
    inputInd$`sdm_i1/sdm_i2/alasan`<-NULL
    inputInd$`sdm_i1/sdm_i2/alasan_001`<-NULL
    
    for (i in 2:9){
      eval(parse(text=paste0("inputInd$`sdm_i1/sdm_i3/alasan_00",i,"`","<-NULL")))
    }
    inputInd$`sdm_i1/sdm_i3/alasan_010`<-NULL
    
    for (i in 11:19){
      eval(parse(text=paste0("inputInd$`sdm_i1/sdm_i4/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 20:22){
      eval(parse(text=paste0("inputInd$`sdm_i1/sdm_i5/alasan_0",i,"`","<-NULL")))
    }
    
    #Menghilangkan n/a pada data frame#
    inputInd[inputInd == "n/a"]  <- NA
    inputInd <- na.omit(inputInd)
    
    name<-filter(inputInd,inputInd$`profil/provinsi`==input$categoryProvince)
    #name<-filter(inputInd,inputInd$`profil/provinsi`=="Aceh")
    
    selectizeInput('selectizeName', 'Pilih nama Anda', choices=list(
      Nama=as.character(sort(name$`profil/nama`))
    ), multiple=FALSE)
  })
  
  ### Ringkasan Hasil Inidividu ####
  output$resTblIndAll <- renderDataTable({
    summInputInd<-readRDS("data/dataIndividu")
    summInputInd$`profil/gender`<-NULL; summInputInd$`profil/jabatan`<-NULL; summInputInd$`profil/akun`<-NULL; summInputInd$`profil/noHP`<-NULL; summInputInd$`profil/email`<-NULL
    summInputInd$`meta/instanceID`<-NULL; summInputInd$`__version__`<-NULL; summInputInd$`_uuid`<-NULL; summInputInd$`_submission_time`<-NULL; summInputInd$`_tags`<-NULL; summInputInd$`_notes`<-NULL
    
    summInputInd$`sdm_i1/sdm_i2/alasan`<-NULL
    summInputInd$`sdm_i1/sdm_i2/alasan_001`<-NULL
    
    for (i in 2:9){
      eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i3/alasan_00",i,"`","<-NULL")))
    }
    summInputInd$`sdm_i1/sdm_i3/alasan_010`<-NULL
    
    for (i in 11:19){
      eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i4/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 20:22){
      eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i5/alasan_0",i,"`","<-NULL")))
    }
    
    ## Menghilangkan n/a pada data frame ##
    summInputInd[summInputInd == "n/a"]  <- NA
    summInputInd <- na.omit(summInputInd)
    
    summInd<- as.data.frame(lapply(summInputInd[,5:length(summInputInd)], as.numeric))
    
    q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
    q6.3<-rowSums(summInd[,12:20]); q6.3<-as.data.frame(q6.3)/9
    q6.4<-rowSums(summInd[,21:23]); q6.4<-as.data.frame(q6.4)/3
    valInd<-cbind(summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
    colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
    summTempIndividu<-as.data.frame(valInd)
    
    summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    summIndikatorInd  <- as.data.frame(summIndikatorInd)
    
    summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==input$categoryProvince)
    #summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Aceh")
    
    ## Membuat tabel Level setiap aspek ##
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-mean(as.matrix(summTempIndividu[3:length(summTempIndividu)]))
    Level6<-round(Level6,digits = 2)
    gap6<-5-Level6
    summIndividu<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(summIndividu)<-c("Aspek Penilaian","Level","GAP")
    
    ## Menampilkan level per indikator ##
    Ind6.1<-mean(valInd$q6.1); Ind6.2<-mean(valInd$q6.2); Ind6.3<-mean(valInd$q6.3); Ind6.4<-mean(valInd$q6.4)
    levelProvInd<-as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
    levelProvInd<-round(levelProvInd, digits=2)
    gapProvInd<-5-levelProvInd
    provInd<-cbind(summIndikatorInd,levelProvInd,gapProvInd)
    colnames(provInd)<-c("Indikator", "Level", "GAP")
    tablesCDA$summaryProvInd <- provInd
    
    datatable(summIndividu,escape = FALSE, rownames = FALSE)
  })
  
  output$resChartIndAll <- renderPlotly({
    provInd <- tablesCDA$summaryProvInd
    plot_ly(provInd, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack')
  })
  
  ####MENU RANGKUMAN####
  
  output$resTblSumm <- renderDataTable({
    #### Tabel Prioritas Tingkat Sistem ####
    summInputSys<-readRDS("data/dataSistem")
    
    summInputSys$`meta/instanceID`<-NULL; summInputSys$`__version__`<-NULL; summInputSys$`_uuid`<-NULL; summInputSys$`_submission_time`<-NULL; summInputSys$`_tags`<-NULL; summInputSys$`_notes`<-NULL
    summInputSys$`regulasi/regulasi1/alasan`<-NULL
    summInputSys$`regulasi/regulasi2/alasan_001`<-NULL
    summInputSys$`integrasi1/integrasi2/alasan_002`<-NULL
    summInputSys$`integrasi1/integrasi3/alasan_003`<-NULL
    summInputSys$`integrasi1/integrasi4/alasan_004`<-NULL
    summInputSys$`integrasi1/integrasi5/alasan_005`<-NULL
    summInputSys$`integrasi1/integrasi6/alasan_006`<-NULL
    summInputSys$`integrasi1/integrasi6/alasan_007`<-NULL
    summInputSys$`proses1/proses2/alasan_008`<-NULL
    summInputSys$`proses1/proses2_001/alasan_009`<-NULL
    summInputSys$`proses1/proses3/alasan_010`<-NULL
    summInputSys$`proses1/proses4/alasan_011`<-NULL
    summInputSys$`proses1/proses4_001/alasan_012`<-NULL
    
    for (i in 13:31){
      eval(parse(text=paste0("summInputSys$`datainfo1/datainfo2/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 32:50){
      eval(parse(text=paste0("summInputSys$`datainfo1/datainfo3/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 51:52){
      eval(parse(text=paste0("summInputSys$`datainfo1/datainfo4/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 53:57){
      eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan2/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 58:63){
      eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan3/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 64:66){
      eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan4/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 67:68){
      eval(parse(text=paste0("summInputSys$`pemantauan1/pemantauan5/alasan_0",i,"`","<-NULL")))
    }
    
    
    summInputSys<-as.data.frame(summInputSys)
    summInputSys$`pemantauan1/pemantauan3/q9.2.6`[summInputSys$`pemantauan1/pemantauan3/q9.2.6` == "n/a"]  <- NA
    summInputSys$`pemantauan1/pemantauan5/q9.4.1`[summInputSys$`pemantauan1/pemantauan5/q9.4.1` == "n/a"]  <- NA
    summInputSys$`pemantauan1/pemantauan5/q9.4.2`[summInputSys$`pemantauan1/pemantauan5/q9.4.2` == "n/a"]  <- NA
    summInputSys[is.na(summInputSys)]<-3
    summSys<- as.data.frame(lapply(summInputSys[,3:length(summInputSys)], as.numeric))
    
    q2.5<-rowSums(summSys[,9:10]); q2.5<- as.data.frame(q2.5)/2
    q7.1 <- rowSums(summSys[,14:32]); q7.1<- as.data.frame(q7.1)/19
    q7.2 <- rowSums(summSys[,33:51]); q7.2<- as.data.frame(q7.2)/19
    q7.3<-rowSums(summSys[,52:53]); q7.3<-as.data.frame(q7.3)/2
    q9.1<-rowSums(summSys[,54:58]); q9.1<-as.data.frame(q9.1)/5
    q9.2<-rowSums(summSys[,59:64]); q9.2<-as.data.frame(q9.2)/6
    q9.3<-rowSums(summSys[,65:67]); q9.3<-as.data.frame(q9.3)/3
    q9.4<-rowSums(summSys[,68:69]); q9.4<-as.data.frame(q9.4)/2
    
    summLevelSistem<-cbind(summInputSys$`provinsi/provinsi_001`,summSys$regulasi.regulasi1.q1.1,summSys$regulasi.regulasi2.q1.2,summSys$integrasi1.integrasi2.q2.1,summSys$integrasi1.integrasi3.q2.2,summSys$integrasi1.integrasi4.q2.3, summSys$integrasi1.integrasi5.q2.4, q2.5, summSys$proses1.proses2.q3.1, summSys$proses1.proses2_001.q3.2, summSys$proses1.proses3.q3.3, summSys$proses1.proses4.q3.4, summSys$proses1.proses4_001.q3.5, q7.1, q7.2, q7.3, q9.1, q9.2, q9.3, q9.4)
    colnames(summLevelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")
    summTempSistem<-as.data.frame((summLevelSistem))
    
    indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
    summIndikatorSys <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))
    
    ## Menampilkan hasil satu provinsi untuk tingkat sistem ##
    summTempSistem<-filter(summTempSistem,summInputSys$`provinsi/provinsi_001`==input$categoryProvince)
    #summTempSistem<-filter(summTempSistem,Provinsi=="Aceh")
    
    ## Membuat tabel Level setiap aspek ##   
    aspekSys<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-mean(as.matrix(summTempSistem[,2:3])); LevelInt<-mean(as.matrix(summTempSistem[4:8])); LevelProses<-mean(as.matrix(summTempSistem[9:13])); LevelData<-mean(as.matrix(summTempSistem[14:16])); LevelPEP<-mean(as.matrix(summTempSistem[17:20]))
    summ_allLevelSys<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    #gapReg<-mean(as.matrix(tempSistem[21:22])); gapInt<-mean(as.matrix(tempSistem[23:27])); gapProses<-mean(as.matrix(tempSistem[28:32])); gapData<-mean(as.matrix(tempSistem[33:35])); gapPEP<-mean(as.matrix(tempSistem[36:39]))
    gapReg<-5-LevelReg; gapInt<-5-LevelInt; gapProses<-5-LevelProses; gapData<-5-LevelData; gapPEP<-5-LevelPEP
    summ_allGapSys<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
    summSistem<-as.data.frame(cbind(aspekSys, summ_allLevelSys, summ_allGapSys))
    colnames(summSistem)<-c("Aspek Penilaian","Level","GAP")
    
    ## Menampilkan level per indikator & prioritas ##
    t_summTempSistem<-t(summTempSistem[2:length(summTempSistem)])
    provSys<-rowMeans(t_summTempSistem)
    provSys<-round(provSys, digits = 2)
    
    tabelSys<-cbind(summIndikatorSys,provSys)
    tabelSys$prioritasSys <- "Tidak prioritas" 
    tabelSys<-within(tabelSys, {prioritasSys<-ifelse(provSys<=3, "Prioritas rendah", prioritasSys)})
    tabelSys<-within(tabelSys, {prioritasSys<-ifelse(provSys<=2, "Prioritas tinggi", prioritasSys)})
    tabelSys<-within(tabelSys, {prioritasSys<-ifelse(provSys<=1, "Prioritas sangat tinggi", prioritasSys)})
    
    colnames(tabelSys)<-c("Indikator","Level","Prioritas")
    
    #### Tabel Prioritas Tingkat Organisasi ####
    summInputOrg<-readRDS("data/dataOrganisasi")
    summInputOrg$`profil/jabatan`<-NULL; summInputOrg$`meta/instanceID`<-NULL; summInputOrg$`__version__`<-NULL
    summInputOrg$`_uuid`<-NULL; summInputOrg$`_submission_time`<-NULL; summInputOrg$`_tags`<-NULL; summInputOrg$`_notes`<-NULL
    
    summInputOrg$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan`<-NULL
    summInputOrg$`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/alasan_001`<-NULL
    
    for (i in 2:4){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat2/alasan_00",i,"`","<-NULL")))
    }
    
    for (i in 5:6){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat3/alasan_00",i,"`","<-NULL")))
    }
    for (i in 7:9){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat4/alasan_00",i,"`","<-NULL")))
    }
    summInputOrg$`perangkat1/perangkat4/alasan_010`<-NULL
    
    for (i in 11:13){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 14:15){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 16:22){
      eval(parse(text=paste0("summInputOrg$`perangkat1/perangkat7/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 23:29){
      eval(parse(text=paste0("summInputOrg$`sdm1/sdm2/alasan_0",i,"`","<-NULL")))
    }
    
    summInputOrg$`sdm1/sdm3/alasan_030`<-NULL
    summInputOrg$`sdm1/sdm4/alasan_031`<-NULL
    
    for (i in 32:33){
      eval(parse(text=paste0("summInputOrg$`sdm1/sdm5/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 34:35){
      eval(parse(text=paste0("summInputOrg$`sdm1/sdm6/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 36:39){
      eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi2/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 40:42){
      eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi3/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 43:44){
      eval(parse(text=paste0("summInputOrg$`teknologi1/teknologi4/alasan_0",i,"`","<-NULL")))
    }
    
    summInputOrg[summInputOrg == "n/a"]  <- NA
    summInputOrg <- na.omit(summInputOrg)
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
    q5.2<-summOrg$sdm1.sdm3.q5.2; q5.3<-summOrg$sdm1.sdm4.q5.3
    q5.4<-rowSums(summOrg[,33:34]); q5.4<- as.data.frame(q5.4)/2
    q5.5<-rowSums(summOrg[,35:36]); q5.5<- as.data.frame(q5.5)/2
    q8.1<-rowSums(summOrg[,37:40]); q8.1<- as.data.frame(q8.1)/4
    q8.2<-rowSums(summOrg[,41:43]); q8.2<- as.data.frame(q8.2)/3
    q8.3<-rowSums(summOrg[,44:45]); q8.3<- as.data.frame(q8.3)/2
    valOrganisasi <- cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    colnames(valOrganisasi)<-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
    summTempOrganisasi<-as.data.frame(valOrganisasi)
    
    indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
    summIndikatorOrg <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
    colnames(summIndikatorOrg)<-"Indikator"
    
    ##Menampilkan hasil satu provinsi untuk tingkat organisasi##
    summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==input$categoryProvince)
    #summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Aceh")
    
    ##Membuat tabel Level setiap aspek##   
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
    
    ##Menampilkan level per indikator & prioritas##
    Ind4.1<-mean(summTempOrganisasi$q4.1); Ind4.2<-mean(summTempOrganisasi$q4.2); Ind4.3<-mean(summTempOrganisasi$q4.3); Ind4.4<-mean(summTempOrganisasi$q4.4); Ind4.5<-mean(summTempOrganisasi$q4.5); Ind4.6<-mean(summTempOrganisasi$q4.6); Ind4.7<-mean(summTempOrganisasi$q4.7)
    Ind5.1<-mean(summTempOrganisasi$q5.1); Ind5.2<-mean(summTempOrganisasi$q5.2); Ind5.3<-mean(summTempOrganisasi$q5.3); Ind5.4<-mean(summTempOrganisasi$q5.4); Ind5.5<-mean(summTempOrganisasi$q5.5)
    Ind8.1<-mean(summTempOrganisasi$q8.1);Ind8.2<-mean(summTempOrganisasi$q8.2);Ind8.3<-mean(summTempOrganisasi$q8.3)
    provOrg<-as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
    provOrg<-round(provOrg,digits = 2)
    
    tabelOrg<-cbind(summIndikatorOrg,provOrg)
    tabelOrg$prioritasOrg<-"Tidak prioritas" 
    tabelOrg<-within(tabelOrg, {prioritasOrg<-ifelse(provOrg<=3, "Prioritas rendah", prioritasOrg)})
    tabelOrg<-within(tabelOrg, {prioritasOrg<-ifelse(provOrg<=2, "Prioritas tinggi", prioritasOrg)})
    tabelOrg<-within(tabelOrg, {prioritasOrg<-ifelse(provOrg<=1, "Prioritas sangat tinggi", prioritasOrg)})
    
    colnames(tabelOrg)<-c("Indikator","Level","Prioritas")
    
    #### Tabel Prioritas Tingkat Individu ####
    summInputInd<-readRDS("data/dataIndividu")
    summInputInd$`profil/gender`<-NULL; summInputInd$`profil/jabatan`<-NULL; summInputInd$`profil/akun`<-NULL; summInputInd$`profil/noHP`<-NULL; summInputInd$`profil/email`<-NULL
    summInputInd$`meta/instanceID`<-NULL; summInputInd$`__version__`<-NULL; summInputInd$`_uuid`<-NULL; summInputInd$`_submission_time`<-NULL; summInputInd$`_tags`<-NULL; summInputInd$`_notes`<-NULL
    
    summInputInd$`sdm_i1/sdm_i2/alasan`<-NULL
    summInputInd$`sdm_i1/sdm_i2/alasan_001`<-NULL
    
    for (i in 2:9){
      eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i3/alasan_00",i,"`","<-NULL")))
    }
    summInputInd$`sdm_i1/sdm_i3/alasan_010`<-NULL
    
    for (i in 11:19){
      eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i4/alasan_0",i,"`","<-NULL")))
    }
    
    for (i in 20:22){
      eval(parse(text=paste0("summInputInd$`sdm_i1/sdm_i5/alasan_0",i,"`","<-NULL")))
    }
    
    ## Menghilangkan n/a pada data frame ##
    summInputInd[summInputInd == "n/a"]  <- NA
    summInputInd <- na.omit(summInputInd)
    
    summInd<- as.data.frame(lapply(summInputInd[,5:length(summInputInd)], as.numeric))
    
    q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
    q6.3<-rowSums(summInd[,12:20]); q6.3<-as.data.frame(q6.3)/9
    q6.4<-rowSums(summInd[,21:23]); q6.4<-as.data.frame(q6.4)/3
    valInd<-cbind(summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
    colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
    summTempIndividu<-as.data.frame(valInd)
    
    summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    summIndikatorInd  <- as.data.frame(summIndikatorInd)
    
    summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==input$categoryProvince)
    #summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Aceh")
    
    ## Membuat tabel Level setiap aspek ##
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-mean(as.matrix(summTempIndividu[3:length(summTempIndividu)]))
    gap6<-5-Level6
    summIndividu<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(summIndividu)<-c("Aspek Penilaian","Level","GAP")
    
    ## Menampilkan level per indikator & prioritas ##
    Ind6.1<-mean(valInd$q6.1); Ind6.2<-mean(valInd$q6.2); Ind6.3<-mean(valInd$q6.3); Ind6.4<-mean(valInd$q6.4)
    provInd<-as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
    provInd<-round(provInd, digits=2)
    
    tabelInd<-cbind(summIndikatorInd,provInd)
    tabelInd$prioritasInd <- "Tidak prioritas" 
    tabelInd<-within(tabelInd, {prioritasInd<-ifelse(provInd<=3, "Prioritas rendah", prioritasInd)})
    tabelInd<-within(tabelInd, {prioritasInd<-ifelse(provInd<=2, "Prioritas tinggi", prioritasInd)})
    tabelInd<-within(tabelInd, {prioritasInd<-ifelse(provInd<=1, "Prioritas sangat tinggi", prioritasInd)})
    
    colnames(tabelInd)<-c("Indikator","Level","Prioritas")
    
    ### Tabel Prioritas Gabungan ####
    allprioritas <- rbind(tabelSys,tabelOrg,tabelInd)
    prioritas <- allprioritas[order(allprioritas$Level),]
    
    ### Tabel Level Per Aspek Semua Tingkat ####
    summary<-as.data.frame(rbind(summSistem, summOrganisasi, summIndividu))
    summary$`Aspek Penilaian`<-NULL
    aspek<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan","4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi", "6. Sumber Daya Manusia - Individu")
    summary<-cbind(aspek,summary)
    finalLevel<-as.numeric(summary$Level)
    finalLevel<-round(finalLevel,digits = 2)
    finalGAP<-as.data.frame(5-finalLevel)
    finalGAP<-round(finalGAP, digits = 2)
    # summary$Level<-as.numeric(summary$Level)
    summary<-as.data.frame(cbind(summary$aspek,finalLevel, finalGAP))
    # summary$GAP<-NULL
    colnames(summary)<-c("Aspek", "Level", "GAP")
    rownames(summary)<-1:9
    tablesCDA$allSummary <- summary
    
    datatable(prioritas,escape = FALSE, rownames = FALSE)
    
  })
  
  output$resChartSumm <- renderPlotly({
    ### Bar Chart Semua Tingkat ####
    summary<-tablesCDA$allSummary
    plot_ly(summary, x=~Aspek, y=~Level, type='bar', name='Level') %>%
      add_trace(y=~GAP, name='GAP') %>%
      layout(
        yaxis = list(title='Nilai'),
        xaxis = list(title='Aspek Penilaian'),
        barmode='stack')
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

