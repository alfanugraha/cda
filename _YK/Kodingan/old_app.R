###*initiate library##
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
library(plotly)

library(leaflet)
library(readxl)
library(dplyr)


source("functional_capacity.R")

outputDir <- "responses"
  
# Which fields get saved
fieldsSystem <- c("sys11q01","sys12q01","sys21q01","sys22q01","sys23q01","sys24q01","sys25q01","sys25q02","sys31q01","sys32q01","sys33q01","sys34q01","sys35q01",
                "sys71q01","sys71q02","sys71q03","sys71q04","sys71q05","sys71q06","sys71q07","sys71q08","sys71q09","sys71q10","sys71q11","sys71q12","sys71q13","sys71q14","sys71q15","sys71q16","sys71q17","sys71q18","sys71q19",
                "sys72q01","sys72q02","sys72q03","sys72q04","sys72q05","sys72q06","sys72q07","sys72q08","sys72q09","sys72q10","sys72q11","sys72q12","sys72q13","sys72q14","sys72q15","sys72q16","sys72q17","sys72q18","sys72q19",
                "sys73q01","sys73q02","sys91q01","sys91q02","sys91q03","sys91q04","sys91q05","sys92q01","sys92q02","sys92q03","sys92q04","sys92q05","sys93q01","sys93q02","sys93q03")
fieldsOrganisation <- c("org41q01","org41q02","org42q01","org42q02","org42q03","org43q01","org43q02","org44q01","org44q02","org44q03","org44q04",
                      "org45q01","org45q02","org45q03","org46q01","org46q02","org47q01","org47q02","org47q03","org47q04","org47q05","org47q06",
                      "org47q07","org51q01","org51q02","org51q03","org51q04","org51q05","org51q06","org51q07","org52q01","org53q01","org54q01",
                      "org54q02","org55q01","org55q02","org81q01","org81q02","org81q03","org81q04","org82q01","org82q02","org82q03","org83q01","org83q02")
fieldsIndividu <- c("ind61q01","ind61q02",
                  "ind62q01","ind62q02","ind62q03","ind62q04","ind62q05","ind62q06","ind62q07","ind62q08","ind62q09",
                  "ind63q01","ind63q02","ind63q03","ind63q04","ind63q05","ind63q06","ind63q07","ind63q08","ind63q09","ind64q01","ind64q02","ind64q03")

saveData <- function(data) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(x = data, file = file.path(outputDir, fileName), row.names = FALSE)
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

###*setup dashboard page####
ui <- source('interface.R')

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  tablesCDA <- reactiveValues(tableSystem=data.frame(), tableOrganisation=data.frame(), tableIndividu=data.frame(), summary=data.frame(),
                              summarySystem=data.frame()
                              )
  
  formSystem <- reactive({
    data <- sapply(fieldsSystem, function(x) input[[x]])
    data <- t(data)
    data
  })
  
  formOrganisation <- reactive({
    data <- sapply(fieldsOrganisation, function(x) input[[x]])
    data <- t(data)
    data
  })
  
  formIndividu <- reactive({
    data <- sapply(fieldsIndividu, function(x) input[[x]])
    data <- t(data)
    data
  })
  
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
    # sistem<-unlist(inputResp[,11:76])
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
  
  output$tabelSys <- renderDataTable({
    tblSystem <- tablesCDA$tableSystem
    tblSystem
  })
  
  output$rateTbl <- renderDataTable({
    desc <- c("REGULASI", "INTEGRASI DALAM PERENCANAAN PEMBANGUNAN DAERAH", "PROSES", "ORGANISASI", "SDM", "DATA DAN INFORMASI", "PEMANTAUAN EVALUASI DAN PELAPORAN")

    tblSystem <- tablesCDA$tableSystem

    # regulasi
    regulasi <- mean(c(tblSystem[1, 1], tblSystem[1, 2]))
    # integrasi dlm perencanaan pembangunan daerah
    integrasi <- mean(c(tblSystem[1, 3], tblSystem[1, 4], tblSystem[1, 5], tblSystem[1, 6], mean(c(tblSystem[1, 7], tblSystem[1, 8])) ))
    # proses
    proses <- mean(c(unlist(tblSystem[1, 9:13])))
    # organisasi
    organisasi <- 0
    # sdm 
    sdm <- 0
    # data dan informasi
    informasi <- mean(c(mean(c(unlist(tblSystem[1, 14:32]))), 
                        mean(c(unlist(tblSystem[1, 33:51]))),
                        mean(c(tblSystem[1, 52], tblSystem[1, 53]))
                        ))
    # pep
    pep <- mean(c(mean(c(unlist(tblSystem[1, 54:58]))),
                  mean(c(unlist(tblSystem[1, 59:63]))),
                  mean(c(unlist(tblSystem[1, 64:66])))
                  ))

    level <- c(regulasi, integrasi, proses, organisasi, sdm, informasi, pep)
    gap <- 5-level
    

    summary <- data.frame(SUMMARY=desc, LEVEL=level, GAP=gap)
    tablesCDA$summary <- summary
    summary
  })
  
  output$rateChart <- renderPlotly({
    plot_ly(tablesCDA$summary, x=~SUMMARY, y=~LEVEL, type='bar', name='LEVEL') %>%
      add_trace(y=~GAP, name='GAP') %>%
      layout(yaxis = list(title='VALUE'), barmode='stack')
  })
  
  # Append data
  observeEvent(input$submitSys, {
    tbl <- formSystem()
    tablesCDA$tableSystem <- rbind(tablesCDA$tableSystem, tbl)
  })
  
  observeEvent(input$submitOrg, {
    tbl <- formOrganisation()
    tablesCDA$tableOrganisation <- rbind(tablesCDA$tableOrganisation, tbl)
  })
  
  observeEvent(input$submitInd, {
    tbl <- formIndividu()
    tablesCDA$tableIndividu <- rbind(tablesCDA$tableIndividu, tbl)
  })
  
  # Export data
  observeEvent(input$exportInd, {
    saveData(tablesCDA$tableIndividu)
  })
 
  ##Mandatory field 
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
}

###*run the apps#### 
shinyApp(ui = ui, server = server)

