# library ####
library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

library(shinyjs)
library(plotly)
library(readxl)
library(magrittr)
library(rlang)
library(plyr)
library(rtf)
library(stringr)
library(gtools)

# initiate variable ####
button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

library(httr)
library(jsonlite)
library(readr)

kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

form_sis <- 327419 #Sistem
form_org <- 327585 #Organisasi
form_ind <- 327418 #Individu

# Define UI
ui <- fluidPage(

  HTML('<meta name="viewport" content="width=device-width">'),
  
#Navbar structure for UI
  navbarPage("", theme = shinytheme("lumen"),
             # tabPanel("Sistem", fluid = TRUE, icon = icon("user-cog"),
             #          tags$style(button_color_css),
             #          h2(textOutput("titleSistem")),
             #          dataTableOutput("resTblSys"),
             #          plotlyOutput("resChartSys")
             # ),
             navbarMenu("Sistem", icon = icon("user-cog"),
                        tabPanel("Ringkasan Hasil", fluid = TRUE,
                                 tags$style(button_color_css),
                                 selectInput("selectedYear", label = "Tahun Analisis", choices = 2019:2030, selected=2019, multiple = FALSE),
                                 h2(textOutput("titleSistem")),
                                 dataTableOutput("resTblSys"),
                                 withSpinner(plotlyOutput("resChartSys"))
                        ),
                        tabPanel("Perbandingan Hasil Tahunan", fluid = TRUE,
                                 tags$style(button_color_css),
                                 h2("Perbandingan Hasil Penilaian Kapasitas Tingkat Sistem Berdasarkan Indikator"),
                                 dataTableOutput("multiTableSistem"),
                                 withSpinner(plotlyOutput("multiChartSistem")))
                        ),
             navbarMenu("Organisasi", icon = icon("sitemap"),
                        tabPanel("Ringkasan Hasil", fluid = TRUE,
                                 tags$style(button_color_css),
                                 h2(textOutput("titleOrganisasi")),
                                 dataTableOutput("resTblOrgAll"),
                                 withSpinner(plotlyOutput("resChartOrgAll"))
                        ),
                        tabPanel("Perbandingan Hasil Tahunan", fluid = TRUE,
                                 tags$style(button_color_css),
                                 h2("Perbandingan Hasil Penilaian Kapasitas Tingkat Organisasi Berdasarkan Indikator"),
                                 dataTableOutput("multiTableOrganisasi"),
                                 withSpinner(plotlyOutput("multiChartOrganisasi")))
                        ),
             navbarMenu("Individu", icon = icon("user"),
                        tabPanel("Ringkasan Hasil", fluid = TRUE,
                                 tags$style(button_color_css),
                                 h2(textOutput("titleIndividu")),
                                 dataTableOutput("resTblIndAll"),
                                 withSpinner(plotlyOutput("resChartIndAll"))
                        ),
                        tabPanel("Perbandingan Hasil Tahunan", fluid = TRUE,
                                 tags$style(button_color_css),
                                 h2("Perbandingan Hasil Penilaian Kapasitas Tingkat Individu Berdasarkan Indikator"),
                                 dataTableOutput("multiTableIndividu"),
                                 withSpinner(plotlyOutput("multiChartIndividu")))
                        ),
             navbarMenu("Rangkuman", icon = icon("poll"),
                        tabPanel("Ringkasan Hasil", fluid = TRUE,
                                 tags$style(button_color_css),
                                 h2(textOutput("titleRangkuman")),
                                 h3(textOutput("yearCDNA")),
                                 dataTableOutput("resTblSumm"),
                                 withSpinner(plotlyOutput("resChartSumm")),
                                 downloadButton('downloadResults', 'Unduh Hasil Analisis', style="color: #fff; background-color: #00a65a; border-color: #008d4c")
                        ),
                        tabPanel("Perbandingan Hasil Tahunan", fluid = TRUE,
                                 tags$style(button_color_css),
                                 h2("Perbandingan Hasil Rangkuman Penilaian Kapasitas"),
                                 dataTableOutput("multiTableRangkuman"),
                                 withSpinner(plotlyOutput("multiChartRangkuman")))
             )
             # tabPanel("Organisasi", fluid = TRUE, icon = icon("sitemap"),
             #          h2(textOutput("titleOrganisasi")),
             #          dataTableOutput("resTblOrgAll"),
             #          plotlyOutput("resChartOrgAll")
             # ),
             # tabPanel("Individu", fluid = TRUE, icon = icon("user"),
             #          tags$style(button_color_css),
             #          h2(textOutput("titleIndividu")),
             #          dataTableOutput("resTblIndAll"),
             #          plotlyOutput("resChartIndAll")
             # ),
             # tabPanel("Rangkuman", fluid = TRUE, icon = icon("poll"),
             #          tags$style(button_color_css),
             #          h2(textOutput("titleRangkuman")),
             #          h3(textOutput("yearCDNA")),
             #          dataTableOutput("resTblSumm"),
             #          plotlyOutput("resChartSumm"),
             #          downloadButton('downloadResults', 'Unduh Hasil Analisis', style="color: #fff; background-color: #00a65a; border-color: #008d4c")
             # )
),
tags$style(type="text/css", ".navbar-nav {float: right; margin: 0;}")
)

# Define server
server <- function(input, output, session) {
  ## Sistem ##
  url_sis <- paste0(kc_server_url,"api/v1/data/",form_sis,"?format=csv")
  rawdata_sis  <- GET(url_sis,authenticate("cdna2019","Icraf2019!"),progress())
  dataSistem  <- read_csv(content(rawdata_sis,"raw",encoding = "UTF-8"))
  
  ## Organisasi ##
  url_org <- paste0(kc_server_url,"api/v1/data/",form_org,"?format=csv")
  rawdata_org <- GET(url_org,authenticate("cdna2019","Icraf2019!"),progress())
  dataOrganisasi <- read_csv(content(rawdata_org,"raw",encoding = "UTF-8"))
  
  ## Individu ##
  url_ind <- paste0(kc_server_url,"api/v1/data/",form_ind,"?format=csv")
  rawdata_ind <- GET(url_ind,authenticate("cdna2019","Icraf2019!"),progress())
  dataIndividu <- read_csv(content(rawdata_ind,"raw",encoding = "UTF-8"))
  
  koboData <- reactiveValues(sistem = dataSistem, organisasi = dataOrganisasi, individu = dataIndividu)
  
  get_wraper <- function(width) {
    function(x) {
      lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
    }
  }

  tablesCDA <- reactiveValues(summarySystem=data.frame(),summaryOrg=data.frame(), summaryInd=data.frame(), allSummary=data.frame(), summaryProvInd=data.frame(), summaryProvOrg=data.frame(), priorityTable=data.frame(), multiyearsIndividu=data.frame(), multiyearsOrganisasi=data.frame(), multiyearsSistem=data.frame(), multiyearsRangkuman=data.frame())
  final_chart <- reactiveValues(chartSistem=NULL, chartOrganisasi=NULL, chartIndividu=NULL, chartSummary=NULL)
  graph_data <- reactiveValues(provInd=NULL, provOrg=NULL, graphSistem=NULL, chartSummary=NULL)
  multiyearsTable <- reactiveValues(multiIndividu=data.frame(),multiOrganisasi=data.frame() ,multiSistem=data.frame())

  categoryProvince <- reactiveValues(provinsi=NULL)
  
  # dynamic value, parse a GET query string from a URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$provinsi)){
      categoryProvince$provinsi <- query$provinsi
    } else {
      categoryProvince$provinsi <- "Jawa Barat" #default province
    }
  })

  output$titleSistem <- renderText({ paste0("Ringkasan Hasil Analisis Tingkat Sistem Provinsi ", categoryProvince$provinsi) })
  output$titleOrganisasi <- renderText({ paste0("Ringkasan Hasil Analisis Tingkat Organisasi Provinsi ", categoryProvince$provinsi) })
  output$titleOPD <- renderText({ paste0("Hasil Analisis ", input$selectizeInstitution) })
  output$titleIndividu <- renderText({ paste0("Ringkasan Hasil Analisis Tingkat Individu Provinsi ", categoryProvince$provinsi) })
  output$titleRangkuman <- renderText({ paste0("Rangkuman Keseluruhan Provinsi ", categoryProvince$provinsi) })
  output$yearCDNA <- renderText({ paste0("Tahun: ", input$selectedYear) })
  # output$yearCDNA <- renderText({ paste0("Tahun: ", 2019) })

  ####MENU SISTEM####
  ### SUBMENU: Ringkasan Hasil Sistem ####
  output$resTblSys <- renderDataTable({
    
    inputSistem <- koboData$sistem
    inputSistem$`pemantauan1/pemantauan3/q9.2.6`[is.na(inputSistem$`pemantauan1/pemantauan3/q9.2.6`)] <- 3
    inputSistem$`pemantauan1/pemantauan5/q9.4.1`[is.na(inputSistem$`pemantauan1/pemantauan5/q9.4.1`)] <- 3
    inputSistem$`pemantauan1/pemantauan5/q9.4.2`[is.na(inputSistem$`pemantauan1/pemantauan5/q9.4.2`)] <- 3
    inputSistem$year <- format(as.Date(inputSistem$`provinsi/tanggal`), format = "%Y")
    inputSistem<-filter(inputSistem,inputSistem$year==input$selectedYear)
    # inputSistem<-filter(inputSistem,inputSistem$year==2019)
    
    ##Define Indikator and Aspek###
    aspek1 <- inputSistem %>% select(`regulasi/regulasi1/q1.1`, `regulasi/regulasi2/q1.2`)
    aspek2 <- inputSistem %>% select(`integrasi1/integrasi2/q2.1`, `integrasi1/integrasi3/q2.2`, `integrasi1/integrasi4/q2.3`, `integrasi1/integrasi5/q2.4`)
    indikator2.5 <- inputSistem %>% select(`integrasi1/integrasi6/q2.5.1`, `integrasi1/integrasi6/q2.5.2`)
    aspek3 <- inputSistem  %>% select(`proses1/proses2/q3.1`, `proses1/proses2_001/q3.2`, `proses1/proses3/q3.3`, `proses1/proses4/q3.4`, `proses1/proses4_001/q3.5`)
    indikator7.1 <- inputSistem %>% select(`datainfo1/datainfo2/q7.1.1`, `datainfo1/datainfo2/q7.1.2`, `datainfo1/datainfo2/q7.1.3`, `datainfo1/datainfo2/q7.1.4`,
                                            `datainfo1/datainfo2/q7.1.5`, `datainfo1/datainfo2/q7.1.6`, `datainfo1/datainfo2/q7.1.7`, `datainfo1/datainfo2/q7.1.8`,
                                            `datainfo1/datainfo2/q7.1.9`, `datainfo1/datainfo2/q7.1.10`, `datainfo1/datainfo2/q7.1.11`, `datainfo1/datainfo2/q7.1.12`,
                                            `datainfo1/datainfo2/q7.1.13`, `datainfo1/datainfo2/q7.1.14`, `datainfo1/datainfo2/q7.1.15`, `datainfo1/datainfo2/q7.1.16`,
                                            `datainfo1/datainfo2/q7.1.17`, `datainfo1/datainfo2/q7.1.18`, `datainfo1/datainfo2/q7.1.19`)
    indikator7.2 <- inputSistem %>% select(`datainfo1/datainfo3/q7.2.1`, `datainfo1/datainfo3/q7.2.2`, `datainfo1/datainfo3/q7.2.3`, `datainfo1/datainfo3/q7.2.4`,
                                            `datainfo1/datainfo3/q7.2.5`, `datainfo1/datainfo3/q7.2.6`, `datainfo1/datainfo3/q7.2.7`, `datainfo1/datainfo3/q7.2.8`,
                                            `datainfo1/datainfo3/q7.2.9`, `datainfo1/datainfo3/q7.2.10`, `datainfo1/datainfo3/q7.2.11`, `datainfo1/datainfo3/q7.2.12`,
                                            `datainfo1/datainfo3/q7.2.13`, `datainfo1/datainfo3/q7.2.14`, `datainfo1/datainfo3/q7.2.15`, `datainfo1/datainfo3/q7.2.16`,
                                            `datainfo1/datainfo3/q7.2.17`, `datainfo1/datainfo3/q7.2.18`, `datainfo1/datainfo3/q7.2.19`)
    indikator7.3 <- inputSistem %>% select(`datainfo1/datainfo4/q7.3.1`, `datainfo1/datainfo4/q7.3.2`)
    indikator9.1 <- inputSistem %>% select(`pemantauan1/pemantauan2/q9.1.1`, `pemantauan1/pemantauan2/q9.1.2`, `pemantauan1/pemantauan2/q9.1.3`, `pemantauan1/pemantauan2/q9.1.4`,
                                            `pemantauan1/pemantauan2/q9.1.5`)
    indikator9.2 <- inputSistem %>% select(`pemantauan1/pemantauan3/q9.2.1`, `pemantauan1/pemantauan3/q9.2.2`, `pemantauan1/pemantauan3/q9.2.3`, `pemantauan1/pemantauan3/q9.2.4`,
                                            `pemantauan1/pemantauan3/q9.2.5`, `pemantauan1/pemantauan3/q9.2.6`)
    indikator9.3 <- inputSistem %>% select(`pemantauan1/pemantauan4/q9.3.1`, `pemantauan1/pemantauan4/q9.3.2`, `pemantauan1/pemantauan4/q9.3.3`)
    indikator9.4 <- inputSistem %>% select(`pemantauan1/pemantauan5/q9.4.1`, `pemantauan1/pemantauan5/q9.4.2`)
    
    temp_summSys <- cbind(inputSistem$`provinsi/provinsi_001`, inputSistem$year, aspek1, aspek2, indikator2.5, aspek3, 
                          indikator7.1, indikator7.2, indikator7.3, indikator9.1, indikator9.2, indikator9.2, indikator9.3, indikator9.4)
    
    sistem<- as.data.frame(lapply(temp_summSys[,3:(length(temp_summSys))], as.numeric))
    
    ##Rata-rata dari Indikator Tingkat Sistem###
    q2.5<-rowSums(sistem[,9:10]); q2.5<- as.data.frame(q2.5)/2
    q7.1 <- rowSums(sistem[,14:32]); q7.1<- as.data.frame(q7.1)/19
    q7.2 <- rowSums(sistem[,33:51]); q7.2<- as.data.frame(q7.2)/19
    q7.3<-rowSums(sistem[,52:53]); q7.3<-as.data.frame(q7.3)/2
    q9.1<-rowSums(sistem[,54:58]); q9.1<-as.data.frame(q9.1)/5
    q9.2<-rowSums(sistem[,59:64]); q9.2<-as.data.frame(q9.2)/6
    q9.3<-rowSums(sistem[,65:67]); q9.3<-as.data.frame(q9.3)/3
    q9.4<-rowSums(sistem[,68:69]); q9.4<-as.data.frame(q9.4)/2
    
    ##Tabel Level dari Indikator###
    levelSistem<-cbind(inputSistem$`provinsi/provinsi_001`,sistem$regulasi.regulasi1.q1.1,sistem$regulasi.regulasi2.q1.2,sistem$integrasi1.integrasi2.q2.1,sistem$integrasi1.integrasi3.q2.2,sistem$integrasi1.integrasi4.q2.3, sistem$integrasi1.integrasi5.q2.4, q2.5, sistem$proses1.proses2.q3.1, sistem$proses1.proses2_001.q3.2, sistem$proses1.proses3.q3.3, sistem$proses1.proses4.q3.4, sistem$proses1.proses4_001.q3.5, q7.1, q7.2, q7.3, q9.1, q9.2, q9.3, q9.4)
    colnames(levelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")

    gap_1.1<-5-levelSistem$q1.1; gap_1.2<-5-levelSistem$q1.2; gap_2.1<-5-levelSistem$q2.1; gap_2.2<-5-levelSistem$q2.2; gap_2.3<-5-levelSistem$q2.3; gap_2.4<-5-levelSistem$q2.4; gap_2.5<-5-levelSistem$q2.5
    gap_3.1<-5-levelSistem$q3.1; gap_3.2<-5-levelSistem$q3.2; gap_3.3<-5-levelSistem$q3.3; gap_3.4<-5-levelSistem$q3.4; gap_3.5<-5-levelSistem$q3.5
    gap_7.1<-5-levelSistem$q7.1; gap_7.2<-5-levelSistem$q7.2; gap_7.3<-5-levelSistem$q7.3; gap_9.1<-5-levelSistem$q9.1; gap_9.2<-5-levelSistem$q9.2; gap_9.3<-5-levelSistem$q9.3; gap_9.4<-5-levelSistem$q9.4
    valGAP<-cbind(gap_1.1,gap_1.2,gap_2.1,gap_2.2,gap_2.3,gap_2.4,gap_2.5,gap_3.1,gap_3.2,gap_3.3,gap_3.4,gap_3.5,gap_7.1,gap_7.2,gap_7.3,gap_9.1,gap_9.2,gap_9.3,gap_9.4)
    valSistem<-cbind(levelSistem,valGAP)
    tempSistem<-as.data.frame((valSistem))

    file_indSys<- read.table("init/system.csv", header=TRUE, sep=",")
    indikatorSys <- as.data.frame(unique(file_indSys$Kapasitas_Fungsional))

    ## Menampilkan hasil satu provinsi ##
    tempSistem<-filter(tempSistem,Provinsi==categoryProvince$provinsi)
    # tempSistem<-filter(tempSistem,Provinsi=="Aceh")

    ## Membuat tabel Level setiap aspek ##
    aspekSys<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-mean(as.matrix(tempSistem[,2:3])); LevelInt<-mean(as.matrix(tempSistem[4:8])); LevelProses<-mean(as.matrix(tempSistem[9:13])); LevelData<-mean(as.matrix(tempSistem[14:16])); LevelPEP<-mean(as.matrix(tempSistem[17:20]))
    allLevelSys<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    allLevelSys<-round(allLevelSys,digits = 2)
    gapReg<-5-LevelReg; gapInt<-5-LevelInt; gapProses<-5-LevelProses; gapData<-5-LevelData; gapPEP<-5-LevelPEP
    allGapSys<-as.data.frame(t(cbind(gapReg,gapInt,gapProses,gapData,gapPEP)))
    allGapSys<-round(allGapSys, digits=2)
    tingkatSys<-as.data.frame(cbind(aspekSys, allLevelSys, allGapSys))
    colnames(tingkatSys)<-c("Aspek Penilaian","Level","GAP")
    tingkatSys<-datatable(tingkatSys,escape = FALSE, rownames = FALSE)

    ## Membuat bar chart untuk tingkat Sistem ##
    t_tempSistem <- t(tempSistem[2:length(tempSistem)])
    provSys <- rowMeans(t_tempSistem)
    provSys<-round(provSys, digits = 2)
    graphSistem<-as.data.frame(cbind(indikatorSys,provSys[1:19],provSys[20:38]))
    colnames(graphSistem)<-c("Indikator","Level","GAP")
    tablesCDA$summarySystem <- graphSistem

    tingkatSys
    # datatable(tingkatSys,escape = FALSE, rownames = FALSE)

    # if(is.null(tingkatSys$Level)==TRUE) {
    #   showModal(ui=modalDialog("Data pada tahun ini belum tersedia", easyClose = TRUE), session=session)
    #   } else if(is.null(tingkatSys$Level)==FALSE) {
    #     print(tingkatSys)
    #   }
  })

  output$resChartSys <- renderPlotly({
    graphSistem <- tablesCDA$summarySystem
    plot_ly(graphSistem, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack') %>%
      layout(legend = list(orientation = 'h')) %>%
      layout(yaxis = list(tickfont = list(size = 8), tickangle = 45, title = ""),
             xaxis = list(title = ""))
      # layout(yaxis=list(title=''), xaxis = list(title = ''), barmode='stack')  %>% 
      # layout(xaxis = list(titlefont = list(size = 12), tickfont = list(size = 12)),
      #         yaxis = list(titlefont = list(size = 6))) %>% 
      # layout(legend = list(orientation = 'h'))
  })

  ## ggplot untuk unduh hasil anlisis ##
  output$resChartSys2 <- renderPlotly({
    nilai1 <- t(graphSistem$Level)
    nilai2 <- t(graphSistem$GAP)
    nilai <- t(cbind(nilai1,nilai2))
    jenis1 <- t(rep("Level", length(graphSistem$Level)))
    jenis2 <- t(rep("Gap", length(graphSistem$GAP)))
    jenis <- t(cbind(jenis1,jenis2))
    indikator <- data.frame(graphSistem$Indikator)
    dataGraphSys <- data.frame(cbind(jenis,nilai,indikator))
    colnames(dataGraphSys) <- c("jenis", "nilai", "indikator")

    dataGraphSys <- ddply(dataGraphSys, .(indikator),
                          transform, pos = cumsum(nilai)-nilai)
    chartSys<-ggplot() + geom_bar(data=dataGraphSys, aes(x=indikator, y=nilai, fill=jenis), stat="identity") +
      geom_text(data=dataGraphSys, aes(x =indikator, y =pos, label =paste0(nilai)), size=4)

    final_chart$chartSistem <- chartSys
  })

  ### SUBMENU: Pebandingan Hasil Tahunan Sistem ####
  output$multiTableSistem<- renderDataTable({

    inputSistem <- koboData$sistem
    inputSistem$`pemantauan1/pemantauan3/q9.2.6`[is.na(inputSistem$`pemantauan1/pemantauan3/q9.2.6`)] <- 3
    inputSistem$`pemantauan1/pemantauan5/q9.4.1`[is.na(inputSistem$`pemantauan1/pemantauan5/q9.4.1`)] <- 3
    inputSistem$`pemantauan1/pemantauan5/q9.4.2`[is.na(inputSistem$`pemantauan1/pemantauan5/q9.4.2`)] <- 3
    inputSistem$year <- format(as.Date(inputSistem$`provinsi/tanggal`), format = "%Y")
    year <- inputSistem$year
    # inputSistem<-filter(inputSistem,inputSistem$year==input$selectedYear)
    # inputSistem<-filter(inputSistem,inputSistem$year==2019)
    
    ##Define Indikator and Aspek###
    aspek1 <- inputSistem %>% select(`regulasi/regulasi1/q1.1`, `regulasi/regulasi2/q1.2`)
    aspek2 <- inputSistem %>% select(`integrasi1/integrasi2/q2.1`, `integrasi1/integrasi3/q2.2`, `integrasi1/integrasi4/q2.3`, `integrasi1/integrasi5/q2.4`)
    indikator2.5 <- inputSistem %>% select(`integrasi1/integrasi6/q2.5.1`, `integrasi1/integrasi6/q2.5.2`)
    aspek3 <- inputSistem  %>% select(`proses1/proses2/q3.1`, `proses1/proses2_001/q3.2`, `proses1/proses3/q3.3`, `proses1/proses4/q3.4`, `proses1/proses4_001/q3.5`)
    indikator7.1 <- inputSistem %>% select(`datainfo1/datainfo2/q7.1.1`, `datainfo1/datainfo2/q7.1.2`, `datainfo1/datainfo2/q7.1.3`, `datainfo1/datainfo2/q7.1.4`,
                                           `datainfo1/datainfo2/q7.1.5`, `datainfo1/datainfo2/q7.1.6`, `datainfo1/datainfo2/q7.1.7`, `datainfo1/datainfo2/q7.1.8`,
                                           `datainfo1/datainfo2/q7.1.9`, `datainfo1/datainfo2/q7.1.10`, `datainfo1/datainfo2/q7.1.11`, `datainfo1/datainfo2/q7.1.12`,
                                           `datainfo1/datainfo2/q7.1.13`, `datainfo1/datainfo2/q7.1.14`, `datainfo1/datainfo2/q7.1.15`, `datainfo1/datainfo2/q7.1.16`,
                                           `datainfo1/datainfo2/q7.1.17`, `datainfo1/datainfo2/q7.1.18`, `datainfo1/datainfo2/q7.1.19`)
    indikator7.2 <- inputSistem %>% select(`datainfo1/datainfo3/q7.2.1`, `datainfo1/datainfo3/q7.2.2`, `datainfo1/datainfo3/q7.2.3`, `datainfo1/datainfo3/q7.2.4`,
                                           `datainfo1/datainfo3/q7.2.5`, `datainfo1/datainfo3/q7.2.6`, `datainfo1/datainfo3/q7.2.7`, `datainfo1/datainfo3/q7.2.8`,
                                           `datainfo1/datainfo3/q7.2.9`, `datainfo1/datainfo3/q7.2.10`, `datainfo1/datainfo3/q7.2.11`, `datainfo1/datainfo3/q7.2.12`,
                                           `datainfo1/datainfo3/q7.2.13`, `datainfo1/datainfo3/q7.2.14`, `datainfo1/datainfo3/q7.2.15`, `datainfo1/datainfo3/q7.2.16`,
                                           `datainfo1/datainfo3/q7.2.17`, `datainfo1/datainfo3/q7.2.18`, `datainfo1/datainfo3/q7.2.19`)
    indikator7.3 <- inputSistem %>% select(`datainfo1/datainfo4/q7.3.1`, `datainfo1/datainfo4/q7.3.2`)
    indikator9.1 <- inputSistem %>% select(`pemantauan1/pemantauan2/q9.1.1`, `pemantauan1/pemantauan2/q9.1.2`, `pemantauan1/pemantauan2/q9.1.3`, `pemantauan1/pemantauan2/q9.1.4`,
                                           `pemantauan1/pemantauan2/q9.1.5`)
    indikator9.2 <- inputSistem %>% select(`pemantauan1/pemantauan3/q9.2.1`, `pemantauan1/pemantauan3/q9.2.2`, `pemantauan1/pemantauan3/q9.2.3`, `pemantauan1/pemantauan3/q9.2.4`,
                                           `pemantauan1/pemantauan3/q9.2.5`, `pemantauan1/pemantauan3/q9.2.6`)
    indikator9.3 <- inputSistem %>% select(`pemantauan1/pemantauan4/q9.3.1`, `pemantauan1/pemantauan4/q9.3.2`, `pemantauan1/pemantauan4/q9.3.3`)
    indikator9.4 <- inputSistem %>% select(`pemantauan1/pemantauan5/q9.4.1`, `pemantauan1/pemantauan5/q9.4.2`)
    
    temp_summSys <- cbind(inputSistem$`provinsi/provinsi_001`, inputSistem$year, aspek1, aspek2, indikator2.5, aspek3, 
                          indikator7.1, indikator7.2, indikator7.3, indikator9.1, indikator9.2, indikator9.2, indikator9.3, indikator9.4)
    
    sistem<- as.data.frame(lapply(temp_summSys[,3:(length(temp_summSys))], as.numeric))
    
    ##Rata-rata dari Indikator Tingkat Sistem###
    q2.5<-rowSums(sistem[,9:10]); q2.5<- as.data.frame(q2.5)/2
    q7.1 <- rowSums(sistem[,14:32]); q7.1<- as.data.frame(q7.1)/19
    q7.2 <- rowSums(sistem[,33:51]); q7.2<- as.data.frame(q7.2)/19
    q7.3<-rowSums(sistem[,52:53]); q7.3<-as.data.frame(q7.3)/2
    q9.1<-rowSums(sistem[,54:58]); q9.1<-as.data.frame(q9.1)/5
    q9.2<-rowSums(sistem[,59:64]); q9.2<-as.data.frame(q9.2)/6
    q9.3<-rowSums(sistem[,65:67]); q9.3<-as.data.frame(q9.3)/3
    q9.4<-rowSums(sistem[,68:69]); q9.4<-as.data.frame(q9.4)/2
    
    ##Tabel Level dari Indikator###
    levelSistem<-cbind(inputSistem$`provinsi/provinsi_001`,sistem$regulasi.regulasi1.q1.1,sistem$regulasi.regulasi2.q1.2,sistem$integrasi1.integrasi2.q2.1,sistem$integrasi1.integrasi3.q2.2,sistem$integrasi1.integrasi4.q2.3, sistem$integrasi1.integrasi5.q2.4, q2.5, sistem$proses1.proses2.q3.1, sistem$proses1.proses2_001.q3.2, sistem$proses1.proses3.q3.3, sistem$proses1.proses4.q3.4, sistem$proses1.proses4_001.q3.5, q7.1, q7.2, q7.3, q9.1, q9.2, q9.3, q9.4)
    colnames(levelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")
    
    tempSistem<-as.data.frame(cbind(year, levelSistem))
    
    file_indSys<- read.table("init/system.csv", header=TRUE, sep=",")
    indikatorSys <- as.character(unique(file_indSys$Kapasitas_Fungsional))
    
    ## Menampilkan hasil satu provinsi ##
    tempSistem<-filter(tempSistem,Provinsi==categoryProvince$provinsi)
    # tempSistem<-filter(tempSistem,Provinsi=="Aceh")
    
    ## Menampilkan level per indikator ##
    tableSistem <- aggregate(tempSistem[,3:length(tempSistem)], list(tempSistem$year), mean)
    roundTableSistem <- round(tableSistem[2:length(tableSistem)], digits=2)

    ## Data grafik setiap indikator ##
    finalTableSistem <- cbind(tableSistem$Group.1,roundTableSistem)
    colnames(finalTableSistem)<-c("Tahun", indikatorSys)
    finalTableSistem.long <- gather(finalTableSistem, variable, value, -Tahun)
    colnames(finalTableSistem.long) <- c("Tahun", "Indikator", "Level")
    tablesCDA$multiyearsSistem <- finalTableSistem.long

    ## Menampilkan table indikator ##
    t_tableSistem <- t(roundTableSistem)
    colnames(t_tableSistem) <- tableSistem$Group.1
    indikatorSys <- as.data.frame(indikatorSys)
    colnames(indikatorSys) <-"Indikator"
    t_tableSistem <- cbind(indikatorSys, t_tableSistem)
    multiyearsTable$multiSistem <- t_tableSistem

    datatable(t_tableSistem,escape = FALSE, rownames = FALSE)
  })

  output$multiChartSistem<- renderPlotly({
    finalTableSistem.long <- tablesCDA$multiyearsSistem
    graph <- ggplot(data = finalTableSistem.long, aes(x = Indikator, y = Level, fill = Tahun)) +
      geom_col(position = position_dodge()) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 6, hjust = 1, face = "plain"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      scale_x_discrete(labels = get_wraper(6))
    ggplotly(graph)
  })

  ####MENU ORGANISASI####
  ### Ringkasan Hasil Organisasi ###
  ### SUBMENU: Ringkasan Hasil Organisasi ####
  output$resTblOrgAll <- renderDataTable({
    
    summInputOrg<-koboData$organisasi
    summInputOrg$year <- format(as.Date(summInputOrg$`profil/tanggal`), format = "%Y")
    summInputOrg<-filter(summInputOrg,summInputOrg$year==input$selectedYear)
    # summInputOrg<-filter(summInputOrg,summInputOrg$year==2019)
    year <- summInputOrg$year
    # year <- 2019
    summInputOrg$`teknologi1/teknologi3/q8.2.3` <- NULL
    summInputOrg<-as.data.frame(summInputOrg)
    
    ##Define Indikator###
    indikator4.1 <- summInputOrg %>% select (`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/q4.1.1`, `perangkat1/Penentuan_Visi_Misi_dan_Tujuan/q4.1.2`)
    indikator4.2 <- summInputOrg %>% select (`perangkat1/perangkat2/q4.2.1`, `perangkat1/perangkat2/q4.2.2`, `perangkat1/perangkat2/q4.2.3`)
    indikator4.3 <- summInputOrg %>% select (`perangkat1/perangkat3/q4.3.1`, `perangkat1/perangkat3/q4.3.2`)
    indikator4.4 <- summInputOrg %>% select (`perangkat1/perangkat4/q4.4.1`, `perangkat1/perangkat4/q4.4.2`, `perangkat1/perangkat4/q4.4.3`, `perangkat1/perangkat4/q4.4.4`)
    indikator4.5 <- summInputOrg %>% select (`perangkat1/perangkat5/q4.5.1`, `perangkat1/perangkat5/q4.5.2`, `perangkat1/perangkat5/q4.5.3`)
    indikator4.6 <- summInputOrg %>% select (`perangkat1/perangkat6/q4.6.1`, `perangkat1/perangkat6/q4.6.2`)
    indikator4.7 <- summInputOrg %>% select (`perangkat1/perangkat7/q4.7.1`, `perangkat1/perangkat7/q4.7.2`, `perangkat1/perangkat7/q4.7.3`, `perangkat1/perangkat7/q4.7.4`,
                                             `perangkat1/perangkat7/q4.7.5`, `perangkat1/perangkat7/q4.7.6`, `perangkat1/perangkat7/q4.7.7`)
    indikator5.1 <- summInputOrg %>% select (`sdm1/sdm2/q5.1.1`, `sdm1/sdm2/q5.1.2`, `sdm1/sdm2/q5.1.3`, `sdm1/sdm2/q5.1.4`, `sdm1/sdm2/q5.1.5`,
                                             `sdm1/sdm2/q5.1.6`, `sdm1/sdm2/q5.1.7`)
    indikator5.2 <- summInputOrg %>% select (`sdm1/sdm3/q5.2`)
    indikator5.3 <- summInputOrg %>% select (`sdm1/sdm4/q5.3`)
    indikator5.4 <- summInputOrg %>% select (`sdm1/sdm5/q5.4.1`, `sdm1/sdm5/q5.4.2`)
    indikator5.5 <- summInputOrg %>% select (`sdm1/sdm6/q5.5.1`, `sdm1/sdm6/q5.5.2`)
    indikator8.1 <- summInputOrg %>% select (`teknologi1/teknologi2/q8.1.1`, `teknologi1/teknologi2/q8.1.2`, `teknologi1/teknologi2/q8.1.3`, `teknologi1/teknologi2/q8.1.4`)
    indikator8.2 <- summInputOrg %>% select (`teknologi1/teknologi3/q8.2.1`, `teknologi1/teknologi3/q8.2.2`)
    indikator8.3 <- summInputOrg %>% select (`teknologi1/teknologi4/q8.3.1`, `teknologi1/teknologi4/q8.3.2`)
    
    temp_summOrg <- cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`, summInputOrg$year, indikator4.1, indikator4.2,
                          indikator4.3, indikator4.4, indikator4.5, indikator4.6, indikator4.7, indikator5.1, indikator5.2, indikator5.3, indikator5.4, indikator5.5,
                          indikator8.1, indikator8.2, indikator8.3)
    
    summOrg<- as.data.frame(lapply(temp_summOrg[,5:length(temp_summOrg)], as.numeric))
    
    ##Rata-rata dari Indikator Tingkat Organisai###
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
    q8.2<-rowSums(summOrg[,41:42]); q8.2<-as.data.frame(q8.2)/2
    q8.3<-rowSums(summOrg[,43:44]); q8.3<-as.data.frame(q8.3)/2
    
    valOrganisasi <-cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    colnames(valOrganisasi) <-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
    summTempOrganisasi <-as.data.frame(cbind(valOrganisasi,year))
    
    indikatorOrg <-read.table("init/organisasi.csv", header=TRUE, sep=";")
    summIndikatorOrg <-as.data.frame(indikatorOrg$Perbaikan)
    colnames(summIndikatorOrg) <-"Indikator"
    
    ## Menampilkan hasil satu provinsi untuk tingkat organisasi ##
    summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==categoryProvince$provinsi)
    # summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Aceh")
    
    ## Membuat tabel Level setiap aspek ##
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
    summOrganisasi<-datatable(summOrganisasi,escape = FALSE, rownames = FALSE)
    
    ## Menampilkan level per indikator ##
    Ind4.1 <-mean(summTempOrganisasi$q4.1); Ind4.2<-mean(summTempOrganisasi$q4.2); Ind4.3<-mean(summTempOrganisasi$q4.3); Ind4.4<-mean(summTempOrganisasi$q4.4); Ind4.5<-mean(summTempOrganisasi$q4.5); Ind4.6<-mean(summTempOrganisasi$q4.6); Ind4.7<-mean(summTempOrganisasi$q4.7)
    Ind5.1 <-mean(summTempOrganisasi$q5.1); Ind5.2<-mean(summTempOrganisasi$q5.2); Ind5.3<-mean(summTempOrganisasi$q5.3); Ind5.4<-mean(summTempOrganisasi$q5.4); Ind5.5<-mean(summTempOrganisasi$q5.5)
    Ind8.1 <-mean(summTempOrganisasi$q8.1);Ind8.2<-mean(summTempOrganisasi$q8.2);Ind8.3<-mean(summTempOrganisasi$q8.3)
    levelProvOrg <-as.data.frame(t(cbind(Ind4.1,Ind4.2,Ind4.3,Ind4.4,Ind4.5,Ind4.6,Ind4.7,Ind5.1,Ind5.2,Ind5.3,Ind5.4,Ind5.5,Ind8.1,Ind8.2,Ind8.3)))
    levelProvOrg <-round(levelProvOrg,digits = 2)
    gapProvOrg <-5-levelProvOrg
    provOrg <-as.data.frame(cbind(summIndikatorOrg,levelProvOrg,gapProvOrg))
    colnames(provOrg) <-c("Indikator", "Level", "GAP")
    tablesCDA$summaryProvOrg <-provOrg

    summOrganisasi
    #datatable(summOrganisasi,escape = FALSE, rownames = FALSE)
  })

  output$resChartOrgAll <- renderPlotly({
    provOrg <-tablesCDA$summaryProvOrg
    plot_ly(provOrg, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack') %>%
      layout(legend = list(orientation = 'h')) %>%
      layout(yaxis = list(tickfont = list(size = 8), tickangle = 45, title = ""),
             xaxis = list(title = ""))
  })

  ## ggplot untuk unduh hasil anlisis ##
  output$resChartOrgAll2 <- renderPlotly({
    nilai1 <- t(provOrg$Level)
    nilai2 <- t(provOrg$GAP)
    nilai <- t(cbind(nilai1,nilai2))
    jenis1 <- t(rep("Level", length(provOrg$Level)))
    jenis2 <- t(rep("Gap", length(provOrg$GAP)))
    jenis <- t(cbind(jenis1,jenis2))
    indikator <- data.frame(provOrg$Indikator)
    dataGraphOrg <- data.frame(cbind(jenis,nilai,indikator))
    colnames(dataGraphOrg) <- c("jenis", "nilai", "indikator")

    dataGraphOrg <- ddply(dataGraphOrg, .(indikator),
                          transform, pos = cumsum(nilai)-nilai)
    chartOrg<-ggplot() + geom_bar(data=dataGraphOrg, aes(x=indikator, y=nilai, fill=jenis), stat="identity") +
      geom_text(data=dataGraphOrg, aes(x =indikator, y =pos, label =paste0(nilai)), size=4)

    final_chart$chartOrganisasi<-chartOrg
  })

  ### SUBMENU: Pebandingan Hasil Tahunan Organisasi ####
  output$multiTableOrganisasi <- renderDataTable({
    
    summInputOrg<-koboData$organisasi
    summInputOrg$`teknologi1/teknologi3/q8.2.3` <- NULL
    summInputOrg$year <- format(as.Date(summInputOrg$`profil/tanggal`), format = "%Y")
    # summInputOrg<-filter(summInputOrg,summInputOrg$year==input$selectedYear)
    # summInputOrg<-filter(summInputOrg,summInputOrg$year==2019)
    year <- summInputOrg$year
    # year <- 2019
    summInputOrg<-as.data.frame(summInputOrg)
    
    ##Define Indikator###
    indikator4.1 <- summInputOrg %>% select (`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/q4.1.1`, `perangkat1/Penentuan_Visi_Misi_dan_Tujuan/q4.1.2`)
    indikator4.2 <- summInputOrg %>% select (`perangkat1/perangkat2/q4.2.1`, `perangkat1/perangkat2/q4.2.2`, `perangkat1/perangkat2/q4.2.3`)
    indikator4.3 <- summInputOrg %>% select (`perangkat1/perangkat3/q4.3.1`, `perangkat1/perangkat3/q4.3.2`)
    indikator4.4 <- summInputOrg %>% select (`perangkat1/perangkat4/q4.4.1`, `perangkat1/perangkat4/q4.4.2`, `perangkat1/perangkat4/q4.4.3`, `perangkat1/perangkat4/q4.4.4`)
    indikator4.5 <- summInputOrg %>% select (`perangkat1/perangkat5/q4.5.1`, `perangkat1/perangkat5/q4.5.2`, `perangkat1/perangkat5/q4.5.3`)
    indikator4.6 <- summInputOrg %>% select (`perangkat1/perangkat6/q4.6.1`, `perangkat1/perangkat6/q4.6.2`)
    indikator4.7 <- summInputOrg %>% select (`perangkat1/perangkat7/q4.7.1`, `perangkat1/perangkat7/q4.7.2`, `perangkat1/perangkat7/q4.7.3`, `perangkat1/perangkat7/q4.7.4`,
                                             `perangkat1/perangkat7/q4.7.5`, `perangkat1/perangkat7/q4.7.6`, `perangkat1/perangkat7/q4.7.7`)
    indikator5.1 <- summInputOrg %>% select (`sdm1/sdm2/q5.1.1`, `sdm1/sdm2/q5.1.2`, `sdm1/sdm2/q5.1.3`, `sdm1/sdm2/q5.1.4`, `sdm1/sdm2/q5.1.5`,
                                             `sdm1/sdm2/q5.1.6`, `sdm1/sdm2/q5.1.7`)
    indikator5.2 <- summInputOrg %>% select (`sdm1/sdm3/q5.2`)
    indikator5.3 <- summInputOrg %>% select (`sdm1/sdm4/q5.3`)
    indikator5.4 <- summInputOrg %>% select (`sdm1/sdm5/q5.4.1`, `sdm1/sdm5/q5.4.2`)
    indikator5.5 <- summInputOrg %>% select (`sdm1/sdm6/q5.5.1`, `sdm1/sdm6/q5.5.2`)
    indikator8.1 <- summInputOrg %>% select (`teknologi1/teknologi2/q8.1.1`, `teknologi1/teknologi2/q8.1.2`, `teknologi1/teknologi2/q8.1.3`, `teknologi1/teknologi2/q8.1.4`)
    indikator8.2 <- summInputOrg %>% select (`teknologi1/teknologi3/q8.2.1`, `teknologi1/teknologi3/q8.2.2`)
    indikator8.3 <- summInputOrg %>% select (`teknologi1/teknologi4/q8.3.1`, `teknologi1/teknologi4/q8.3.2`)
    
    temp_summOrg <- cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`, summInputOrg$year, indikator4.1, indikator4.2,
                          indikator4.3, indikator4.4, indikator4.5, indikator4.6, indikator4.7, indikator5.1, indikator5.2, indikator5.3, indikator5.4, indikator5.5,
                          indikator8.1, indikator8.2, indikator8.3)
    
    summOrg<- as.data.frame(lapply(temp_summOrg[,5:length(temp_summOrg)], as.numeric))
    
    ##Rata-rata dari Indikator Tingkat Organisai###
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
    q8.2<-rowSums(summOrg[,41:42]); q8.2<-as.data.frame(q8.2)/2
    q8.3<-rowSums(summOrg[,43:44]); q8.3<-as.data.frame(q8.3)/2
    
    valOrganisasi <-cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    colnames(valOrganisasi) <-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
    summTempOrganisasi <-as.data.frame(cbind(year, valOrganisasi))

    indikatorOrg <-read.table("init/organisation.csv", header=TRUE, sep=",")
    summIndikatorOrg <-as.character(unique(indikatorOrg$Kapasitas_Fungsional))

    ## Menampilkan hasil satu provinsi untuk tingkat organisasi ##
    summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==categoryProvince$provinsi)
    # summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Aceh")

    ## Menampilkan level per indikator ##
    tableOrganisasi <- aggregate(summTempOrganisasi[,5:length(summTempOrganisasi)], list(summTempOrganisasi$year), mean)
    roundTableOrganisasi <- round(tableOrganisasi[2:length(tableOrganisasi)], digits=2)

    ## Data grafik setiap indikator ##
    finalTableOrganisasi <- cbind(tableOrganisasi$Group.1,roundTableOrganisasi)
    colnames(finalTableOrganisasi)<-c("Tahun", summIndikatorOrg)
    finalTableOrganisasi.long <- gather(finalTableOrganisasi, variable, value, -Tahun)
    colnames(finalTableOrganisasi.long) <- c("Tahun", "Indikator", "Level")
    tablesCDA$multiyearsOrganisasi <- finalTableOrganisasi.long

    ## Menampilkan table indikator ##
    t_tableOrganisasi <- t(roundTableOrganisasi)
    colnames(t_tableOrganisasi) <- tableOrganisasi$Group.1
    summIndikatorOrg <- as.data.frame(summIndikatorOrg)
    colnames(summIndikatorOrg) <-"Indikator"
    t_tableOrganisasi <- cbind(summIndikatorOrg, t_tableOrganisasi)
    multiyearsTable$multiOrganisasi <- t_tableOrganisasi

    datatable(t_tableOrganisasi,escape = FALSE, rownames = FALSE)
  })

  output$multiChartOrganisasi <- renderPlotly({
    finalTableOrganisasi.long <- tablesCDA$multiyearsOrganisasi
    graph <- ggplot(data = finalTableOrganisasi.long, aes(x = Indikator, y = Level, fill = Tahun)) +
      geom_col(position = position_dodge()) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7, hjust = 1, face = "plain"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      scale_x_discrete(labels = get_wraper(8))
    ggplotly(graph)
  })

  ####MENU INDIVIDU####
  ### SUBMENU: Ringkasan Hasil Inidividu ####
  output$resTblIndAll <- renderDataTable({
    
    summInputInd<-koboData$individu
    summInputInd$`sdm_i1/sdm_i4/q6.3.7`<-NULL
    summInputInd$`sdm_i1/sdm_i3/q6.2.10`[is.na(summInputInd$`sdm_i1/sdm_i3/q6.2.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.10`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.11`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.11`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.12`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.12`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.13`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.13`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.14`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.14`)] <- 3
    
    summInputInd$year <- format(as.Date(summInputInd$`profil/tanggal`), format = "%Y")
    summInputInd<-filter(summInputInd,summInputInd$year==input$selectedYear)
    # summInputInd<-filter(summInputInd,summInputInd$year==2019)
    year <- summInputInd$year
    
    indikator6.1 <- summInputInd %>% select (`sdm_i1/sdm_i2/q6.1.1`, `sdm_i1/sdm_i2/q6.1.2`)
    indikator6.2 <- summInputInd %>% select (`sdm_i1/sdm_i3/q6.2.1`, `sdm_i1/sdm_i3/q6.2.2`, `sdm_i1/sdm_i3/q6.2.3`, `sdm_i1/sdm_i3/q6.2.4`,
                                             `sdm_i1/sdm_i3/q6.2.5`, `sdm_i1/sdm_i3/q6.2.6`, `sdm_i1/sdm_i3/q6.2.7`, `sdm_i1/sdm_i3/q6.2.8`,
                                             `sdm_i1/sdm_i3/q6.2.9`, `sdm_i1/sdm_i3/q6.2.10`)
    indikator6.3 <- summInputInd %>% select (`sdm_i1/sdm_i4/q6.3.1`, `sdm_i1/sdm_i4/q6.3.2`, `sdm_i1/sdm_i4/q6.3.3`,`sdm_i1/sdm_i4/q6.3.4`,
                                             `sdm_i1/sdm_i4/q6.3.5`, `sdm_i1/sdm_i4/q6.3.6`, `sdm_i1/sdm_i4/q6.3.8`, `sdm_i1/sdm_i4/q6.3.9`,
                                             `sdm_i1/sdm_i4/q6.3.10`, `sdm_i1/sdm_i4/q6.3.11`, `sdm_i1/sdm_i4/q6.3.12`, `sdm_i1/sdm_i4/q6.3.13`,
                                             `sdm_i1/sdm_i4/q6.3.14`)
    indikator6.4 <- summInputInd %>% select (`sdm_i1/sdm_i5/q6.4.1`, `sdm_i1/sdm_i5/q6.4.2`, `sdm_i1/sdm_i5/q6.4.3`)
    
    temp_summInd <- cbind(summInputInd$`profil/provinsi`, summInputInd$`profil/nama`, summInputInd$`profil/institusi`, indikator6.1, indikator6.2, indikator6.3, indikator6.4)
    
    summInd<- as.data.frame(lapply(temp_summInd[,4:length(temp_summInd)], as.numeric))
    
    q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(summInd[,3:12]); q6.2<-as.data.frame(q6.2)/10
    q6.3<-rowSums(summInd[,13:25]); q6.3<-as.data.frame(q6.3)/13
    q6.4<-rowSums(summInd[,26:28]); q6.4<-as.data.frame(q6.4)/3
    valInd<-cbind(summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
    colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
    summTempIndividu<-as.data.frame(cbind(valInd,year))
    
    summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    summIndikatorInd  <- as.data.frame(summIndikatorInd)
    
    summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==categoryProvince$provinsi)
    # summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Aceh")
    
    ## Membuat tabel Level setiap aspek ##
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-mean(as.matrix(summTempIndividu[3:(length(summTempIndividu)-1)]))
    Level6<-round(Level6,digits = 2)
    gap6<-5-Level6
    summIndividu<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(summIndividu)<-c("Aspek Penilaian","Level","GAP")
    
    ## Menampilkan level per indikator ##
    Ind6.1<-mean(summTempIndividu$q6.1); Ind6.2<-mean(summTempIndividu$q6.2); Ind6.3<-mean(summTempIndividu$q6.3); Ind6.4<-mean(summTempIndividu$q6.4)
    levelProvInd<-as.data.frame(t(cbind(Ind6.1,Ind6.2,Ind6.3,Ind6.4)))
    levelProvInd<-round(levelProvInd, digits=2)
    gapProvInd<-5-levelProvInd
    provInd<-as.data.frame(cbind(summIndikatorInd,levelProvInd,gapProvInd))
    colnames(provInd)<-c("Indikator", "Level", "GAP")
    tablesCDA$summaryProvInd <- provInd

    datatable(summIndividu,escape = FALSE, rownames = FALSE)
  })

  output$resChartIndAll <- renderPlotly({
    provInd <- tablesCDA$summaryProvInd
    plot_ly(provInd, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h') %>%
      add_trace(x=~GAP, name= 'GAP') %>%
      layout(yaxis=list(title='Indikator'), barmode='stack') %>%
      layout(legend = list(orientation = 'h')) %>%
      layout(yaxis = list(tickfont = list(size = 8), tickangle = 45, title = ""),
             xaxis = list(title = ""))
  })

  ### SUBMENU: Pebandingan Hasil Tahunan Individu ####
  output$multiTableIndividu <- renderDataTable({
    
    summInputInd<-koboData$individu
    summInputInd$`sdm_i1/sdm_i4/q6.3.7`<-NULL
    summInputInd$`sdm_i1/sdm_i3/q6.2.10`[is.na(summInputInd$`sdm_i1/sdm_i3/q6.2.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.10`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.11`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.11`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.12`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.12`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.13`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.13`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.14`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.14`)] <- 3
    
    summInputInd$year <- format(as.Date(summInputInd$`profil/tanggal`), format = "%Y")
    # summInputInd<-filter(summInputInd,summInputInd$year==input$selectedYear)
    # summInputInd<-filter(summInputInd,summInputInd$year==2019)
    year <- summInputInd$year
    
    indikator6.1 <- summInputInd %>% select (`sdm_i1/sdm_i2/q6.1.1`, `sdm_i1/sdm_i2/q6.1.2`)
    indikator6.2 <- summInputInd %>% select (`sdm_i1/sdm_i3/q6.2.1`, `sdm_i1/sdm_i3/q6.2.2`, `sdm_i1/sdm_i3/q6.2.3`, `sdm_i1/sdm_i3/q6.2.4`,
                                             `sdm_i1/sdm_i3/q6.2.5`, `sdm_i1/sdm_i3/q6.2.6`, `sdm_i1/sdm_i3/q6.2.7`, `sdm_i1/sdm_i3/q6.2.8`,
                                             `sdm_i1/sdm_i3/q6.2.9`, `sdm_i1/sdm_i3/q6.2.10`)
    indikator6.3 <- summInputInd %>% select (`sdm_i1/sdm_i4/q6.3.1`, `sdm_i1/sdm_i4/q6.3.2`, `sdm_i1/sdm_i4/q6.3.3`,`sdm_i1/sdm_i4/q6.3.4`,
                                             `sdm_i1/sdm_i4/q6.3.5`, `sdm_i1/sdm_i4/q6.3.6`, `sdm_i1/sdm_i4/q6.3.8`, `sdm_i1/sdm_i4/q6.3.9`,
                                             `sdm_i1/sdm_i4/q6.3.10`, `sdm_i1/sdm_i4/q6.3.11`, `sdm_i1/sdm_i4/q6.3.12`, `sdm_i1/sdm_i4/q6.3.13`,
                                             `sdm_i1/sdm_i4/q6.3.14`)
    indikator6.4 <- summInputInd %>% select (`sdm_i1/sdm_i5/q6.4.1`, `sdm_i1/sdm_i5/q6.4.2`, `sdm_i1/sdm_i5/q6.4.3`)
    
    temp_summInd <- cbind(summInputInd$`profil/provinsi`, summInputInd$`profil/nama`, summInputInd$`profil/institusi`, indikator6.1, indikator6.2, indikator6.3, indikator6.4)
    
    summInd<- as.data.frame(lapply(temp_summInd[,4:length(temp_summInd)], as.numeric))
    
    q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(summInd[,3:12]); q6.2<-as.data.frame(q6.2)/10
    q6.3<-rowSums(summInd[,13:25]); q6.3<-as.data.frame(q6.3)/13
    q6.4<-rowSums(summInd[,26:28]); q6.4<-as.data.frame(q6.4)/3

    valInd<-cbind(summInputInd$`profil/provinsi`, year, summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
    colnames(valInd)<-c("Provinsi", "Tahun", "Nama", "q6.1","q6.2","q6.3","q6.4")
    summTempIndividu<-as.data.frame(valInd)

    summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    summIndikatorInd  <- as.data.frame(summIndikatorInd)
    colnames(summIndikatorInd)[names(summIndikatorInd)=="summIndikatorInd"] <- "Indikator"

    summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==categoryProvince$provinsi)
    # summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Jawa Barat")

    ## Menampilkan level per indikator ##
    tableIndividu <- aggregate(summTempIndividu[,4:length(summTempIndividu)], list(summTempIndividu$Tahun), mean)
    roundTableIndividu <- round(tableIndividu[2:length(tableIndividu)], digits=2)

    ## Data grafik setiap indikator ##
    finalTableIndividu <- cbind(tableIndividu$Group.1,roundTableIndividu)
    colnames(finalTableIndividu)<-c("Tahun", "6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas & Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan & Motivasi")
    finalTableIndividu.long <- gather(finalTableIndividu, variable, value, -Tahun)
    colnames(finalTableIndividu.long) <- c("Tahun", "Indikator", "Level")
    tablesCDA$multiyearsIndividu <- finalTableIndividu.long

    ## Menampilkan table indikator ##
    t_tableIndividu <- t(roundTableIndividu)
    colnames(t_tableIndividu) <- tableIndividu$Group.1
    t_tableIndividu <- cbind(summIndikatorInd, t_tableIndividu)
    colnames(t_tableIndividu)[names(t_tableIndividu)=="summIndikatorInd"] <- "Indikator"
    multiyearsTable$multiIndividu <- t_tableIndividu

    datatable(t_tableIndividu,escape = FALSE, rownames = FALSE)
  })

  output$multiChartIndividu <- renderPlotly({
    finalTableIndividu.long <- tablesCDA$multiyearsIndividu
    graph <- ggplot(data = finalTableIndividu.long, aes(x = Indikator, y = Level, fill = Tahun)) +
      geom_col(position = position_dodge()) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7, hjust = 1, face = "plain"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())+
      scale_x_discrete(labels = get_wraper(25))
    ggplotly(graph)
  })

  ## ggplot untuk unduh hasil anlisis ##
  output$resChartIndAll2 <- renderPlotly({
    nilai1 <- t(provInd$Level)
    nilai2 <- t(provInd$GAP)
    nilai <- t(cbind(nilai1,nilai2))
    jenis1 <- t(rep("Level", length(provInd$Level)))
    jenis2 <- t(rep("Gap", length(provInd$GAP)))
    jenis <- t(cbind(jenis1,jenis2))
    indikator <- data.frame(provInd$Indikator)
    dataGraphInd <- data.frame(cbind(jenis,nilai,indikator))
    colnames(dataGraphInd) <- c("jenis", "nilai", "indikator")

    dataGraphInd <- ddply(dataGraphInd, .(indikator),
                          transform, pos = cumsum(nilai)-nilai)
    chartInd<-ggplot() + geom_bar(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis), stat="identity") +
      geom_text(data=dataGraphInd, aes(x =indikator, y =pos, label =paste0(nilai)), size=4)

    chartInd<-ggplot(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis)) +
      geom_bar(stat="identity") +
      coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank())

    final_chart$chartIndividu<-chartInd
  })


  ####MENU RANGKUMAN####
  ### SUBMENU: Ringkasan Hasil Rangkuman ####
  output$resTblSumm <- renderDataTable({
    #### Tabel Prioritas Tingkat Sistem ####
    summInputSys <- koboData$sistem
    summInputSys$`pemantauan1/pemantauan3/q9.2.6`[is.na(summInputSys$`pemantauan1/pemantauan3/q9.2.6`)] <- 3
    summInputSys$`pemantauan1/pemantauan5/q9.4.1`[is.na(summInputSys$`pemantauan1/pemantauan5/q9.4.1`)] <- 3
    summInputSys$`pemantauan1/pemantauan5/q9.4.2`[is.na(summInputSys$`pemantauan1/pemantauan5/q9.4.2`)] <- 3
    summInputSys$year <- format(as.Date(summInputSys$`provinsi/tanggal`), format = "%Y")
    summInputSys<-filter(summInputSys,summInputSys$year==input$selectedYear)
    # summInputSys<-filter(summInputSys,summInputSys$year==2019)
    
    ##Define Indikator and Aspek###
    aspek1 <- summInputSys %>% select(`regulasi/regulasi1/q1.1`, `regulasi/regulasi2/q1.2`)
    aspek2 <- summInputSys %>% select(`integrasi1/integrasi2/q2.1`, `integrasi1/integrasi3/q2.2`, `integrasi1/integrasi4/q2.3`, `integrasi1/integrasi5/q2.4`)
    indikator2.5 <- summInputSys %>% select(`integrasi1/integrasi6/q2.5.1`, `integrasi1/integrasi6/q2.5.2`)
    aspek3 <- summInputSys  %>% select(`proses1/proses2/q3.1`, `proses1/proses2_001/q3.2`, `proses1/proses3/q3.3`, `proses1/proses4/q3.4`, `proses1/proses4_001/q3.5`)
    indikator7.1 <- summInputSys %>% select(`datainfo1/datainfo2/q7.1.1`, `datainfo1/datainfo2/q7.1.2`, `datainfo1/datainfo2/q7.1.3`, `datainfo1/datainfo2/q7.1.4`,
                                            `datainfo1/datainfo2/q7.1.5`, `datainfo1/datainfo2/q7.1.6`, `datainfo1/datainfo2/q7.1.7`, `datainfo1/datainfo2/q7.1.8`,
                                            `datainfo1/datainfo2/q7.1.9`, `datainfo1/datainfo2/q7.1.10`, `datainfo1/datainfo2/q7.1.11`, `datainfo1/datainfo2/q7.1.12`,
                                            `datainfo1/datainfo2/q7.1.13`, `datainfo1/datainfo2/q7.1.14`, `datainfo1/datainfo2/q7.1.15`, `datainfo1/datainfo2/q7.1.16`,
                                            `datainfo1/datainfo2/q7.1.17`, `datainfo1/datainfo2/q7.1.18`, `datainfo1/datainfo2/q7.1.19`)
    indikator7.2 <- summInputSys %>% select(`datainfo1/datainfo3/q7.2.1`, `datainfo1/datainfo3/q7.2.2`, `datainfo1/datainfo3/q7.2.3`, `datainfo1/datainfo3/q7.2.4`,
                                            `datainfo1/datainfo3/q7.2.5`, `datainfo1/datainfo3/q7.2.6`, `datainfo1/datainfo3/q7.2.7`, `datainfo1/datainfo3/q7.2.8`,
                                            `datainfo1/datainfo3/q7.2.9`, `datainfo1/datainfo3/q7.2.10`, `datainfo1/datainfo3/q7.2.11`, `datainfo1/datainfo3/q7.2.12`,
                                            `datainfo1/datainfo3/q7.2.13`, `datainfo1/datainfo3/q7.2.14`, `datainfo1/datainfo3/q7.2.15`, `datainfo1/datainfo3/q7.2.16`,
                                            `datainfo1/datainfo3/q7.2.17`, `datainfo1/datainfo3/q7.2.18`, `datainfo1/datainfo3/q7.2.19`)
    indikator7.3 <- summInputSys %>% select(`datainfo1/datainfo4/q7.3.1`, `datainfo1/datainfo4/q7.3.2`)
    indikator9.1 <- summInputSys %>% select(`pemantauan1/pemantauan2/q9.1.1`, `pemantauan1/pemantauan2/q9.1.2`, `pemantauan1/pemantauan2/q9.1.3`, `pemantauan1/pemantauan2/q9.1.4`,
                                            `pemantauan1/pemantauan2/q9.1.5`)
    indikator9.2 <- summInputSys %>% select(`pemantauan1/pemantauan3/q9.2.1`, `pemantauan1/pemantauan3/q9.2.2`, `pemantauan1/pemantauan3/q9.2.3`, `pemantauan1/pemantauan3/q9.2.4`,
                                            `pemantauan1/pemantauan3/q9.2.5`, `pemantauan1/pemantauan3/q9.2.6`)
    indikator9.3 <- summInputSys %>% select(`pemantauan1/pemantauan4/q9.3.1`, `pemantauan1/pemantauan4/q9.3.2`, `pemantauan1/pemantauan4/q9.3.3`)
    indikator9.4 <- summInputSys %>% select(`pemantauan1/pemantauan5/q9.4.1`, `pemantauan1/pemantauan5/q9.4.2`)
    
    temp_summSys <- cbind(summInputSys$`provinsi/provinsi_001`, summInputSys$year, aspek1, aspek2, indikator2.5, aspek3, 
                          indikator7.1, indikator7.2, indikator7.3, indikator9.1, indikator9.2, indikator9.2, indikator9.3, indikator9.4)
    
    summSys<- as.data.frame(lapply(temp_summSys[,3:(length(temp_summSys))], as.numeric))
    
    ##Rata-rata dari Indikator Tingkat Sistem###
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
    summTempSistem<-filter(summTempSistem,summInputSys$`provinsi/provinsi_001`==categoryProvince$provinsi)
    # summTempSistem<-filter(summTempSistem,Provinsi=="Aceh")

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
    summInputOrg<-koboData$organisasi
    summInputOrg$`perangkat1/perangkat4/q4.4.3`[summInputOrg$`perangkat1/perangkat4/q4.4.3` == "n/a"]  <- 3
    summInputOrg$year <- format(as.Date(summInputOrg$`profil/tanggal`), format = "%Y")
    summInputOrg<-filter(summInputOrg,summInputOrg$year==input$selectedYear)
    # summInputOrg<-filter(summInputOrg,summInputOrg$year==2019)
    summInputOrg$`teknologi1/teknologi3/q8.2.3` <- NULL
    summInputOrg<-as.data.frame(summInputOrg)
    
    ##Define Indikator###
    indikator4.1 <- summInputOrg %>% select (`perangkat1/Penentuan_Visi_Misi_dan_Tujuan/q4.1.1`, `perangkat1/Penentuan_Visi_Misi_dan_Tujuan/q4.1.2`)
    indikator4.2 <- summInputOrg %>% select (`perangkat1/perangkat2/q4.2.1`, `perangkat1/perangkat2/q4.2.2`, `perangkat1/perangkat2/q4.2.3`)
    indikator4.3 <- summInputOrg %>% select (`perangkat1/perangkat3/q4.3.1`, `perangkat1/perangkat3/q4.3.2`)
    indikator4.4 <- summInputOrg %>% select (`perangkat1/perangkat4/q4.4.1`, `perangkat1/perangkat4/q4.4.2`, `perangkat1/perangkat4/q4.4.3`, `perangkat1/perangkat4/q4.4.4`)
    indikator4.5 <- summInputOrg %>% select (`perangkat1/perangkat5/q4.5.1`, `perangkat1/perangkat5/q4.5.2`, `perangkat1/perangkat5/q4.5.3`)
    indikator4.6 <- summInputOrg %>% select (`perangkat1/perangkat6/q4.6.1`, `perangkat1/perangkat6/q4.6.2`)
    indikator4.7 <- summInputOrg %>% select (`perangkat1/perangkat7/q4.7.1`, `perangkat1/perangkat7/q4.7.2`, `perangkat1/perangkat7/q4.7.3`, `perangkat1/perangkat7/q4.7.4`,
                                             `perangkat1/perangkat7/q4.7.5`, `perangkat1/perangkat7/q4.7.6`, `perangkat1/perangkat7/q4.7.7`)
    indikator5.1 <- summInputOrg %>% select (`sdm1/sdm2/q5.1.1`, `sdm1/sdm2/q5.1.2`, `sdm1/sdm2/q5.1.3`, `sdm1/sdm2/q5.1.4`, `sdm1/sdm2/q5.1.5`,
                                             `sdm1/sdm2/q5.1.6`, `sdm1/sdm2/q5.1.7`)
    indikator5.2 <- summInputOrg %>% select (`sdm1/sdm3/q5.2`)
    indikator5.3 <- summInputOrg %>% select (`sdm1/sdm4/q5.3`)
    indikator5.4 <- summInputOrg %>% select (`sdm1/sdm5/q5.4.1`, `sdm1/sdm5/q5.4.2`)
    indikator5.5 <- summInputOrg %>% select (`sdm1/sdm6/q5.5.1`, `sdm1/sdm6/q5.5.2`)
    indikator8.1 <- summInputOrg %>% select (`teknologi1/teknologi2/q8.1.1`, `teknologi1/teknologi2/q8.1.2`, `teknologi1/teknologi2/q8.1.3`, `teknologi1/teknologi2/q8.1.4`)
    indikator8.2 <- summInputOrg %>% select (`teknologi1/teknologi3/q8.2.1`, `teknologi1/teknologi3/q8.2.2`)
    indikator8.3 <- summInputOrg %>% select (`teknologi1/teknologi4/q8.3.1`, `teknologi1/teknologi4/q8.3.2`)
    
    temp_summOrg <- cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`, summInputOrg$year, indikator4.1, indikator4.2,
                          indikator4.3, indikator4.4, indikator4.5, indikator4.6, indikator4.7, indikator5.1, indikator5.2, indikator5.3, indikator5.4, indikator5.5,
                          indikator8.1, indikator8.2, indikator8.3)
    
    summOrg<- as.data.frame(lapply(temp_summOrg[,5:length(temp_summOrg)], as.numeric))
    
    ##Rata-rata dari Indikator Tingkat Organisai###
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
    q8.2<-rowSums(summOrg[,41:42]); q8.2<-as.data.frame(q8.2)/2
    q8.3<-rowSums(summOrg[,43:44]); q8.3<-as.data.frame(q8.3)/2
    valOrganisasi <- cbind(summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
    colnames(valOrganisasi)<-c("Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
    summTempOrganisasi<-as.data.frame(valOrganisasi)

    indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
    summIndikatorOrg <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
    colnames(summIndikatorOrg)<-"Indikator"

    ##Menampilkan hasil satu provinsi untuk tingkat organisasi##
    summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==categoryProvince$provinsi)
    # summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Aceh")

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

    ## Menampilkan level per indikator & prioritas ##
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

    ### Tabel Prioritas Tingkat Individu ####
    summInputInd<-koboData$individu
    summInputInd$`sdm_i1/sdm_i4/q6.3.7`<-NULL
    summInputInd$`sdm_i1/sdm_i3/q6.2.10`[is.na(summInputInd$`sdm_i1/sdm_i3/q6.2.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.10`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.11`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.11`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.12`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.12`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.13`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.13`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.14`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.14`)] <- 3

    summInputInd$year <- format(as.Date(summInputInd$`profil/tanggal`), format = "%Y")
    summInputInd<-filter(summInputInd,summInputInd$year==input$selectedYear)
    # summInputInd<-filter(summInputInd,summInputInd$year==2019)
    
    indikator6.1 <- summInputInd %>% select (`sdm_i1/sdm_i2/q6.1.1`, `sdm_i1/sdm_i2/q6.1.2`)
    indikator6.2 <- summInputInd %>% select (`sdm_i1/sdm_i3/q6.2.1`, `sdm_i1/sdm_i3/q6.2.2`, `sdm_i1/sdm_i3/q6.2.3`, `sdm_i1/sdm_i3/q6.2.4`,
                                             `sdm_i1/sdm_i3/q6.2.5`, `sdm_i1/sdm_i3/q6.2.6`, `sdm_i1/sdm_i3/q6.2.7`, `sdm_i1/sdm_i3/q6.2.8`,
                                             `sdm_i1/sdm_i3/q6.2.9`, `sdm_i1/sdm_i3/q6.2.10`)
    indikator6.3 <- summInputInd %>% select (`sdm_i1/sdm_i4/q6.3.1`, `sdm_i1/sdm_i4/q6.3.2`, `sdm_i1/sdm_i4/q6.3.3`,`sdm_i1/sdm_i4/q6.3.4`,
                                             `sdm_i1/sdm_i4/q6.3.5`, `sdm_i1/sdm_i4/q6.3.6`, `sdm_i1/sdm_i4/q6.3.8`, `sdm_i1/sdm_i4/q6.3.9`,
                                             `sdm_i1/sdm_i4/q6.3.10`, `sdm_i1/sdm_i4/q6.3.11`, `sdm_i1/sdm_i4/q6.3.12`, `sdm_i1/sdm_i4/q6.3.13`,
                                             `sdm_i1/sdm_i4/q6.3.14`)
    indikator6.4 <- summInputInd %>% select (`sdm_i1/sdm_i5/q6.4.1`, `sdm_i1/sdm_i5/q6.4.2`, `sdm_i1/sdm_i5/q6.4.3`)
    
    temp_summInd <- cbind(summInputInd$`profil/provinsi`, summInputInd$`profil/nama`, summInputInd$`profil/institusi`, indikator6.1, indikator6.2, indikator6.3, indikator6.4)
    
    summInd<- as.data.frame(lapply(temp_summInd[,4:length(temp_summInd)], as.numeric))
    
    q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(summInd[,3:12]); q6.2<-as.data.frame(q6.2)/10
    q6.3<-rowSums(summInd[,13:25]); q6.3<-as.data.frame(q6.3)/13
    q6.4<-rowSums(summInd[,26:28]); q6.4<-as.data.frame(q6.4)/3
    valInd<-cbind(summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
    colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
    summTempIndividu<-as.data.frame(valInd)

    summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    summIndikatorInd  <- as.data.frame(summIndikatorInd)

    summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==categoryProvince$provinsi)
    # summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Aceh")

    ## Membuat tabel Level setiap aspek ##
    Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
    Level6<-mean(as.matrix(summTempIndividu[3:length(summTempIndividu)]))
    gap6<-5-Level6
    summIndividu<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6, gap6))
    colnames(summIndividu)<-c("Aspek Penilaian","Level","GAP")

    ## Menampilkan level per indikator & prioritas ##
    Ind6.1<-mean(summTempIndividu$q6.1); Ind6.2<-mean(summTempIndividu$q6.2); Ind6.3<-mean(summTempIndividu$q6.3); Ind6.4<-mean(summTempIndividu$q6.4)
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

    ### Tabel Level Per Aspek Semua Tingkat ###
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
    # tablesCDA$priorityTable <- prioritas

    initial_indikator <- substring(prioritas$Indikator,1, 3)
    tabel <- data.frame(cbind(initial_indikator, prioritas$Level, prioritas$Prioritas))
    colnames(tabel)<-c("Indikator", "Level", "Prioritas")
    tabel$Level <- as.numeric(levels(tabel$Level))[tabel$Level]
    tabel$Rekomendasi <- "Tidak ada rekomendasi"

    ###Define the recommendation of each indicator####
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="1.1" & Level<=3 , "Penyusunan Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="1.2" & Level<=3 , "Sosialisasi Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD dan Pembuatan petunjuk operasional dalam regulasi yang mengatur PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.1" & Level<=3 , "Pengarusutamaan konsep pembangunan rendah karbon dalam visi misi daerah", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.2" & Level<=3 , "Pengarusutamaan isu strategis pembangunan rendah karbon dalam perencanaan pembangunan daerah ", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.3" & Level<=3 , "Pengarusutamaan prioritas pembangunan rendah karbon dalam perencanaan pembangunan daerah ", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.4" & Level<=3 , "Pengarusutamaan indikator pembangunan rendah karbon dalam perencanaan pembangunan daerah ", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="2.5" & Level<=3 , "Pengarusutamaan program pembangunan rendah karbon/aksi mitigasi sebagai program/kegiatan pembangunan daerah yang tertuang dalam RPJMD serta Renstra K/L", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.1" & Level<=3 , "Penyelesaian Kaji Ulang RAD GRK dalam rangka penyusunan Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.2" & Level<=3 , "Tanpa rekomendasi", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.3" & Level<=3 , "Penyelesaian Kaji Ulang RAD GRK dalam rangka penyusunan Peraturan Gubernur tentang Kaji Ulang RAD GRK/PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.4" & Level<=3 , "Tanpa rekomendasi", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="3.5" & Level<=3 , "Pengarusutamaan program pembangunan rendah karbon/aksi mitigasi sebagai program/kegiatan pembangunan daerah yang tertuang dalam RPJMD serta Renstra K/L", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.1" & Level<=3 , "Revitalisasi organisasi kelompok kerja", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.2" & Level<=3 , "Adaptasi struktur organisasi yang disesuaikan dengan kebutuhan implementasi pembangunan rendah karbon", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.3" & Level<=3 , "Penyusunan mekanisme pengambilan keputusan yang inklusif dan berbasiskan data yang shahih", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.4" & Level<=3 , "Penyusunan prosedur/proses kerja serta pengelolaan kelembagaan yang mencakup seluruh aktivitas Pokja", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.5" & Level<=3 , "Fasilitasi penyediaan anggaran bagi operasional Pokja termasuk proses penganggaran di dalam APBD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.6" & Level<=3 , "Fasilitasi terbentuknya harmonisasi kerja dalam Pokja PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="4.7" & Level<=3 , "Penyusunan mekanisme kerja sama antara Pokja dengan pihak eksternal", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.1" & Level<=3 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.2" & Level<=3 , "Penguatan kapasitas anggota Pokja dalam proses pengarusutamaan PPRKD ke dalam perencanaan pembangunan daerah", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.3" & Level<=3 , "Penguatan kapasitas anggota Pokja dalam proses penulisan/pelaporan pembangunan rendah karbon", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.4" & Level<=3 , "Penyusunan mekanisme kerja sama antara Pokja dengan pihak eksternal", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="5.5" & Level<=3 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.1" & Level<=3 , "Fasilitasi terbentuknya harmonisasi kerja dalam Pokja PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.2" & Level<=3 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.3" & Level<=3 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="6.4" & Level<=3 , "Fasilitasi terbentuknya harmonisasi kerja dalam Pokja PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="7.1" & Level<=3 , "Penyediaan data yang berkualitas dalam pelaksanaan PPRKD", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="7.2" & Level<=3 , "Pengelolaan data  yang berkualitas dalam pelaksanaan pembangunan rendah karbon", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="7.3" & Level<=3 , "Pengelolaan data  yang berkualitas dalam pelaksanaan pembangunan rendah karbon", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="8.1" & Level<=3 , "Peningkatan penggunaan perangkat lunak dalam pengelolaan data dan analisis teknis dalam rangka perencanaan dan pemantauan, evaluasi, serta pelaporan pembangunan rendah karbon", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="8.2" & Level<=3 , "Pengadaan perangkat keras penunjang aktivitas perencanaan, pemantauan, evaluasi, dan pelaporan pembangunan rendah karbon", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="8.3" & Level<=3 , "Penyediaan kapasitas jaringan yang mendukung proses perencanaan, pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.1" & Level<=3 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.2" & Level<=3 , "Peningkatan kapasitas teknis anggota Pokja dalam penyusunan PPRKD serta pemantauan, evaluasi, dan pelaporan", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.3" & Level<=3 , "Penyusunan prosedur pelibatan berbagai pihak termasuk kabupaten/kota dan swasta dalam proses pemantauan, evaluasi, dan pelaporan program pembangunan rendah karbon", Rekomendasi)})
    tabel <- within(tabel, {Rekomendasi<-ifelse(Indikator=="9.4" & Level<=3 , "Penyusunan prosedur pemanfaatan data pemantauan, evaluasi, dan pelaporan bagi kepentingan berbagai pihak", Rekomendasi)})

    tabel$Indikator <- prioritas$Indikator
    tablesCDA$priorityTable <- tabel

    datatable(tabel,escape = FALSE, rownames = FALSE)

  })
  ## Bar Chart Semua Tingkat ##
  output$resChartSumm <- renderPlotly({
    summary<-tablesCDA$allSummary
    plot_ly(summary, x=~Aspek, y=~Level, type='bar', name='Level') %>%
      add_trace(y=~GAP, name='GAP') %>%
      layout(
        yaxis = list(title=''),
        xaxis = list(title=''),
        barmode='stack')
  })
  ## ggplot untuk unduh hasil anlisis ##
  output$resChartSumm2 <- renderPlotly({
    nilai1 <- t(summary$Level)
    nilai2 <- t(summary$GAP)
    nilai <- t(cbind(nilai1,nilai2))
    jenis1 <- t(rep("Level", length(summary$Level)))
    jenis2 <- t(rep("Gap", length(summary$GAP)))
    jenis <- t(cbind(jenis1,jenis2))
    indikator <- data.frame(summary$Indikator)
    dataGraphSumm <- data.frame(cbind(jenis,nilai,indikator))
    colnames(dataGraphSumm) <- c("jenis", "nilai", "indikator")

    dataGraphSumm <- ddply(dataGraphSumm, .(indikator),
                           transform, pos = cumsum(nilai)-nilai)
    chartSumm<-ggplot() + geom_bar(data=dataGraphSumm, aes(x=indikator, y=nilai, fill=jenis), stat="identity") +
      geom_text(data=dataGraphSumm, aes(x =indikator, y =nilai, label =paste0(nilai)), size=4)

    final_chart$chartSummary<-chartSumm
  })

  output$downloadResults <- downloadHandler(
    filename = paste0(categoryProvince$provinsi, "_hasil.doc"),
    content = function(file){
      graphSistem<-tablesCDA$summarySystem
      provOrg<-tablesCDA$summaryProvOrg
      provInd<-tablesCDA$summaryProvInd
      tabel<-tablesCDA$priorityTable
      summary<-tablesCDA$allSummary

      ## ggplot unduhan hasil analisis ####
      ## individu ###
      nilai1 <- t(provInd$Level)
      nilai2 <- t(provInd$GAP)
      nilai <- t(cbind(nilai1,nilai2))
      jenis1 <- t(rep("Level", length(provInd$Level)))
      jenis2 <- t(rep("Gap", length(provInd$GAP)))
      jenis <- t(cbind(jenis1,jenis2))
      indikator <- data.frame(provInd$Indikator)
      dataGraphInd <- data.frame(cbind(jenis,nilai,indikator))
      colnames(dataGraphInd) <- c("jenis", "nilai", "indikator")
      chartInd<-ggplot(data=dataGraphInd, aes(x=indikator, y=nilai, fill=jenis)) +
        geom_bar(stat="identity") +
        coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
        theme(legend.position="bottom", legend.direction="horizontal",
              legend.title = element_blank()) +
        geom_text(data=dataGraphInd, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)

      ## organisasi ###
      nilai1 <- t(provOrg$Level)
      nilai2 <- t(provOrg$GAP)
      nilai <- t(cbind(nilai1,nilai2))
      jenis1 <- t(rep("Level", length(provOrg$Level)))
      jenis2 <- t(rep("Gap", length(provOrg$GAP)))
      jenis <- t(cbind(jenis1,jenis2))
      indikator <- data.frame(provOrg$Indikator)
      dataGraphOrg <- data.frame(cbind(jenis,nilai,indikator))
      colnames(dataGraphOrg) <- c("jenis", "nilai", "indikator")
      chartOrg<-ggplot(data=dataGraphOrg, aes(x=indikator, y=nilai, fill=jenis)) +
        geom_bar(stat="identity") +
        coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
        theme(legend.position="bottom", legend.direction="horizontal",
              legend.title = element_blank()) +
        geom_text(data=dataGraphOrg, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)

      ## sistem ###
      nilai1 <- t(graphSistem$Level)
      nilai2 <- t(graphSistem$GAP)
      nilai <- t(cbind(nilai1,nilai2))
      jenis1 <- t(rep("Level", length(graphSistem$Level)))
      jenis2 <- t(rep("Gap", length(graphSistem$GAP)))
      jenis <- t(cbind(jenis1,jenis2))
      indikator <- data.frame(graphSistem$Indikator)
      dataGraphSys <- data.frame(cbind(jenis,nilai,indikator))
      colnames(dataGraphSys) <- c("jenis", "nilai", "indikator")
      chartSys<-ggplot(data=dataGraphSys, aes(x=indikator, y=nilai, fill=jenis)) +
        geom_bar(stat="identity") +
        coord_flip() + guides(fill=guide_legend()) + xlab("Indikator") + ylab("Nilai") +
        theme(legend.position="bottom", legend.direction="horizontal",
              legend.title = element_blank()) +
        geom_text(data=dataGraphSys, aes(x =indikator, y =nilai, label =paste0(nilai)), size=2)

      ## rangkuman ###
      nilai1 <- t(summary$Level)
      nilai2 <- t(summary$GAP)
      nilai <- t(cbind(nilai1,nilai2))
      jenis1 <- t(rep("Level", length(summary$Level)))
      jenis2 <- t(rep("Gap", length(summary$GAP)))
      jenis <- t(cbind(jenis1,jenis2))
      aspek <- data.frame(summary$Aspek)
      dataGraphSumm <- data.frame(cbind(jenis,nilai,aspek))
      colnames(dataGraphSumm) <- c("jenis", "nilai", "aspek")
      chartSumm<-ggplot(data=dataGraphSumm, aes(x=aspek, y=nilai, fill=jenis)) +
        geom_bar(stat="identity") +
        coord_flip() + guides(fill=guide_legend()) + xlab("Aspek") + ylab("Nilai") +
        theme(legend.position="bottom", legend.direction="horizontal",
              legend.title = element_blank()) +
        geom_text(data=dataGraphSumm, aes(x =aspek, y =nilai, label =paste0(nilai)), size=2)

      ## Isi unduhan hasil analisis ####
      title <- "\\b\\fs32 Hasil Analisis Penilaian Kapasistas Mandiri\\b0\\fs20"
      fileresult = file.path(tempdir(), paste0(categoryProvince$provinsi, "_hasil.doc"))
      rtffile <- RTF(fileresult, font.size = 9)
      addParagraph(rtffile, title)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Tabel 1 Aspek Penilaian Tingkat Sistem per Provinsi\\b0\\fs14")
      addTable(rtffile, graphSistem, font.size = 8)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Grafik 1 Indikator Penilaian Tingkat Sistem per Provinsi\\b0\\fs14")
      addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartSys)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Tabel 2 Aspek Penilaian Tingkat Organiasi per Provinsi\\b0\\fs14")
      addTable(rtffile, provOrg, font.size = 8)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Grafik 2 Indikator Penilaian Tingkat Organiasi per Provinsi\\b0\\fs14")
      addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartOrg)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Tabel 3 Aspek Penilaian Tingkat Individu per Provinsi\\b0\\fs14")
      addTable(rtffile, provInd, font.size = 8)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Grafik 3 Indikator Penilaian Tingkat Individu per Provinsi\\b0\\fs14")
      addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartInd)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Tabel 4 Rangkuman Hasil dan Tingkat Prioritas per Provinsi\\b0\\fs14")
      addTable(rtffile, tabel, font.size = 8)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs14 Grafik 4 Grafik 9 Aspek Penilaian per Provinsi\\b0\\fs14")
      addPlot(rtffile, plot.fun = print, width = 7, height = 3, res = 100, chartSumm)
      done(rtffile)

      file.copy(fileresult, file)
    }
  )
  ### SUBMENU: Pebandingan Hasil Tahunan Rangkuman ####
  output$multiTableRangkuman <- renderDataTable({
    t_tableSistem <- multiyearsTable$multiSistem
    t_tableOrganisasi <- multiyearsTable$multiOrganisasi
    t_tableIndividu <- multiyearsTable$multiIndividu

    summarySistem <- colMeans(t_tableSistem[2:length(t_tableSistem)])
    t_summarySistem <- as.data.frame(t(summarySistem))
    tingkatSistem <- "Sistem"
    t_summarySistem <- cbind(tingkatSistem, t_summarySistem)
    colnames(t_summarySistem)[names(t_summarySistem)=="tingkatSistem"] <- "Tingkat"

    summaryOrganisasi <- colMeans(t_tableOrganisasi[2:length(t_tableOrganisasi)])
    t_summaryOrganisasi <- as.data.frame(t(summaryOrganisasi))
    tingkatOrganisasi <- "Organisasi"
    t_summaryOrganisasi <- cbind(tingkatOrganisasi, t_summaryOrganisasi)
    colnames(t_summaryOrganisasi)[names(t_summaryOrganisasi)=="tingkatOrganisasi"] <- "Tingkat"

    summaryIndividu <- colMeans(t_tableIndividu[2:length(t_tableIndividu)])
    t_summaryIndividu <- as.data.frame(t(summaryIndividu))
    tingkatIndividu <- "Individu"
    t_summaryIndividu <- cbind(tingkatIndividu, t_summaryIndividu)
    colnames(t_summaryIndividu)[names(t_summaryIndividu)=="tingkatIndividu"] <- "Tingkat"

    # multiyearsSummary <- bind_rows(t_summarySistem, t_summaryOrganisasi, t_summaryIndividu)
    multiyearsSummary <- smartbind(t_summarySistem, t_summaryOrganisasi, t_summaryIndividu) #library(gtools)
    multiyearsSummary[,2:length(multiyearsSummary)] <- round(multiyearsSummary[,2:length(multiyearsSummary)], digits = 2)
    # roundSummary <- round(multiyearsSummary[,2:length(multiyearsSummary)], digits = 2)
    # temp_multiyearsSummary <- cbind(multiyearsSummary$Tingkat, roundSummary)
    # colnames(multiyearsSummary)[names(multiyearsSummary)=="multiyearsSummary$Tingkat"] <- "Tingkat"

    multiyearsSummary.long <- gather(multiyearsSummary, variable, value, -Tingkat)
    multiyearsSummary.long[,3:length(multiyearsSummary.long)] <- round(multiyearsSummary.long[,3:length(multiyearsSummary.long)], digits = 2)
    tablesCDA$multiyearsRangkuman <- multiyearsSummary.long

    datatable(multiyearsSummary,escape = FALSE, rownames = FALSE)
  })

  output$multiChartRangkuman <- renderPlotly({
    multiyearsSummary.long <- tablesCDA$multiyearsRangkuman
    graph <- ggplot(data = multiyearsSummary.long, aes(x = variable, y = value, fill = Tingkat)) +
      geom_col(position = position_dodge()) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 7, hjust = 1, face = "plain"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
    ggplotly(graph)
  })

}
# Run the application
shinyApp(ui = ui, server = server)

