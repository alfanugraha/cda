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

saveRDS(dataSistem, "data/dataSistem")
saveRDS(dataOrganisasi, "data/dataOrganisasi")
saveRDS(dataIndividu, "data/dataIndividu")

# Define UI
ui <- fluidPage(
  
  HTML('<meta name="viewport" content="width=device-width">'),
  # tags$head( tags$meta(name = "viewport", content = "width=1600")
  
  #Navbar structure for UI
  navbarPage("", theme = shinytheme("lumen"),
             tabPanel("Nasional", fluid = TRUE, icon = icon("globe"),
                      tags$style(button_color_css),
                      selectInput("selectedYear", label="Tahun", choices = 2019:2030, selected = 2019),
                      withSpinner(plotlyOutput(outputId = "grafikNasional"))
             )
  ),
  tags$style(type="text/css", ".navbar-nav {float: right; margin: 0;}")
)

# Define server
server <- function(input, output, session) {
  tablesCDA <- reactiveValues(summarySystem=data.frame(),summaryOrg=data.frame(), summaryInd=data.frame(), allSummary=data.frame(), summaryProvInd=data.frame(), summaryProvOrg=data.frame(), priorityTable=data.frame())
  final_chart <- reactiveValues(chartSistem=NULL, chartOrganisasi=NULL, chartIndividu=NULL, chartSummary=NULL)
  graph_data <- reactiveValues(provInd=NULL, provOrg=NULL, graphSistem=NULL, chartSummary=NULL)
  
  output$grafikNasional <- renderPlotly({
    ### TINGKAT SISTEM ####
    
    summInputSys <- readRDS("data/dataSistem")
    # summInputSys$`pemantauan1/pemantauan3/q9.2.6`[summInputSys$`pemantauan1/pemantauan3/q9.2.6` == "n/a"]  <- NA
    # summInputSys$`pemantauan1/pemantauan5/q9.4.1`[summInputSys$`pemantauan1/pemantauan5/q9.4.1` == "n/a"]  <- NA
    # summInputSys$`pemantauan1/pemantauan5/q9.4.2`[summInputSys$`pemantauan1/pemantauan5/q9.4.2` == "n/a"]  <- NA
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
    
    ##Tabel Level dari Indikator###
    summLevelSistem<-cbind(summInputSys$`provinsi/provinsi_001`,summSys$regulasi.regulasi1.q1.1,summSys$regulasi.regulasi2.q1.2,summSys$integrasi1.integrasi2.q2.1,summSys$integrasi1.integrasi3.q2.2,summSys$integrasi1.integrasi4.q2.3, summSys$integrasi1.integrasi5.q2.4, q2.5, summSys$proses1.proses2.q3.1, summSys$proses1.proses2_001.q3.2, summSys$proses1.proses3.q3.3, summSys$proses1.proses4.q3.4, summSys$proses1.proses4_001.q3.5, q7.1, q7.2, q7.3, q9.1, q9.2, q9.3, q9.4)
    colnames(summLevelSistem)<-c("Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")
    summTempSistem<-as.data.frame((summLevelSistem))
    
    ##Membuat tabel Level setiap aspek###
    aspekSys<-c("Provinsi","Regulasi/peraturan daerah","Integrasi dalam Perencanaan Pembangunan Daerah", "Proses", "Data dan Informasi", "Pemantauan, Evaluasi, dan Pelaporan")
    LevelReg<-rowMeans(as.matrix(summTempSistem[,2:3])); LevelInt<-rowMeans(as.matrix(summTempSistem[4:8])); LevelProses<-rowMeans(as.matrix(summTempSistem[9:13])); LevelData<-rowMeans(as.matrix(summTempSistem[14:16])); LevelPEP<-rowMeans(as.matrix(summTempSistem[17:20]))
    summ_allLevelSys<-as.data.frame((cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
    finalSistem<-cbind(summTempSistem$Provinsi, summ_allLevelSys)
    colnames(finalSistem)[names(finalSistem)=="summTempSistem$Provinsi"] <- "Provinsi"
    
    table_nasionalSistem <- ddply(finalSistem, 'Provinsi', summarize, a1 = mean(LevelReg), a2 = mean(LevelInt), a3 = mean(LevelProses), a4 = mean(LevelData), a5 = mean(LevelPEP))
    colnames(table_nasionalSistem)<-aspekSys
    
    ### TINGKAT ORGANISASI ####
    
    summInputOrg<-readRDS("data/dataOrganisasi")
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
    
    ##Membuat tabel Level setiap aspek###
    LevelOrg <- rowMeans(summTempOrganisasi[,4:10])
    LevelSDM <- rowMeans(summTempOrganisasi[,11:15])
    LevelTek <- rowMeans(summTempOrganisasi[,16:18])
    LevelOrg_gabungan<-as.data.frame((cbind(LevelOrg,LevelSDM,LevelTek)))
    finalOrganisasi<-as.data.frame(cbind(summTempOrganisasi$Provinsi, LevelOrg_gabungan))
    colnames(finalOrganisasi)[names(finalOrganisasi)=="summTempOrganisasi$Provinsi"] <- "Provinsi"
    
    table_nasionalOrganisasi <- ddply(finalOrganisasi, 'Provinsi', summarize, a1 = mean(LevelOrg), a2 = mean(LevelSDM), a3 = mean(LevelTek))
    aspekOrg<-c("Provinsi","Organisasi","Sumber Daya Manusia", "Teknologi")
    colnames(table_nasionalOrganisasi)<-aspekOrg
    
    ### TINGKAT INDIVIDU ####
    
    summInputInd<-readRDS("data/dataIndividu")
    summInputInd$`sdm_i1/sdm_i4/q6.3.7`<-NULL
    summInputInd$`sdm_i1/sdm_i3/q6.2.10`[is.na(summInputInd$`sdm_i1/sdm_i3/q6.2.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.10`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.10`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.11`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.11`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.12`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.12`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.13`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.13`)] <- 3
    summInputInd$`sdm_i1/sdm_i4/q6.3.14`[is.na(summInputInd$`sdm_i1/sdm_i4/q6.3.14`)] <- 3
    
    # summInputInd[summInputInd == "n/a"]  <- NA
    # summInputInd <- na.omit(summInputInd)
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
    
    ##Membuat tabel Level setiap aspek##
    LevelSDM_ind <- rowMeans(summTempIndividu[,3:6])
    finalIndividu<-as.data.frame(cbind(data.frame(summTempIndividu$Provinsi), LevelSDM_ind))
    colnames(finalIndividu)[names(finalIndividu)=="summTempIndividu.Provinsi"] <- "Provinsi"
    
    table_nasionalIndividu <- ddply(finalIndividu, 'Provinsi', summarize, a1 = mean(LevelSDM_ind))
    aspekInd<-c("Provinsi","Sumber Daya Manusia - Individu")
    colnames(table_nasionalIndividu)<-aspekInd
    
    ### ALL STAGES ####
    tableSDM <- merge(table_nasionalOrganisasi, table_nasionalIndividu)
    meanSDM <- (tableSDM$`Sumber Daya Manusia` + tableSDM$`Sumber Daya Manusia - Individu`)/2
    tableSDM$`Sumber Daya Manusia` <- round(meanSDM, digits = 2)
    tableSDM$Organisasi <- round(tableSDM$Organisasi, digits = 2)
    tableSDM$Teknologi <- round(tableSDM$Teknologi, digits = 2)
    tableSDM$`Sumber Daya Manusia - Individu` <- NULL
    tableSDM <- as.data.frame(tableSDM)
    
    dataNasional <- merge(table_nasionalSistem, tableSDM)
    
    ### Membuat Grafik Nasional ###
    chartNasional <- gather(dataNasional, variable, value, -Provinsi)
    colnames(chartNasional)<-c("Provinsi","Aspek","Level")
    chartNasional$Level <- round(chartNasional$Level, digits = 2)
    
    graph <- ggplot(chartNasional, aes(fill=Aspek, y=Level, x=Provinsi)) +
      geom_bar(position="stack", stat="identity") +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y=element_blank(),
            legend.text = element_text(size = 8))
    ggplotly(graph) %>% layout(legend = list(orientation = "h", x = 0, y = -0.3))
    
    # graph <- ggplot(chartNasional, aes(fill=Aspek, x=reorder(Provinsi, -Level), y=Level, text = paste("Provinsi:", Provinsi))) +
    #   geom_bar(position="stack", stat="identity") +
    #   coord_flip() +
    #   theme_minimal() +
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #         axis.text.x = element_blank(),
    #         axis.text.y = element_text(size = 6),
    #         axis.title.x = element_blank(),
    #         axis.title.y=element_blank(),
    #         legend.text = element_text(size = 8))
    # ggplotly(graph, tooltip=c("text", "Level", "Aspek")) %>% layout(legend = list(orientation = "h", x = 0, y = -0.3))
    
  })
}
# Run the application
shinyApp(ui = ui, server = server)

