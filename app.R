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

library(httr)
library(jsonlite)
library(readr)

library(RPostgreSQL)
library(DBI)

kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

form_moodle <- 327418 #Individu

driver <- dbDriver('PostgreSQL')
dbname <- 'moodle2'
host <- "pepstaging.duckdns.org"
port <- '5432'
user <- 'moodleaksara'
password <- 'moodleaksaradbpassword'
query <- paste0('SELECT u.firstname, u.lastname, u.username, cmlist.sectionname,cmlist.sectionid, cmlist.courseid, gi.itemname as quizname, gg.finalgrade, gi.grademax, gi.grademin, gg.timecreated, gg.timemodified
FROM mdl_quiz AS q
JOIN (
SELECT cm.instance, cs.name as sectionname, cs.id as sectionid, c.id as courseid
FROM mdl_course_sections as cs
JOIN mdl_course AS c ON c.id = cs.course
JOIN mdl_course_modules AS cm ON cm.section = cs.id
JOIN mdl_modules AS m ON m.id = cm.module
WHERE c.shortname LIKE \'%\' AND (cs.name LIKE \'%\' OR cs.name IS NULL) AND m.name = \'quiz\'
) as cmlist ON cmlist.instance = q.id
JOIN mdl_grade_items AS gi ON gi.iteminstance = cmlist.instance AND gi.itemmodule = \'quiz\' AND gi.itemtype = \'mod\'
JOIN mdl_grade_grades AS gg ON gg.itemid = gi.id
JOIN mdl_user AS u ON u.id = gg.userid
WHERE u.username LIKE \'%\'')


SQLCommand <- function(query){
  on.exit(dbDisconnect(con))
  
  con <- dbConnect(drv = dbDriver("PostgreSQL"), 
                   dbname=dbname, host=host, 
                   port=port, user = user, 
                   password = password)
  
  tmp <- dbGetQuery(con, query)
}

SQLWriteValues <- function(values, table){
  on.exit(dbDisconnect(con))
  
  con <- dbConnect(drv = dbDriver("PostgreSQL"), 
                   dbname=dbname, host=host, 
                   port=port, user = user, 
                   password = password)
  
  postgresqlWriteTable(con, table, values, append=TRUE, row.names=FALSE)
  return(NULL)
}

# Define UI
ui <- fluidPage(
  
  HTML('<meta name="viewport" content="width=1024">'),
  
  #Navbar structure for UI
  navbarPage("", theme = shinytheme("lumen"),
             tabPanel("E-learning AKSARA", fluid = TRUE, icon = icon("globe"),
                      h2(textOutput("titleTable1")),
                      dataTableOutput("valueTable"),
                      br(),
                      h2(textOutput("titleTable2")),
                      dataTableOutput("recommendationTable")
             )
  ),
  tags$style(type="text/css", ".navbar-nav {float: right; margin: 0;}")
)

# Define server
server <- function(input, output, session) {
  ## Individu ##
  url_moodle<- paste0(kc_server_url,"api/v1/data/",form_moodle,"?format=csv")
  rawdata_moodle <- GET(url_moodle,authenticate("cdna2019","Icraf2019!"),progress())
  metadata_moodle <- read_csv(content(rawdata_moodle,"raw",encoding = "UTF-8"))
  
  kuis_prk <- SQLCommand(query)
  # kuis_prk <- subset(kuis_prk, select=c(firstname, lastname, email, sectionname, sectionid, courseid, quizname, finalgrade))
  
  koboData <- reactiveValues(rekomendasi = metadata_moodle, kuis = kuis_prk)
  data <- reactiveValues(maindata=data.frame(), table_poin=data.frame(), userEmail="")
  
  link_kuis <- read.table('init/link_kuis.csv', header = T, sep = ",")
  # conv <- read.table('init/conv.csv', header = T, sep = ",")
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$username)){
      data$userEmail <- query$username
    } else {
      data$userEmail <- "kementan_admin_pbi"
    }
  })
  
  # observeEvent(input$showButton,{
  output$titleTable1 <- renderText({ paste0("Nilai Individu") })
  output$titleTable2 <- renderText({ paste0("Tabel Rekomendasi Modul E-Learning") })

  output$valueTable <- renderDataTable({
    # tempData <-readRDS("data/dataMoodle")
    tempData <- koboData$rekomendasi
    tempData$`sdm_i1/sdm_i4/q6.3.7`<-NULL
    
    tempData <- as.data.frame(tempData[129:nrow(tempData),]) # this line would be dangerous if the data have been truncated
    data$maindata <- tempData
    userEmail <- data$userEmail
    
    namaKolom = colnames(tempData[1:length(tempData)])
    # filterData <- tempData[which(tempData$`profil/email` == "y.karimah@cgiar.org"), names(tempData) %in% namaKolom]
    filterData <- tempData[which(tempData$`profil/email` == userEmail), names(tempData) %in% namaKolom]

    indikator6.1 <- filterData %>% select (`sdm_i1/sdm_i2/q6.1.1`, `sdm_i1/sdm_i2/q6.1.2`)
    indikator6.2 <- filterData %>% select (`sdm_i1/sdm_i3/q6.2.1`, `sdm_i1/sdm_i3/q6.2.2`, `sdm_i1/sdm_i3/q6.2.3`, `sdm_i1/sdm_i3/q6.2.4`,
                                             `sdm_i1/sdm_i3/q6.2.5`, `sdm_i1/sdm_i3/q6.2.6`, `sdm_i1/sdm_i3/q6.2.7`, `sdm_i1/sdm_i3/q6.2.8`,
                                             `sdm_i1/sdm_i3/q6.2.9`, `sdm_i1/sdm_i3/q6.2.10`)
    indikator6.3 <- filterData %>% select (`sdm_i1/sdm_i4/q6.3.1`, `sdm_i1/sdm_i4/q6.3.2`, `sdm_i1/sdm_i4/q6.3.3`,`sdm_i1/sdm_i4/q6.3.4`,
                                             `sdm_i1/sdm_i4/q6.3.5`, `sdm_i1/sdm_i4/q6.3.6`, `sdm_i1/sdm_i4/q6.3.8`, `sdm_i1/sdm_i4/q6.3.9`,
                                             `sdm_i1/sdm_i4/q6.3.10`, `sdm_i1/sdm_i4/q6.3.11`, `sdm_i1/sdm_i4/q6.3.12`, `sdm_i1/sdm_i4/q6.3.13`,
                                             `sdm_i1/sdm_i4/q6.3.14`)
    indikator6.4 <- filterData %>% select (`sdm_i1/sdm_i5/q6.4.1`, `sdm_i1/sdm_i5/q6.4.2`, `sdm_i1/sdm_i5/q6.4.3`)
    
    temp_numData <- cbind(filterData$`profil/email`,filterData$`profil/provinsi`, filterData$`profil/sektor`,filterData$`profil/subsektor`, 
                          filterData$`profil/subsektor_001`, filterData$`profil/subsektor_002`, filterData$`profil/tanggal`,
                          indikator6.1, indikator6.2, indikator6.3, indikator6.4)
    numData<- as.data.frame(lapply(temp_numData[,8:(length(temp_numData))], as.numeric))
    
    #Kesesuaian peran
    kategori1<-rowSums(numData[,1:2]); kategori1<-as.data.frame(kategori1)/2
    
    #Pengetahuan
    kategori2<-rowSums(numData[,3:12]); kategori2<-as.data.frame(kategori2)/10
    
    #Keterampilan
    kategori3<-rowSums(numData[,13:25]); kategori3<-as.data.frame(kategori3)/13
    
    #Pengembangan dan Motivasi
    kategori4<-rowSums(numData[,26:28]); kategori4<-as.data.frame(kategori4)/3
    
    graphData <- cbind(kategori1, kategori2, kategori3, kategori4)
    t_graphData <- as.data.frame(cbind(V1=c("Kesesuaian Peran dalam Implementasi RAD FRK/PPRKD dengan Tugas", "Pengetahuan", "Keterampilan", "Pengembangan dan Motivasi"),t(graphData)))
    colnames(t_graphData) <- c("Kategori", "Nilai")
    rownames(t_graphData) <- 1:nrow(t_graphData)
    
    # for quiz
    kuisData <- koboData$kuis
    kuisData <- kuisData[which(kuisData$sectionid %in% unique(link_kuis$sectionid)),]
    kuisData <- kuisData[which(!is.na(kuisData$finalgrade)),]
    kuisData <- within(kuisData, {poin_cdna<-ifelse(finalgrade > 90, 2.5, ifelse(finalgrade > 80, 1.875, ifelse(finalgrade > 70, 1.25, ifelse(finalgrade >= 60, 0.625, 0))))})
    kuisData <- merge(kuisData, link_kuis, by=c('sectionid', 'quizname'))
    
    kuisFilter <- kuisData[which(kuisData$username == userEmail),]
    
    nKuisFilter <- nrow(kuisFilter)
    if(nKuisFilter==0){
      t_graphData$additional <- 0
      print('please check the number of filter')
    } else {
      kat2 <- kuisFilter[kuisFilter$Kategori=="Keterampilan",]
      nKat2 <- nrow(kat2)
      add_kat2 <- ifelse(nKat2 > 0, sum(kat2$poin_cdna) / nKat2, 0)
      
      kat3 <- kuisFilter[kuisFilter$Kategori=="Pengetahuan",]
      nKat3 <- nrow(kat3)
      add_kat3 <- ifelse(nKat3 > 0, sum(kat3$poin_cdna) / nKat3, 0)
      
      t_graphData <- within(t_graphData, {additional<-ifelse(Kategori == "Keterampilan" , add_kat2, ifelse(Kategori == "Pengetahuan", add_kat3, 0))})
    }
    
    t_graphData$Nilai <- as.numeric( t_graphData$Nilai)
    t_graphData$total <- t_graphData$Nilai + t_graphData$additional
    t_graphData <- within(t_graphData, {total <- ifelse(Kategori == "Keterampilan" & total >= 5 , 5, total)})
    t_graphData <- within(t_graphData, {total <- ifelse(Kategori == "Pengetahuan" & total >= 5 , 5, total)})
    data$table_poin <- t_graphData
    t_graphData$additional <- NULL
    
    colnames(t_graphData) <- c("Kategori", "Nilai CDNA", "Nilai Peningkatan")
    
    datatable(t_graphData,escape = FALSE, rownames = FALSE, options = list(dom='ti')) %>%
      formatRound(columns=c('Nilai CDNA', 'Nilai Peningkatan'), digits=2)
  })
  
  output$recommendationTable <- renderDataTable({
    tempData <- data$maindata
    table_poin <- data$table_poin 
    userEmail <- data$userEmail
    namaKolom = colnames(tempData[1:length(tempData)])
    # filterData <- tempData[which(tempData$`profil/email` == "y.karimah@cgiar.org"), names(tempData) %in% namaKolom]
    filterData <- tempData[which(tempData$`profil/email` == userEmail), names(tempData) %in% namaKolom]
  
    indikator6.1 <- filterData %>% select (`sdm_i1/sdm_i2/q6.1.1`, `sdm_i1/sdm_i2/q6.1.2`)
    indikator6.2 <- filterData %>% select (`sdm_i1/sdm_i3/q6.2.1`, `sdm_i1/sdm_i3/q6.2.2`, `sdm_i1/sdm_i3/q6.2.3`, `sdm_i1/sdm_i3/q6.2.4`,
                                           `sdm_i1/sdm_i3/q6.2.5`, `sdm_i1/sdm_i3/q6.2.6`, `sdm_i1/sdm_i3/q6.2.7`, `sdm_i1/sdm_i3/q6.2.8`,
                                           `sdm_i1/sdm_i3/q6.2.9`, `sdm_i1/sdm_i3/q6.2.10`)
    indikator6.3 <- filterData %>% select (`sdm_i1/sdm_i4/q6.3.1`, `sdm_i1/sdm_i4/q6.3.2`, `sdm_i1/sdm_i4/q6.3.3`,`sdm_i1/sdm_i4/q6.3.4`,
                                           `sdm_i1/sdm_i4/q6.3.5`, `sdm_i1/sdm_i4/q6.3.6`, `sdm_i1/sdm_i4/q6.3.8`, `sdm_i1/sdm_i4/q6.3.9`,
                                           `sdm_i1/sdm_i4/q6.3.10`, `sdm_i1/sdm_i4/q6.3.11`, `sdm_i1/sdm_i4/q6.3.12`, `sdm_i1/sdm_i4/q6.3.13`,
                                           `sdm_i1/sdm_i4/q6.3.14`)
    indikator6.4 <- filterData %>% select (`sdm_i1/sdm_i5/q6.4.1`, `sdm_i1/sdm_i5/q6.4.2`, `sdm_i1/sdm_i5/q6.4.3`)
    
    temp_numData <- cbind(filterData$`profil/email`,filterData$`profil/provinsi`, filterData$`profil/sektor`,filterData$`profil/subsektor`, 
                          filterData$`profil/subsektor_001`, filterData$`profil/subsektor_002`, filterData$`profil/tanggal`,
                          indikator6.1, indikator6.2, indikator6.3, indikator6.4)
    numData<- as.data.frame(lapply(temp_numData[,8:(length(temp_numData))], as.numeric))
    
    poin_add_pengetahuan <- table_poin[which(table_poin$Kategori=="Pengetahuan"),]$additional
    poin_add_keterampilan <- table_poin[which(table_poin$Kategori=="Keterampilan"),]$additional
    
    rekomenIklim <- (numData$sdm_i1.sdm_i3.q6.2.1 + numData$sdm_i1.sdm_i3.q6.2.2 + poin_add_pengetahuan)/2
    rekomenPRKI <- numData$sdm_i1.sdm_i3.q6.2.5 + poin_add_pengetahuan
    rekomenPPRKN <- numData$sdm_i1.sdm_i3.q6.2.6 + poin_add_pengetahuan
    rekomenCDNA <- numData$sdm_i1.sdm_i3.q6.2.7 + poin_add_pengetahuan
    rekomenPengantar <- numData$sdm_i1.sdm_i3.q6.2.7 + poin_add_pengetahuan
    rekomenEkonomi <- (numData$sdm_i1.sdm_i3.q6.2.7 + numData$sdm_i1.sdm_i3.q6.2.8 + numData$sdm_i1.sdm_i4.q6.3.4 + poin_add_keterampilan)/3
    rekomenSatEnergi <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.5 + poin_add_keterampilan)/2
    rekomenSatLimbah <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.5 + poin_add_keterampilan)/2
    rekomenSatLahan <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.2 + numData$sdm_i1.sdm_i4.q6.3.5 + poin_add_keterampilan)/3
    rekomenBAU <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.6 + poin_add_keterampilan)/2
    rekomenIntervensi <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.8 + numData$sdm_i1.sdm_i4.q6.3.9 + poin_add_keterampilan)/3
    rekomenTradeoff <- (numData$sdm_i1.sdm_i4.q6.3.10 + numData$sdm_i1.sdm_i4.q6.3.11 + poin_add_keterampilan)/2
    rekomenHutan <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13 + poin_add_keterampilan)/3
    rekomenTani <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13 + poin_add_keterampilan)/3
    rekomenEnergi <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13 + poin_add_keterampilan)/3
    rekomenTransportasi <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13 + poin_add_keterampilan)/3
    rekomenLimbah <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13 + poin_add_keterampilan)/3
    rekomenAplikasi <- (numData$sdm_i1.sdm_i3.q6.2.10 + numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14 + poin_add_keterampilan)/3
    rekomenKontributor <- (numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14 + poin_add_keterampilan)/2
    rekomenAdmin <- (numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14 + poin_add_keterampilan)/2
    rekomenEditor <- (numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14 + poin_add_keterampilan)/2
    
    if (filterData$`profil/id`==2 & filterData$`profil/sektor`==1 & filterData$`profil/subsektor`==1){
      
      modulEnergi <- read_excel("init/modul.xlsx", sheet = "Energi")
      nilaiEnergi <- data.frame(rbind(rekomenIklim, rekomenPRKI, rekomenPPRKN, rekomenCDNA, rekomenPengantar, rekomenEkonomi, rekomenSatEnergi, rekomenBAU, rekomenIntervensi, rekomenTradeoff, rekomenEnergi, rekomenAplikasi, rekomenKontributor))
      tabelEnergi <- cbind(modulEnergi, nilaiEnergi)
      tabelEnergi <- cbind(Kursus = rownames(tabelEnergi), tabelEnergi)
      colnames(tabelEnergi) <- c("ID", "Modul", "Nilai")
      rownames(tabelEnergi) <- 1:nrow(tabelEnergi)
      tabelEnergi$Rekomendasi <- "Tidak ada rekomendasi"
      
      ###Define the recommendation of each indicator####
      for (i in 1:nrow(tabelEnergi)){
        eval(parse(text=paste0("tabelEnergi <- within(tabelEnergi, {Rekomendasi<-ifelse(ID=='", as.character(tabelEnergi$ID[i]) , "' & Nilai<3 , 'Perlu mempelajari modul', Rekomendasi)})")))
      }
      
      tabelEnergi$ID <- NULL
      tabelEnergi$Nilai <- NULL
      
      link_kuis$Modul <- link_kuis$modul 
      tblInsertMoodle <- merge(tabelEnergi, link_kuis, by='Modul')
      tblInsertMoodle <- tblInsertMoodle[which(tblInsertMoodle$Rekomendasi=="Perlu mempelajari modul"), ]
      
      sectid<-paste(tblInsertMoodle$sectionid, collapse=",")
      query_check <- SQLCommand(paste0("SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username='", userEmail, "'"))
      if(query_check == 0) {
        KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
        NewRecord <- data.frame(id=KeyPlusOne, username=userEmail, sectionid=sectid)
        SQLWriteValues(NewRecord, 'mdl_course_list_cdna')
      } else if(query_check == 1) {
        query_update<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", sectid, "' WHERE username = '", userEmail, "'")
        SQLCommand(query_update)
      }
      
      datatable(tabelEnergi, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    } else if (filterData$`profil/id`==2 & filterData$`profil/sektor`==1 & filterData$`profil/subsektor`==2) {
      
      modulTransportasi <- read_excel("init/modul.xlsx", sheet = "Energi_Trans")
      nilaiTransportasi <- data.frame(rbind(rekomenIklim, rekomenPRKI, rekomenPPRKN, rekomenCDNA, rekomenPengantar, rekomenEkonomi, rekomenSatEnergi, rekomenBAU, rekomenIntervensi, rekomenTradeoff, rekomenTransportasi, rekomenAplikasi, rekomenKontributor))
      tabelTransportasi <- cbind(modulTransportasi, nilaiTransportasi)
      tabelTransportasi <- cbind(Kursus = rownames(tabelTransportasi), tabelTransportasi)
      colnames(tabelTransportasi) <- c("ID", "Modul", "Nilai")
      rownames(tabelTransportasi) <- 1:nrow(tabelTransportasi)
      tabelTransportasi$Rekomendasi <- "Tidak ada rekomendasi"
      
      ###Define the recommendation of each indicator####
      for (i in 1:nrow(tabelTransportasi)){
        eval(parse(text=paste0("tabelTransportasi <- within(tabelTransportasi, {Rekomendasi<-ifelse(ID=='", as.character(tabelTransportasi$ID[i]) , "' & Nilai<3 , 'Perlu mempelajari modul', Rekomendasi)})")))
      }
      
      tabelTransportasi$ID <- NULL
      tabelTransportasi$Nilai <- NULL
      
      link_kuis$Modul <- link_kuis$modul 
      tblInsertMoodle <- merge(tabelTransportasi, link_kuis, by='Modul')
      tblInsertMoodle <- tblInsertMoodle[which(tblInsertMoodle$Rekomendasi=="Perlu mempelajari modul"), ]
      
      sectid<-paste(tblInsertMoodle$sectionid, collapse=",")
      query_check <- SQLCommand(paste0("SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username='", userEmail, "'"))
      if(query_check == 0) {
        KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
        NewRecord <- data.frame(id=KeyPlusOne, username=userEmail, sectionid=sectid)
        SQLWriteValues(NewRecord, 'mdl_course_list_cdna')
      } else if(query_check == 1) {
        query_update<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", sectid, "' WHERE username = '", userEmail, "'")
        SQLCommand(query_update)
      }
      
      datatable(tabelTransportasi, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    } else if (filterData$`profil/id`==2 & filterData$`profil/sektor`==2 & filterData$`profil/subsektor_001`==1) {
      
      modulHutan<- read_excel("init/modul.xlsx", sheet = "Lahan_Hutan")
      nilaiHutan <- data.frame(rbind(rekomenIklim, rekomenPRKI, rekomenPPRKN, rekomenCDNA, rekomenPengantar, rekomenEkonomi, rekomenSatLahan, rekomenBAU, rekomenIntervensi, rekomenTradeoff, rekomenHutan, rekomenAplikasi, rekomenKontributor))
      tabelHutan <- cbind(modulHutan, nilaiHutan)
      tabelHutan <- cbind(Kursus = rownames(tabelHutan), tabelHutan)
      colnames(tabelHutan) <- c("ID", "Modul", "Nilai")
      rownames(tabelHutan) <- 1:nrow(tabelHutan)
      tabelHutan$Rekomendasi <- "Tidak ada rekomendasi"
      
      ###Define the recommendation of each indicator####
      for (i in 1:nrow(tabelHutan)){
        eval(parse(text=paste0("tabelHutan <- within(tabelHutan, {Rekomendasi<-ifelse(ID=='", as.character(tabelHutan$ID[i]) , "' & Nilai<3 , 'Perlu mempelajari modul', Rekomendasi)})")))
      }
      
      tabelHutan$ID <- NULL
      tabelHutan$Nilai <- NULL
      
      link_kuis$Modul <- link_kuis$modul 
      tblInsertMoodle <- merge(tabelHutan, link_kuis, by='Modul')
      tblInsertMoodle <- tblInsertMoodle[which(tblInsertMoodle$Rekomendasi=="Perlu mempelajari modul"), ]
      
      sectid<-paste(tblInsertMoodle$sectionid, collapse=",")
      query_check <- SQLCommand(paste0("SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username='", userEmail, "'"))
      if(query_check == 0) {
        KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
        NewRecord <- data.frame(id=KeyPlusOne, username=userEmail, sectionid=sectid)
        SQLWriteValues(NewRecord, 'mdl_course_list_cdna')
      } else if(query_check == 1) {
        query_update<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", sectid, "' WHERE username = '", userEmail, "'")
        SQLCommand(query_update)
      }
      
      datatable(tabelHutan, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    } else if (filterData$`profil/id`==2 & filterData$`profil/sektor`==2 & filterData$`profil/subsektor_001`==2) {
      
      modulTani<- read_excel("init/modul.xlsx", sheet = "Lahan_Tani")
      nilaiTani <- data.frame(rbind(rekomenIklim, rekomenPRKI, rekomenPPRKN, rekomenCDNA, rekomenPengantar, rekomenEkonomi, rekomenSatLahan, rekomenBAU, rekomenIntervensi, rekomenTradeoff, rekomenTani, rekomenAplikasi, rekomenKontributor))
      tabelTani <- cbind(modulTani, nilaiTani)
      tabelTani <- cbind(Kursus = rownames(tabelTani), tabelTani)
      colnames(tabelTani) <- c("ID", "Modul", "Nilai")
      rownames(tabelTani) <- 1:nrow(tabelTani)
      tabelTani$Rekomendasi <- "Tidak ada rekomendasi"
      
      ###Define the recommendation of each indicator####
      for (i in 1:nrow(tabelTani)){
        eval(parse(text=paste0("tabelTani <- within(tabelTani, {Rekomendasi<-ifelse(ID=='", as.character(tabelTani$ID[i]) , "' & Nilai<3 , 'Perlu mempelajari modul', Rekomendasi)})")))
      }
      
      tabelTani$ID <- NULL
      tabelTani$Nilai <- NULL
      
      link_kuis$Modul <- link_kuis$modul 
      tblInsertMoodle <- merge(tabelTani, link_kuis, by='Modul')
      tblInsertMoodle <- tblInsertMoodle[which(tblInsertMoodle$Rekomendasi=="Perlu mempelajari modul"), ]
      
      sectid<-paste(tblInsertMoodle$sectionid, collapse=",")
      query_check <- SQLCommand(paste0("SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username='", userEmail, "'"))
      if(query_check == 0) {
        KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
        NewRecord <- data.frame(id=KeyPlusOne, username=userEmail, sectionid=sectid)
        SQLWriteValues(NewRecord, 'mdl_course_list_cdna')
      } else if(query_check == 1) {
        query_update<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", sectid, "' WHERE username = '", userEmail, "'")
        SQLCommand(query_update)
      }
      
      datatable(tabelTani, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    } else if (filterData$`profil/id`==2 & filterData$`profil/sektor`==3 & filterData$`profil/subsektor_002`==1) {
      
      modulLimbah<- read_excel("init/modul.xlsx", sheet = "Limbah")
      nilaiLimbah <- data.frame(rbind(rekomenIklim, rekomenPRKI, rekomenPPRKN, rekomenCDNA, rekomenPengantar, rekomenEkonomi, rekomenSatLimbah, rekomenBAU, rekomenIntervensi, rekomenTradeoff, rekomenLimbah, rekomenAplikasi, rekomenKontributor))
      tabelLimbah <- cbind(modulLimbah, nilaiLimbah)
      tabelLimbah <- cbind(Kursus = rownames(tabelLimbah), tabelLimbah)
      colnames(tabelLimbah) <- c("ID", "Modul", "Nilai")
      rownames(tabelLimbah) <- 1:nrow(tabelLimbah)
      tabelLimbah$Rekomendasi <- "Tidak ada rekomendasi"
      
      ###Define the recommendation of each indicator####
      for (i in 1:nrow(tabelLimbah)){
        eval(parse(text=paste0("tabelLimbah <- within(tabelLimbah, {Rekomendasi<-ifelse(ID=='", as.character(tabelLimbah$ID[i]) , "' & Nilai<3, 'Perlu mempelajari modul', Rekomendasi)})")))
      }
      
      tabelLimbah$ID <- NULL
      tabelLimbah$Nilai <- NULL
      
      link_kuis$Modul <- link_kuis$modul 
      tblInsertMoodle <- merge(tabelLimbah, link_kuis, by='Modul')
      tblInsertMoodle <- tblInsertMoodle[which(tblInsertMoodle$Rekomendasi=="Perlu mempelajari modul"), ]
      
      sectid<-paste(tblInsertMoodle$sectionid, collapse=",")
      query_check <- SQLCommand(paste0("SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username='", userEmail, "'"))
      if(query_check == 0) {
        KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
        NewRecord <- data.frame(id=KeyPlusOne, username=userEmail, sectionid=sectid)
        SQLWriteValues(NewRecord, 'mdl_course_list_cdna')
      } else if(query_check == 1) {
        query_update<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", sectid, "' WHERE username = '", userEmail, "'")
        SQLCommand(query_update)
      }
      
      datatable(tabelLimbah, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    } else if (filterData$`profil/id`==1) {
      
      modulAdmin<- read_excel("init/modul.xlsx", sheet = "Admin")
      nilaiAdmin <- data.frame(rbind(rekomenIklim, rekomenPRKI, rekomenPPRKN, rekomenCDNA, rekomenPengantar, rekomenEkonomi, rekomenBAU, rekomenIntervensi, rekomenTradeoff, rekomenAplikasi, rekomenAdmin))
      tabelAdmin <- cbind(modulAdmin, nilaiAdmin)
      tabelAdmin <- cbind(Kursus = rownames(tabelAdmin), tabelAdmin)
      colnames(tabelAdmin) <- c("ID", "Modul", "Nilai")
      rownames(tabelAdmin) <- 1:nrow(tabelAdmin)
      tabelAdmin$Rekomendasi <- "Tidak ada rekomendasi"
      
      ###Define the recommendation of each indicator####
      for (i in 1:nrow(tabelAdmin)){
        eval(parse(text=paste0("tabelAdmin <- within(tabelAdmin, {Rekomendasi<-ifelse(ID=='", as.character(tabelAdmin$ID[i]) , "' & Nilai<3 , 'Perlu mempelajari modul', Rekomendasi)})")))
      }
      
      tabelAdmin$ID <- NULL
      tabelAdmin$Nilai <- NULL
      
      link_kuis$Modul <- link_kuis$modul 
      tblInsertMoodle <- merge(tabelAdmin, link_kuis, by='Modul')
      tblInsertMoodle <- tblInsertMoodle[which(tblInsertMoodle$Rekomendasi=="Perlu mempelajari modul"), ]
      
      sectid<-paste(tblInsertMoodle$sectionid, collapse=",")
      query_check <- SQLCommand(paste0("SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username='", userEmail, "'"))
      if(query_check == 0) {
        KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
        NewRecord <- data.frame(id=KeyPlusOne, username=userEmail, sectionid=sectid)
        SQLWriteValues(NewRecord, 'mdl_course_list_cdna')
      } else if(query_check == 1) {
        query_update<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", sectid, "' WHERE username = '", userEmail, "'")
        SQLCommand(query_update)
      }
      
      datatable(tabelAdmin, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    } else {
      
      modulEditor<- read_excel("init/modul.xlsx", sheet = "Editor")
      nilaiEditor <- data.frame(rbind(rekomenIklim, rekomenPRKI, rekomenPPRKN, rekomenCDNA, rekomenPengantar, rekomenEkonomi, rekomenBAU, rekomenIntervensi, rekomenTradeoff, rekomenAplikasi, rekomenEditor))
      tabelEditor <- cbind(modulEditor, nilaiEditor)
      tabelEditor <- cbind(Kursus = rownames(tabelEditor), tabelEditor)
      colnames(tabelEditor) <- c("ID", "Modul", "Nilai")
      rownames(tabelEditor) <- 1:nrow(tabelEditor)
      tabelEditor$Rekomendasi <- "Tidak ada rekomendasi"
      
      ###Define the recommendation of each indicator####
      for (i in 1:nrow(tabelEditor)){
        eval(parse(text=paste0("tabelEditor <- within(tabelEditor, {Rekomendasi<-ifelse(ID=='", as.character(tabelEditor$ID[i]) , "' & Nilai<3 , 'Perlu mempelajari modul', Rekomendasi)})")))
      }
      
      tabelEditor$ID <- NULL
      tabelEditor$Nilai <- NULL
      
      link_kuis$Modul <- link_kuis$modul 
      tblInsertMoodle <- merge(tabelEditor, link_kuis, by='Modul')
      tblInsertMoodle <- tblInsertMoodle[which(tblInsertMoodle$Rekomendasi=="Perlu mempelajari modul"), ]
      
      sectid<-paste(tblInsertMoodle$sectionid, collapse=",")
      query_check <- SQLCommand(paste0("SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username='", userEmail, "'"))
      if(query_check == 0) {
        KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
        NewRecord <- data.frame(id=KeyPlusOne, username=userEmail, sectionid=sectid)
        SQLWriteValues(NewRecord, 'mdl_course_list_cdna')
      } else if(query_check == 1) {
        query_update<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", sectid, "' WHERE username = '", userEmail, "'")
        SQLCommand(query_update)
      }
      
      datatable(tabelEditor, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    }
  })
  # })
}

# Run the application
shinyApp(ui = ui, server = server)