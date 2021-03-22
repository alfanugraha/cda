# library(koboloadeR)
# koboloadeR::kobo_apps("data_viewer")

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

kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

# form_moodle <- 612594 #Individu
form_moodle_merger <- 327418 #Individu

## Individu ##
# url_moodle<- paste0(kc_server_url,"api/v1/data/",form_moodle,"?format=csv")
# rawdata_moodle <- GET(url_moodle,authenticate("cdna2019","Icraf2019!"),progress())
# dataMoodle <- read_csv(content(rawdata_moodle,"raw",encoding = "UTF-8"))
# 
# saveRDS(dataMoodle, "data/dataMoodle")

## Individu Gabungan##
url_moodle_merger<- paste0(kc_server_url,"api/v1/data/",form_moodle_merger,"?format=csv")
rawdata_moodle_merger <- GET(url_moodle_merger,authenticate("cdna2019","Icraf2019!"),progress())
dataMoodle_merger <- read_csv(content(rawdata_moodle_merger,"raw",encoding = "UTF-8"))

saveRDS(dataMoodle_merger, "data/dataMoodle_merger")

dataMoodle_merger$`profil/nama`
dataMoodle_merger$`profil/institusi`
dataMoodle_merger$`profil/provinsi`
dataMoodle_merger$`profil/email`
dataMoodle_merger$`profil/tanggal`

# Define UI
ui <- fluidPage(
  
  HTML('<meta name="viewport" content="width=1024">'),
  
  #Navbar structure for UI
  navbarPage("", theme = shinytheme("lumen"),
             tabPanel("E-learning AKSARA", fluid = TRUE, icon = icon("globe"),
                      # tags$style(button_color_css),
                      selectInput("selectedUser", label="Nama Pengguna", choices = c("Admin 1", "Kontributor 1", "Kontributor 2",  "Kontributor 3", "Kontributor 4","Kontributor 5", "Umum 1", "Umum 2", "Umum 3")),
                      h2("Nilai Individu"),
                      withSpinner(dataTableOutput("valueTable")),
                      br(),
                      h2("Tabel Rekomendasi E-Learning"),
                      withSpinner(dataTableOutput("recommendationTable"))
                      # downloadButton('downloadResults', 'Unduh Hasil', style="color: #fff; background-color: #00a65a; border-color: #008d4c")
             )
  ),
  tags$style(type="text/css", ".navbar-nav {float: right; margin: 0;}")
)

# Define server
server <- function(input, output, session) {
  data <- reactiveValues(maindata=data.frame())
  
  output$valueTable <- renderDataTable({
    # tempData <-readRDS("data/dataMoodle")
    tempData <- readRDS("data/dataMoodle_merger")
    
    # tempData$`profil/gender` <- NULL; tempData$`profil/nama` <- NULL; tempData$`profil/provinsi` <- NULL; tempData$`profil/tanggal` <- NULL; tempData$`profil/institusi` <- NULL; tempData$`profil/noHP` <- NULL; tempData$`profil/email` <- NULL
    # tempData$`meta/instanceID`<-NULL; tempData$`__version__`<-NULL; tempData$`_uuid`<-NULL; tempData$`_submission_time`<-NULL; tempData$`_tags`<-NULL; tempData$`_notes`<-NULL; tempData$`_version_`<-NULL; tempData$`_validation_status`<-NULL
    # 
    # tempData$`sdm_i1/sdm_i2/alasan` <- NULL; tempData$`sdm_i1/sdm_i2/alasan_001` <- NULL
    # 
    # for (i in 2:9){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i3/alasan_00",i,"`","<-NULL")))
    # }
    # for (i in 10:11){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i3/alasan_0",i,"`","<-NULL")))
    # }
    # 
    # for (i in 12:25){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i4/alasan_0",i,"`","<-NULL")))
    # }
    # 
    # for (i in 26:28){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i5/alasan_0",i,"`","<-NULL")))
    # }
    
    tempData$`sdm_i1/sdm_i4/q6.3.7`<-NULL
    
    tempData[tempData == "n/a"]  <- NA
    tempData <- as.data.frame(tempData)
    tempData$nama <- c("Admin 1", "Kontributor 1", "Kontributor 2", "Kontributor 3","Umum 1","Kontributor 4", "Umum 2", "Umum 3", "Kontributor 5")
    data$maindata <- tempData
    
    namaKolom = colnames(tempData[1:length(tempData)])
    filterData <- tempData[which(tempData$nama == input$selectedUser), names(tempData) %in% namaKolom]
    # selectedUser <- "Admin 1"
    # filterData <- tempData[which(tempData$nama == selectedUser), names(tempData) %in% namaKolom]
    tempData$`profil/gender`
    tempData$`profil/id`
    tempData$`profil/sektor`
    tempData$`profil/subsektor` #belum ada data dummy
    tempData$`profil/subsektor_001`
    tempData$`profil/subsektor_002` #belum ada data dummy
    
    # temp_numData <- cbind()
    numData<- as.data.frame(lapply(filterData[,6:(length(filterData)-1)], as.numeric))
    
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
    
    datatable(t_graphData,escape = FALSE, rownames = FALSE, options = list(dom='ti')) %>%
      formatRound(columns='Nilai', digits=2)
  })
  
  output$recommendationTable <- renderDataTable({
    tempData <- data$maindata
    # tempData <-readRDS("data/dataMoodle")
    # 
    # tempData$`profil/gender` <- NULL; tempData$`profil/nama` <- NULL; tempData$`profil/provinsi` <- NULL; tempData$`profil/tanggal` <- NULL; tempData$`profil/institusi` <- NULL; tempData$`profil/noHP` <- NULL; tempData$`profil/email` <- NULL
    # tempData$`meta/instanceID`<-NULL; tempData$`__version__`<-NULL; tempData$`_uuid`<-NULL; tempData$`_submission_time`<-NULL; tempData$`_tags`<-NULL; tempData$`_notes`<-NULL; tempData$`_version_`<-NULL; tempData$`_validation_status`<-NULL
    # 
    # tempData$`sdm_i1/sdm_i2/alasan` <- NULL; tempData$`sdm_i1/sdm_i2/alasan_001` <- NULL
    # 
    # for (i in 2:9){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i3/alasan_00",i,"`","<-NULL")))
    # }
    # for (i in 10:11){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i3/alasan_0",i,"`","<-NULL")))
    # }
    # 
    # for (i in 12:25){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i4/alasan_0",i,"`","<-NULL")))
    # }
    # 
    # for (i in 26:28){
    #   eval(parse(text=paste0("tempData$`sdm_i1/sdm_i5/alasan_0",i,"`","<-NULL")))
    # }
    # 
    # tempData[tempData == "n/a"]  <- NA
    # tempData <- as.data.frame(tempData)
    # tempData$nama <- c("Admin 1", "Kontributor 1", "Kontributor 2", "Umum 1", "Kontributor 3", "Umum 2", "Umum 3", "Kontributor 4")
    namaKolom = colnames(tempData[1:length(tempData)])
    filterData <- tempData[which(tempData$nama == input$selectedUser), names(tempData) %in% namaKolom]
    
    numData<- as.data.frame(lapply(filterData[,6:(length(filterData)-1)], as.numeric))
    
    rekomenIklim <- (numData$sdm_i1.sdm_i3.q6.2.1 + numData$sdm_i1.sdm_i3.q6.2.2)/2
    rekomenPRKI <- numData$sdm_i1.sdm_i3.q6.2.5
    rekomenPPRKN <- numData$sdm_i1.sdm_i3.q6.2.6
    rekomenCDNA <- numData$sdm_i1.sdm_i3.q6.2.7
    rekomenPengantar <- numData$sdm_i1.sdm_i3.q6.2.7
    rekomenEkonomi <- (numData$sdm_i1.sdm_i3.q6.2.7 + numData$sdm_i1.sdm_i3.q6.2.8 + numData$sdm_i1.sdm_i4.q6.3.4)/3
    rekomenSatEnergi <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.5)/2
    rekomenSatLimbah <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.5)/2
    rekomenSatLahan <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.2 + numData$sdm_i1.sdm_i4.q6.3.5)/3
    rekomenBAU <- (numData$sdm_i1.sdm_i4.q6.3.1 + numData$sdm_i1.sdm_i4.q6.3.6)/2
    rekomenIntervensi <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.8 + numData$sdm_i1.sdm_i4.q6.3.9)/3
    rekomenTradeoff <- (numData$sdm_i1.sdm_i4.q6.3.10 + numData$sdm_i1.sdm_i4.q6.3.11)/2
    rekomenHutan <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13)/3
    rekomenTani <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13)/3
    rekomenEnergi <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13)/3
    rekomenTransportasi <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13)/3
    rekomenLimbah <- (numData$sdm_i1.sdm_i3.q6.2.9 + numData$sdm_i1.sdm_i4.q6.3.12 + numData$sdm_i1.sdm_i4.q6.3.13)/3
    rekomenAplikasi <- (numData$sdm_i1.sdm_i3.q6.2.10 + numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14)/3
    rekomenKontributor <- (numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14)/2
    rekomenAdmin <- (numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14)/2
    rekomenEditor <- (numData$sdm_i1.sdm_i4.q6.3.3 + numData$sdm_i1.sdm_i4.q6.3.14)/2
    
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
      datatable(tabelEditor, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom='ti'))
      
    }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)