### SISTEM ####
output$multiTableSistem<- renderDataTable({

  inputSistem<-readRDS("data/dataSistem")
  inputSistem$year <- format(as.Date(inputSistem$`provinsi/tanggal`), format = "%Y")
  year <- inputSistem$year
  # inputSistem<-filter(inputSistem,inputSistem$year==input$selectedYear)
  # inputSistem<-filter(inputSistem,inputSistem$year==2019)

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

  tempSistem<-as.data.frame(cbind(year, levelSistem))

  file_indSys<- read.table("init/system.csv", header=TRUE, sep=",")
  indikatorSys <- as.character(unique(file_indSys$Kapasitas_Fungsional))

  ## Menampilkan hasil satu provinsi ##
  # tempSistem<-filter(tempSistem,Provinsi==categoryProvince$provinsi)
  tempSistem<-filter(tempSistem,Provinsi=="Bali")

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
          axis.text.x = element_text(size = 7, hjust = 1, face = "plain"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggplotly(graph)
})

### ORGANISASI ####
output$multiTableOrganisasi <- renderDataTable({
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

    summInputOrg$`perangkat1/perangkat4/q4.4.3`[summInputOrg$`perangkat1/perangkat4/q4.4.3` == "n/a"]  <- 3
    summInputOrg[summInputOrg == "n/a"]<-NA
    summInputOrg<-na.omit(summInputOrg)
    summInputOrg$year <- format(as.Date(summInputOrg$`profil/tanggal`), format = "%Y")
    # summInputOrg<-filter(summInputOrg,summInputOrg$year==input$selectedYear)
    # summInputOrg<-filter(summInputOrg,summInputOrg$year==2019)
    # year <- 2019
    year <- summInputOrg$year
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
    summTempOrganisasi <-as.data.frame(cbind(year, valOrganisasi))

    indikatorOrg <-read.table("init/organisation.csv", header=TRUE, sep=",")
    summIndikatorOrg <-as.character(unique(indikatorOrg$Kapasitas_Fungsional))

    ## Menampilkan hasil satu provinsi untuk tingkat organisasi ##
    # summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==categoryProvince$provinsi)
    summTempOrganisasi <-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Bali")

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
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  ggplotly(graph)
})

### INDIVIDU ####
output$multiTableIndividu <- renderDataTable({
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
    summInputInd$year <- format(as.Date(summInputInd$`profil/tanggal`), format = "%Y")
    year <- as.data.frame(summInputInd$year)


    summInputInd <- as.data.frame(summInputInd)

    summInd<- as.data.frame(lapply(summInputInd[,5:(length(summInputInd)-1)], as.numeric))

    q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
    q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
    q6.3<-rowSums(summInd[,12:20]); q6.3<-as.data.frame(q6.3)/9
    q6.4<-rowSums(summInd[,21:23]); q6.4<-as.data.frame(q6.4)/3
    valInd<-cbind(summInputInd$`profil/provinsi`, year, summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
    colnames(valInd)<-c("Provinsi", "Tahun", "Nama", "q6.1","q6.2","q6.3","q6.4")
    summTempIndividu<-as.data.frame(valInd)

    summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
    summIndikatorInd  <- as.data.frame(summIndikatorInd)

    # summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==categoryProvince$provinsi)
    summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Bali")

    ## Menampilkan level per indikator ##
    tableIndividu <- aggregate(summTempIndividu[,4:length(summTempIndividu)], list(summTempIndividu$Tahun), mean)
    roundTableIndividu <- round(tableIndividu[2:length(tableIndividu)], digits=2)

    ## Data grafik setiap indikator ##
    finalTableIndividu <- cbind(tableIndividu$Group.1,roundTableIndividu)
    colnames(finalTableIndividu)<-c("Tahun", "6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas & Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan & Motivasi")
    require(tidyr)
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
          axis.title.y=element_blank())
  ggplotly(graph)
})




### RANGKUMAN ####
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
roundSummary <- round(multiyearsSummary[,2:length(multiyearsSummary)], digits = 2)
multiyearsSummary <- cbind(multiyearsSummary$Tingkat, roundSummary)
colnames(multiyearsSummary)[names(multiyearsSummary)=="multiyearsSummary$Tingkat"] <- "Tingkat"

datatable(multiyearsSummary,escape = FALSE, rownames = FALSE)

multiyearsSummary.long <- gather(multiyearsSummary, variable, value, -Tingkat)

graph <- ggplot(data = multiyearsSummary.long, aes(x = variable, y = value, fill = Tingkat)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 7, hjust = 1, face = "plain"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggplotly(graph)

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
summInputSys$year <- format(as.Date(summInputSys$`provinsi/tanggal`), format = "%Y")
yearSys <- summInputSys$year

summSys<- as.data.frame(lapply(summInputSys[,3:(length(summInputSys)-1)], as.numeric))

q2.5<-rowSums(summSys[,9:10]); q2.5<- as.data.frame(q2.5)/2
q7.1 <- rowSums(summSys[,14:32]); q7.1<- as.data.frame(q7.1)/19
q7.2 <- rowSums(summSys[,33:51]); q7.2<- as.data.frame(q7.2)/19
q7.3<-rowSums(summSys[,52:53]); q7.3<-as.data.frame(q7.3)/2
q9.1<-rowSums(summSys[,54:58]); q9.1<-as.data.frame(q9.1)/5
q9.2<-rowSums(summSys[,59:64]); q9.2<-as.data.frame(q9.2)/6
q9.3<-rowSums(summSys[,65:67]); q9.3<-as.data.frame(q9.3)/3
q9.4<-rowSums(summSys[,68:69]); q9.4<-as.data.frame(q9.4)/2

summLevelSistem<-cbind(yearSys, summInputSys$`provinsi/provinsi_001`,summSys$regulasi.regulasi1.q1.1,summSys$regulasi.regulasi2.q1.2,summSys$integrasi1.integrasi2.q2.1,summSys$integrasi1.integrasi3.q2.2,summSys$integrasi1.integrasi4.q2.3, summSys$integrasi1.integrasi5.q2.4, q2.5, summSys$proses1.proses2.q3.1, summSys$proses1.proses2_001.q3.2, summSys$proses1.proses3.q3.3, summSys$proses1.proses4.q3.4, summSys$proses1.proses4_001.q3.5, q7.1, q7.2, q7.3, q9.1, q9.2, q9.3, q9.4)
colnames(summLevelSistem)<-c("Tahun","Provinsi","q1.1","q1.2","q2.1","q2.2","q2.3","q2.4","q2.5","q3.1","q3.2","q3.3","q3.4","q3.5","q7.1","q7.2","q7.3","q9.1","q9.2","q9.3","q9.4")
summTempSistem<-as.data.frame((summLevelSistem))

indikatorSistem <- read.table("init/system.csv", header=TRUE, sep=",")
summIndikatorSys <- as.data.frame(unique(indikatorSistem$Kapasitas_Fungsional))

## Menampilkan hasil satu provinsi untuk tingkat sistem ##
# summTempSistem<-filter(summTempSistem,summInputSys$`provinsi/provinsi_001`==categoryProvince$provinsi)
summTempSistem<-filter(summTempSistem,Provinsi=="Aceh")

## Membuat tabel Level setiap aspek ##
aspekSys<-c("1. Regulasi/peraturan daerah","2. Integrasi dalam Perencanaan Pembangunan Daerah", "3. Proses", "7. Data dan Informasi", "9. Pemantauan, Evaluasi, dan Pelaporan")
LevelReg<-mean(as.matrix(summTempSistem[,3:4])); LevelInt<-mean(as.matrix(summTempSistem[5:9])); LevelProses<-mean(as.matrix(summTempSistem[10:14])); LevelData<-mean(as.matrix(summTempSistem[15:17])); LevelPEP<-mean(as.matrix(summTempSistem[18:21]))
summ_allLevelSys<-as.data.frame(t(cbind(LevelReg,LevelInt, LevelProses, LevelData, LevelPEP)))
summSistem<-as.data.frame(cbind(aspekSys, summ_allLevelSys))
colnames(summSistem)<-c("Aspek Penilaian","Level")
meanSistem <- round(summarise(summSistem, Average = mean(Level, na.rm = T)), digits = 2)

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

summInputOrg$`perangkat1/perangkat4/q4.4.3`[summInputOrg$`perangkat1/perangkat4/q4.4.3` == "n/a"]  <- 3
summInputOrg[summInputOrg == "n/a"]  <- NA
summInputOrg <- na.omit(summInputOrg)
summInputOrg$year <- format(as.Date(summInputOrg$`profil/tanggal`), format = "%Y")
yearOrg <- summInputOrg$year
# summInputOrg<-filter(summInputOrg,summInputOrg$year==input$selectedYear)
# summInputOrg<-filter(summInputOrg,summInputOrg$year==2019)
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
valOrganisasi <- cbind(yearOrg, summInputOrg$`profil/provinsi`, summInputOrg$`profil/institusi`, summInputOrg$`profil/nama`,q4.1,q4.2,q4.3,q4.4,q4.5,q4.6,q4.7,q5.1,q5.2,q5.3,q5.4,q5.5,q8.1,q8.2,q8.3)
colnames(valOrganisasi)<-c("Tahun","Provinsi", "Institusi", "Nama", "q4.1", "q4.2", "q4.3", "q4.4", "q4.5", "q4.6", "q4.7", "q5.1", "q5.2", "q5.3", "q5.4", "q5.5", "q8.1", "q8.2", "q8.3" )
summTempOrganisasi<-as.data.frame(valOrganisasi)

indikatorOrg <- read.table("init/organisation.csv", header=TRUE, sep=",")
summIndikatorOrg <- as.data.frame(unique(indikatorOrg$Kapasitas_Fungsional))
colnames(summIndikatorOrg)<-"Indikator"

##Menampilkan hasil satu provinsi untuk tingkat organisasi##
# summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`==categoryProvince$provinsi)
summTempOrganisasi<-filter(summTempOrganisasi,summInputOrg$`profil/provinsi`=="Aceh")

##Membuat tabel Level setiap aspek##
Level4<-rowSums(summTempOrganisasi[,5:11])/length(summTempOrganisasi[,5:11])
LevelOrg<-mean(Level4)
Level5 <- rowSums(summTempOrganisasi[,12:16])/length(summTempOrganisasi[,12:16])
LevelSDM<-mean(Level5)
Level8 <- rowSums(summTempOrganisasi[,17:19])/length(summTempOrganisasi[,17:19])
LevelTek<-mean(Level8)
LevelOrg_gabungan<-as.data.frame(t(cbind(LevelOrg,LevelSDM,LevelTek)))
Aspek_Penilaian<-c("4. Organisasi","5. Sumber Daya Manusia - Organisasi", "8. Teknologi")
summOrganisasi<-as.data.frame(cbind(Aspek_Penilaian, LevelOrg_gabungan))
colnames(summOrganisasi)<- c("Aspek Penilaian", "Level")
meanOrganisasi <- round(summarise(summOrganisasi, Average = mean(Level, na.rm = T)), digits = 2)

### Tabel Prioritas Tingkat Individu ####
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
summInputInd$year <- format(as.Date(summInputInd$`profil/tanggal`), format = "%Y")
yearInd <- summInputInd$year
# summInputInd<-filter(summInputInd,summInputInd$year==input$selectedYear)
# summInputInd<-filter(summInputInd,summInputInd$year==2019)

summInd<- as.data.frame(lapply(summInputInd[,5:length(summInputInd)], as.numeric))

q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
q6.3<-rowSums(summInd[,12:20]); q6.3<-as.data.frame(q6.3)/9
q6.4<-rowSums(summInd[,21:23]); q6.4<-as.data.frame(q6.4)/3
valInd<-cbind(yearInd,summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
colnames(valInd)<-c("Tahun","Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
summTempIndividu<-as.data.frame(valInd)

summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
summIndikatorInd  <- as.data.frame(summIndikatorInd)

# summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==categoryProvince$provinsi)
summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Aceh")

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

datatable(t_tableIndividu,escape = FALSE, rownames = FALSE)

## Membuat tabel Level setiap aspek ##
Indikator_Penilaian_Ind<-"6. Sumber Daya Manusia - Individu"
Level6<-mean(as.matrix(summTempIndividu[4:length(summTempIndividu)]))
summIndividu<-as.data.frame(cbind(Indikator_Penilaian_Ind, Level6))
colnames(summIndividu)<-c("Aspek Penilaian","Level")
meanIndividu <- round(Level6, digits = 2)

stagesCol <- c("Sistem", "Organisasi", "Individu")
meanAllStages <- as.data.frame(cbind(stagesCol,(rbind(meanSistem, meanOrganisasi, meanIndividu))))
colnames(meanAllStages) <- c("Tingkat", "Level")
