library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(plotly)

library(httr)
library(jsonlite)
library(readr)

kobo_server_url <- "https://kf.kobotoolbox.org/"
kc_server_url <- "https://kc.kobotoolbox.org/"

form_ind <- 327418 #Individu

## Individu ##
url_ind <- paste0(kc_server_url,"api/v1/data/",form_ind,".csv")
rawdata_ind <- GET(url_ind,authenticate("cdna2019","Icraf2019!"),progress())
dataIndividu <- read_csv(content(rawdata_ind,"raw",encoding = "UTF-8"))

saveRDS(dataIndividu, "data/dataIndividu")

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

summInputInd$`sdm_i1/sdm_i4/q6.3.7`<-NULL

## Menghilangkan n/a pada data frame ##
summInputInd[summInputInd == "n/a"]  <- NA
summInputInd <- na.omit(summInputInd)
summInputInd$year <- format(as.Date(summInputInd$`profil/tanggal`), format = "%Y")
# summInputInd<-filter(summInputInd,summInputInd$year==input$selectedYear)
# year <- summInputInd$year
summInputInd<-filter(summInputInd,summInputInd$year==2019)
year <- 2019

summInputInd <- as.data.frame(summInputInd)

summInd<- as.data.frame(lapply(summInputInd[,5:(length(summInputInd)-1)], as.numeric))

q6.1<-rowSums(summInd[,1:2]); q6.1<-as.data.frame(q6.1)/2
q6.2<-rowSums(summInd[,3:11]); q6.2<-as.data.frame(q6.2)/9
q6.3<-rowSums(summInd[,12:19]); q6.3<-as.data.frame(q6.3)/8
q6.4<-rowSums(summInd[,20:22]); q6.4<-as.data.frame(q6.4)/3
valInd<-cbind(summInputInd$`profil/provinsi`,summInputInd$`profil/nama`, q6.1,q6.2,q6.3,q6.4)
colnames(valInd)<-c("Provinsi", "Nama", "q6.1","q6.2","q6.3","q6.4" )
summTempIndividu<-as.data.frame(cbind(valInd,year))

summIndikatorInd <- c("6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi","6.2. Pengetahuan","6.3. Keterampilan","6.4. Pengembangan dan Motivasi")
summIndikatorInd  <- as.data.frame(summIndikatorInd)

summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`==categoryProvince$provinsi)
summTempIndividu<-filter(summTempIndividu,summInputInd$`profil/provinsi`=="Aceh")

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
# tablesCDA$summaryProvInd <- provInd

datatable(summIndividu,escape = FALSE, rownames = FALSE)

  # provInd <- tablesCDA$summaryProvInd
  plot_ly(provInd, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h') %>%
    add_trace(x=~GAP, name= 'GAP') %>%
    layout(yaxis=list(title='Indikator'), barmode='stack') %>%
    layout(legend = list(orientation = 'h')) %>%
    layout(yaxis = list(tickfont = list(size = 8), tickangle = 45, title = ""),
           xaxis = list(title = ""))
