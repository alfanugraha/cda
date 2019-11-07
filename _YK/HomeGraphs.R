library(readxl)
library(plotly)
library(ggplot2)
library(dplyr)

inputRespInd<-read_excel("data/cdna_ind2.xlsx")
test <- plyr::count(inputRespInd$provinsi)
colnames(test)<-c("Provinsi", "Responden")
plot_ly(test,x=~Provinsi,y=~Responden,type='bar') 
# %>%
#   layout(title="Grafik Jumlah Responden seIndonesia",
#          xaxis="Provinsi",
#          yaxis="Jumlah Responden")

test="Ahmad.yudann@gmail.com"
inputnoHP ="085368093314"
if (inputnoHP=="085368093314"){
  data<-filter(inputRespInd,inputRespInd$noHP==inputnoHP)
  View(data)
} else {
  print("Anda bukan pengguna, data tidak dapat ditampilkan")
}

stg <- data.frame(StudentID = c( rep("A", 2), rep("B", 3), rep("A", 2), rep("C", 5), rep("D", 2)  ),
                  SectorID  = c(rep("Team_1", 3), rep("Team_2", 3), rep("Team_3", 5), rep("Team_1", 3)),               
                  ClassID     = c(rep("Class_1", 7), rep("Class_2", 7) )            
)
library(plyr)
cnt <- plyr::count(stg$StudentID)
stg$StudentID <- factor(stg$StudentID, 
                        levels = cnt$x[order(cnt$freq, decreasing = TRUE)])
