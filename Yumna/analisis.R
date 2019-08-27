library(ggplot2)
library(plotly)


###HASIL ANALISIS SISTEM
# sistem <- read.delim("clipboard", header=TRUE)

#Menggunakan ggplot harus mengubah format data
# gsistem<-ggplot(sistem, aes(x=Indikator,y=Nilai, fill=Bar)) +
#   geom_bar(stat="identity", position=position_stack(reverse=TRUE)) +
#   coord_flip()
# ggplotly(gsistem)

sistem2<- read.delim("clipboard", header=TRUE)

#Menggunakan plotly dapat menggunakan format data seperti di excel
plot_ly(sistem2, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
  add_trace(x=~Gap, name= 'Gap') %>%
  layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Sistem") 

###HASIL ANALISIS ORGANISASI
organisasi <- read.delim("clipboard", header = TRUE)

plot_ly(organisasi, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
  add_trace(x=~Gap, name= 'Gap') %>%
  layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Organisasi")

###HASIL ANALISIS INDIVIDU
individu <- read.delim("clipboard", header=TRUE)

plot_ly(individu, y=~Indikator, x=~Level, type='bar', name='Level', orientation= 'h')%>%
  add_trace(x=~Gap, name= 'Gap') %>%
  layout(yaxis=list(title='Indikator'), barmode='stack', title="Level dan Gap Indikator Penilaian Kapasitas Tingkat Individu")
