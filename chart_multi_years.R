library(ggplot2)
library(plotly)

dummy <- read.table("init/dummy_grafik.csv", header=TRUE, sep=";")
p <- ggplot(data=dummy, aes(x=Indikator, y=Level, fill=factor(Tahun))) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1, face = "plain")) 

# Modify legend titles
p <- p + labs(fill = "Tahun")

ggplotly(p)

# Use custom colors
p + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
p + scale_fill_brewer(palette="Blues")
