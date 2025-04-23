# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Read the data (adjust the file path)
data <- read.csv("C:/Users/12282/Documents/Werk/032425 FluAB R2 Comparison.csv")

split_data <-split(data, data$Target_Name)
 
  p <- ggplot(split_data[["Influenza-A"]], aes(x=Group, y=Copies_per_uL)) + 
    geom_violin() + 
    geom_point(aes(color=WWTP)) +
    scale_shape_manual(values=c(Negative=1, Positive=19))+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    labs(x = "R1 vs R2", y = "Copies/uL")
  
 print(p)

 tiff("C:/Users/12282/Documents/Werk/Plots/032425 FluA R1vsR2 Violin.tiff", units="in", width=10, height=10, res=500, compression = 'lzw')
 print(p)
 
 p1 <- ggplot(split_data[["Influenza-B"]], aes(x=Group, y=Copies_per_uL)) + 
   geom_violin() + 
   geom_point(aes(color=WWTP)) +
   scale_shape_manual(values=c(Negative=1, Positive=19))+
   theme_bw()+
   theme(axis.text.x = element_text(angle=90, hjust=1)) +
   labs(x = "R1 vs R2", y = "Copies/uL")
 
 print(p1)
 
 tiff("C:/Users/12282/Documents/Werk/Plots/032425 FluB R1vsR2 Violin.tiff", units="in", width=10, height=10, res=500, compression = 'lzw')
 print(p1)
dev.off()