# Analysis and plots for RNA degradation in sewershed
# Created: 2021.03.05
# Modified: 2021.03.05
# Author: C. McCall 

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(googlesheets4)

sheeturls <- list(wwtp_HHD_data = 'https://docs.google.com/spreadsheets/d/1dBESjgWSFsOBodFFpYNhWIOAQ2a3wvoLkrn12V_rFck/edit#gid=786732664'
)

N1N2_sheet <- c('030121 Rice')
Variant_sheet <- c('0301 WWTP B117')

N1N2_data <- read_sheet(sheeturls$wwtp_HHD_data, sheet = N1N2_sheet)
Variant_data <- read_sheet(sheeturls$wwtp_HHD_data, sheet = Variant_sheet)

#Renaming column headers

names(N1N2_data)[names(N1N2_data) == "Copies_per_uL"] <- "CopiesuL_N1N2"
names(Variant_data)[names(Variant_data) == "Copies/ul RNA_all"] <- "CopiesuL_S"

#Join datasets
all_data <- full_join(N1N2_data, Variant_data)

NvsS_gene_plot <- ggplot(data=all_data, mapping = aes(x=CopiesuL_N1N2, y=CopiesuL_S)) +
  geom_point() + geom_smooth(method = "lm", alpha = .15, na.rm = TRUE, se=TRUE) +
  xlab('N1 and N2 genes (copies/ul)') + ylab('S gene (copies/ul)') + ggtitle("N vs S gene") + 
  theme_bw() + theme(axis.title = element_text(face = "bold")) 

NvsS_gene_plot

# store in RMarkdown sheet with date/week in the title of the document. The variant over time with also be stored in the R markdown sheet building on itseld each week

# Weekly variant only data ----


weekly_sheets <- c('0223 WWTP B117', '0301 WWTP B117')

weekly_variant_data <- read_sheet(sheeturls$wwtp_HHD_data, sheet = weekly_sheets)



