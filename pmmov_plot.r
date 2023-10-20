library(tidyverse)
require(data.table)
require(dplyr)
require(ggpubr)

#Get a list with all csv files from the directory that is set as 'working directory'
filelist = list.files(pattern="*.csv$")

#read all csv files with data.table::fread() and put in df_input_list
df_input_list <- lapply(filelist, fread)

#get the filenames, remove extension for use as "id"
names(df_input_list) <- gsub(filelist, pattern="\\..*", replacement="")

#Merge all the dataframes and use the filenames as id    
pmmov_merged <- bind_rows(df_input_list, .id = "id")

#Function to group WWTPs based on the first character#
categorize_WWTP <- function(first_char) {
  case_when(
    first_char %in% c("A", "B") ~ 1,
    first_char %in% c("C", "D") ~ 2, 
    first_char %in% c("E", "F", "G", "H") ~ 3, 
    first_char %in% c("I","J","K","L","M","N","O","P") ~ 4, 
    first_char %in% c("Q","R","S","T","U","V","W","X","Y","Z") ~ 5, 
    TRUE ~ 6  # Group 3 for other characters
  )
}

#Remove DI|NTC|std cells
pmmov_merged <-data.frame(pmmov_merged)
pmmovp <- pmmov_merged %>%
  filter(!str_detect(WWTP, "std|DI|NTC|Blank")) %>%
  filter(str_detect(Target_Name, "pMMoV")) %>%
  arrange(WWTP) %>%  
  mutate(Concentration = case_when(
    (Copies_Per_Liter_WW == 0) ~ 1,
    (Copies_Per_Liter_WW > 0) ~ Copies_Per_Liter_WW
  )) %>%
  mutate(first_char = substr(WWTP, 1, 1),
    group_num  = categorize_WWTP(first_char)) %>%
  select(-"first_char")

pmmovp <- pmmovp %>%
  select(Date, WWTP, Concentration, Positivity, group_num)

pmmovp <- na.omit(pmmovp)

plots_list <- list()
for (i in 1:5){
  p <- pmmovp [pmmovp$group_num == i,] %>%
    ggplot(., aes(x=WWTP, y=Concentration)) + 
    geom_violin() + 
    geom_point(aes(color=Date, shape=Positivity)) +
    scale_shape_manual(values=c(Negative=1, Positive=19))+
    scale_y_continuous(trans='log10', limits = c(1,10000000)) +
    ylab("Log-transformed concentration \n (copies/L-wastewater)")+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90, hjust=1))
  
  plots_list[[i]] <- p
}

combined_plot <- ggarrange(plotlist = plots_list, ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")


tiff('pMMoV ddPCR Results_plot.tiff', units="in", width=10, height=10, res=500, compression = 'lzw')

combined_plot

dev.off()
