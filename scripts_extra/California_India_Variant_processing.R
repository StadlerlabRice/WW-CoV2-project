# California and India Variant data processing
# Created: 2021.05.28
# Last Updated: 2021.06.15
# Author: C. McCall 
require(tidyverse)
library(ggplot2)
library(ggpubr)
library(magrittr)

#template_volume_ddpcr <- tibble(Target = c('N1', 'N2', 'BCoV', 'pMMoV', 'N501Y', 'Del69-70'),
                                #template_vol = c(10, 10, 4, 4, 9, 9) /22 * 20) # ul template volume per well of the 20 ul ddPCR reaction for each target


df_CAIndia <- read_csv("Documents/R_Files/Variant/Input_files/dd.0614_Rice_WWTP_CA-India.csv")

title <- "0614 Rice WWTP"
elution_volume <- 50

#=======================================================

# convert long data into wide according to type (wild_type, mutation) and removed unnecessary columns
data_wide_concCI <- spread(df_CAIndia, Type, Copies_per_uL_RNA)
data_wide_concCI <- subset(data_wide_concCI, select = -c(Match_Column, Match_Conc, Target_rawfile, Well_rawfile, Combined, Conc_copies_20uL_rawfile))

#subsetting data, remove NA rows, calculate Copies/L WW
df_mut_CAIndia <- subset(data_wide_concCI, select = -c(wild_type)) 
df_mut_CAIndia <- na.omit(df_mut_CAIndia) 
df_mut_CAIndia %<>% mutate(df_mut_CAIndia, Copies_Per_Liter_WW_mutation = mutation *(1e6/300) * (elution_volume/Filtered_WW_vol)) %>% #Chemagic concentration factor = 300
  rename(Copies_per_uL_RNA_mutation = mutation, PositiveDroplets_mutation = PositiveDroplets)
df_mut_CAIndia$Target_Name[df_mut_CAIndia$Target_Name == 'L452R'] <- 'L452'
df_mut_CAIndia$Target_Name[df_mut_CAIndia$Target_Name == 'E484Q'] <- 'E484'

df_wt_CAIndia <- subset(data_wide_concCI, select = -c(mutation))
df_wt_CAIndia <- na.omit(df_wt_CAIndia)
df_wt_CAIndia <- mutate(df_wt_CAIndia, Copies_Per_Liter_WW_wild_type = wild_type *(1e6/300) * (elution_volume/Filtered_WW_vol))
df_wt_CAIndia <- df_wt_CAIndia %>% rename(Copies_per_uL_RNA_wild_type = wild_type, PositiveDroplets_wild_type = PositiveDroplets)
df_wt_CAIndia$Target_Name[df_wt_CAIndia$Target_Name == 'L452wt'] <- 'L452'
df_wt_CAIndia$Target_Name[df_wt_CAIndia$Target_Name == 'E484wt'] <- 'E484'

df_mut_wt_CAIndia <- full_join(df_mut_CAIndia, df_wt_CAIndia)
df_mut_wt_CAIndia %<>% mutate(df_mut_wt_CAIndia, Copies_Per_Liter_WW_all = Copies_Per_Liter_WW_wild_type + Copies_Per_Liter_WW_mutation,
                            Copies_per_uL_RNA_all = Copies_per_uL_RNA_wild_type + Copies_per_uL_RNA_mutation,
                            PositiveDroplets_all = PositiveDroplets_wild_type + PositiveDroplets_mutation,
                            percentage_mutation = ((Copies_per_uL_RNA_mutation/Copies_per_uL_RNA_all) * 100) %>% round(2), 
                            Variant = "") %>% 
  mutate_if(is.numeric, ~round(., 2))

##Output for complete data sheet

# ordering the columns in the complete dataframe
col_order <- c("Facility", "WWTP", "Date", "Lab", "Target_Name", "Filtered_WW_vol", "percentage_mutation",	
               "Copies_per_uL_RNA_wild_type",	"Copies_per_uL_RNA_mutation",	"Copies_per_uL_RNA_all", "Copies_Per_Liter_WW_wild_type",
               "Copies_Per_Liter_WW_mutation", "Copies_Per_Liter_WW_all",	"AcceptedDroplets",	"PositiveDroplets_wild_type",	
               "PositiveDroplets_mutation",	"PositiveDroplets_all",	"Variant",	"Sample_ID",	"Sample_Type",	"Well_Position")

df_CAIndia_complete <- df_mut_wt_CAIndia[, col_order]

# writing data file to complete_data_files folder in local drive
write_csv(df_CAIndia_complete, str_c('Documents/R_Files/Variant/Complete_data_files/', title, ' CA_India_complete_data.csv'), na = '') # output csv file

## Output for HHD

#Columns removed for HHD - also remove Blanks, DI, and standards
df_CAIndia_HHD <- subset(df_CAIndia_complete, select = -c(Filtered_WW_vol, Copies_per_uL_RNA_wild_type, Copies_Per_Liter_WW_wild_type, 
                                                          PositiveDroplets_wild_type, Well_Position)) 
  df_CAIndia_HHD <- dplyr::filter(df_CAIndia_HHD, !grepl('DI|Blank|stdCA|stdIndia|stdMix|stdWHC', WWTP))

  # writing data file to HHD folder in local drive
write_csv(df_CAIndia_HHD, str_c('Documents/R_Files/Variant/HHD/', title, ' CA_India.csv'), na = '') # output csv file



