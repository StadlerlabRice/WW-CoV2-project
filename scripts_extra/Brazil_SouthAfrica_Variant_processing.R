# Brazil/Japan and South Africa Variant (5PLEX) data formatting 
# Created: 2021.05.28
# Last Updated: 2021.06.15
# Author: C. McCall 

require(tidyverse)
library(ggplot2)
library(ggpubr)

# read in csv file from local drive. See template for required columns and input data
df_5PLEX <- read_csv("scripts_extra/dd.0614_Rice_WWTP_5PLEX.csv")

title <- "0614 Rice WWTP"
elution_volume <- 50

#---------------------------------------

# convert long data into wide according to type (wild_type, mutation) and removed unnecessary columns
data_wide_conc5 <- spread(df_5PLEX, Type, Copies_per_uL_RNA) %>% 
  select(-c(Match_Column, Match_Conc, Target_rawfile, Well_rawfile, Combined, Conc_copies_20uL_rawfile))

# subset data by target, remove NA rows, calculate Copies/L WW 
df_mut_484K <- subset(data_wide_conc5, select = -c(wild_type, mutation_BJ, mutation_SA))
df_mut_484K <- na.omit(df_mut_484K)
df_mut_484K %<>% mutate(df_mut_484K, Copies_Per_Liter_WW_mutation_484K = mutation *(1e6/300) * (elution_volume/Filtered_WW_vol)) %>% #Chemagic concentration factor = 300
  rename(Copies_per_uL_RNA_484K = mutation, PositiveDroplets_484K = PositiveDroplets)
df_mut_484K$Target_Name[df_mut_484K$Target_Name == '484K'] <- '484'

df_mut_BJ <- subset(data_wide_conc5, select = -c(wild_type, mutation, mutation_SA))
df_mut_BJ <- na.omit(df_mut_BJ)
df_mut_BJ %<>% mutate(df_mut_BJ, Copies_Per_Liter_WW_mutation_BJ = mutation_BJ *(1e6/300) * (elution_volume/Filtered_WW_vol)) %>% 
  rename(Copies_per_uL_RNA_mutation_BJ = mutation_BJ, PositiveDroplets_mutation_BJ = PositiveDroplets)
df_mut_BJ$Target_Name[df_mut_BJ$Target_Name == '417T'] <- '417'

df_mut_SA <- subset(data_wide_conc5, select = -c(wild_type, mutation, mutation_BJ))
df_mut_SA <- na.omit(df_mut_SA)
df_mut_SA %<>% mutate(df_mut_SA, Copies_Per_Liter_WW_mutation_SA = mutation_SA *(1e6/300) * (elution_volume/Filtered_WW_vol)) %>% 
  rename(Copies_per_uL_RNA_mutation_SA = mutation_SA, PositiveDroplets_mutation_SA = PositiveDroplets)
df_mut_SA$Target_Name[df_mut_SA$Target_Name == '417N'] <- '417'

df_wt <- subset(data_wide_conc5, select = -c(mutation, mutation_BJ, mutation_SA))
df_wt <- na.omit(df_wt)
df_wt %<>% mutate(df_wt, Copies_Per_Liter_WW_wild_type = wild_type *(1e6/300) * (elution_volume/Filtered_WW_vol)) %>% 
  rename(Copies_per_uL_RNA_wild_type = wild_type, PositiveDroplets_wild_type = PositiveDroplets)
df_wt$Target_Name[df_wt$Target_Name == 'K417 wild-type'] <- '417'
df_wt$Target_Name[df_wt$Target_Name == 'E484 wild-type'] <- '484'

# joining mutation and wild-type dataframes
df_mut_417 <- full_join(df_mut_BJ, df_mut_SA)
df_wt_417 <- left_join(df_wt, df_mut_417) 
df_wt_mut_5PLEX <- left_join(df_wt_417, df_mut_484K)

# creating duplicate columns for 484 target columns, merging and cleaning dataframe

df_wt_mut_5PLEX <- mutate(df_wt_mut_5PLEX, PositiveDroplets_484K2 = PositiveDroplets_484K,
             Copies_per_uL_RNA_484K2 = Copies_per_uL_RNA_484K,
             Copies_Per_Liter_WW_mutation_484K2 = Copies_Per_Liter_WW_mutation_484K 
             )
df_wt_mut_5PLEX[] <-  t(apply(df_wt_mut_5PLEX, 1, function(x) c(x[!is.na(x)], x[is.na(x)])))

#remove banks columns
df_wt_mut_5PLEX <- subset(df_wt_mut_5PLEX, select = -c(PositiveDroplets_484K, Copies_per_uL_RNA_484K, Copies_Per_Liter_WW_mutation_484K, 
                             PositiveDroplets_484K2, Copies_per_uL_RNA_484K2, Copies_Per_Liter_WW_mutation_484K2))
# convert columns into numeric as needed 
df_wt_mut_5PLEX <- transform(df_wt_mut_5PLEX, Copies_Per_Liter_WW_mutation_BJ = as.numeric(Copies_Per_Liter_WW_mutation_BJ), 
                Copies_Per_Liter_WW_wild_type = as.numeric(Copies_Per_Liter_WW_wild_type), Copies_Per_Liter_WW_mutation_SA = as.numeric(Copies_Per_Liter_WW_mutation_SA), 
                Copies_per_uL_RNA_wild_type = as.numeric(Copies_per_uL_RNA_wild_type), Copies_per_uL_RNA_mutation_BJ = as.numeric(Copies_per_uL_RNA_mutation_BJ),
                Copies_per_uL_RNA_mutation_SA = as.numeric(Copies_per_uL_RNA_mutation_SA), 
                PositiveDroplets_wild_type = as.numeric(PositiveDroplets_wild_type), PositiveDroplets_mutation_BJ = as.numeric(PositiveDroplets_mutation_BJ),
                PositiveDroplets_mutation_SA = as.numeric(PositiveDroplets_mutation_SA)
                )

# additional calculations for "all" and percentage variant columns
df_wt_mut_5PLEX %<>% mutate(df_wt_mut_5PLEX, Copies_Per_Liter_WW_all = ifelse(df_wt_mut_5PLEX$Target_Name == 484, Copies_Per_Liter_WW_wild_type + Copies_Per_Liter_WW_mutation_BJ, 
               Copies_Per_Liter_WW_wild_type + Copies_Per_Liter_WW_mutation_BJ + Copies_Per_Liter_WW_mutation_SA),
               
               
                              Copies_per_uL_RNA_all = ifelse(df_wt_mut_5PLEX$Target_Name == 484, Copies_per_uL_RNA_wild_type + Copies_per_uL_RNA_mutation_BJ,
                              Copies_per_uL_RNA_wild_type + Copies_per_uL_RNA_mutation_BJ + Copies_per_uL_RNA_mutation_SA),
               
                              PositiveDroplets_all = ifelse(df_wt_mut_5PLEX$Target_Name == 484, PositiveDroplets_wild_type + PositiveDroplets_mutation_BJ,
                                                            PositiveDroplets_wild_type + PositiveDroplets_mutation_BJ + PositiveDroplets_mutation_SA),
                                                            
                              percentage_mutation_BJ = ((Copies_per_uL_RNA_mutation_BJ/Copies_per_uL_RNA_all) * 100) %>% round(2),
                              percentage_mutation_SA = ((Copies_per_uL_RNA_mutation_SA/Copies_per_uL_RNA_all) * 100) %>% round(2),
                              Variant = "") %>% 
  
  mutate_if(is.numeric, ~round(., 2))

##Output for complete data sheet

# ordering the columns in the complete dataframe
col_order <- c("Facility", "WWTP", "Date", "Lab", "Target_Name", "Filtered_WW_vol", "percentage_mutation_BJ", "percentage_mutation_SA",	
               "Copies_per_uL_RNA_wild_type",	"Copies_per_uL_RNA_mutation_BJ", "Copies_per_uL_RNA_mutation_SA", "Copies_per_uL_RNA_all", "Copies_Per_Liter_WW_wild_type",
               "Copies_Per_Liter_WW_mutation_BJ", "Copies_Per_Liter_WW_mutation_SA", "Copies_Per_Liter_WW_all",	"AcceptedDroplets",	"PositiveDroplets_wild_type",	
               "PositiveDroplets_mutation_BJ", "PositiveDroplets_mutation_SA", "PositiveDroplets_all",	"Variant",	"Sample_ID",	"Sample_Type",	"Well_Position")

df_5PLEX_complete <- df_wt_mut_5PLEX[, col_order]

# writing data file to complete_data_files folder in local drive
write_csv(df_5PLEX_complete, str_c('Documents/R_Files/Variant/Complete_data_files/', title, ' BJ_SA_complete_data.csv'), na = '') # output csv file to complete data folder

## Output for HHD

#Columns removed for HHD - also remove Blanks, DI, and standards
df_5PLEX_HHD <- subset(df_5PLEX_complete, select = -c(Filtered_WW_vol, Copies_per_uL_RNA_wild_type, Copies_Per_Liter_WW_wild_type, 
                                                          PositiveDroplets_wild_type, Well_Position)) 
df_5PLEX_HHD <- dplyr::filter(df_5PLEX_HHD, !grepl('DI|Blank|stdBJ|stdSA|stdMix', WWTP))

# writing data file to HHD folder in local drive
write_csv(df_5PLEX_HHD, str_c('Documents/R_Files/Variant/HHD/', title, ' BJ_SA.csv'), na = '') # output HHD csv file




