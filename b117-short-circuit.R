# B117 short circuit

# user input ----

# Input names
original_sheet_name <- 'dd.WW134_0223_WWTP_B117'
title_name <- '0223 WWTP B117'

regular_WWTP_run_output <- TRUE # make TRUE of you want to output the WWTP only data and manhole samples sheets 
# (make FALSE for controls, testing etc. where only "complete data" sheet is output)


# Prelims -----
source('C:/Users/new/Box Sync/Stadler lab/Data/COVID-qPCR work/1-processing_functions.R', encoding = 'UTF-8')

rps <- str_c(original_sheet_name, c('', '-variant') ) 


# Label the raw files
process_ddpcr(original_sheet_name)
  # This creates a single labelled sheet in the data dump with both all and variant data
  # Adds a row : "variant_status" : all, or Variant

# Action begins ----

# Get the mergeddataset from data dump
raw_quant_data <- read_sheet(sheeturls$data_dump, sheet = original_sheet_name, range = 'A:M')

# processing 
# Select relevant columns, get the variant and WT side by side and calculate the variant/ all percentage

text_cols <- c('Sample_name', 'Sample_ID', 'variant_status', 'Target', 'Well Position') # select constant columns
value_cols <- c('Copy #', 'AcceptedDroplets', 'Positives', 'Threshold') # all the value columns that change with threshold

processed_quant_data <- raw_quant_data %>% 
  
  mutate(Sample_ID = if_else(biological_replicates == ''|is.na(biological_replicates), # stitch together a Sample_ID
                             str_c(Sample_name, '.', assay_variable) , 
                             str_c(Sample_name, '.', assay_variable, '-', biological_replicates)),
 ) %>% 
  
  select( all_of(text_cols), all_of(value_cols)) %>%  # select only important columns for pivoting
  pivot_wider(names_from = variant_status, values_from = all_of(value_cols)) %>%  # put variant and all side by side
  
  mutate(percentage_variant = (`Copy #_Variant` / `Copy #_all` * 100) %>% round(2), # calculate % of variant, round it off
         .after = `Well Position`) 


# Data output ----
check_ok_and_write(processed_quant_data, sheeturls$complete_data, title_name) # save results to a google sheet, ask for overwrite

# streamlined output for the HHD

control_cols_for_deselection <- str_c('.', c('DI', 'NTC', 'Blank', 'WHC', 'Mix', 'b117'), '-', collapse = '|')
if(regular_WWTP_run_output) 
{
  # Shortcut temporary
  processed_HHD_dat <- processed_quant_data %>% 
    select(-matches('Accepted|Threshold')) %>% 
    filter(!str_detect(Sample_ID, control_cols_for_deselection))
    
  
  # presentable data for health department
  # present_HHD_data <- processed_quant_data %>%
  #   
  #   rename('Copies_per_uL' = `Copies/ul RNA`,
  #          'Copies_Per_Liter_WW' = `Copies/l WW`,
  #          'Recovery_Rate' = `Percentage_recovery_BCoV`,
  #          Target_Name = `Target Name`) %>%
  #   select(-contains('Vol'), -`Spiked-in Copies/l WW`, -contains('Droplet'), -'Well Position')
  # 
  # present_only_WW <- present_WW_data %>% 
  #   filter(WWTP %in% all_WWTP_names) # retain only WWTP data
  # 
  # Write data if not empty
  if(processed_HHD_dat %>% plyr::empty() %>% !.){
    check_ok_and_write(processed_HHD_dat, sheeturls$wwtp_only_data, title_name) # save results to a google sheet, ask for overwrite
    write_csv(processed_HHD_dat, path = str_c('excel files/Weekly data to HHD/', title_name, '.csv'), na = '') # output csv file
  }
}