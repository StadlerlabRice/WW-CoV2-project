# B117 short circuit

# Prelim ----
source('C:/Users/new/Box Sync/Stadler lab/Data/COVID-qPCR work/1-processing_functions.R', encoding = 'UTF-8')

# Input names
rps <- c('dd.WW127_Schools+WWTP_B117', 'dd.WW127_Schools+WWTP_B117-variant') 
  # this can be automated if (B117 is in the name, look for a -variant too)
title_name <- '0209 school+WWTP B117'

# Label the raw files
map(rps, process_ddpcr)
  # This creates two different labelled sheets in the data dump
  # can merge data after getting them before further processing by row joining
  # Add another row for all, vs variants (teach the 2-calc to respect this row)

# Action begins ----

# this section is ad-hoc, but will be re-written when integrating into the pipeline

# Get and merge the two datasets from data dump
raw_quant_data <- map_dfr(rps, 
                          ~ read_sheet(sheeturls$data_dump, sheet = .x, range = 'A:L') %>% 
                            mutate(variant_status = if_else(str_detect(.x, regex('-variant', ignore_case = T) ),
                                                            'Variant',
                                                            'all'), .after = 3)
                          ) 

# processing 
# Select relevant columns, get the variant and WT side by side and calculate the variant/ all percentage

text_cols <- c('Sample_name', 'Tube ID', 'variant_status', 'Target', 'Well Position') # select constant columns
value_cols <- c('Copy #', 'AcceptedDroplets', 'Positives', 'Threshold') # all the value columns that change with threshold

processed_quant_data <- raw_quant_data %>% 
  select( all_of(text_cols), all_of(value_cols)) %>%  # select only important columns
  pivot_wider(names_from = variant_status, values_from = all_of(value_cols)) %>%  # put variant and all side by side
  
  mutate(percentage_variant = (`Copy #_Variant` / `Copy #_all` * 100) %>% round(2), # calculate % of variant, round it off
         .after = `Well Position`) 


# Data output ----
check_ok_and_write(processed_quant_data, sheeturls$complete_data, title_name) # save results to a google sheet, ask for overwrite

