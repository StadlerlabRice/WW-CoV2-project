# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file
source('./inputs_for_analysis.R') # Source the file with user inputs

# Parameters ----------------------------------------------------------------------

# Spike in and concentration details
elution_volume <- 50 # ul - RNA extraction final volume

# copies/ul viral suspension spiked in                                          (Spike ID: S18)
# all_spike_virus_conc <- tibble(Target = c('BCoV_M', 'BRSV_N'), spike_virus_conc = c(6817.7, NA))
spike_virus_volume <- 50 # ul of viral suspension spiked in x ml WW; (x ~ 350 - 450 and varies for each sample)

# sheets to read from qPCR data dump excel file
read_these_sheets <- c( 'WW44_Baylor 713-715_N1N2_Std28')
title_name <- '713-715 Baylor_qPCR N1N2'

# Biobot_id sheet
bb_sheets <- c('Week 16.2 (7/29)')

# Extra categories for plotting separately (separate by | like this 'Vaccine|Troubleshooting')
extra_categories = 'Std|Control' # for excluding this category from a plot, make the switch (exclude_sample = TRUE)
special_samples = 'HCJ|SOH|HW' # putting special samples in a separate sheet

# To rename samples with descriptive category/names (usually for extra TR experiments)
# - For other samples: Make it empty an string (like this '' ) to avoid messing up names inadvertantly
# translate_id_manual <- c('WW/Field', 'Water/Field', 'WW/Lab',  'Water/Lab', 'Solids/Present', 'Solids/Removed', 'Bead-beating/Dilter', 'Bead-beating/No filter') %>% setNames(str_c('^', c('A','B','C','D','E','F','G','H')))
translate_id_manual <- c('none' = 'none')

same_bottle_for_triplicates <- 'yes' # 'yes' if 1 bottle yields 3 x 50 ml. 'no' if 3 independant bottles yield 50 ml each


# Input data ----------------------------------------------------------------------

# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet
list_raw_quant_data <- map(read_these_sheets, ~ read_sheet(sheeturls$data_dump, sheet = ., range = 'A:H')) 

# bind multiple reads and clean up names
raw_quant_data <- bind_rows(list_raw_quant_data) %>% 
  rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it 
  # unite('Biobot_id', c(Sample_name, assay_variable), sep = '', remove = F) %>%
  select(-`Well Position`) %>% 
  mutate_at('assay_variable', as.character) %>% 
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% 
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) # make a unique column for matching volumes 


# Load metadata ----------------------------------------------------------------------

# determine which biobot sheets to get for Rice data : Under development
# bb_sheet_match <- qpcr_polished %>% 
#   filter(!str_detect(Target, 'Baylor')) %>% # filter only Rice data
#   pull(Sample_name) %>% 
#   str_match('[:digit:]+') %>% 
#   unique() %>% 
#   as.character() %>% 
#   .[!is.na(.)]
# 
# # Or this one
# bb_sheet_match <- read_these_sheets %>% 
#      str_extract_all('[:digit:]{3}') %>% unlist()  


# Bring WWTP names from google sheet: "Biobot Sample IDs"
biobot_lookup <- map_df(bb_sheets, 
                        ~ read_sheet(sheeturls$biobot_id , sheet = .x) %>% 
                          rename('Biobot_id' = matches('Biobot|Comments', ignore.case = T), 'WWTP' = contains('SYMBOL', ignore.case = T), 'FACILITY NAME' = matches('FACILITY NAME', ignore.case = T)) %>% 
                          mutate('Biobot_id' = str_remove(`Biobot_id`,'\\.'), WWTP = as.character(WWTP)) %>% 
                          select(`Biobot_id`, `FACILITY NAME`, WWTP)
)


# Get volumes data from google sheet : "Sample registry"
volumes_data_Rice <- read_sheet(sheeturls$sample_registry , sheet = 'Concentrated samples') %>% 
  rename('WW_vol' = `Total WW vol measured (ml)`, 
         'Label_tube' = `Label on tube`, 
         vol_extracted = `WW volume extracted (ml)`,
         Vaccine_ID = `Stock ID of Spike`,
         'Biobot_id' = `Biobot/other ID`) %>% 
  mutate('WW_vol' = coalesce(WW_vol, `Total WW volume calculated (ml)`) ) %>% 
  
  select(WW_vol, Label_tube, vol_extracted, Vaccine_ID, `Biobot_id`) %>% 
  distinct() %>% 
  mutate_at('Label_tube', ~str_remove_all(., " ")) %>% 
  mutate_at('Biobot_id', str_remove,  ' ') %>% 
  
  # removing wrongly entered volumes for replicates, replacing with the max volume 
  # (this is a temporary fix, a wiser function that looks for empty elements in weight column should be written)
  {if(same_bottle_for_triplicates == 'yes') { 
    mutate(., unique_labels = str_remove(Label_tube,'[1-9]$')) %>%  # this will be changed to the 'Bottle' column soon
      group_by(unique_labels) %>% 
      mutate_at('WW_vol', max, na.rm = T) %>% 
      ungroup() %>% 
      select(-unique_labels)
  }
  }

# baylor's ID, vols ----------------------------------------------------------------------   

baylor_trigger <- raw_quant_data$Target %>% str_detect('Baylor') %>% any()

# Getting baylor's volumes from a separate excel file (from Austen's email)
if (baylor_trigger) {
  
  baylor_volumes_and_biobots <- raw_quant_data %>% 
    filter(str_detect(Target, 'Baylor')) %>%  
    pull(Sample_name) %>% unique() %>%  # get all Baylor sheets in data
    
    # read the sheets matching the names, while renaming columns
    map_dfr( ~ read_xlsx (str_c('excel files/Baylor/', 'Baylor_sample volumes', '.xlsx'), sheet = .x) %>%
               rename('WWTP' = contains('SYMBOL', ignore.case = T), 
                      'FACILITY NAME' = matches('FACILITY NAME', ignore.case = T), 
                      'Biobot_id' = `Sample ID`) %>% 
               mutate('Label_tube' = str_c(.x, WWTP)) ) %>% 
    mutate_at('Biobot_id', ~str_remove(., '\\.')) %>% 
    
    pivot_longer(cols = starts_with('Volume'), names_to = 'index', values_to = 'WW_vol') %>% 
    separate(index, into =  c(NA, 'biological_replicates')) %>% 
    mutate(Label_tube = str_c(Label_tube, biological_replicates), vol_extracted = 1.5) %>% 
    select(-biological_replicates)
}

# Vaccine spike concentrations
spike_list <- read_sheet(sheeturls$data_dump, sheet = 'Preamble', range = 'B6:K', col_types = 'Dcccnnnccn') %>% 
  rename(spike_virus_conc = matches('Stock conc.'), Sample_name = Week)  


# Attach metadata ----------------------------------------------------------------------

# Join WWTP names to qPCR dataset for Rice data

vol_R <- raw_quant_data %>% 
  filter(!str_detect(Target, 'Baylor')) %>% # filter only Rice data
  
  left_join(volumes_data_Rice, by = 'Label_tube') %>%
  mutate_at('Biobot_id', ~if_else(is.na(.x), str_c(Sample_name, assay_variable), .x)) %>% # stand-by name for missing cols
  
  left_join(spike_list %>% select(Vaccine_ID, Target, spike_virus_conc),  by = c('Vaccine_ID', 'Target') ) %>% 
  left_join(biobot_lookup, by = 'Biobot_id') %>% 
  
  mutate_at(c('WWTP', 'FACILITY NAME'), ~if_else(str_detect(., '^X')|is.na(.), assay_variable, .)) 

# Join Baylor WWTP volumes
if (baylor_trigger) {
  
  vol_B <- raw_quant_data %>% 
    filter(str_detect(Target, 'Baylor')) %>% 
    # select(-`Biobot_id`) %>% 
    left_join(baylor_volumes_and_biobots, by = 'Label_tube') %>% 
    fuzzyjoin::regex_right_join(spike_list %>% select(Sample_name, Vaccine_ID, Target, spike_virus_conc),
                                ., by = c('Sample_name', 'Target')) %>% 
    select(-`Sample_name.x`, -`Target.x`) %>% rename(Sample_name = `Sample_name.y`, Target = Target.y)
  
  
} else vol_B <- tibble(NULL)

# Calculations ----

# Copies/ul RNA to copies/l WW. Spike in concentration, % recovery are calculated
# join the results with the WWTP identifiers and names
processed_quant_data <- bind_rows(vol_R, vol_B) %>% 
  
  # Calculations for spiked in and recovered copies of the virus
  mutate(`Actual spike-in` = spike_virus_conc * spike_virus_volume / (WW_vol * 1e-3), Recovered = `Copy #` * 1e3 * elution_volume/vol_extracted, `Recovery fraction` = 100 * Recovered/`Actual spike-in`) %>% 
  mutate_cond(str_detect(Sample_name, 'Vaccine'), `Actual spike-in` = spike_virus_conc * spike_virus_volume / (.050 * 1e-3), Recovered = `Copy #` * 1e6 * 50/20, `Recovery fraction` = 100 * Recovered/`Actual spike-in`) %>% 
  mutate_cond(str_detect(Target, 'Baylor'), `Actual spike-in` = spike_virus_conc * spike_virus_volume / (WW_vol * 1e-3), Recovered = `Copy #` * 1e6 /30, `Recovery fraction` = 100 * Recovered/`Actual spike-in`) %>% 
  select(-spike_virus_conc) %>% 
  
  # arranging data by facility name alphabetical
  arrange(`FACILITY NAME`) %>% 
  mutate_at('WWTP', as_factor)

# Adding a dummy CT column (if only ddPCR data is being loaded; which lacks the CT column) - for compatibility with qPCR code
if(processed_quant_data %>%  {!'CT' %in% colnames(.)}) processed_quant_data$CT = NA

# Summary and long_format ----
# (under development)
minimal_label_columns <- c('Target', 'Sample_name', 'WWTP')

# Extract minimal columns from processed data (good for running averages and std deviations for plotting)
processed_minimal = list( raw.dat = processed_quant_data %>% 
                            select(all_of(minimal_label_columns), where(is.numeric), -matches('vol')))
# Group by all the text columns and calculate mean and standard deviation for biological replicates
processed_minimal$summ.dat <- processed_minimal$raw.dat %>% 
  group_by_at(all_of(minimal_label_columns)) %>% 
  summarize_all(.funs = lst(mean, sd), na.rm = T)

# Convert the above minimal data into long format (convenient for plotting multiple data types on the same plot)
long_processed_minimal <- processed_minimal %>% map(pivot_longer, cols = where(is.numeric),
                                                    names_to = 'Measurement', values_to = 'value')                                                   
long_processed_minimal$summ.dat %<>% separate(Measurement, into = c('Measurement','val'),"_") %>% 
  pivot_wider(names_from = val, values_from = value) # Seperate mean and variance and group by variable of measurement



# Data output ----------------------------------------------------------------------


presentable_data <- processed_quant_data %>% 
  
  # renaming variables
  rename('Facility' = `FACILITY NAME`, 'Original Sample Volume' = WW_vol, Ct = CT, 'Target Name' = Target) %>%
  rename('Copies/ul RNA' = `Copy #`, 'Copies/l WW' = Recovered, 'Spiked-in Copies/l WW' = `Actual spike-in`) %>%
  rename('Volume Filtered' = vol_extracted) %>% 
  
  # Adding new variables, modifying existing variables
  mutate(Date = 'Manual entry', Sample_ID = NA, Lab = if_else(str_detect(`Target Name`, 'Baylor'), 'B', 'R')) %>% 
  mutate(Detection_Limit = if_else(str_detect(`Target Name`, 'N1|N2'), 330, 
                                   if_else(str_detect(`Target Name`, 'Baylor'), 23500, 705) 
  ) , Sample_Type = NA, Comments = NA) %>% 
  mutate_at('Sample_name', ~as.character(.)) %>%
  mutate_at('Facility', ~if_else(. == assay_variable, str_c(Sample_name, '/', assay_variable), .)) %>%
  mutate(Tube_ID = if_else(biological_replicates == ''|is.na(biological_replicates), str_c(Sample_name, ' ', assay_variable), str_c(Sample_name, ' ', assay_variable, '', biological_replicates)) ) %>% 
  mutate(WWTP_ID = if_else(biological_replicates == ''|is.na(biological_replicates), str_c(Sample_name, '.', WWTP) , str_c(Sample_name, '.', WWTP, '-', biological_replicates))) %>% 
  mutate('Limit of detection (copies/ul RNA)' = if_else(str_detect(`Target Name`, 'N1|N2'), 0.3, 0.5 )) %>% 
  
  # Arrange rows by WWTP facility 
  arrange(Facility, biological_replicates) %>%
  # unite('Facility', c(Facility, biological_replicates), sep = "-", na.rm = T) %>%
  mutate_cond(str_detect(`Target Name`, '^N'), `Recovery fraction` = NA, `Spiked-in Copies/l WW` = NA) %>%
  
  # Selecting column order
  select(Facility, WWTP, Date, Lab, `Target Name`, `Original Sample Volume`, `Volume Filtered`, Ct, `Copies/ul RNA`, `Copies/l WW`, Sample_ID, Detection_Limit, Sample_Type, `Spiked-in Copies/l WW`, `Recovery fraction`, WWTP_ID, Tube_ID, Comments) %>%
  mutate_at('Target Name', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2')))



# Output data - including controls
check_ok_and_write(presentable_data, sheeturls$complete_data, title_name) # save results to a google sheet, ask for overwrite


# presentable data for health department
present_WW_data <- presentable_data %>% 
  filter(!str_detect(Facility, str_c(extra_categories, "|Vaccine|NTC|Blank"))) %>%  # retain only WWTP data
  rename('Copies_per_uL_RNA' = `Copies/ul RNA`, 'Copies_Per_Liter_WW' = `Copies/l WW`, 'Recovery_Rate' = `Recovery fraction`, Target_Name = `Target Name`) %>% 
  mutate_at('Target_Name', ~str_remove(., '/Baylor')) %>% 
  select(-contains('Volume'), -`Spiked-in Copies/l WW`, -Tube_ID)

present_only_WW <- present_WW_data %>% filter(!str_detect(Facility, special_samples))

# Write data if not empty
if(present_only_WW %>% plyr::empty() %>% !.){
  check_ok_and_write(present_only_WW, sheeturls$wwtp_only_data, title_name) # save results to a google sheet, ask for overwrite
}

present_special_samples <- present_WW_data %>% filter(str_detect(Facility, special_samples))

# Write data if not empty
if(present_special_samples %>% plyr::empty() %>% !.){
  check_ok_and_write(present_special_samples, sheeturls$wwtp_only_data, str_c(title_name, ' special samples')) # save results to a google sheet, ask for overwrite
}

# Plotting into html -----------------------------------------------------------------------


# calling r markdown file
rmarkdown::render('make_html_plots.rmd', output_file = str_c(title_name, '.html'))
