# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file
source('./inputs_for_analysis.R') # Source the file with user inputs

# Parameters ----------------------------------------------------------------------

# sheets to read from qPCR data dump excel file
read_these_sheets <- c( 'dd.WW52_Conc method 2b_N1N2 + BCoV2',
                        'dd.WW53_Conc Meth 2_N1N2',
                        'dd.WW54_Conc Meth 2_BCoV2',
                        'dd.WW55_S38 boil_BCoV2',
                        'WW79_Conc Methods 2a_pMMoV_Std58',
                        'WW80_Conc Methods 2b_pMMoV_Std59')

title_name <- 'Concentration methods paper-2'

# Biobot_id sheet
bb_sheets <- c('Week 26 (10/5)')

# Extra categories for plotting separately (separate by | like this 'Vaccine|Troubleshooting')
extra_categories = 'Std|Control|e811|Acetone' # for excluding this category from a plot, make the switch (exclude_sample = TRUE)
special_samples = 'HCJ|SOH|ODM|AO' # putting special samples in a separate sheet

regular_WWTP_run_output <- F # make TRUE of you want to output the WWTP only data and special samples sheets 
      # (make FALSE for controls, testing etc. where only "complete data" sheet is output)

# rarely changed parameters

# Spike in and concentration details
elution_volume <- 50 # ul - RNA extraction final volume

# copies/ul viral suspension spiked in : This is auto-matched from the list of vaccine data in data dump/Vaccine_summary
spike_virus_volume <- 50 # ul of viral suspension spiked in x ml WW; (x ~ 350 - 450 and varies for each sample)

# Extra metadata locations for conc methods

conc_factors_url <- 'https://docs.google.com/spreadsheets/d/1_32AE3IkBRD3oGSYcYqZwknHZEHiGKtoy1zK5VVTzsI/edit#gid=214903643'
dilution_flname <- 'dilutions of template_BCoV2_dd.WW54, 52' %>% 
  str_c('qPCR analysis/Methods paper/Archive/', ., '.xlsm')
ultrafiltration_flname <- 'Ultrafiltration Tube Weights' %>% 
  str_c('qPCR analysis/Methods paper/Archive/', ., '.xlsx')

# Input data ----------------------------------------------------------------------

# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet
list_raw_quant_data <- map(read_these_sheets, 
                           ~ read_sheet(sheeturls$data_dump, sheet = ., range = 'A:H') %>% 
                             select(-any_of('Conc(copies/µL)'))) 

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


# Bring WWTP name to biobot_ID mapping from google sheet: "Biobot Sample IDs"
biobot_lookup <- map_df(bb_sheets, 
                        ~ read_sheet(sheeturls$biobot_id , sheet = .x) %>% 
                          rename('Biobot_id' = matches('Biobot|Comments|Sample ID', ignore.case = T), 'WWTP' = contains('SYMBOL', ignore.case = T), 'FACILITY NAME' = matches('FACILITY NAME', ignore.case = T)) %>% 
                          mutate('Biobot_id' = str_remove(`Biobot_id`,'\\.| '), WWTP = as.character(WWTP)) %>% 
                          select(`Biobot_id`, `FACILITY NAME`, WWTP)
)


# List of all WWTPs
all_WWTP_names <- biobot_lookup %>% pull(WWTP) %>% unique()

# Get volumes data from google sheet : "Sample registry"
volumes_data_Rice <- read_sheet(sheeturls$sample_registry , sheet = 'Concentrated samples') %>% 
  rename('WW_vol' = `Total WW vol measured (ml)`, 
         'Label_tube' = `Label on tube`, 
         vol_extracted = `WW volume extracted (ml)`,
         Vaccine_ID = `Stock ID of Spike`,
         'Biobot_id' = `Biobot/other ID`,
         WW_weight = `Total WW weight measured (kg)`) %>% 
  mutate('WW_vol' = coalesce(WW_vol, `Total WW volume calculated (ml)`) ) %>% 
  
  select(WW_vol, Label_tube, vol_extracted, Vaccine_ID, `Biobot_id`, WW_weight) %>% # select only the useful columns
  distinct() %>% # for removing repeated data in early stuff, before 608 (interferes with the merging of volumes for same bottles)
  mutate_at('Label_tube', ~str_remove_all(., " ")) %>% 
  mutate_at('Biobot_id', str_remove,  ' ') %>% 
  
  # Extrapolating volumes for same bottle - If weights are written out, we assume different bottles with different volumes
  mutate(., unique_labels = str_remove_all(Label_tube,'^m|[1-9]$'))  %>%  # this will be changed to the 'Bottle' column soon
  group_by(unique_labels) %>% 
  mutate(across(WW_vol, ~ifelse(is.na(WW_weight), max(., na.rm = T), .))) %>% 
  ungroup() %>% 
  select(-unique_labels, -WW_weight)


# conc.methods metadata ----


concentration_factors <- read_sheet(conc_factors_url, sheet = 'Summary') %>% 
  mutate(Sample_name = Method) %>%  # making it compatible for joining
  rename(concentration.factor = Concentration_factor)

ultrafiltration_volume <- read_xlsx(ultrafiltration_flname, range = 'A1:D22') %>% 
  mutate(across(Label_tube, ~ str_remove(.x, ' ') )) %>% 
  rename(UF_vol_ul = Total_volume_ul) %>% 
  select(Label_tube, UF_vol_ul)

dilution_vol1 <- read_xlsx(dilution_flname, sheet = 'volumes_plate 2A+', range = 'B2:Q10') %>% read_plate_to_column('dil_vol')
dilution_samples2 <- read_xlsx(dilution_flname, sheet = 'samples_plate 2A+', range = 'B2:Q10') %>% read_plate_to_column('Sample_name')

dilution_factors_base <- left_join(dilution_vol1, dilution_samples2) %>%  # join the dilution volumes to names
  select(-`Well Position`) %>% 
  mutate(dilution.factor = 1) # dummy column - edited in next step

# dilution factors for each target
dilution_factors <- tibble(Target = c('N1_multiplex', 'N2_multiplex', 'BCoV2', 'pMMoV_Vgp1'), lsts = list(dilution_factors_base) ) %>% 
  unnest(cols = c(lsts)) %>% 
  
  mutate_cond(str_detect(Target, 'BCoV2'), dilution.factor = (dil_vol + 196)/dil_vol * 200/4) %>% 
  mutate_cond(str_detect(Target, 'pMMoV'), dilution.factor = (dil_vol + 196)/dil_vol) %>% 
  mutate_cond(str_detect(Target, '^N') & str_detect(Sample_name, 'HA_H.1'), dilution.factor = 3) %>%
  mutate_cond(str_detect(Sample_name, 'Vacboil'), dilution.factor = 20/dil_vol * 200/4 * 200/4) %>%
  
  # Processing the same as regular data for ease of column joining (will incorporate by condition BCoV or pMMoV)
  separate(`Sample_name`,c(NA, 'Sample_name'),'-') %>% separate(`Sample_name`,c('Sample_name','Tube ID'),'_') %>% 
  mutate(`Tube ID` = if_else(`Sample_name` == 'NTC', '0', `Tube ID`)) %>% 
  separate(`Tube ID`, c('assay_variable', 'biological_replicates'), remove = F) %>%  # Separate out biological replicates 
  unite('Tube ID', c(assay_variable, biological_replicates), sep = '.', remove = F, na.rm = T) %>% # remaking Tube ID - removes spaces after 'dot'
  # unite('Biobot ID', c(`Sample_name`, assay_variable), sep = '', remove = F) %>%
  
  mutate_at('assay_variable', as.character) %>% 
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% 
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) # make a unique column for matching volumes 

  

rm(dilution_vol1, dilution_samples2, dilution_factors_base) # clear temporary data


# baylor's ID, vols ----------------------------------------------------------------------   

baylor_trigger <- raw_quant_data$Target %>% str_detect('Baylor') %>% any()

# Getting baylor's volumes from a separate excel file (from Austen's email)
if (baylor_trigger) {
  
  baylor_volumes_and_biobots <- raw_quant_data %>% 
    filter(str_detect(Target, 'Baylor')) %>%  
    pull(Sample_name) %>% unique() %>%  # get all Baylor sheet names present in data
    .[!str_detect(., 'NTC|Std')] %>%  # Excluding NTC or Standards from being counted as Baylor data (data is still output)
    
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
spike_list <- read_sheet(sheeturls$data_dump, sheet = 'Vaccine_summary', range = 'B6:K', col_types = 'Dcccnnnccn') %>% 
  
  filter(str_detect(Vaccine_ID, 'Sb')) %>% # filter for boiled quants
  mutate(across(Vaccine_ID, ~ str_remove(.x, 'b'))) %>%  # remove 'b' to make the regular vaccine ID for matching
  rename(spike_virus_conc = matches('Stock conc.'), Sample_name = Week)  


# Attach metadata ----------------------------------------------------------------------

# Join WWTP names to qPCR dataset for Rice data

vol_R <- raw_quant_data %>% 
  filter(!str_detect(Target, 'Baylor')) %>% # filter only Rice data
  
  left_join(volumes_data_Rice, by = 'Label_tube') %>%
  mutate_at('Biobot_id', ~if_else(is.na(.x), str_c(Sample_name, assay_variable), .x)) %>% # stand-by name for missing cols
  
  left_join(spike_list %>% select(Vaccine_ID, Target, spike_virus_conc),  by = c('Vaccine_ID', 'Target') ) %>% 
  left_join(biobot_lookup, by = 'Biobot_id') %>% 
  
  mutate_at(c('WWTP', 'FACILITY NAME'), ~if_else(str_detect(., '^X')|is.na(.), assay_variable, .)) %>% 
  mutate(original_sample_name = Sample_name , Sample_name = harmonize_week(Sample_name)) %>%  # retain min of consecutive dates
  
  left_join(dilution_factors) %>%  # attach dilution factors
  mutate(across(dilution.factor, ~ coalesce(.x, 1)), across(`Copy #`, ~ .x * dilution.factor)) # correcting for dilution of RNA template
  
# Join Baylor WWTP volumes
if (baylor_trigger) {
  
  vol_B <- raw_quant_data %>% 
    filter(str_detect(Target, 'Baylor')) %>% 
    # select(-`Biobot_id`) %>% 
    left_join(baylor_volumes_and_biobots, by = 'Label_tube') %>% 
    fuzzyjoin::regex_left_join(., 
                                spike_list %>% select(Sample_name, Vaccine_ID, Target, spike_virus_conc),
                                by = c('Sample_name', 'Target')) %>% 
    select(-`Sample_name.y`, -`Target.y`) %>% rename(Sample_name = `Sample_name.x`, Target = Target.x)
  
  
} else vol_B <- tibble(NULL)

# Calculations ----

# Copies/ul RNA to copies/l WW. Spike in concentration, % recovery are calculated
# join the results with the WWTP identifiers and names
processed_quant_data <- bind_rows(vol_R, vol_B) %>% 
  
  # attach ultrafiltration volumes - to correct UF conc. factor
  left_join(ultrafiltration_volume) %>% 
  mutate(across(UF_vol_ul, ~ coalesce(.x, 1500) )) %>% 
  
  # attach concentration factors for various methods
  left_join(concentration_factors) %>% 
  mutate(across(concentration.factor, ~ .x * 1500/UF_vol_ul )) %>% 
  
  # Calculations for spiked in and recovered copies of the virus
  mutate(`Actual spike-in` = spike_virus_conc * spike_virus_volume / (WW_vol * 1e-3), 
         Recovered = `Copy #` * 1e6 / concentration.factor , 
         `Recovery fraction` = Recovered/`Actual spike-in`) %>% 
  
  # mutate_cond(str_detect(Sample_name, 'Vaccine'), `Actual spike-in` = spike_virus_conc * spike_virus_volume / (.050 * 1e-3), Recovered = `Copy #` * 1e6 * 50/20, `Recovery fraction` = 100 * Recovered/`Actual spike-in`) %>% 
  mutate_cond(str_detect(Target, 'Baylor'), `Actual spike-in` = spike_virus_conc * spike_virus_volume / (WW_vol * 1e-3), Recovered = `Copy #` * 1e6 /30, `Recovery fraction` = Recovered/`Actual spike-in`) %>% 
  select(-spike_virus_conc) %>% 
  
  # arranging data by facility name alphabetical
  arrange(`FACILITY NAME`) %>% 
  mutate_at('WWTP', as_factor)

# Adding a dummy CT column (if only ddPCR data is being loaded; which lacks the CT column) - for compatibility with qPCR code
if(processed_quant_data %>%  {!'CT' %in% colnames(.)}) processed_quant_data$CT = NA


# Summary and long_format ----

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
  rename(Concentration_method = Method_full_name) %>% 
  rename('Facility' = `FACILITY NAME`, 'Original Sample Volume' = WW_vol, Ct = CT, 'Target Name' = Target) %>%
  rename('Copies/ul RNA' = `Copy #`, 'Copies/l WW' = Recovered, 'Spiked-in Copies/l WW' = `Actual spike-in`) %>%
  rename('Volume Filtered' = vol_extracted) %>% 
  
  # Adding new variables, modifying existing variables
  mutate(Date = title_name %>% str_extract('[:digit:]{3,4}') %>% str_replace('([:digit:]+)([:digit:]{2})', '\\1/\\2/20') , 
         Lab = if_else(str_detect(`Target Name`, 'Baylor'), 'B', 'R'),
         Detection_Limit = if_else(str_detect(`Target Name`, 'N1|N2'), 330, 
                                   if_else(str_detect(`Target Name`, 'Baylor'), 23500, 705) 
                                   ) ,
         Sample_Type = 'Composite', Comments = NA) %>% 
  mutate_at(c('Sample_name', 'original_sample_name'), ~as.character(.)) %>%
  mutate_at('Facility', ~if_else(. == assay_variable, str_c(original_sample_name, '/', assay_variable), .)) %>%
  mutate(Tube_ID = if_else(biological_replicates == ''|is.na(biological_replicates), str_c(original_sample_name, ' ', assay_variable), str_c(original_sample_name, ' ', assay_variable, '', biological_replicates)) ,
         WWTP_ID = if_else(biological_replicates == ''|is.na(biological_replicates), str_c(Sample_name, '.', WWTP) , str_c(Sample_name, '.', WWTP, '-', biological_replicates)),
         Sample_ID = WWTP_ID,
         'Limit of detection (copies/ul RNA)' = if_else(str_detect(`Target Name`, 'N1|N2'), 0.3, 0.5 )) %>% 
  
  # Arrange rows by WWTP facility 
  arrange(Facility, biological_replicates) %>%
  # unite('Facility', c(Facility, biological_replicates), sep = "-", na.rm = T) %>%
  mutate_cond(str_detect(`Target Name`, '^N'), `Recovery fraction` = NA, `Spiked-in Copies/l WW` = NA) %>%
  
  # Selecting column order
  select(Facility, WWTP, Date, Lab, `Target Name`, `Original Sample Volume`, `Volume Filtered`, Ct, `Copies/ul RNA`, `Copies/l WW`, Sample_ID, Detection_Limit, Sample_Type, `Spiked-in Copies/l WW`, `Recovery fraction`,
         WWTP_ID, Tube_ID, Comments, Method, Concentration_method, dilution.factor, concentration.factor) %>%
  mutate_at('Target Name', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2'))) %>% 
  mutate_at('Target Name', ~str_remove(., '/Baylor'))


# Missing value check - Brings user attention to missing values in the sample registry

# work in progress - this is not where -Inf shoud be checked for since this has all data from begining of time
if(map(presentable_data, ~ -Inf %in% .x) %>% any())
  
  {missing_values_sample_registry <- presentable_data %>% filter_if(is.numeric, any_vars( . < 0))
  View(missing_values_sample_registry)
  proceed_with_errors_key <- menu(c('Yes', 'No'), title = 'Missing values identified in the sample registry,
check the data output in the console and choose if you wish to continue processing data')

  if(proceed_with_errors_key == 2) stop("Cancel selected, script aborted.")
  if(proceed_with_errors_key == 1) 
    { print('Missing values are being converted to NaNs to avoid error in writing data')
    presentable_data %<>% mutate(across(where(is.numeric),  ~ if_else(.x == -Inf, NaN, .x)))
  }
}


# Output data - including controls
check_ok_and_write(presentable_data %>% select(-Sample_ID), sheeturls$complete_data, title_name) # save results to a google sheet, ask for overwrite

# switch for output to sheet sent to HHD
if(regular_WWTP_run_output)
{
  # presentable data for health department
  present_WW_data <- presentable_data %>%
    filter(WWTP %in% all_WWTP_names) %>%  # retain only WWTP data
    rename('Copies_per_uL' = `Copies/ul RNA`,
           'Copies_Per_Liter_WW' = `Copies/l WW`,
           'Recovery_Rate' = `Recovery fraction`,
           Target_Name = `Target Name`) %>%
    select(-contains('Volume'), -`Spiked-in Copies/l WW`, -Tube_ID, -WWTP_ID)
  
  present_only_WW <- present_WW_data %>% filter(!str_detect(Facility, special_samples))
  
  # Write data if not empty
  if(present_only_WW %>% plyr::empty() %>% !.){
    check_ok_and_write(present_only_WW, sheeturls$wwtp_only_data, title_name) # save results to a google sheet, ask for overwrite
    write_csv(present_only_WW, path = str_c('excel files/Weekly data to HHD/', title_name, '.csv'), na = '') # output csv file
  }
  
  present_special_samples <- presentable_data %>% filter(str_detect(Facility, special_samples))
  
  # Write data if not empty
  if(present_special_samples %>% plyr::empty() %>% !.){
    check_ok_and_write(present_special_samples, sheeturls$wwtp_only_data, str_c(title_name, ' special samples')) # save results to a google sheet, ask for overwrite
    write_csv(present_special_samples, path = str_c('excel files/Weekly data to HHD/', title_name, ' manhole samples.csv'), na = '') # output CSV file
  }
}


# Plotting into html -----------------------------------------------------------------------


# calling r markdown file
rmarkdown::render('make_html_plots.rmd', output_file = str_c('./qPCR analysis/', title_name, '.html'))
