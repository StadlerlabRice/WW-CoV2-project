# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file
source('./inputs_for_analysis.R') # Source the file with user inputs

# Parameters ----------------------------------------------------------------------

# sheets to read from qPCR data dump excel file
read_these_sheets <- c( 'dd.WW140_0303_LS_N1N2', 'dd.WW141_0304_LS_N1N2', 'dd.WW142_0305_LS-CON_N1N2')

title_name <- '0303 LS'

regular_WWTP_run_output <- TRUE # make TRUE of you want to output the WWTP only data and manhole samples sheets 
      # (make FALSE for controls, testing etc. where only "complete data" sheet is output)

# rarely changed parameters

# Spike in and concentration details
elution_volume <- 50 # ul - RNA extraction final volume

# copies/ul viral suspension spiked in : This is auto-matched from the list of vaccine data in data dump/Vaccine_summary
spike_virus_volume <- 50 # ul of viral suspension spiked in x ml WW; (x ~ 350 - 450 and varies for each sample)

samples_to_remove <- regex('DI|NTC|Blank', ignore_case = TRUE) # control samples that wont be sent to HHD

# Preliminary ----

# Attaching names to data
 
source('./1-processing_functions.R') # Source the file with the ddPCR and qPCR name's attaching functions

# running function to attach names. Data is saved to sheet : "qPCR data dump"
map(read_these_sheets, process_ddpcr)

# Input data ----------------------------------------------------------------------

# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet
list_raw_quant_data <- map(read_these_sheets, 
                           ~ read_sheet(sheeturls$data_dump, sheet = ., range = 'A:M')) 

# bind multiple reads and clean up names
raw_quant_data <- bind_rows(list_raw_quant_data) %>% 
  rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it 
  # select(-any_of('Well Position')) %>%  # I wonder why Well Position needed to be removed..?
  mutate_at('assay_variable', as.character) %>% 
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% 
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) %>%  # make a unique column for matching volumes 
  mutate(across(Label_tube, ~str_remove(., ' ') )) # remove spaces from the Sample_name (Started with 1123 Pavan with 1123 69S names)

  # temporary for chemagic during transition
  # mutate(extraction_method = if_else(str_detect(Sample_name, '^C'), 'Chemagic', 'Maxwell')) %>% # note if Chemagic with a C
  # mutate(across(Label_tube, ~ str_remove(., '^C'))) # remove the prefix C to make samples match to the sample registry


# Load metadata ----------------------------------------------------------------------

# get all bayou, manhole and WWTP sample names (remain same every week)
biobot_lookup <- map_df(c('All Bayou', 'All manhole', 'All wastewater'), 
                        ~ read_sheet(sheeturls$biobot_id , sheet = .x, range = 'A:C', col_types = 'ccc') %>% 
                          rename('WWTP' = contains('SYMBOL', ignore.case = T), 
                                 'FACILITY NAME' = matches('FACILITY NAME', ignore.case = T),
                                 'Type' = 'Facility Type') %>% 
                          mutate(WWTP = as.character(WWTP) %>% str_remove(' '), # convert to char and removed spaces
                                 'assay_variable' = WWTP))

# List of all WWTPs
WWTP_symbols <- biobot_lookup %>%
  filter(Type == 'Wastewater') %>% 
  pull(WWTP) %>% 
  paste(collapse = "|")

# list all manhole names (for regex matching)
manhole_sample_symbols <- biobot_lookup %>%
  filter(!str_detect(Type, 'Wastewater|Bayou')) %>% 
  pull(WWTP) %>% 
  paste(collapse = "|")

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
  rename(spike_virus_conc = matches('Stock conc.'), Sample_name = Week)  


# Attach metadata ----------------------------------------------------------------------

# Join WWTP names to qPCR dataset for Rice data

vol_R <- raw_quant_data %>% 
  filter(!str_detect(Target, 'Baylor')) %>% # filter only Rice data
  
  # join sample registry data
  left_join(volumes_data_Rice, by = 'Label_tube') %>%
  mutate_at('Biobot_id', ~if_else(is.na(.x), str_c(Sample_name, assay_variable), .x)) %>% # stand-by name for missing cols
  
  # join vaccine quantification
  left_join(spike_list %>% select(Vaccine_ID, Target, spike_virus_conc),  by = c('Vaccine_ID', 'Target') ) %>% 
  
  left_join(biobot_lookup) %>%  # join biobot_IDs
  

  mutate_at(c('WWTP', 'FACILITY NAME'), ~if_else(str_detect(., '^X')|is.na(.), assay_variable, .)) %>% 
  mutate(original_sample_name = Sample_name , Sample_name = harmonize_week(Sample_name)) # retain min of consecutive dates

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
  
  # Calculations for spiked in and recovered copies of the virus
  mutate(`Actual spike-in` = spike_virus_conc * spike_virus_volume / (WW_vol * 1e-3), 
         Recovered = `Copy #` *(1e6/300) * (elution_volume/vol_extracted), # Chemagic concentration factor = 300 
         Detection_Limit = as.numeric(LimitOfDet * (1e6/300) * (elution_volume/vol_extracted) ), # Chemagic concentration factor = 300 
         `Percentage_recovery_BCoV` = 100 * Recovered/`Actual spike-in`) %>% 
  # (source for chemagic conc. factor: concentration factor calc : https://docs.google.com/spreadsheets/d/19oRiRcRVS23W3HqRKjhMutJKC2lFOpNK8aNUkC-No-s/edit#gid=2134801800)
  
  mutate_cond(str_detect(Sample_name, 'Vaccine'), `Actual spike-in` = spike_virus_conc * spike_virus_volume / (.050 * 1e-3), Recovered = `Copy #` * 1e6 * 50/20, `Percentage_recovery_BCoV` = 100 * Recovered/`Actual spike-in`) %>% 
  mutate_cond(str_detect(Target, 'Baylor'), `Actual spike-in` = spike_virus_conc * spike_virus_volume / (WW_vol * 1e-3), Recovered = `Copy #` * 1e6 /30, `Percentage_recovery_BCoV` = 100 * Recovered/`Actual spike-in`) %>% 
    
  select(-spike_virus_conc) %>% 
  
  # arranging data by facility name alphabetical
  arrange(`FACILITY NAME`) %>% 
  mutate_at('WWTP', as_factor)

# Adding a dummy CT column (if only ddPCR data is being loaded; which lacks the CT column) - for compatibility with qPCR code
if(processed_quant_data %>%  {!'CT' %in% colnames(.)}) processed_quant_data$CT = NA

# Vaccine ID duplication value check - Brings user attention to duplicate values in the Vaccine_summary in data dump
processed_quant_data$Vaccine_ID %>% 
  unique() %>%  # find all the Vaccine IDs in use for the current week data
  paste(collapse = '|') %>% # make a regular expression (regex) combining them
  
  {filter(spike_list, str_detect(Vaccine_ID, .))} %>% 
  unique %>%  # filter the list of vaccine data with these IDs (from data dump) that are unique
  
  {if(nrow (.) > 1) {
    duplicate_vaccine_values <- . 
    view(duplicate_vaccine_values)
    stop("Duplicate vaccine IDs found in the data dump, 
         please check the table: *duplicate_vaccine_values* for more information")
  }
    else if(nrow(.) > 0 &&  .$spike_virus_conc == 0){
      zero_vaccine_values <- . 
      view(zero_vaccine_values)
      stop("Zeros found in vaccine quants in the data dump, please fix") 
    }
  }

# Data output ----------------------------------------------------------------------


presentable_data <- processed_quant_data %>% 
  
  # renaming variables
  rename('Facility' = `FACILITY NAME`, 'Received_WW_vol' = WW_vol, Ct = CT, 'Target Name' = Target) %>%
  rename('Copies/ul RNA' = `Copy #`, 'Copies/l WW' = Recovered, 'Spiked-in Copies/l WW' = `Actual spike-in`) %>%
  rename('Volume Filtered' = vol_extracted,
         PositiveDroplets = Positives) %>% 
  
  # Adding new variables, modifying existing variables
  mutate(Date = Sample_name %>% 
           str_extract('[:digit:]{3,4}') %>% # Extract the date component of the sample name
           str_replace('([:digit:]+)([:digit:]{2})', '\\1/\\2/21') ,  # format it as mm/dd/yy
         Lab = if_else(str_detect(`Target Name`, 'Baylor'), 'B', 'R'),
         # Detection_Limit = if_else(str_detect(`Target Name`, 'N1|N2'), 330, 
         #                           if_else(str_detect(`Target Name`, 'Baylor'), 23500, 705) 
         #                           ) ,
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
  mutate_cond(str_detect(`Target Name`, '^N'), `Percentage_recovery_BCoV` = NA, `Spiked-in Copies/l WW` = NA) %>%
  
  # Selecting column order
  select(Facility, WWTP, Date, Lab, `Target Name`, 
         `Received_WW_vol`, `Volume Filtered`, 
         `Copies/ul RNA`, `Copies/l WW`, 
         Ct, AcceptedDroplets, PositiveDroplets, Sample_ID, 
         Detection_Limit, Positivity, 
         Sample_Type, `Spiked-in Copies/l WW`, `Percentage_recovery_BCoV`, 
         WWTP_ID, Tube_ID, Comments, any_of('variant_status'), 'Well Position') %>%
  
  mutate_at('Target Name', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2'))) %>% 
  mutate_at('Target Name', ~str_remove(., '/Baylor')) %>% 

  
  # B117 special plug
  {if(str_detect(read_these_sheets, 'B117') %>% any) {
    calculate_B117_percentage_variant(.) %>% # calculating the percentage of variant vs total S copies 
    select(-any_of('variant_status')) %>% 
    relocate(percentage_variant, .after = 'Target Name') }
    else .
  } %>% 
  
  mutate(across(where(is.numeric), ~ round(., 2))) # rounding off all numerical things


# Missing value check ---- 


# Brings user attention to missing values in the sample registry

# Missing manhole samples in Biobot ID sheet
missing_entries_in_Biobot_registry <- presentable_data %>%  # identify samples in the set that are 
  filter(!str_detect(WWTP, paste(WWTP_symbols,  # neither WWTPS
                                 manhole_sample_symbols,  # nor manholes
                                 samples_to_remove,  # nor controls : DI, NTC, Blanks
                                 sep = '|')))

if(missing_entries_in_Biobot_registry %>% plyr::empty() %>% !.)
{View(missing_entries_in_Biobot_registry)
proceed_with_errors_key <- menu(c('Yes', 'No'), title = 'Missing entries identified in the sample registry (most likely manhole),
check the data output in the console and choose if you wish to continue processing data')

if(proceed_with_errors_key == 2) stop("Cancel selected, script aborted.")
if(proceed_with_errors_key == 1) 
{ print('Missing Biobot ID entries will be named as Date/WWTP in the Facility Name')
}
}

# Volume filtered columns empty => read as NA
if(is.na(presentable_data$`Volume Filtered`) %>% any())  # old: map(presentable_data, ~ -Inf %in% .x) %>% any()
  
  {missing_values_sample_registry <- presentable_data %>% filter(is.na(`Volume Filtered`) & !str_detect(WWTP, samples_to_remove)) # old : filter_if(is.numeric, any_vars( . < 0))
  View(missing_values_sample_registry)
  proceed_with_errors_key <- menu(c('Yes', 'No'), title = 'Missing values identified in the sample registry : WW volume extracted (ml),
check the data output in the console and choose if you wish to continue processing data, by assuming 50 ml default')

  if(proceed_with_errors_key == 2) stop("Cancel selected, script aborted.")
  if(proceed_with_errors_key == 1) 
    { print('Missing volumes filtered are being converted to 50 ml (default) to avoid error in processing data')
    presentable_data %<>% replace_na(list(`Volume Filtered` = 50))
  }
}

print('Missing values (~ Received volume) are being converted to NaNs to avoid error in writing data')
presentable_data %<>% mutate(across(where(is.numeric),  ~ if_else(.x == -Inf, NaN, .x)))


# Output data - including controls
check_ok_and_write(presentable_data %>% select(-Sample_ID), sheeturls$complete_data, title_name) # save results to a google sheet, ask for overwrite

# switch for output to sheet sent to HHD
if(regular_WWTP_run_output)
{
  
  if(str_detect(read_these_sheets, 'B117') %>% any) {
    source('./b117-short-circuit.R')
    stop('B117 output printed successfully, exiting script. This is not an error!')
  }
  
  # presentable data for health department
  present_WW_data <- presentable_data %>%
    
    rename('Copies_per_uL' = `Copies/ul RNA`,
           'Copies_Per_Liter_WW' = `Copies/l WW`,
           'Recovery_Rate' = `Percentage_recovery_BCoV`,
           Target_Name = `Target Name`) %>%
    select(-contains('Vol'), -`Spiked-in Copies/l WW`, -Tube_ID, -WWTP_ID, -contains('Droplet'), -'Well Position')
  
  present_only_WW <- present_WW_data %>% 
    filter(str_detect(WWTP, WWTP_symbols)) # retain only WWTP data
  
  # Write data if not empty
  if(present_only_WW %>% plyr::empty() %>% !.){
    check_ok_and_write(present_only_WW, sheeturls$wwtp_only_data, title_name) # save results to a google sheet, ask for overwrite
    write_csv(present_only_WW, path = str_c('excel files/Weekly data to HHD/', title_name, '.csv'), na = '') # output csv file
  }
  
  present_manhole_samples <- present_WW_data %>% filter(str_detect(WWTP, manhole_sample_symbols))
  
  # Write data if not empty
  if(present_manhole_samples %>% plyr::empty() %>% !.){
    check_ok_and_write(present_manhole_samples, sheeturls$wwtp_only_data, str_c(title_name, ' manhole samples')) # save results to a google sheet, ask for overwrite
    write_csv(present_manhole_samples, path = str_c('excel files/Weekly data to HHD/', title_name, ' manhole samples.csv'), na = '') # output CSV file
  }
}



# Summary and long_format ------------------------------------
# For ease of plotting data with mean and standard deviations

minimal_label_columns <- c('Target', 'Sample_name', 'WWTP')

# Extract minimal columns from processed data (good for running averages and std deviations for plotting)
processed_minimal = list( raw.dat = processed_quant_data %>% 
                            select(all_of(minimal_label_columns), where(is.numeric), -matches('vol')) %>% 
                            rename(Percentage.recovery.BCoV = 'Percentage_recovery_BCoV')) # removing underscores to enable adding mean, sd prefixes in summarize
# Group by all the text columns and calculate mean and standard deviation for biological replicates
processed_minimal$summ.dat <- processed_minimal$raw.dat %>% 
  group_by_at(all_of(minimal_label_columns)) %>% 
  summarize_all(.funs = lst(mean, sd), na.rm = T)

# Convert the above minimal data into long format (convenient for plotting multiple data types on the same plot)
long_processed_minimal <- processed_minimal %>% map(pivot_longer, cols = where(is.numeric),
                                                    names_to = 'Measurement', values_to = 'value')                                                   
long_processed_minimal$summ.dat %<>% separate(Measurement, into = c('Measurement','val'),"_") %>% 
  pivot_wider(names_from = val, values_from = value) # Seperate mean and variance and group by variable of measurement

# Adding back the underscore in columns (ex: Percentage_recovery_BCoV)
processed_minimal %<>% map( ~ rename(.x, Percentage_recovery_BCoV = contains('Percentage.recovery.BCoV')))
long_processed_minimal %<>% map(
  ~ mutate(.x, across (Measurement,
                       ~ str_replace(.x, 'Percentage.recovery.BCoV', 'Percentage_recovery_BCoV')
  )
  )
)


# Plotting into html -----------------------------------------------------------------------


# calling r markdown file
rmarkdown::render('make_html_plots.rmd', output_file = str_c('./qPCR analysis/', title_name, '.html'))
