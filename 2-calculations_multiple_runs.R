# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file

# Parameters ----------------------------------------------------------------------

# Enter all the parameters in the google sheet : User inputs, metadata, queries for R code
# url = https://docs.google.com/spreadsheets/d/1SAINnazqMrjTBSuhYiIBbx8B7_reHzaEuwGTdkNA6wk/edit#gid=288270795
# Duplicate the 'template', name it after the data set (same as title_name) and BRING it to the FIRST position in sheets

# Read the input parameters sheet and create all the parameters
source('./scripts_general functions/g.12-sheet-to-params.R') 

# Preliminary ----

# Source script
source('./1-processing_functions.R') # Source the file with the ddPCR and qPCR name's attaching functions



# Input data ----------------------------------------------------------------------

# running function to attach samples names to data. Attached data is ALSO saved to sheet : "qPCR data dump"
list_quant_data <- map(read_these_sheets, 
                           ~ process_ddpcr(.x) %>% 
                             select(1:15))

# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet -- Obsolete
# list_quant_data2 <- map(read_these_sheets, 
#                            ~ read_sheet(sheeturls$data_dump, sheet = ., range = 'A:M')) 

# bind multiple ddPCR runs and clean up names
quant_data <- bind_rows(list_quant_data) %>% 
  
  rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convention, this will rename it 
  # select(-any_of('Well Position')) %>%  # I wonder why Well Position needed to be removed..?
  mutate_at('assay_variable', as.character) %>%  # if assay_variable has all numbers, this will turn them into char
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>%  # make empty biological_replicates into empty string ('')
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% # remove dot (.) from Tube ID
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) %>%  # make a unique column for matching volumes 
  mutate(across(Label_tube, ~str_remove(., ' ') )) # remove spaces from the Sample_name (Started with 1123 Pavan with 1123 69S names)


# Check if pellet samples are present using the prefix 'p' in the assay_variable
pellets_present <- quant_data$assay_variable %>% str_detect('^p') %>% any()


# Load metadata ----------------------------------------------------------------------


# Get volumes data from google sheet : "Sample registry"
volumes.data_registry <- 
  range_speedread(ss = sheeturls$sample_registry,  # this is 20x faster than read_sheet!
                  sheet = 'Concentrated samples',
                  range = 'A:H', # specify range to read
                  col_types = 'ccncccnn')  %>% # pre-specify column types within the range
  
  # rename col names
  rename('Label_tube' = `Label on tube`, 
         'Biobot_id' = `Biobot/other ID`,
         Filtered_WW_vol = `WW volume filtered (ml)`,
         
         Sample_Type = `Grab vs Composite`,
         No_of_Hours_Missed = `HHD Notes (# of samples missed/hours)`, 

         # spiking relevant data
         Vaccine_ID = `Stock ID of Spike`,
         'Received_WW_vol' = `Total WW volume received (ml)`, 
         WW_weight = `Total WW weight measured (kg)`) %>% 
  
  select(Received_WW_vol, Label_tube, Filtered_WW_vol, Vaccine_ID, 
         `Biobot_id`,
         WW_weight, Sample_Type, No_of_Hours_Missed) %>% # select only the useful columns
  
  distinct() %>% # for removing repeated data in early stuff, before 608 (interferes with the merging of volumes for same bottles)
  mutate_at('Label_tube', ~str_remove_all(., " ")) %>% 
  mutate_at('Biobot_id', str_remove,  ' ') %>% 
  
  # Extrapolating volumes for same bottle - If weights are written out, we assume different bottles with different volumes
  mutate(., unique_labels = str_remove_all(Label_tube,'[:digit:]$'))  %>% # Remove the trailing single digit that tells the replicate # 
  # this will be changed to the 'Bottle' column? Too much data entry
  
  group_by(unique_labels) %>% 
  mutate(across(Received_WW_vol, 
                       ~ifelse(is.na(WW_weight), # If any of the replicates doesn't have weight
                               if(all(is.na(.))) NA else max(., na.rm = T), # the make it's volume the max
                               .) # Unless the volume is all NA in which case NA is used insteado of the max
                ) ) %>% # This will not allw MAX to create -Inf :: to prevent errors in write_sheet 
  ungroup() %>% 
  select(-unique_labels, -WW_weight)

# Get pellet weight related data (monkeypox, for copies/g calculation)
week_name <- str_extract(title_name, '[:digit:]{6}')
if(pellets_present) 
{pellet_weight_data <- read_sheet(sheeturls$pellet_weights, sheet = str_c(week_name, ' Pellets')) %>% # get the Tube Label, pellet_mass and dry_mass_fraction
  mutate(across(Label_tube, ~str_remove(., ' ') )) %>% # remove spaces from the Sample_name
  select(Label_tube, pellet_wet_mass, dry_mass_fraction) %>% 
  mutate(across(c(pellet_wet_mass, dry_mass_fraction), as.numeric)) }


# Vaccine spike concentrations
spike_list <- read_sheet(sheeturls$data_dump, sheet = 'Vaccine_summary', range = 'B6:K', col_types = 'Dcccnnnccn') %>% 
  rename(spiking_virus_vaccine_stock_conc = matches('Stock conc.'), Sample_name = Week)  


# Attach metadata ----------------------------------------------------------------------

# Join WWTP names to qPCR dataset for Rice data

meta.attached_quant_data <- quant_data %>% 
  
  # join sample registry data
  left_join(volumes.data_registry, by = 'Label_tube') %>%
  mutate_at('Biobot_id', ~if_else(is.na(.x), str_c(Sample_name, assay_variable), .x)) %>% # stand-by name for missing cols
  
  # join vaccine quantification
  left_join(spike_list %>% select(Vaccine_ID, Target, spiking_virus_vaccine_stock_conc),  by = c('Vaccine_ID', 'Target') ) %>% 
  
  # Extra analysis for Pellet samples -- Monkeypox..?
  {if(pellets_present) {
  mutate(.data = ., sample_type = if_else(str_detect(assay_variable, '^p'), 'Pellet', 'Liquid')) %>% # pellet or liquid?
  mutate(across(assay_variable, ~ str_remove(.x, '^p'))) } # Remove the ^p from the pellet samples (will add back to WWTP column)
    else . } %>% # If no pellet samples present, pipe the input to the next step
  
  
  # Joining full names of the WWTPs and other samples : form Biobot_ID google sheet
  left_join(biobot_lookup) %>%  # join biobot_IDs
  
  # for pellet data : attach weights and re-attach ^p to the beginning of WWTP abbreviations
  {if(pellets_present) left_join(., pellet_weight_data, by = 'Label_tube') %>% 
      mutate(across(WWTP, ~ if_else(sample_type == 'Pellet', str_c('p', .x), .x)))  
    else . } %>% # If no pellet samples present, pipe the input to the next step 
  
  # clean up controls etc. that don't appear in biobot_lookup
  mutate(across('WWTP', ~if_else(str_detect(., '^X')|is.na(.), assay_variable, .)), # wwtp == assay_var
         across('Facility', ~if_else(str_detect(., '^X')|is.na(.), str_c(Sample_name, '/', assay_variable), .)) ) %>% 
          # Facility = Sample_name/assay_variable
  
  rename(Target_Name = Target) # rename to match the final output desired by HHD


# Detect for spiking : look for Vaccine_ID
spiking_present <- meta.attached_quant_data$Vaccine_ID %>% is.na() %>% all() %>% {!.} # if no vaccine ID is detected then absent
# use this later to streamline code



# Calculations -------------------

# Copies_per_uL_RNA to Copies_Per_Liter_WW. Spike in concentration, % recovery are calculated
# join the results with the WWTP identifiers and names
processed_quant_data <- meta.attached_quant_data %>% 
  
  # Calculations for Copies_Per_Liter_WW from Copies_per_uL_RNA
  mutate(Copies_Per_Liter_WW = Copies_per_uL_RNA *(1e6/300) * (elution_volume/Filtered_WW_vol), # Chemagic concentration factor = 300 
         Detection_Limit = as.numeric(LimitOfDet * (1e6/300) * (elution_volume/Filtered_WW_vol) ),  # Chemagic concentration factor = 300 
         
         # Poisson confidence intervals in copies/L WW
         PoissonConfMax_Per_Liter_WW = PoissonConfMax_per_uL_RNA *(1e6/300) * (elution_volume/Filtered_WW_vol),  
         PoissonConfMin_Per_Liter_WW = PoissonConfMin_per_uL_RNA *(1e6/300) * (elution_volume/Filtered_WW_vol)) %>% 
  
  # calculations for pellet - dividing by mass
  {if(pellets_present) mutate(., Copies_Per_Gram_DW = Copies_per_uL_RNA * 50/(pellet_wet_mass * dry_mass_fraction))
    else . } %>% # If no pellet samples present, pipe the input to the next step
         
  # conditional calculations reg surrogate spiked virus
  # Calculations for Surrogate_virus_input_per.L.WW (input) and Percentage_recovery (output/input * 100)
  mutate(Surrogate_virus_input_per.L.WW = spiking_virus_vaccine_stock_conc * spike_virus_volume / (Received_WW_vol * 1e-3), 
         Percentage_recovery_BCoV = 100 * Copies_Per_Liter_WW/Surrogate_virus_input_per.L.WW) %>% 
  
  # (source for chemagic conc. factor calculations : Google sheet below
  # "concentration factor calc" : https://docs.google.com/spreadsheets/d/19oRiRcRVS23W3HqRKjhMutJKC2lFOpNK8aNUkC-No-s/edit#gid=2134801800)
  
  # Remove the columns not relevant targets that are not surrogate viruses (BCoV or BRSV) or for Vaccine stocks
  mutate_cond(!str_detect(Target_Name, 'BCoV|BRSV') |
                str_detect(Sample_name, 'Vaccine'),  # And vaccines
              Surrogate_virus_input_per.L.WW = NA, 
              Percentage_recovery_BCoV = NA) %>% 
  
  # And for vaccine (ie. Vaccine stock quants), remove Copies_Per_Liter_WW
  mutate_cond(str_detect(Sample_name, 'Vaccine'),
              Copies_Per_Liter_WW = NA) %>%  # not relevant for vaccine stock
              
    
  select(-spiking_virus_vaccine_stock_conc) %>% 
  
  # arranging data by facility name alphabetical
  arrange(Facility) %>% 
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
    else if(nrow(.) > 0 &&  .$spiking_virus_vaccine_stock_conc == 0){
      zero_vaccine_values <- . 
      view(zero_vaccine_values)
      stop("Zeros found in vaccine quants in the data dump, please fix") 
    }
  }

# Data for output ----------------------------------------------------------------------


presentable_data <- processed_quant_data %>% 
  
  rename(Ct = CT) %>% # format that HHD/Kathy needs
  
  # Adding new variables, modifying existing variables
  mutate(Date = Sample_name %>% 
           str_extract('[:digit:]{6}') %>% # Extract the date component of the sample name : 6 digits 'mmddyy'
           str_replace('([:digit:]{2})([:digit:]{2})([:digit:]{2})', '\\1/\\2/\\3') ,  # format it as mm/dd/yy
         Lab = 'R', 
         Comments = NA) %>%  # add new columns asked by HHD
  
  # modifying variables..
  mutate(across(Sample_Type, ~ if_else(is.na (.x), 'unreported', .))) %>% # change blank types in registry into 'unreported'
  mutate(across('Sample_name', as.character) ) %>% # covert sample name into char
  # mutate_at('Facility', ~if_else(. == assay_variable, str_c(Sample_name, '/', assay_variable), .)) %>%
  mutate(Sample_ID = if_else(biological_replicates == ''|is.na(biological_replicates), 
                             str_c(Sample_name, '.', WWTP) , 
                             str_c(Sample_name, '.', WWTP, '-', biological_replicates))) %>% 
  
  # Arrange rows by WWTP facility 
  arrange(Facility, biological_replicates) %>%
  
  # Selecting column order
  select(Facility, WWTP, Date, Lab, Target_Name, 
         `Received_WW_vol`, Filtered_WW_vol, 
         Copies_per_uL_RNA, 
         PoissonConfMax_per_uL_RNA, PoissonConfMin_per_uL_RNA, # 95% confidence intervals
         Copies_Per_Liter_WW, any_of('Copies_Per_Gram_DW'), 
         Ct, AcceptedDroplets, PositiveDroplets, Sample_ID, 
         Detection_Limit, Positivity, 
         Sample_Type, Surrogate_virus_input_per.L.WW, `Percentage_recovery_BCoV`, 
         Comments, any_of('variant_status'), 'Well Position',
         PoissonConfMax_Per_Liter_WW, PoissonConfMin_Per_Liter_WW) %>%
  
  mutate_at('Target_Name', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2'))) %>% 
  mutate_at('Target_Name', ~str_remove(., '/Baylor')) %>% 

  
  # B117 special plug
  {if(str_detect(read_these_sheets, 'B117') %>% any) {
    calculate_B117_percentage_variant(.) %>% # calculating the percentage of variant vs total S copies 
    select(-any_of('variant_status')) %>% 
    relocate(percentage_variant, .after = 'Target_Name') }
    else .
  } %>% 
  
  mutate(across(where(is.numeric), ~ round(., 2))) # rounding off all numerical things


# Missing value check ---- 


# Brings user attention to missing values in the sample registry

# Missing manhole samples in Biobot ID sheet
missing_entries_in_Biobot_registry <- presentable_data %>%  # identify samples in the set that are 
  filter(!str_detect(WWTP, paste(WWTP_symbols_regex,  # neither WWTPS
                                 manhole_symbols_regex,  # nor manholes
                                 sep = '|')) &
           !(str_detect(WWTP, samples_to_remove)| WWTP == 'NTC') ) # nor controls : DI, NTC, Blanks

if(missing_entries_in_Biobot_registry %>% plyr::empty() %>% !.)
{View(missing_entries_in_Biobot_registry)
proceed_with_errors_key <- menu(c('Yes', 'No'), title = 'Missing entries identified in the Biobot ID sheet (most likely manhole),
check the data output in the console and choose if you wish to continue processing data')

if(proceed_with_errors_key == 2) stop("Cancel selected, script aborted.")
if(proceed_with_errors_key == 1) 
{ print('Missing Biobot ID entries will be named as Date/WWTP in the Facility Name')
}
}

# Volume filtered columns empty => read as NA
missing_values_sample_registry <- presentable_data %>% 
  filter(is.na(Filtered_WW_vol) &  # Filtered_WW_vol is NA (when left blank in sample registry)
           !(str_detect(WWTP, samples_to_remove) | WWTP == 'NTC') ) # Exclude the Blanks, controls etc.

if(missing_values_sample_registry %>% plyr::empty() %>% !.) 
  {View(missing_values_sample_registry)
  proceed_with_errors_key <- menu(c('Yes', 'No'), 
  title = 'Missing values identified in the sample registry : WW volume extracted (ml) / Filtered_ww_vol in this table,
check the data output in the console and choose if you wish to continue processing data, by assuming 50 ml default')

  if(proceed_with_errors_key == 2) stop("Cancel selected, script aborted.")
  if(proceed_with_errors_key == 1) 
    { print('Missing volumes filtered are being converted to 50 ml (default) to avoid error in processing data')
    presentable_data %<>% replace_na(list(Filtered_WW_vol = 50))
  }
}

# Old : checking for -Inf in Received_WW_vol (only relevant when doing BCoV -- can put an IF condition)
# if(map(presentable_data, ~ -Inf %in% .x) %>% any())
# missing_values_sample_registry <- filter_if(is.numeric, any_vars( . < 0))



# HHD data output ----


# switch for output to sheet sent to HHD
if(HHD_data_output)
{
  
  # presentable data for health department
  present_HHD_data <- presentable_data %>%
    
    # for B117, don't do any of the below changes to the presentable_data
    {if(str_detect(read_these_sheets, 'B117') %>% any) 
      {.} else {
      
      # Otherwise proceed below  
        # rename to format that HHD/Kathy needs
      rename(., 'Copies_per_uL' = Copies_per_uL_RNA, # remove 'RNA' from the names
             'PoissonConfMax_per_uL' = PoissonConfMax_per_uL_RNA,
             'PoissonConfMin_per_uL' = PoissonConfMin_per_uL_RNA,
             
             'Recovery_Rate' = `Percentage_recovery_BCoV`) %>%
          
          # amended 10/26/21 - removed CIs from HHD data
          select(-contains('Vol'), -Surrogate_virus_input_per.L.WW, -'Well Position', 
                 -PoissonConfMax_Per_Liter_WW, -PoissonConfMin_Per_Liter_WW, -PoissonConfMax_per_uL, -PoissonConfMin_per_uL)  
      }
    }
  
  present_only_WW <- present_HHD_data %>% 
    filter(WWTP %in% WWTP_symbols) # retain only WWTP data 
  
  # Write data if not empty
  if(present_only_WW %>% plyr::empty() %>% !.){
    check_ok_and_write(present_only_WW, sheeturls$HHD_data, title_name) # save results to a google sheet, ask for overwrite
    write_csv(present_only_WW, file = str_c('excel files/Weekly data to HHD/', title_name, '.csv'), na = '') # output csv file
  }
  
  # Inclusive reporting : # This ensures that no sample is missed from the reporting just because it does not exist in the biobot ID sheet
  present_manhole_samples <- present_HHD_data %>%  # identify the remaining samples
    filter(! WWTP %in% WWTP_symbols & 
             !str_detect(WWTP, samples_to_remove)  # and controls : DI, NTC, Blanks, WHC etc.
    )
  
  # Write data if not empty
  if(present_manhole_samples %>% plyr::empty() %>% !.){
    
    # save results to a google sheet, ask for overwrite
    check_ok_and_write(present_manhole_samples, sheeturls$HHD_data, str_c(title_name, ' -manhole')) 
    write_csv(present_manhole_samples, str_c('excel files/Weekly data to HHD/', title_name, ' -manhole.csv'), na = '') # output CSV file
  }
  
  
  # Count the outgoing samples : All controls and NTC ; WW samples ; manhole samples (- NTC)
  num_outgoing <- map2_int(list(presentable_data, 
                                present_only_WW, 
                                present_manhole_samples),
                           c(paste(samples_to_remove, 'NTC', sep = '|'), '.*', manhole_symbols_regex), # Select controls or all samples
                           
                           ~ pull(.x, Sample_ID) %>% # Make a vector - only the unique Sample_ID
                             unique %>%  # remove duplicates for different targets - N1/N2/B117..
                             str_subset(.y) %>% 
                             length()
  )
  
} else num_outgoing <- rep(NaN, 3)
  

# Run log ----

# This is an approximate method to figure out if PK did the run or Camille
script_user_name <- gargle::gargle_oauth_sitrep()$email[1] %>% # select the first email from the list of authorized emails
  str_match('(.*)@.*') %>% .[,2]  # extract the username before the @

# Get blank samples to report any contaminations
negative_controls_maxPositiveDroplets <- presentable_data %>% 
  filter(across(WWTP,
                ~ . == 'NTC',
                . == 'DI',
                str_detect(., regex('Blank', ignore_case = TRUE)) )) %>% 
  select(WWTP, PositiveDroplets) %>% 
  group_by(WWTP) %>% 
  summarise(across(PositiveDroplets, max)) %>%  # get the maximum # of positivedroplets among replicates
  mutate(across(.fns = as.character)) # make both rows character for merging
  # arrange(WWTP) # arranges in alphabetical order : Blank, DI, NTC
  
# Sample number tally

# Incoming
sample_colm_incoming <- quant_data %>% 
  unite(week_facility, c(Sample_name, `Tube ID`)) %>% # make a column containing both the date and Tube ID
  pull(week_facility) %>% 
  unique() # Remove the duplicates for the 2 targets N1.N2 (only counting samples)

num_samples_input <- length(sample_colm_incoming)
# num_controls_input <- str_count(sample_colm_incoming, samples_to_remove) %>% 
#   sum()


# Generate the run log
run_log <- matrix( 
  c('', '',   # insert 2 empty lines
  '', '',
  '## RUN LOG --------------------', '',
  'Script user :', script_user_name,  
  'Run at :', Sys.time() %>% as.character,
  'Sample tally --------------------', '',
  'Entering script --------', '',
  'Total replicates :', num_samples_input,
  'Leaving script --------', '',
  'Controls :', num_outgoing[1],
  'WWTP :', num_outgoing[2],
  'Manholes and others :', num_outgoing[3],
  'TALLY CHECK', num_samples_input == sum(num_outgoing),
  'Missing samples', num_samples_input - sum(num_outgoing),
  'Contamination check --------------------', '',
  'Control type', 'max PositiveDroplets'
  ),
  nrow = 2
) 

df_run_log <- t(run_log) %>% as_tibble(.name_repair = 'unique') %>%  # Transpose matrix and make tibble
  rbind(setNames(negative_controls_maxPositiveDroplets, names(.)) ) # attach the # positive droplets of negative controls


# Attach run log to the same sheet as the input
sheet_append(ss = sheeturls$user_inputs, data = df_run_log, title_name)


# Plotting into html -----------------------------------------------------------------------

# Designating columns to maintain constant when pivoting for scatter plots
minimal_label_columns <- c('Target_Name', 'Sample_name', 'WWTP') # for scatter plots


# calling r markdown file
rmarkdown::render('2.1-make_html_plots.rmd', 
                  output_file = str_c('./qPCR analysis/', title_name, '.html'))


# Complete data output ----

# This has been moved to the end to avoid stopping the script in the event of errors
# errors have been noticed for curl::memory limitation while running from Mac/Camille

# Output all data - including controls
write_csv(presentable_data, 
          file = str_c('excel files/Complete data/', title_name, '.csv'), 
          na = '') # output csv file of all the data

# save to google sheet, ignore if taking longer than 20 seconds (for Camille's mac problem with google sheets)
check_ok_and_write(presentable_data, sheeturls$complete_data, title_name) # save results to a google sheet, ask for overwrite
