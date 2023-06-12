# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file

# Parameters ----------------------------------------------------------------------

# Enter all the parameters in the google sheet : User inputs, metadata, queries for R code
# url = https://docs.google.com/spreadsheets/d/1SAINnazqMrjTBSuhYiIBbx8B7_reHzaEuwGTdkNA6wk/edit#gid=288270795
# Duplicate the 'template', name it after the data set (same as title_name) and BRING it to the FIRST position in sheets

# Read the input parameters sheet and create all the parameters
source('./scripts_general functions/g.12-sheet-to-params.R') 

# fix missing parameters with defaults
if(!exists('pellet_weights_present')) pellet_weights_present <- 'FALSE' # make false if no user input
if(!exists('vaccine_spike_present')) vaccine_spike_present <- 'FALSE' # make false if no user input

# Preliminary ----

# Source script
source('./1-processing_functions.R') # Source the file with the ddPCR and qPCR name's attaching functions

# Set template volumes used in ddPCR
template_volume_ddpcr <- 
  
  {if(unusual_template_volumes_present) { 
    # get the template volumes for each assay target from the metadata sheet -- only if required
    read_sheet(ss = sheeturls$user_inputs, sheet = 'ddPCR template volumes', range = 'A:C', col_types = 'c-n')
    
    # load the default volume
    } else tibble(Target = 'default', template_volume_per_22ulrxn = default_template_per_well)
  } %>%
  
  # correction for 20 out of 22 ul loaded into ddPCR droplet generation 
  mutate(template_vol = template_volume_per_22ulrxn * 20/22) 
  
default_template_vol_corrected <- default_template_per_well * 20/22 # same correction as above for ddPCR loading



# Input data ----------------------------------------------------------------------

# running function to attach samples names to data. Attached data is ALSO saved to sheet : "qPCR data dump"
list_quant_data <- map(read_these_sheets, 
                           ~ process_ddpcr(.x) %>% 
                             select(1:15))


# bind multiple ddPCR runs and clean up names
quant_data <- bind_rows(list_quant_data) %>% 
  
  rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convention, this will rename it 
  # select(-any_of('Well Position')) %>%  # I wonder why Well Position needed to be removed..?
  mutate_at('assay_variable', as.character) %>%  # if assay_variable has all numbers, this will turn them into char
  mutate_at('biological_replicates', ~str_replace_na(., '')) %>%  # make empty biological_replicates into empty string ('')
  mutate_at('Tube ID', ~str_remove(., "\\.")) %>% # remove dot (.) from Tube ID
  unite('Label_tube', c('Sample_name', 'Tube ID'), sep = "", remove = F) %>%  # make a unique column for matching volumes 
  mutate(across(Label_tube, ~str_remove(., ' ') )) # remove spaces from the Sample_name (Started with 1123 Pavan with 1123 69S names)


# Load metadata ----------------------------------------------------------------------

# Get volumes data from google sheet : "Sample registry"
volumes.data_registry <- get_volumes_from_sample_registry()


# get pellet weight data -- if required
if(pellet_weights_present) pellet_weight_data <- get_pellet_metadata()


# Attach metadata ----------------------------------------------------------------------

# Join WWTP names to qPCR dataset for Rice data

meta.attached_quant_data <- quant_data %>% 
  
  # join sample registry data
  left_join(volumes.data_registry, by = 'Label_tube') %>%
  mutate_at('Biobot_id', ~if_else(is.na(.x), str_c(Sample_name, assay_variable), .x)) %>% # stand-by name for missing cols
  
  
  # Extra processing for Pellet samples -- was used for Monkeypox quants
  mutate(sample_type = if_else(str_detect(assay_variable, '^p'), 'Pellet', 'Liquid')) %>% # pellet or liquid?
  mutate(across(assay_variable, ~ str_remove(.x, '^p'))) %>% # Remove the ^p from the pellet samples (will add back to WWTP column)
  
  
  # Joining full names of the WWTPs and other samples : form Biobot_ID google sheet
  left_join(biobot_lookup) %>%  # join biobot_IDs
  
  # for pellet data : attach weights and re-attach ^p to the beginning of WWTP abbreviations
  mutate(across(WWTP, ~ if_else(sample_type == 'Pellet', str_c('p', .x), .x))) %>% 
  
  # Extra processing for pellet weights
  {if(pellet_weights_present) 
    left_join(., pellet_weight_data, by = 'Label_tube') else . } %>% # If no pellet samples present, pipe the input to the next step 
  
  # clean up controls etc. that don't appear in biobot_lookup
  mutate(across('WWTP', ~if_else(str_detect(., '^X')|is.na(.), assay_variable, .)), # wwtp == assay_var
         across('Facility', ~if_else(str_detect(., '^X')|is.na(.), str_c(Sample_name, '/', assay_variable), .)) ) %>% 
  
  rename(Target_Name = Target) # rename to match the final output desired by HHD


# clean up intermediate data sources whose job is done
rm(volumes.data_registry) 

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
  {if(pellet_weights_present) mutate(., Copies_Per_Gram_DW = Copies_per_uL_RNA * 50/(pellet_wet_mass * dry_mass_fraction))
    else . } %>% # If no pellet samples present, pipe the input to the next step
         
  
  # Calculations for Surrogate_virus_input_per.L.WW (input) and Percentage_recovery (output/input * 100)
  {if(vaccine_spike_present) calculations_for_vaccine_spikeins(.) else {
    
    mutate(., Surrogate_virus_input_per.L.WW = NaN, `Percentage_recovery_BCoV` = NaN) # add empty columns 
  }} %>% 
  
  
  # arranging data by facility name alphabetical
  arrange(Facility) %>% 
  mutate_at('WWTP', as_factor)

# Adding a dummy CT column (if only ddPCR data is being loaded; which lacks the CT column) - for compatibility with qPCR code
if(processed_quant_data %>%  {!'CT' %in% colnames(.)}) processed_quant_data$CT = NA


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
         Sample_Type, Surrogate_virus_input_per.L.WW, No_of_Hours_Missed, `Percentage_recovery_BCoV`, 
         Comments, any_of('variant_status'), 'Well Position',
         PoissonConfMax_Per_Liter_WW, PoissonConfMin_Per_Liter_WW) %>%
  
  mutate_at('Target_Name', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2'))) %>% 
  mutate_at('Target_Name', ~str_remove(., '/Baylor')) %>% 

  
  # B117 special plug ; # calculating the percentage of variant vs total S copies 
  {if(str_detect(read_these_sheets, 'B117') %>% any) {calculate_B117_percentage_variant(.) 
     } else .
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
             'PoissonConfMax_per_uL' = PoissonConfMax_per_uL_RNA, # PoissonConf no longer sent to HHD
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
  
  # Inclusive reporting : 
  # This ensures that no sample is missed from the reporting just because it does not exist in the biobot ID sheet
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
  filter(if_any(WWTP,
                ~ (. == 'NTC'|
                . == 'DI'|
                str_detect(., regex('Blank', ignore_case = TRUE)) )
                ) ) %>% 
  select(WWTP, PositiveDroplets) %>% 
  group_by(WWTP) %>% 
  summarise(across(PositiveDroplets, max)) %>%  # get the maximum # of positivedroplets among replicates
  mutate(across(everything(), as.character)) # make both rows character for merging
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
