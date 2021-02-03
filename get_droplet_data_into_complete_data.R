# getting droplets and other threshold information from the raw data into the complete data

source('./general_functions.R') # Source the general_functions file

# parameters ----

read_these_sheets <- c( 'dd.WW60_Conc Method 3-1 (Baylor)_N1N2',
                        'dd.WW61_Conc Method 3-2 Baylor_BCoV',
                        'dd.WW62_Conc Method 3-3 Baylor_N1N2_BCoV',
                        'WW85_Conc Method 3-1 Baylor_pMMoV_Std64',
                        'WW86_Conc Method 3-2 Baylor_pMMoV_Std65')

input_sheet <- 'Concentration methods paper-3'

# regular data ----

all_data_input <- read_sheet(sheeturls$complete_data, sheet = input_sheet) %>% 
  rename(`Concentration method` = Concentration_method, 
         Target = `Target Name`,
         Fraction.recovered = `Recovery fraction`,
         Detection.limit = Detection_Limit,
         `Copies/L WW` = 'Copies/l WW', `Copies/uL RNA` = 'Copies/ul RNA') %>% # for compatibility with old plotting functions
  filter(!str_detect(Facility, 'Std|Vac')) %>% 
  mutate(across(WWTP, ~ str_replace(., '0', 'NTC')))
  

# additional data ----

addl_raw_quant_data <- map_dfr(read_these_sheets, 
                               ~ read_sheet(sheeturls$data_dump, sheet = .x) %>%
                                 rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it
                                 
                                 filter(!str_detect(Sample_name, 'Std|Vac')) %>%  # remove standards and vaccine 
                                 
                                 # Changing problematic columns
                                 rename(Concentration = any_of('Conc(copies/µL)'),
                                        AcceptedDroplets = any_of('Accepted Droplets'),
                                        Threshold = any_of('Threshold1'),
                                        MeanAmplitudeofPositives = any_of('MeanAmplitudeOfPositives'),
                                        MeanAmplitudeofNegatives = any_of('MeanAmplitudeOfNegatives')) %>%  # rename the column name - if exported from Quantasoft analysis Pro
                                 mutate(across(any_of('Concentration'), as.numeric)) %>%  # Remove the NO CALLS and make it numeric column  
                                 mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
                                 mutate(across(matches('Total|Poisson|Mean|Ch|Ratio|Abundance|Linkage|CNV|Copies|Det'), as.numeric)) %>%  # convert ambiguous columns into numeric
                                 mutate(across(where(is.list), as.character)) %>% # convert any stray lists into character
                                 
                                 mutate(across(Target, ~ str_replace_all(., c('BCoV.*' = 'BCoV', '/Baylor' = '')) ) ) %>% 
                                 mutate('Run_ID' = str_extract(.x, '(?<=dd\\.WW)[:digit:]+'), .before = 1) # Extract the number of the ddPCR run
) 

addl_selected <-  addl_raw_quant_data %>% 
  select(1:7, 'AcceptedDroplets',    'Positives', 
         'Threshold', 'MeanAmplitudeofPositives',    'MeanAmplitudeofNegatives', 'MeanAmplitudeTotal',
         'PoissonConfMax',    'PoissonConfMin') %>%  # select the columns to add to the complete data 
  mutate(WWTP_ID = if_else(biological_replicates == ''|is.na(biological_replicates), str_c(Sample_name, '.', assay_variable) , 
                           str_c(Sample_name, '.', assay_variable, '-', biological_replicates)),
         across('Target', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2', 'BCoV$' = 'BCoV2'))),
         .before = 1) %>%  # add a unique column for each row to match to the complete data (for merge)
  select(-(2:6))

mrg_dat <- left_join(all_data_input, addl_selected)

# merge is not good, NTCs have non unique names, things getting jumbled up - also due to the dilution factor column
# write_sheet(mrg_dat, ss = sheeturls$complete_data, sheet = 'extra: Conc methods paper-3')

