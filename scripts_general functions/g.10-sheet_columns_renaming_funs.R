# Backwards compatibility function
# Useful for Metaanalysis - where multiple sheets will be read which might have different column names (due to changes in code)
# meta-analysis = > reading sheets across runs, across weeks etc.
# example : Looking at NTCs across weeks / Looking for trends in specific or all sites across weeks

# Raw ddPCR read ----

raw_ddpcr_renamer <- function(.df)
{
  .df %>% 
    
    rename(PositiveDroplets = Positives) %>%  # rename to be more meaningful
    
    # rename the column names - if exported from Quantasoft analysis Pro
    rename(CopiesPer20uLWell = matches('Copies/20.*LWell'),
           Concentration = matches('Conc\\(copies/.*L)'),
           AcceptedDroplets = any_of('Accepted Droplets'),
           Threshold = any_of('Threshold1'),
           MeanAmplitudeofPositives = any_of('MeanAmplitudeOfPositives'),  # 'Of' to 'of'
           MeanAmplitudeofNegatives = any_of('MeanAmplitudeOfNegatives')) %>%  # rename the column name - if exported from Quantasoft analysis Pro
    
    
    mutate(across(any_of('Concentration'), as.numeric)) %>%  # Remove the NO CALLS and make it numeric column  
    
    mutate(across(matches('Total|Poisson|Mean|Ch|Ratio|Abundance|Linkage|CNV|Copies|Det'), as.numeric)) %>%  # convert ambiguous columns into numeric
    mutate(across(where(is.list), as.character)) # convert any stray lists into character
  
  
}


# read from data dump ----

# -> This is going to be obsolete soon, when data dump is not going to be written anymore

data_dump_renamer <- function(.df)
{
   # Input .df is an individual sheet that is scraped from the google sheet data dump
  
  .df %>% 
    
    rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convention, this will rename it
    
    # Changing problematic columns
    rename(Concentration = any_of('Conc(copies/µL)'),
           PositiveDroplets = any_of('Positives'),
           AcceptedDroplets = any_of('Accepted Droplets'),
           Threshold = any_of('Threshold1'),
           MeanAmplitudeofPositives = any_of('MeanAmplitudeOfPositives'),
           MeanAmplitudeofNegatives = any_of('MeanAmplitudeOfNegatives')) %>%  # rename the column name - if exported from Quantasoft analysis Pro
    mutate(across(any_of('Concentration'), as.numeric)) %>%  # Remove the NO CALLS and make it numeric column  
    mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
    mutate(across(matches('Total|Poisson|Mean|Ch|Ratio|Abundance|Linkage|CNV|Copies|Det'), as.numeric)) %>%  # convert ambiguous columns into numeric
    mutate(across(where(is.list), as.character)) %>% # convert any stray lists into character
    
    
    # other columns with extra changes made from old times 
    mutate(across(Target_Name, ~ str_replace_all(., c('BCoV.*' = 'BCoV', '/Baylor' = '')) ) ) %>% # accounts for BCoV.2 and Baylor runs
    mutate('Run_ID' = str_extract(.x, '(?<=dd\\.WW)[:digit:]+'), .before = 1) # Extract the number of the ddPCR run
  
}

# Usage
# get all ddPCR sheets
# all_dd.data_list <- map(dd_selected_sheet_names, 
#                         ~ read_xlsx(flpath.dat, sheet = .x) %>%
#                           data_dump_renamer)



# Read from complete data ----

compl_data_renamer <- function(.df)
{
  # Input .df is an individual sheet that is scraped from the google sheet qPCR_complete_data
  
  .df %>% 
    
    # mutate('Week' = str_match(.x, '^[:digit:]*')) %>% # add a column for the week name (untested)
    
    # backward compatibility (accounting for column name changes)
    rename('Target_Name' = matches('Target'), 
           'Received_WW_vol' = matches('Received_WW_vol|Original Sample Volume'), # if both old and new names are present, it will throw error
           'Percentage_recovery_BCoV' = matches('Recovery fraction'),
           'Ct' = matches('^CT', ignore.case = T)) %>% 
    
    mutate_at('Target_Name', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2'))) %>% 
    
    # change variable types
    mutate(across(where(is.list), as.character)) %>% 
    mutate(across(Date, as.character))
}

# Usage
# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet
# list_rawqpcr <- map(read_these_sheets, ~ read_sheet(sheeturls$complete_data , sheet = .x) %>% 
#                       compl_data_renamer) 


# Rename Lift station abbreviations ----

# Some Lift stations have different abbreviations before they were standardized;
# This function will change them all to as they are in the Biobot Sample IDs sheet/All manhole subsheet
# https://docs.google.com/spreadsheets/d/1ghb_GjTS4yMFbzb65NskAlm-2Gb5M4SNYi4FHE4YVyI/edit#gid=122083893

rename_old_LS_abbrs <- function(.vector)
{
  str_replace_all(.vector,
                  
                  # List the abbreviations to be changed as show below 
                  # c('old_abbreviations' = 'new_abbreviations', ... ) (old abbrv is a REGEX)
                  c('ALIEC' = 'ALIEF',
                    'SPOL' = 'SPO1',
                    'WESTH|Westhollow' = 'WHW',
                    'SCOTT' = 'SCOTT1',
                    'Worem' = 'WEOR',
                    'BISSA' = 'BISS4',
                    'RICH1|Richmond' = 'RICH',
                    'WCID$' = 'WCID78',
                    'Woodw' = 'WWAY',
                    'nELDR' = 'NELD',
                    'StellaLink' = 'STEL',
                    'Westpark' = 'WPAR',
                    'CLN' = 'CLN2')
                  )
  
}
