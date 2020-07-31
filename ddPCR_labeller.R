# Read in the ddPCR data file and attach the names from plate template
# Author: Prashant Kalvapalle;  June 30 2020

# User inputs ----


flnm.here <- 'dd.WW11_720_N1/N2'  # set the filename

process_ddpcr <- function(flnm = flnm.here)
{
  
  template_volume <- 10 /22 * 20 # ul template volume per well of the ddPCR reaction
  
  # Loading pre-reqisites ----
  
  
  # Loading libraries, functions and user inputs
  source('./general_functions.R') # Source the general_functions file
  
  # Data input ----
  
  # Read the ddPCR master file
  
  # Read in qPCR data and labels from plate template
  fl <- read_sheet(sheeturls$raw_ddpcr, sheet = flnm) # read excel file exported by Quantstudio
  plate_template <- get_template_for(flnm, sheeturls$templates)  # Get the plate template matching file name, convert to 1 column 
  
  # Polishing ----
  
  
  # Load desired qPCR result sheet and columns
  bring_results <- fl %>% # select only the results used for plotting, calculations etc. and arrange them according to sample order
    select(-Sample) %>% 
    mutate_at('Well', ~ str_replace(., '(?<=[:alpha:])0(?=[:digit:])', '') ) %>% rename('Well Position' = Well) %>% 
    right_join(plate_template, by = 'Well Position') %>%  # Incorporate samples names from the google sheet by matching well position
    mutate_at('Target', ~str_replace_all(., c('N1' = 'N1_multiplex' , 'N2' = 'N2_multiplex'))) %>% 
    filter(!is.na(Target)) %>% 
    mutate('Copy #' = CopiesPer20uLWell/ template_volume) %>% 
    select(`Sample Name`, `Copy #`, Target, everything())
  
  
  # polishing qPCR data - Make Biobot ID column clean
  # isolate the primer pair and assay_variable into 3 columns : Sample name, assay variable and primer pair 
  polished_results <- bring_results %>% separate(`Sample Name`,c(NA, 'Sample Name'),'-') %>% separate(`Sample Name`,c('Sample Name','Tube ID'),'_') %>% 
    mutate(`Tube ID` = if_else(`Sample Name` == 'NTC', '0', `Tube ID`)) %>% 
    separate(`Tube ID`, c('assay_variable', 'biological_replicates'), remove = F) %>%  # Separate out biological replicates 
    unite('Tube ID', c(assay_variable, biological_replicates), sep = '.', remove = F, na.rm = T) %>% # remaking Tube ID - removes spaces after 'dot'
    unite('Biobot ID', c(`Sample Name`, assay_variable), sep = '', remove = F) %>%
    
    mutate_at('assay_variable', as.character) %>% 
    mutate_at('biological_replicates', ~str_replace_na(., ''))
  
  
  # Data output ----
  # this is usually commented out to prevent overwriting existing data; turn on only when needed for a single run
  
  write_sheet(polished_results, sheeturls$data_dump, sheet = flnm) # save results to a google sheet
  
}

# processing data
# process_ddpcr(flnm.here)