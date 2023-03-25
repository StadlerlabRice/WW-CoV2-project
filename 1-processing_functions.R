# Don't need to run this script standalone if you are running 2-calculations_multiple_runs : This script is called from there
#---------------------------------------------------------------------

# Read in the qPCR and ddPCR raw data, attach to sample names and process to copies/ul RNA (ddPCR) or
# Cq to copy number (qPCR) 

# Author: Prashant Kalvapalle;  June 16 2020.
# Merged with other /R files Aug 4, 2020


# Run-command ----
# map(read_these_sheets, process_all_pcr)

# How to use this script - Copy and run the command below with the file names in variable: read_these_sheets (same as calculations_multiple_runs.R)
  # Exceptions: When samples from Baylor are present on any plate, you need to process each plate individually 
  # with a regular expression (REGEX) indicating wells where Baylor data is present
  # For baylor wells, choose : none, '.*' for all, '[A-H][1-9]$|10' for columns 1 - 10; '.*(?!3).$' for everything except 3rd column etc.


# Loading pre-requisites ----

# Loading libraries, functions and user inputs; 
# use only if running this script independently

# source('./inputs_for_analysis.R') # Source the file with user inputs
# source('./0-general_functions_main.R') # Source the general_functions file



# ddPCR processing ----


# ddPCR processing: Attach sample labels from template table, calculate copies/ul using template volume/reaction, make names similar to qPCR data 
process_ddpcr <- function(flnm = flnm.here)
{ 
  # Data input ----
  
  # Read in ddPCR data and labels from plate template
  fl <- read_sheet(sheeturls$raw_ddpcr, sheet = flnm) # read excel file exported by Quantstudio
  plate_template <- get_template_for(flnm, sheeturls$templates)  # Get the plate template matching file name, convert to 1 column 
  # TODO : saves time, but not sheet reading? : streamline this call to read only the top 10 plates and search the rest only if the key is not found
  
  # B117 extra : grabbing the extra file named -variant for the B117 assay 
  if(str_detect(flnm, regex('B117', ignore_case = TRUE))) fl <- B117_read_in_files
  
  # Polishing ----
  
  
  # Load desired ddPCR result sheet, join the sample names from template
  bring_results <- fl %>% 
    select(-any_of('Sample')) %>% # Remove column called "Sample" if exists, it will be loaded from plate template sheet
    
    raw_ddpcr_renamer %>% # rename columns and remove NO CALLs to account for Quantasoft analysis pro export  
  
    mutate(across('Well', ~ str_replace(., '(?<=[:alpha:])0(?=[:digit:])', '') )) %>% rename('Well Position' = Well) %>% # remove leading 0 from A01..
    right_join(plate_template, by = 'Well Position') %>%  # Incorporate samples names from the google sheet by matching well position
    
    # mutate_at('Target', ~str_replace_all(., c('N1' = 'N1_multiplex' , 'N2' = 'N2_multiplex'))) %>%  # old -- for compatibility with qPCR
    # filter(!is.na(Target)) %>% # I don't recall why this is here
    
    # Adding different template volumes for each target for division // or default (typically 10 ul/22 ul rxn)
    left_join(., template_volume_ddpcr, by = 'Target') %>% # join array of template volume - different for N1,N2 and BCOV2
    replace_na(list(template_vol = default_template_vol_corrected)) %>% # use default volume for unmatched ones
    
    # Make a CopiesPer20uLWell if doesn't exist : for QX600 exported from Quantasoft Analysis Pro
    {if(!'CopiesPer20uLWell' %in% colnames(.)) mutate(., CopiesPer20uLWell = Concentration * 20) else .} %>%
    
    # copies per ul : calculations
    mutate('Copies_per_uL_RNA' = CopiesPer20uLWell/ template_vol, # template_vol is the ul of RNA per 20 ul well
           'PoissonConfMax_per_uL_RNA' = col_or_NAs('PoissonConfMax', .) * 20 / template_vol,   # converting Poisson confidence intervals into copies/ul RNA units
           'PoissonConfMin_per_uL_RNA' = col_or_NAs('PoissonConfMin', .) * 20 / template_vol) %>%
    
    select(`Sample_name`, Copies_per_uL_RNA, Target, everything()) # order - put important columns first
  
  
  
  # polishing qPCR data - Make Biobot ID column clean
  
  # isolate the primer pair and assay_variable into 3 columns : Sample name, assay variable and primer pair 
  polished_results <- bring_results %>% separate(`Sample_name`,c(NA, 'Sample_name'),'-') %>% separate(`Sample_name`,c('Sample_name','Tube ID'),'_') %>% 
    mutate(`Tube ID` = if_else(`Sample_name` == 'NTC', '0', `Tube ID`)) %>% 
    separate(`Tube ID`, c('assay_variable', 'biological_replicates'), sep = '\\.', remove = F) %>%  # Separate out biological replicates 
    unite('Tube ID', c(assay_variable, biological_replicates), sep = '.', remove = F, na.rm = T) %>% # remaking Tube ID - removes spaces after 'dot'
    # unite('Biobot ID', c(`Sample_name`, assay_variable), sep = '', remove = F) %>%
    
    mutate_at('assay_variable', as.character) %>% 
    mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
   
    # TODO : cleanup -- remove redundant column name renaming/manipulations : Use the final names throughout 
    
    # Account for dilutions to detect BCoV samples (50x) and BCoV vaccine sample (50 x 50)
    {if(vaccine_spike_present)  {
      environment(account_for_BCoV_dilutions) <- environment() # set environment to current
      account_for_BCoV_dilutions(.)
    } else .
      
      } 
    
    
  # Append LOD information ----
  
  final_data_with_LODs <- complete_LOD_table(polished_results)%>% 
    select(1:6, AcceptedDroplets, PositiveDroplets, # Bring important columns to beginning 
           Positivity, LimitOfDet, any_of('Threshold'), 
           any_of('variant_status'), 
           PoissonConfMax_per_uL_RNA, PoissonConfMin_per_uL_RNA, 
           everything()) 
  
  # Data output ----
  # removing unnecessary data outputs to save time
  
  # check_ok_and_write(final_data_with_LODs, sheeturls$data_dump, flnm) # save results to a google sheet
      # If you don't want to write to google sheets, use the command below and comment the one above
  # write_csv(final_data_with_LODs, str_c('excel files/Archive/Data dump files/', flnm, '.csv'), na = '')
  
  # Vaccine processing ----
  
  # Extract and save vaccine data into Vaccines sheet in data dump ; along with mean and sd to `Vaccine_summary`
  if(vaccine_spike_present) 
  {
    environment(process_vaccine_data_to_googlesheet) <- environment() # set environment to current
    process_vaccine_data_to_googlesheet() # run additional script 
  }
  
  
  
  # Return the data frame
  return(final_data_with_LODs)
}




# Calls ----

# function calls (copy and run from console if you need individual processing)
# process_qpcr(flnm.here)

# check the filename and call the appropriate ddPCR, standard curve or qpcr processing functions
process_all_pcr <- function(flname, baylor_wells = 'none')
{ # use only when qPCR data is encountered 
  # and there are no special features on plate : such as overriding standard curves

  # source qPCR related functions
  str_c('./scripts_general functions/', 
        c("g.4-qPCR_specific_funs.R", 'g.14-qPCR_std_curve_processing_fns.R') ) %>% 
    source()
  
  # if it is a ddPCR file (dd.WWxx), call the ddPCR processor
  if(str_detect(flname, 'dd.WW.*')) process_ddpcr(flname, baylor_wells = baylor_wells)
  
  # if it is a standard curve holding file (Stdx), call standard curve processor
  if(str_detect(flname, 'Std[:digit:]*')) process_standard_curve(flname)
  
  # if it is a qPCR file (WWxx), call the qpcr processor
  if(str_detect(flname, '(?<!dd\\.)WW[:digit:]*')) process_qpcr(flname, baylor_wells = baylor_wells)
}