# Don't need to run this if you are running 2-calculations_multiple_runs : This script is called from there
#---------------------------------------------------------------------

# Read in the qPCR and ddPCR raw data, attach to sample names and process Cq to copy number (qPCR) 
# Author: Prashant Kalvapalle;  June 16 2020.
# Merged with other /R files Aug 4, 2020


# Run-command ----
# map(read_these_sheets, process_all_pcr)

# How to use this script - Copy and run the command below with the file names in variable: read_these_sheets (same as calculations_multiple_runs.R)
  # Exceptions: When samples from Baylor are present on any plate, you need to process each plate individually 
  # with a regular expression (REGEX) indicating wells where Baylor data is present
  # For baylor wells, choose : none, '.*' for all, '[A-H][1-9]$|10' for columns 1 - 10; '.*(?!3).$' for everything except 3rd column etc.


# Loading pre-requisites ----

# Loading libraries, functions and user inputs

# source('./inputs_for_analysis.R') # Source the file with user inputs
source('./0-general_functions_main.R') # Source the general_functions file



# ddPCR processing ----


# ddPCR processing: Attach sample labels from template table, calculate copies/ul using template volume/reaction, make names similar to qPCR data 
process_ddpcr <- function(flnm = flnm.here, baylor_wells = 'none', adhoc_dilution_wells = 'none')
{ # Baylor wells : choose 1) none, 2) '.*' for all, 3) '[A-H]([1-9]$|10)' etc. for specific wells 
  
  template_volume_ddpcr <- tibble(Target = c('N1_multiplex', 'N2_multiplex', 'BCoV', 'pMMoV', 'N501Y', 'Del69-70'),
                                  template_vol = c(10, 10, 4, 4, 9, 9) /22 * 20) # ul template volume per well of the 20 ul ddPCR reaction for each target
  
  RNA_dilution_factor_BCoV <- 50  # RNA dilution factor for diluted BCoV samples
  Vaccine_additional_RNA_dilution_factor_BCoV <- 50  # In addition to the above: RNA dilution factor for BCoV vaccine samples - both extracted and boiled
  
  # Ad hoc - marking the samples from baylor (will append /baylor to target name)
  # Work in progress or remove feature?
  
  
  
  # Data input ----
  
  # Read in ddPCR/qPCR data and labels from plate template
  fl <- read_sheet(sheeturls$raw_ddpcr, sheet = flnm) # read excel file exported by Quantstudio
  plate_template <- get_template_for(flnm, sheeturls$templates)  # Get the plate template matching file name, convert to 1 column 
  
  # B117 extra : grabbing the extra file named -variant for the B117 assay 
  if(str_detect(flnm, regex('B117', ignore_case = TRUE))){
    rps <- str_c(flnm, c('', '-variant') ) # Reading in two sheets, the second one has -variant attached
    
    fl <- map_dfr(rps,  # read the two files successively and attach them by row (df*r*)
                  ~ read_sheet(sheeturls$raw_ddpcr, sheet = .x) %>% 
                    mutate(variant_status = if_else(str_detect(.x, '-variant'),
                                                    'Variant',
                                                    'all'), .before = 1))}
  
  # Polishing ----
  
  
  # Load desired qPCR result sheet and columns
  bring_results <- fl %>% 
    select(-Sample) %>% # Remove sample, it will be loaded from plate template sheet
    
    raw_ddpcr_renamer %>% # rename columns and remove NO CALLs to account for Quantasoft analysis pro export  
  
    mutate_at('Well', ~ str_replace(., '(?<=[:alpha:])0(?=[:digit:])', '') ) %>% rename('Well Position' = Well) %>% 
    right_join(plate_template, by = 'Well Position') %>%  # Incorporate samples names from the google sheet by matching well position
    
    # mutate_at('Target', ~str_replace_all(., c('N1' = 'N1_multiplex' , 'N2' = 'N2_multiplex'))) %>%  # old -- for compatibility with qPCR
    # filter(!is.na(Target)) %>% # I don't recall why this is here
    
    # Adding different template volumes for each target for division
    left_join(template_volume_ddpcr) %>% # join array of template volume - different for N1,N2 and BCOV2
    mutate('Copies_per_uL_RNA' = CopiesPer20uLWell/ template_vol) %>%  
    
    select(`Sample_name`, Copies_per_uL_RNA, Target, everything())
  
  
  # polishing qPCR data - Make Biobot ID column clean
  
  # isolate the primer pair and assay_variable into 3 columns : Sample name, assay variable and primer pair 
  polished_results <- bring_results %>% separate(`Sample_name`,c(NA, 'Sample_name'),'-') %>% separate(`Sample_name`,c('Sample_name','Tube ID'),'_') %>% 
    mutate(`Tube ID` = if_else(`Sample_name` == 'NTC', '0', `Tube ID`)) %>% 
    separate(`Tube ID`, c('assay_variable', 'biological_replicates'), sep = '\\.', remove = F) %>%  # Separate out biological replicates 
    unite('Tube ID', c(assay_variable, biological_replicates), sep = '.', remove = F, na.rm = T) %>% # remaking Tube ID - removes spaces after 'dot'
    # unite('Biobot ID', c(`Sample_name`, assay_variable), sep = '', remove = F) %>%
    
    mutate_at('assay_variable', as.character) %>% 
    mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
    
    mutate(raw_copy_number_per_ul_rna = `Copy #`) %>%  # taking a backup of the copy number column before doing calculations for dilution factors
    mutate(across(`Copy #`, 
                  ~ if_else(str_detect(Target, 'BCoV') & !str_detect(Sample_name, 'NTC'), 
                            .x * RNA_dilution_factor_BCoV, 
                            .x))
    ) %>% # Correcting for template dilution in case of BCoV ddPCRs (excluding NTC wells)
    mutate_cond(str_detect(Sample_name, 'Vaccine') & str_detect(Target, 'BCoV'), 
                across(`Copy #`, ~ .x * Vaccine_additional_RNA_dilution_factor_BCoV)) %>%  # Correcting for BCoV Vaccine with a higher dilution
    
    # Ad-hoc corrections for errors in making plate - sample dilutions etc.
    mutate_cond(str_detect(`Well Position`, adhoc_dilution_wells), # Regex of wells to manipulate
                across(`Copy #`, ~ . / 50) # dilution corrections or other changes
    ) %>% 
    
    # Adding tag to target for baylor smaples
    { if(!str_detect(baylor_wells, 'none|None')) { 
      mutate_at(., 'Target', as.character) %>% 
        mutate_cond(str_detect(`Well Position`, baylor_wells), Target = str_c(Target, '/Baylor'))
    } else .
    }
  
  # Append LOD information ----
  
  final_data_with_LODs <- complete_LOD_table(polished_results)%>% 
    select(1:6, AcceptedDroplets, Positives, Positivity, LimitOfDet, Threshold, any_of('variant_status'), everything()) # Bring important columns to begining 
  
  # Data output ----
  
  check_ok_and_write(final_data_with_LODs, sheeturls$data_dump, flnm) # save results to a google sheet
  
  # Vaccine processing ----
  
  
  # Saving vaccine data into Vaccines sheet in data dump: For easy book keeping
  vaccine_data <- final_data_with_LODs %>% filter(str_detect(Sample_name, 'Vaccine|Vaccineb|Vacboil') & !str_detect(Target, 'N._multiplex')) %>%
    mutate('Prepared on' = '',
           Week = str_extract(flnm, '[:digit:]{3,4}') %>% unlist() %>% str_c(collapse = ', '),
           Vaccine_ID = assay_variable, 
           .before = 1) %>% 
    mutate(Run_ID = str_extract(flnm, 'dd.WW[:digit:]*'), CT = NA) %>% 
    select(`Prepared on`,	Week,	Vaccine_ID,	`Well Position`,	CT,	Target,	Sample_name,	assay_variable,	`Tube ID`,	biological_replicates,	`Copy #`,	Run_ID)
  
  # Add to existing sheet
  if(vaccine_data %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data, 'Vaccines')
  
  # Mean of vaccine data
  vaccine_data.mean <- vaccine_data %>% ungroup() %>% 
    select(1:3, Target, `Copy #`, Run_ID) %>% group_by(across(-`Copy #`)) %>% 
    summarise(across(`Copy #`, list(Mean_qPCR = mean, SD_qPCR = sd), na.rm = T), .groups = 'keep') %>% 
    mutate('[Stock conc.] copies/ul' = `Copy #_Mean_qPCR` * if_else(str_detect(Vaccine_ID, 'S[:digit:]+'), 50/20, 1), # adding a RNA extraction conc. factor only if not boiled (Sbxx naming)
           'Estimated factor' = '',
           Comments = '',
           'Conc normalized to estimated factor' = '') %>% 
    relocate(Run_ID, .after = last_col()) %>% 
    mutate('x' = '', .before = 1)
  
  # Add to existing sheet in Vaccine_summary
  if(vaccine_data.mean %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data.mean, 'Vaccine_summary')
  
  
  # Return the data frame
  return(final_data_with_LODs)
}




# Calls ----

# function calls (copy and run from console if you need individual processing)
# process_qpcr(flnm.here)
# process_standard_curve(flnm.here)
# process_ddpcr(flnm.here)

# Grand function call

# if dd in file name, get droplet file(- from google drive)
# googledrive::drive_get(path =  "~/Stadler research group/COV2 Wastewater Surveillance/Results and raw data/ddPCR/Weekly Results in Individual Files/", file = ..)
# write to Raw.ddpcr (just in case?>?)
# 
# if not then invoke process_qpcr
#  (chekc for standard curve and process it)


# check the filename and call the appropriate ddPCR, standard curve or qpcr processing functions
process_all_pcr <- function(flname, baylor_wells = 'none')
{ # use only when there are no special features on plate : Like Baylor wells, or overriding standard curves

  # if it is a ddPCR file (dd.WWxx), call the ddPCR processor
  if(str_detect(flname, 'dd.WW.*')) process_ddpcr(flname, baylor_wells = baylor_wells)
  
  # if it is a standard curve holding file (Stdx), call standard curve processor
  if(str_detect(flname, 'Std[:digit:]*')) process_standard_curve(flname)
  
  # if it is a qPCR file (WWxx), call the qpcr processor
  if(str_detect(flname, '(?<!dd\\.)WW[:digit:]*')) process_qpcr(flname, baylor_wells = baylor_wells)
}


# Other stuff : temporarily housed here. WIll be moved to general functions
# Work in progress : need to test
read_gdrive_csv <- function(read_these_sheets)
{
  library(googledrive)
  
  # read_these_sheets <- c( 'dd.WW123_0201_Schools+WWTP_B117_Rerun', 'dd.WW138_0301_SCHOOLS_N1N2')
  
  run_ids_to_read <- read_these_sheets %>% str_extract('dd.WW[:digit:]*') %>% paste0(collapse = '|')
  csv_to_read <- read_these_sheets %>% str_c('.csv')
  
  all_csv_files <- drive_get(id = 'https://drive.google.com/drive/u/0/folders/1aIek7-aqHe2l7EnUfZZUtox44bj3SZVQ') %>% 
    drive_ls()
  
  csv_subset <- all_csv_files %>% 
    filter(str_detect(name, run_ids_to_read) &  # detect all the run ids
             str_detect(name, '.csv') &  # that are also csv files
             !str_detect(name, 'variant')) # that are not variants (in the case of B117)
  
  # download csvs temporarily
  map(csv_subset, drive_download)
  
  # read csvs
  lst_output <- map(csv_to_read, read_csv)
  
  # clean up
  map(csv_to_read, unlink) # deletes all the csvs downloaded from googledrive
  
  return(lst_output) # return a list of all the csv files read in
}