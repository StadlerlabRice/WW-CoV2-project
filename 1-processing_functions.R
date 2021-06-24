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

# Loading libraries, functions and user inputs; 
# use only if running this script independently

# source('./inputs_for_analysis.R') # Source the file with user inputs
# source('./0-general_functions_main.R') # Source the general_functions file



# ddPCR processing ----


# ddPCR processing: Attach sample labels from template table, calculate copies/ul using template volume/reaction, make names similar to qPCR data 
process_ddpcr <- function(flnm = flnm.here, baylor_wells = 'none', adhoc_dilution_wells = 'none')
{ # Baylor wells : choose 1) none, 2) '.*' for all, 3) '[A-H]([1-9]$|10)' etc. for specific wells 
  
  # get the template volumes for each assay from the metadata sheet
  template_volume_ddpcr <- read_sheet(ss = sheeturls$user_inputs, sheet = 'ddPCR template volumes', range = 'A:B')
  
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
  
  # collect variables for combined operations (copies and Poisson confidence intervals)
  variables_per_uL_RNA <-  c('Copies_per_uL_RNA', 'PoissonConfMax_per_uL_RNA', 'PoissonConfMin_per_uL_RNA')
  
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
    mutate('Copies_per_uL_RNA' = CopiesPer20uLWell/ template_vol, # template_vol is the ul of RNA per 20 ul well
           'PoissonConfMax_per_uL_RNA' = PoissonConfMax * 20 / template_vol,   # converting Poisson confidence intervals into copies/ul RNA units
           'PoissonConfMin_per_uL_RNA' = PoissonConfMin * 20 / template_vol) %>%
    
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
    
    mutate(backup_copies_per.ul.rna_if.undiluted = Copies_per_uL_RNA) %>%  # taking a backup of the copy number column before doing calculations for dilution factors
    mutate(across(any_of(variables_per_uL_RNA), 
                  ~ if_else(str_detect(Target, 'BCoV') & !str_detect(Sample_name, 'NTC'), 
                            .x * RNA_dilution_factor_BCoV, 
                            .x))
    ) %>% # Correcting for template dilution in case of BCoV ddPCRs (excluding NTC wells)
    mutate_cond(str_detect(Sample_name, 'Vaccine') & str_detect(Target, 'BCoV'), 
                across(any_of(variables_per_uL_RNA), ~ .x * Vaccine_additional_RNA_dilution_factor_BCoV)) %>%  # Correcting for BCoV Vaccine with a higher dilution
    
    # Ad-hoc corrections for errors in making plate - sample dilutions etc.
    mutate_cond(str_detect(`Well Position`, adhoc_dilution_wells), # Regex of wells to manipulate
                across(any_of(variables_per_uL_RNA), ~ . / 50) # dilution corrections or other changes
    ) %>% 
    
    # Adding tag to target for baylor smaples
    { if(!str_detect(baylor_wells, 'none|None')) { 
      mutate_at(., 'Target', as.character) %>% 
        mutate_cond(str_detect(`Well Position`, baylor_wells), Target = str_c(Target, '/Baylor'))
    } else .
    }
  
  # Append LOD information ----
  
  final_data_with_LODs <- complete_LOD_table(polished_results)%>% 
    select(1:6, AcceptedDroplets, PositiveDroplets, # Bring important columns to beginning 
           Positivity, LimitOfDet, Threshold, 
           any_of('variant_status'), 
           PoissonConfMax_per_uL_RNA, PoissonConfMin_per_uL_RNA, 
           everything()) 
  
  # Data output ----
  
  check_ok_and_write(final_data_with_LODs, sheeturls$data_dump, flnm) # save results to a google sheet
      # If you don't want to write to google sheets, use the command below and comment the one above
  # write_csv(final_data_with_LODs, str_c('excel files/Archive/Data dump files/', flnm, '.csv'), na = '')
  
  # Vaccine processing ----
  
  
  # Saving vaccine data into Vaccines sheet in data dump: For easy book keeping
  vaccine_data <- final_data_with_LODs %>% filter(str_detect(Sample_name, 'Vaccine|Vaccineb|Vacboil|stdVac') & !str_detect(Target, '^N.')) %>%
    mutate('Prepared on' = '',
           Week = str_extract(flnm, '[:digit:]{3,4}') %>% unlist() %>% str_c(collapse = ', '),
           Vaccine_ID = assay_variable, 
           .before = 1) %>% 
    mutate(Run_ID = str_extract(flnm, 'dd.WW[:digit:]*'), CT = NA) %>% 
    select(`Prepared on`,	Week,	Vaccine_ID,	`Well Position`,	
           CT,	Target,	Sample_name,	assay_variable,	`Tube ID`,	
           biological_replicates,	Copies_per_uL_RNA,	Run_ID)
  
  # Add to existing sheet
  if(vaccine_data %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data, 'Vaccines')
  
  # Mean of vaccine data
  vaccine_data.mean <- vaccine_data %>% ungroup() %>% 
    select(1:3, Target, Copies_per_uL_RNA, Run_ID) %>% group_by(across(-Copies_per_uL_RNA)) %>% 
    summarise(across(Copies_per_uL_RNA, list(Mean_qPCR = mean, SD_qPCR = sd), na.rm = T), .groups = 'keep') %>% 
    
    # adding a RNA extraction conc. factor only if not boiled (Sbxx naming)
    mutate('[Stock conc.] copies/ul' = `Copies_per_uL_RNA_Mean_qPCR` * if_else(str_detect(Vaccine_ID, 'S[:digit:]+'), 50/20, 1), 
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


