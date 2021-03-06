# Don't need to run this if you are running 2-calculations_multiple_runs : This script is called from there
---------------------------------------------------------------------

# Read in the qPCR and ddPCR raw data, attach to sample names and process Cq to copy number (qPCR) 
# Author: Prashant Kalvapalle;  June 16 2020.
# Merged with other /R files Aug 4, 2020


# Run-command ----
# How to use this script - Copy and run the command below with the file names in variable: read_these_sheets (same as calculations_multiple_runs.R)
  # Exceptions: When samples from Baylor are present on any plate, you need to process each plate individually with a regular expression (REGEX)
  # indicating wells where Baylor data is present
# map(read_these_sheets, process_all_pcr)
  # For baylor wells, choose : none, '.*' for all, '[A-H][1-9]$|10' for columns 1 - 10; '.*(?!3).$' for everything except 3rd column etc.


# Loading pre-reqisites ----

# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file
source('./inputs_for_analysis.R') # Source the file with user inputs


# ddPCR processing: Attach sample labels from template table, calculate copies/ul using template volume/reaction, make names similar to qPCR data 
process_ddpcr <- function(flnm = flnm.here, baylor_wells = 'none', adhoc_dilution_wells = 'none')
{ # Baylor wells : choose 1) none, 2) '.*' for all, 3) '[A-H]([1-9]$|10)' etc. for specific wells 
  
  template_volume_ddpcr <- tibble(Target = c('N1_multiplex', 'N2_multiplex', 'BCoV', 'pMMoV', 'N501Y', 'Del69-70'),
                                  template_vol = c(10, 10, 4, 4, 9, 9) /22 * 20) # ul template volume per well of the 20 ul ddPCR reaction for each target
  
  RNA_dilution_factor_BCoV <- 50  # RNA dilution factor for diluted BCoV samples
  Vaccine_additional_RNA_dilution_factor_BCoV <- 50  # In addition to the above: RNA dilution factor for BCoV vaccine samples - both extracted and boiled
  
  # Ad hoc - marking the samples from baylor (will append /baylor to target name)
  # Work in progress?
  
  
  # Loading pre-reqisites ----
  
  
  # Loading libraries, functions and user inputs
  source('./general_functions.R') # Source the general_functions file
  
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
    rename(CopiesPer20uLWell = matches('Copies/20.*µLWell')) %>% # rename the column name - if exported from Quantasoft analysis Pro
    rename(Concentration = matches('Conc(copies/.*µL)')) %>%  # rename the column name - if exported from Quantasoft analysis Pro
    mutate(across(any_of('Concentration'), as.numeric)) %>%  # Remove the NO CALLS and make it numeric column  
    
    mutate_at('Well', ~ str_replace(., '(?<=[:alpha:])0(?=[:digit:])', '') ) %>% rename('Well Position' = Well) %>% 
    right_join(plate_template, by = 'Well Position') %>%  # Incorporate samples names from the google sheet by matching well position
    mutate_at('Target', ~str_replace_all(., c('N1' = 'N1_multiplex' , 'N2' = 'N2_multiplex'))) %>% 
    filter(!is.na(Target)) %>% 
    
    # Adding different template volumes for each target for division
    left_join(template_volume_ddpcr) %>% # join array of template volume - different for N1,N2 and BCOV2
    mutate('Copy #' = CopiesPer20uLWell/ template_vol) %>%  
    
    select(`Sample_name`, `Copy #`, Target, everything()) %>% 
    
    
    # Changing rest of the problematic columns  - if exported from Quantasoft analysis Pro
    rename(AcceptedDroplets = any_of('Accepted Droplets'),
           Threshold = any_of('Threshold1'),
           MeanAmplitudeofPositives = any_of('MeanAmplitudeOfPositives'),
           MeanAmplitudeofNegatives = any_of('MeanAmplitudeOfNegatives')) %>%  # rename the column name - if exported from Quantasoft analysis Pro
    mutate(across(matches('Total|Poisson|Mean|Ch|Ratio|Abundance|Linkage|CNV|Copies|Det'), as.numeric)) %>%  # convert ambiguous columns into numeric
    mutate(across(where(is.list), as.character)) # convert any stray lists into character
  
  
  
  
  # polishing qPCR data - Make Biobot ID column clean
  
  # isolate the primer pair and assay_variable into 3 columns : Sample name, assay variable and primer pair 
  polished_results <- bring_results %>% separate(`Sample_name`,c(NA, 'Sample_name'),'-') %>% separate(`Sample_name`,c('Sample_name','Tube ID'),'_') %>% 
    mutate(`Tube ID` = if_else(`Sample_name` == 'NTC', '0', `Tube ID`)) %>% 
    separate(`Tube ID`, c('assay_variable', 'biological_replicates'), remove = F) %>%  # Separate out biological replicates 
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
  
  polished_results <- complete_LOD_table(polished_results)%>% 
    select(1:6, AcceptedDroplets, Positives, Positivity, LimitOfDet, Threshold, any_of('variant_status'), everything()) # Bring important columns to begining 
  
  # Data output ----
  
  check_ok_and_write(polished_results, sheeturls$data_dump, flnm) # save results to a google sheet
  
  # Vaccine processing ----
  
  
  # Saving vaccine data into Vaccines sheet in data dump: For easy book keeping
  vaccine_data <- polished_results %>% filter(str_detect(Sample_name, 'Vaccine|Vaccineb|Vacboil') & !str_detect(Target, 'N._multiplex')) %>%
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
  
}


# Obsolete stuff ---------------------------------------------------------------------------------
# Standard curve ----

process_standard_curve <- function(flnm)
{ # note: Quantity of copies/well must be written in the template sheet for the standards
  
  # Preliminary naming ----
  
  # file path and google sheet urls
  flpath <- str_c('excel files/',flnm,'.xls') # this completes the file path
  
  # Extract a short name for the standard curve from the file name provide. short name has Stdxx_Target_WWxx
  fl_namer <- c('Std[:alnum:]*', 'BCoV|N1|N2|BRSV|pMMoV', 'WW[:alnum:]*') %>% 
    map_chr(~str_match(flnm, .)) %>% 
    str_c(collapse = '_')
  
  if(is.na(fl_namer)) 
  {
    stop(str_c('Filename:', flnm,  ' - input to the standard curve function, is either missing the standard curve ID (ex: Std25), the WW ID (Ex: WW61) or the Target name (Ex: BCoV) \n
         Check README for proper naming convention: https://github.com/ppreshant/WW-CoV2-project/'))
  }
  
  title_name <- str_c('Standard curve: ', fl_namer ,' - Fastvirus 4x') # title name for plots
  
  
  # Data input ----
  
  fl <- readqpcr(flpath) # read file
  
  # Bring sample names from template google sheet
  plate_template <- get_template_for(flnm, sheeturls$templates)
  
  # this gives a vector to order the samples columnwise in the PCR plate or strip 
  # (by default : data is shown row-wise) => This command will enable plotting column wise order
  sample_order = columnwise_index(fl) 
  
  bring_results <- fl$Results %>% select(`Well Position`, `Sample Name`, CT, starts_with('Tm'),`Target Name`, Task) %>% rename(Target = `Target Name`) %>%  .[sample_order,] %>%  
    
    # select only the results used for plotting, calculations etc. and arrange them according to sample order
    select(-`Sample Name`) %>% right_join(plate_template, by = 'Well Position') %>%  # Incorporate samples names from the google sheet by matching well position
    separate(Sample_name, c(NA, 'Category', 'Quantity'), sep = '-|_') %>% mutate_at('Quantity', ~ replace_na(as.numeric(.), 0)) %>% 
    filter(!is.na(Target))
  
  # optional filtering to remove low concentration points in standard curve
  bring_results %<>% filter(Quantity > 1| Quantity == 0) # filtering only standard curve within the linear range
  
  # plotting ----
  
  plt <- bring_results %>% filter(str_detect(Category, 'NTC|Std')) %>%  plotstdcurve(title_name, 'log(Copy #)') # plot standard curve
  
  # # Extract the names of the targets in use
  # targets_used <- fl$Results %>% filter(Task == 'STANDARD') %>% pull(`Target Name`) %>% unique(.)  
  
  # Isolating standard curve variables (Quantity,CT) of the different targets into groups
  standard_curve_vars <- bring_results %>% filter(Task == 'STANDARD')  %>% select(Quantity, CT, Target) %>% group_by(Target) # select required columns and group
  
  # Apply linear regression and find the model fitting results (equation and slope, R2 values) for each target
  std_table <- standard_curve_vars %>% do(., equation = lm_std_curve(.), params = lm_std_curve(., trig = 'coeff'), dat = .[1,] ) # "do" applies functions to each group of the data
  std_table$params %<>% bind_rows() # Convert parameters and data into tibbles : "do" function makes htem lists
  std_table$dat %<>% bind_rows()  
  
  std_table$dat$CT <- max(standard_curve_vars$CT, na.rm = T) - 2 * seq_along(std_table$Target) + 2 # manual numbering for neat labelling with geom_text
  
  # Add labels to plot - linear regression equation
  plt.with.eqn <- plt + geom_text(data = std_table$dat, label = std_table$equation, parse = TRUE, show.legend = F, hjust = 'inward', nudge_x = 0, force = 10)
  print(plt.with.eqn)
  
  # Let the user approve the plot (in case some standards need to be excluded/ incorrect standards concentration order)
  proceed_with_standards <- menu(c('Yes', 'No'), title = paste("Check the standard curve plot:", 
                                                 fl_namer, 
                                                 "on the right side in Rstudio. 
   Do you wish to continue with saving the standard curve parameters? Select NO if you wish to change something and re-run the script", sep=" "))
  
  if (proceed_with_standards == 2){
    stop("Cancel selected, script aborted.")
  }
  
  
  # Save plot
  ggsave(str_c('qPCR analysis/Standards/', fl_namer , '.png'), width = 5, height = 4)
  
  # Data output ----
  
  
  # processing linear regression out
  efficiency_table <- tibble(Slope = std_table$params %>% 
                               pull(slope), y_intercept = std_table$params %>% 
                               pull(y_intercept) , Efficiency = 10^(-1/Slope), '% Efficiency' = (Efficiency -1)*100 , 'R-square' = std_table$params %>% 
                               pull(r_square) %>% round(3)
  ) %>% 
    mutate(Target = std_table$dat$`Target`) %>% 
    select(Target, everything()) %>% 
    mutate(ID = fl_namer, .before = 1)
  
  # Writing data
  sheet_append(sheeturls$data_dump, efficiency_table, sheet = 'Standard curves') # Add parameters to standard curves in data dump google sheet
  
}



# qPCR processing ----


# qPCR processing: Calculate copy number from Cq and attach sample labels from template table 
process_qpcr <- function(flnm = flnm.here, std_override = NULL, baylor_wells = 'none')
  # enter the file name, standard curve mentioned within filename is used unless override is provided
{ # baylor_wells = # choose : none, '.*' for all, '[A-H]([1-9]$|10)' for columns 1 - 10; '.*(?!3).$' for everything except 3rd column etc.  
  #(will append /baylor to target name; Ad hoc - marking the samples from baylor)
  
  # Data input ----
  
  # Preperation steps
  flpath <- str_c('excel files/',flnm,'.xls') # this completes the file path
  
  # dynamic update of standard curve parameters : default parameters in the inputs_for_analysis.R file
  std_to_retrieve <- if_else(is.null(std_override), str_match(flnm, 'Std[:alnum:]*'), std_override)
  
  std_par_update <- read_sheet(sheeturls$data_dump, sheet = 'Standard curves', range = 'A:G', col_types = 'ccnnnnn') %>% 
    filter(str_detect(ID, std_to_retrieve ))
  
  # substitute the new std curve parameters in the old matrix
  std_par %<>% filter(!str_detect(Target,
                                  std_par_update$Target %>% str_c(collapse = "|"))) %>% 
    bind_rows(std_par_update)
  
  # error catching for repeated standard curve names for the same target
  if(std_par_update %>% unique() %>% group_by(Target) %>% summarize(count = n()) %>% pull(count) %>% {. > 1} %>% any())
  {
    stop( str_c('Duplicate entries for the same Target found for standard curve: ', std_to_retrieve, '. \n
  check the Standard curves sheet here : https://docs.google.com/spreadsheets/d/1ouk-kCJHERRhOMNP07lXfiC3aGB4wtWXpnYf5-b2CI4/edit#gid=1980064476'))
  }
  
  # Read in qPCR data and labels from plate template
  fl <- readqpcr(flpath) # read excel file exported by Quantstudio
  plate_template <- get_template_for(flnm, sheeturls$templates)
  
  sample_order = columnwise_index(fl) # this gives a vector to order the samples columnwise in the PCR plate or strip (by default : data is shown row-wise) => This command will enable plotting column wise order
  
  # Load desired qPCR result sheet and columns
  bring_results <- fl$Results %>% select(`Well Position`, `Sample Name`, CT, starts_with('Tm'),`Target Name`) %>% rename(Target = `Target Name`) %>%  .[sample_order,] %>%  
    
    # select only the results used for plotting, calculations etc. and arrange them according to sample order
    select(-`Sample Name`) %>% right_join(plate_template, by = 'Well Position') %>%  # Incorporate samples names from the google sheet by matching well position
    mutate_at('Target', ~str_replace(., 'BSRV', 'BRSV')) %>% 
    filter(!is.na(Target))
  
  # Remove unneccesary data
  rm(fl)  # remove old data for sparsity
  
  # Data polishing ----
  
  
  # Separate the sample name into columns and make factors in the right order for plotting (same order as the plate setup)
  
  # isolate the primer pair and assay_variable into 3 columns : Sample name, assay variable and primer pair 
  polished_results <- bring_results %>% separate(`Sample_name`,c(NA, 'Sample_name'),'-') %>% separate(`Sample_name`,c('Sample_name','Tube ID'),'_') %>% 
    mutate(`Tube ID` = if_else(`Sample_name` == 'NTC', '0', `Tube ID`)) %>% 
    separate(`Tube ID`, c('assay_variable', 'biological_replicates'), remove = F) %>%  # Separate out biological replicates 
    unite('Tube ID', c(assay_variable, biological_replicates), sep = '.', remove = F, na.rm = T) %>% # remaking Tube ID - removes spaces after 'dot'
    arrange(assay_variable, biological_replicates) %>% mutate_if(is.character,as_factor) # Factorise the sample name and rearrange in column order of appearance on the plate (for plotting)
  
  # select samples to plot (or to exclude write a similar command)
  results_relevant <- polished_results %>% filter(str_detect(`Sample_name`, paste('^', plot_select_facet, sep = ''))) %>%  
    
    # Include only desired facets : str_detect will find for regular expression; ^x => starting with x
    filter(!str_detect(`Sample_name`, plot_exclude_facet)) %>%  # exclude unwanted facets (sample_name) 
    filter(!str_detect(assay_variable, plot_exclude_assay_variable)) %>%  # excluding unwanted x axis variables from assay_variable
    
    # Adding tag to target for baylor smaples
    { if(!str_detect(baylor_wells, 'none|None')) { 
      mutate_at(., 'Target', as.character) %>% 
        mutate_cond(str_detect(`Well Position`, baylor_wells), Target = str_c(Target, '/Baylor'))
    } else .
    }
  
  # Computation ----
  
  
  # Computing copy number from standard curve linear fit information
  results_abs <- results_relevant %>% group_by(Target) %>% do(., absolute_backcalc(., std_par)) %>%  # iteratively calculates copy #'s from standard curve parameters of each Target
    mutate(`Copy #` = `Copy #`/template_volume_qpcr) # Normalizing copy number per micro litre of template in the reaction
  
  # Finding mean and standard deviation within replicates (both technical and biological)
  
  summary_results <- results_abs %>%  group_by(`Sample_name`, Target, assay_variable) %>% summarise_at(vars(`Copy #`), lst(mean, sd), na.rm = T) # find mean and SD of individual copy #s for each replicate
  results_abs$`Copy #` %<>% replace_na(0) # make unamplified values 0 for plotting
  
  plt <- results_abs %>% ggplot(aes(x = `Tube ID`, y = `Copy #`, color = Target)) + ylab('Copies/ul RNA extract') +    # Specify the plotting variables 
    geom_point(size = 2) + facet_grid(~`Sample_name`, scales = 'free_x', space = 'free_x') + # plot points and facetting
    ggtitle(flnm) + xlab(plot_assay_variable)
  plt.formatted <- plt %>% format_classic(.) %>% format_logscale_y() # formatting plot, axes labels, title and logcale plotting
  
  print(plt.formatted)
  
  # Data output ----
  
  # Check for pre-existing file and write. Ask for overwrite permission
  check_ok_and_write(results_abs, sheeturls$data_dump, flnm)
  
  # write_sheet(results_abs, sheeturls$data_dump, sheet = flnm) # save results to a google sheet
  # ggsave('qPCR analysis/', WW1_Baylor-bovine_pilot.png', plot = plt.formatted, width = 8, height = 4)
 
   
  # Saving vaccine data into Vaccines sheet in data dump: For easy book keeping
  vaccine_data <- results_abs %>% filter(str_detect(Sample_name, 'Vaccine')) %>%
    mutate('Prepared on' = '',
           Week = str_extract(flnm, '[:digit:]{3,4}') %>% unlist() %>% str_c(collapse = ', '),
           Vaccine_ID = assay_variable, 
           .before = 1) %>% 
    mutate(Run_ID = str_extract(flnm, 'WW[:digit:]*'))
  
  # Add to existing sheet
  if(vaccine_data %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data, 'Vaccines')
  
  # Mean of vaccine data
  vaccine_data.mean <- vaccine_data %>% ungroup() %>% 
    select(1:3, Target, `Copy #`, Run_ID) %>% group_by(across(-`Copy #`)) %>% 
    summarise(across(`Copy #`, list(Mean_qPCR = mean, SD_qPCR = sd), na.rm = T), .groups = 'keep') %>% 
    mutate('[Stock conc.] copies/ul' = `Copy #_Mean_qPCR` * 50/20,
           'Estimated factor' = '',
           Comments = '',
           'Conc normalized to estimated factor' = '') %>% 
    relocate(Run_ID, .after = last_col()) %>% 
    mutate('x' = '', .before = 1)
  
  # Add to existing sheet in Vaccine_summary
  if(vaccine_data.mean %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data.mean, 'Vaccine_summary')
}

# ddPCR processing ----


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
