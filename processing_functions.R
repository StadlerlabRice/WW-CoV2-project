# Read in the qPCR file and do manual analysis and plotting
# Author: Prashant Kalvapalle;  June 16 2020.
# Merged with other /R files Aug 4, 2020

# Loading pre-reqisites ----

# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file
source('./inputs_for_analysis.R') # Source the file with user inputs


# Standard curve ----
# choose file name, in the same directory as Rproject
flnm.here <- 'WW36_720_BCoV_Std19'  # set the filename (if running through this file; uncomment the function call in the end)

process_standard_curve <- function(flnm)
{
  
  # Preliminary naming ----
  
  # file path and google sheet urls
  flpath <- str_c('excel files/',flnm,'.xls') # this completes the file path
  
  # Extract a short name for the standard curve from the file name provide. short name has Stdxx_Target_WWxx
  fl_namer <- c('Std[:alnum:]*', 'BCoV|N1|N2|BRSV|pMMoV', 'WW[:alnum:]*') %>% 
    map_chr(~str_match(flnm, .)) %>% 
    str_c(collapse = '_')
  
  title_name <- str_c('Standard curve: ', fl_namer ,' - Fastvirus 4x') # title name for plots
  
  
  # Data input ----
  
  fl <- readqpcr(flpath) # read file
  
  # Bring sample names from template google sheet
  plate_template <- get_template_for(flnm, sheeturls$templates)
  
  sample_order = columnwise_index(fl) # this gives a vector to order the samples columnwise in the PCR plate or strip (by default : data is shown row-wise) => This command will enable plotting column wise order
  
  bring_results <- fl$Results %>% select(`Well Position`, `Sample Name`, CT, starts_with('Tm'),`Target Name`, Task) %>% rename(Target = `Target Name`) %>%  .[sample_order,] %>%  # select only the results used for plotting, calculations etc. and arrange them according to sample order
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
  plt + geom_text(data = std_table$dat, label = std_table$equation, parse = TRUE, show.legend = F, hjust = 'inward', nudge_x = 0, force = 10)
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
process_qpcr <- function(flnm = flnm.here, std_override = NULL)
  # enter the file name, standard curve mentioned within filename is used unless override is provided
{
  
  # Data input ----
  
  # Preperation steps
  flpath <- str_c('excel files/',flnm,'.xls') # this completes the file path
  
  # dynamic update of standard curve parameters : default parameters in the inputs_for_analysis.R file
  std_to_retrieve <- if_else(is.null(std_override), str_match(flnm, 'Std[:alnum:]*'), std_override)
  
  std_par_update <- read_sheet(sheeturls$data_dump, sheet = 'Standard curves', range = 'A:G', col_types = 'ccnnnnn') %>% 
    filter(str_detect(ID, std_to_retrieve ))
  
  # substitute the new std curve parameteters in the old matrix
  std_par %<>% filter(!str_detect(Target,
                                  std_par_update$Target %>% str_c(collapse = "|"))) %>% 
    bind_rows(std_par_update)
  
  # Read in qPCR data and labels from plate template
  fl <- readqpcr(flpath) # read excel file exported by Quantstudio
  plate_template <- get_template_for(flnm, sheeturls$templates)
  
  sample_order = columnwise_index(fl) # this gives a vector to order the samples columnwise in the PCR plate or strip (by default : data is shown row-wise) => This command will enable plotting column wise order
  
  # Load desired qPCR result sheet and columns
  bring_results <- fl$Results %>% select(`Well Position`, `Sample Name`, CT, starts_with('Tm'),`Target Name`) %>% rename(Target = `Target Name`) %>%  .[sample_order,] %>%  # select only the results used for plotting, calculations etc. and arrange them according to sample order
    select(-`Sample Name`) %>% right_join(plate_template, by = 'Well Position') %>%  # Incorporate samples names from the google sheet by matching well position
    mutate_at('Target', ~str_replace(., 'BSRV', 'BRSV')) %>% 
    filter(!is.na(Target))
  
  # Remove unneccesary data
  rm(fl, plate_template_raw)  # remove old data for sparsity
  
  # Data polishing ----
  
  
  # Separate the sample name into columns and make factors in the right order for plotting (same order as the plate setup)
  
  # isolate the primer pair and assay_variable into 3 columns : Sample name, assay variable and primer pair 
  polished_results <- bring_results %>% separate(`Sample_name`,c(NA, 'Sample_name'),'-') %>% separate(`Sample_name`,c('Sample_name','Tube ID'),'_') %>% 
    mutate(`Tube ID` = if_else(`Sample_name` == 'NTC', '0', `Tube ID`)) %>% 
    separate(`Tube ID`, c('assay_variable', 'biological_replicates'), remove = F) %>%  # Separate out biological replicates 
    unite('Tube ID', c(assay_variable, biological_replicates), sep = '.', remove = F, na.rm = T) %>% # remaking Tube ID - removes spaces after 'dot'
    arrange(assay_variable, biological_replicates) %>% mutate_if(is.character,as_factor) # Factorise the sample name and rearrange in column order of appearance on the plate (for plotting)
  
  # select samples to plot (or to exclude write a similar command)
  results_relevant <- polished_results %>% filter(str_detect(`Sample_name`, paste('^', plot_select_facet, sep = ''))) %>%  # Include only desired facets : str_detect will find for regular expression; ^x => starting with x
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
    mutate(`Copy #` = `Copy #`/template_volume) # Normalizing copy number per micro litre of template in the reaction
  
  # Finding mean and standard deviation within replicates (both technical and biological)
  
  summary_results <- results_abs %>%  group_by(`Sample_name`, Target, assay_variable) %>% summarise_at(vars(`Copy #`), rlang::list2(mean, sd), na.rm = T) # find mean and SD of individual copy #s for each replicate
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
  
}

# ddPCR processing ----


# ddPCR processing: Attach sample labels from template table, calculate copies/ul using template volume/reaction, make names similar to qPCR data 
process_ddpcr <- function(flnm = flnm.here, baylor_wells = 'none')
{ # Baylor wells : choose 1) none, 2) '.*' for all, 3) '[A-H][1-9]$|10' etc. for specific wells 
  
  template_volume <- 10 /22 * 20 # ul template volume per well of the ddPCR reaction
  
  # Ad hoc - marking the samples from baylor (will append /baylor to target name)
  
  
  
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
    select(`Sample_name`, `Copy #`, Target, everything())
  
  
  # polishing qPCR data - Make Biobot ID column clean
  # isolate the primer pair and assay_variable into 3 columns : Sample name, assay variable and primer pair 
  polished_results <- bring_results %>% separate(`Sample_name`,c(NA, 'Sample_name'),'-') %>% separate(`Sample_name`,c('Sample_name','Tube ID'),'_') %>% 
    mutate(`Tube ID` = if_else(`Sample_name` == 'NTC', '0', `Tube ID`)) %>% 
    separate(`Tube ID`, c('assay_variable', 'biological_replicates'), remove = F) %>%  # Separate out biological replicates 
    unite('Tube ID', c(assay_variable, biological_replicates), sep = '.', remove = F, na.rm = T) %>% # remaking Tube ID - removes spaces after 'dot'
    # unite('Biobot ID', c(`Sample_name`, assay_variable), sep = '', remove = F) %>%
    
    mutate_at('assay_variable', as.character) %>% 
    mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
    
    # Adding tag to target for baylor smaples
    { if(!str_detect(baylor_wells, 'none|None')) { 
      mutate_at(., 'Target', as.character) %>% 
        mutate_cond(str_detect(`Well Position`, baylor_wells), Target = str_c(Target, '/Baylor'))
    } else .
    }
  
  # Data output ----
  # this is usually commented out to prevent overwriting existing data; turn on only when needed for a single run
  
  check_ok_and_write(polished_results, sheeturls$data_dump, flnm) # save results to a google sheet
  
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

