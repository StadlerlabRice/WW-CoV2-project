# g.14 : qPCR and std curve processing fns

# Copied over from 1-processing_functions

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
    mutate(Copies_per_uL_template = `Copy #`/template_volume_qpcr) # Normalizing copy number per micro litre of template in the reaction
  
  # Finding mean and standard deviation within replicates (both technical and biological)
  
  summary_results <- results_abs %>%  group_by(`Sample_name`, Target, assay_variable) %>% summarise_at(vars(Copies_per_uL_template), lst(mean, sd), na.rm = T) # find mean and SD of individual copy #s for each replicate
  results_abs$Copies_per_uL_template %<>% replace_na(0) # make unamplified values 0 for plotting
  
  plt <- results_abs %>% ggplot(aes(x = `Tube ID`, y = Copies_per_uL_template, color = Target)) + ylab('Copies/ul RNA extract') +    # Specify the plotting variables 
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
    select(1:3, Target, Copies_per_uL_template, Run_ID) %>% group_by(across(-Copies_per_uL_template)) %>% 
    summarise(across(Copies_per_uL_template, list(Mean_qPCR = mean, SD_qPCR = sd), na.rm = T), .groups = 'keep') %>% 
    mutate('[Stock conc.] copies/ul' = `Copy #_Mean_qPCR` * 50/20,
           'Estimated factor' = '',
           Comments = '',
           'Conc normalized to estimated factor' = '') %>% 
    relocate(Run_ID, .after = last_col()) %>% 
    mutate('x' = '', .before = 1)
  
  # Add to existing sheet in Vaccine_summary
  if(vaccine_data.mean %>% plyr::empty() %>% {!.}) sheet_append(sheeturls$data_dump, vaccine_data.mean, 'Vaccine_summary')
}