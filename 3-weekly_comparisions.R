# Loading libraries ----

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file

# User inputs ----

# sheets to read from "qPCR complete data" google sheet
# sheet name(s) in the raw data file (qPCR data dump) - Separate by comma (,)
read_these_sheets <- c('091123 Rice Schools Sampler Test pMMoV', '091823 Rice Schools pMMoV',
                       '092723 Rice Schools pMMoV', '100223 - 100323 Rice Schools pMMoV',
                       '100923 - 101123 Rice Schools and Congregate pMMoV', '101623 - 101823 Rice Schools and Congregate pMMoV',
                       '102323 - 102523 Rice Schools and Congregate pMMoV', '103023 - 110123 Rice Schools and Congregate pMMoV',
                       '110623 - 111523 Rice Schools and Congregate pMMoV', '112123 - 112923 Rice Schools and Congregate pMMoV')

# if you are interested in only pMMoV stuff, then make this TRUE
filter_for_pMMoV_data <- TRUE # optional filtering for samples that have pMMoV

title_name <- '091123 - 112923 Schools and Congregate pMMoV comparisons' # name of the filename for writing presentable data and plot title

# Extra categories to exclude from plotting (separate by | like this 'Vaccine|Troubleshooting')
extra_categories = 'Std|std|Vaccine|Control|Water|NTC|Blank|DI' 
# |TR was also there in extra_categories, don't recall what this is so I removed it - PK


# Data input ----

# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet
list_rawdata <- map(read_these_sheets,
                    ~ read_sheet(sheeturls$complete_data , sheet = .x) %>%  
                      mutate('Run_week' = str_extract(.x, '[:digit:]*(?= Rice)')) %>%
                      
                      # backward compatibility (accounting for column name changes)
                      compl_data_renamer) 

rawdata <- bind_rows(list_rawdata)  # bind all the sheets' results into 1 data frame/tibble

results_abs <- rawdata %>% 
  filter(!str_detect(Facility, extra_categories)) %>%  # remove unnecessary data
  filter(!str_detect(Target_Name, 'BRSV')) %>%  # remove BRSV -- was cluttering the graphs


  # change Run_week as a number and make it factor in ascending order
  mutate(across('Run_week', ~ as.numeric(.x) %>% as_factor)) %>% 
  
  # remove older Run_week's samples when reruns have been done
 # pick_latest_rerun %>% 
  
  rename(Date_formatted = Date) %>% 
  # Make a date as a number from each sample_ID
  mutate(Date = str_extract(Sample_ID, '[:digit:]*(?=\\.)') %>% # extract the digit(s) followed by the .
           as.numeric %>% as.factor) # convert to a number and then a factor

#Join Facility Type to dataframe
  results_abs <- results_abs %>%
    left_join(biobot_lookup %>% select(WWTP, Type), by = "WWTP")
  
#Categorize WWTP based on facility type, then arrange alphabetically
  results_abs <- results_abs %>%
    arrange(WWTP) %>%  
    mutate(type_group = case_when(
      Type %in% c("Jail", "NH", "Shelter") ~ 1,
      Type == "School" ~ 2,
      TRUE ~ 3  # Group 3 for other types
    ))
  
  categorize_WWTP <- function(type_group, WWTP) {
    
    case_when(
      type_group == 1 & substr(WWTP, 1, 1) %in% c("A", "B", "C", "D", "E", "F", "G", "H") ~ 1,
      type_group == 1 & substr(WWTP, 1, 1) %in% c("I", "J", "K", "L", "M", "N", "O", "P", "Q", "R") ~ 2,
      type_group == 1 & substr(WWTP, 1, 1) %in% c("S", "T", "U", "V", "W", "X", "Y", "Z") ~ 3,
      
      type_group == 2 & substr(WWTP, 1, 1) %in% c("A", "B") ~ 4,
      type_group == 2 & substr(WWTP, 1, 1) %in% c("C", "D") ~ 5,
      type_group == 2 & substr(WWTP, 1, 1) %in% c("E", "F", "G", "H") ~ 6,
      type_group == 2 & substr(WWTP, 1, 1) %in% c("I", "J", "K", "L", "M", "N", "O", "P", "Q") ~ 7,
      type_group == 2 & substr(WWTP, 1, 1) %in% c("R", "S", "T", "U", "V", "W", "X", "Y", "Z") ~ 8,
      
      TRUE ~ 9  # Handle other characters
    )
  }
  
  results_abs <- results_abs %>%
    arrange(WWTP) %>%
    mutate(first_char = substr(WWTP, 1, 1),
           group_num = categorize_WWTP(type_group, WWTP))

# convert copies/L into wide format based on Targets
Copies_Per_Liter_WW_wide <- 
  results_abs %>% 
  select(WWTP, Date, Target_Name, Copies_Per_Liter_WW, Sample_ID, group_num, Type) %>% 
  group_by(Target_Name, WWTP) %>% # this is what the replicates are determined on
  mutate(id = row_number()) %>% # need to make a unique id since NTCs run on different plates have the same sample_ID
  unite('WWTP_replicate', c('WWTP', id), remove = FALSE) %>% select(-id) %>% 
  pivot_wider(names_from = Target_Name, values_from = Copies_Per_Liter_WW)

# optional filtering step
if(filter_for_pMMoV_data) {
  Copies_Per_Liter_WW_wide <- 
    Copies_Per_Liter_WW_wide %>% 
    filter(!is.na(pMMoV))
  
  subset_samples_with_N12.pMMoV <-  # picks the WWTP name and Run_week
    Copies_Per_Liter_WW_wide %>% 
    select(WWTP, Sample_ID) %>% 
    # separate(WWTP, c('WWTP_unique', NA), sep = '_') %>% 
    unique 

  
  # Take the subset of the data of identified above from the main dataset (overwrites)
  results_abs <- semi_join(results_abs, subset_samples_with_N12.pMMoV) # semi_join(x,y):keeps only rows in both x and y
}

# Plots to html ----

# calling r markdown file
rmarkdown::render('pMMoV-Timeseries-Plotting.Rmd',
#rmarkdown::render('pMMoV-Violin-Plotting.Rmd', #Use this line for only pMMoV plots
#rmarkdown::render('3.1-Weekly_comparison-plots.Rmd', 
                output_file = str_c('./qPCR analysis/Weekly ', title_name, ".html"))

# optional save command for specific plots
# ggsave( str_c('qPCR analysis/Extra graphs/', title_name, '.pdf'),
#         plot = plt_timeseries_pMMoV, width = 8, height = 6)

# optional output csv files
write_csv(results_abs, 
          file = str_c('excel files/Archive/Weekly-', title_name, '.csv'), col_names = TRUE)
