# Loading libraries ----

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file

# User inputs ----

# sheets to read from "qPCR complete data" google sheet
read_these_sheets <- c('111121 Rice Lift Station and pMMoV', '110921 Rice Congregate and pMMoV',
                       '110921 Rice pMMoV-N1N2',
                       '110321 Rice pMMoV-N1N2', '110321 Rice Congregate') # sheet name(s) in the raw data file (qPCR data dump) - Separate by comma (,)

# if you are interested in only pMMoV stuff, then make this TRUE
filter_for_both_N1.N2.and.pMMoV_data <- TRUE # optional filtering for samples that have both N1-N2 and pMMoV

title_name <- 'pMMoV comparisons-111121' # name of the filename for writing presentable data and plot title

# Extra categories to exclude from plotting (separate by | like this 'Vaccine|Troubleshooting')
extra_categories = 'Std'
# extra_categories = 'Std|Vaccine|Control|Water|NTC|TR|Blank'


# Data input ----

# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet
list_rawdata <- map(read_these_sheets, 
                    ~ read_sheet(sheeturls$complete_data , sheet = .x) %>%  
                      mutate('Week' = str_extract(.x, '[:digit:]*(?= Rice)')) %>%
                      
                      # backward compatibility (accounting for column name changes)
                      compl_data_renamer) 

rawdata <- bind_rows(list_rawdata)  # bind all the sheets' results into 1 data frame/tibble

results_abs <- rawdata %>% filter(!str_detect(Facility, extra_categories)) %>%  # remove unnecessary data
  filter(!str_detect(Target_Name, 'BRSV')) %>%  # remove BRSV -- was cluttering the graphs
  # change week as a number and make it factor in ascending order
  mutate(across('Week', ~ as.numeric(.x) %>% as_factor)) 

# convert copies/L into wide format based on Targets
Copies_Per_Liter_WW_wide <- results_abs %>% select(WWTP, Target_Name, Week, Copies_Per_Liter_WW) %>% group_by(Week, Target_Name, WWTP) %>% mutate(id = row_number()) %>% unite('WWTP', c('WWTP', id)) %>% pivot_wider(names_from = Target_Name, values_from = Copies_Per_Liter_WW) 

# optional filtering step
if(filter_for_both_N1.N2.and.pMMoV_data) {
  Copies_Per_Liter_WW_wide <- 
    Copies_Per_Liter_WW_wide %>% 
    filter( !is.na(`SARS CoV-2 N1`) & 
              !is.na(`SARS CoV-2 N2`) &
              !is.na(pMMoV))
  
  subset_samples_with_N12.pMMoV <-  # picks the WWTP name and Week
    Copies_Per_Liter_WW_wide %>% 
    select(WWTP, Week) %>% 
    separate(WWTP, c('WWTP_unique', NA), sep = '_') %>% 
    unique
  
  results_abs <- 
    results_abs %>% 
    filter(Week %in% subset_samples_with_N12.pMMoV$Week,
           WWTP %in% subset_samples_with_N12.pMMoV$WWTP_unique)
}

# Plots to html ----

# calling r markdown file
rmarkdown::render('3.1-weekly_comparison plots.rmd', output_file = str_c('./qPCR analysis/Weekly ', title_name, '.html'))

# optional save command for specific plots
# ggsave( str_c('qPCR analysis/Extra graphs/', title_name, '.pdf'), plot = plt_timeseries_pMMoV, width = 8, height = 6)
