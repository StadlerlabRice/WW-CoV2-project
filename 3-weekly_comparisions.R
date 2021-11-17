# Loading libraries ----

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file
source('./inputs_for_analysis.R') # Source the file with user inputs

# sheets to read from "qPCR data to present + recoveries"" google sheet
read_these_sheets <- c('111121 Rice Lift Station', '110921 Rice Congregate and pMMoV',
                       '110921 Rice pMMoV',
                       '110321 Rice Congregate', '110321 Rice pMMoV') # sheet name(s) in the raw data file (qPCR data dump) - Separate by comma (,)

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

rawdata <- bind_rows(list_rawdata) # bind all the sheets' results into 1 data frame/tibble

results_abs <- rawdata %>% filter(!str_detect(Facility, extra_categories)) %>%  # remove unnecessary data
  filter(!str_detect(Target_Name, 'BRSV')) %>% 
  mutate_at('Week', ~ as_factor(.))

# Plots to html ----

# calling r markdown file
rmarkdown::render('3.1-weekly_comparison plots.rmd', output_file = str_c('./qPCR analysis/Weekly ', title_name, '.html'))

