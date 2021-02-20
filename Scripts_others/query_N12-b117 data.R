# finds specific samples in N1N2 and B117 data
# This was used to report to Baylor for the sequencing effort for the B117 confirmation
# Prashant; 19/2/20

# Prelim ----

# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file

read_these_sheets <- c( '020821 Rice (redo)')

query_for_subset <- 'query-0209-baylor-seq-subset'

sv_file_name <- '0209 to Baylor_for sequencing'

# Input ----

raw_quant_data <- map_dfr(read_these_sheets, 
                           ~ read_sheet(sheeturls$complete_data, sheet = .)) %>% 
  
  mutate(across(where(is.list), as.character)) # convert all problematic list columns into character

# Needs to be modified in the future to input B117 data as well

# raw_quant_data <- bind_rows(list_raw_quant_data) %>% 
#   select(1:6, 'Accepted Droplets',	'Positives', 
#          'Threshold1', 'MeanAmplitudeOfPositives',	'MeanAmplitudeOfNegatives',
#          'PoissonConfMax',	'PoissonConfMin', everything()) # put the important columns first

query_data <- read_sheet(sheeturls$user_inputs, sheet = query_for_subset)

# processing ----

mid_dat <- raw_quant_data %>% 
  filter(Tube_ID %in% query_data$Tube_ID)

# output ----

write_csv(mid_dat, path = str_c('excel files/Archive/', sv_file_name, '.csv'))
