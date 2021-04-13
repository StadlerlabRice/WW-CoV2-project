# Plotting and analyzing linkage between two channels of targets on the ddPCR : using linkage column 
# Prashant; 13/4/21

# user inputs  ---

# Download the 'data dump' google sheet as an xlsx file -> Makes it much easier when reading multiple sheets
# Reading one by one is too slow from google sheets when plotting long term trends in NTCs
# PK- I'm using data dump instead of complete data because not all complete data has droplet counts; for recent data, complete data should work
flpath.dat <- 'excel files/Archive/Backup--qPCR data dump_13-4-21.xlsx' 

dd.ids_to_read  <- 1:178  # provide the number range of dd.WWxaa to dd.WWxbb

title_name <- str_c('Linkage', '-dd ww', 
                    str_c(range(dd.ids_to_read), collapse = '-'))

samples_to_remove <- regex('DI|NTC|Blank|B117|MIX|WHC', ignore_case = TRUE)

# Prelim ----

source('./0-general_functions_main.R') # Source the general_functions file

# Import data ----

# get sheet names in data dump local copy
sheets_in_dump <- excel_sheets(flpath.dat)

# filter ddPCR sheets
# dd_sheet_names <- sheets_in_dump %>% .[str_detect(., 'dd.')] # Extract only ddPCR data with the dd. prefix
dd_selected_sheet_names <- sheets_in_dump %>% .[str_detect(., str_c('dd.WW', dd.ids_to_read, collapse = '|'))] # Extract selected number range of dd.WWxaa to dd.WWxbb 

# get all ddPCR sheets
all_dd.data_list <- map(dd_selected_sheet_names, 
                        ~ read_xlsx(flpath.dat, sheet = .x) %>%
                          data_dump_renamer %>% 
                          mutate('Run_ID' = str_extract(.x, '(?<=dd\\.WW)[:digit:]+'), .before = 1) # Extract the number of the ddPCR run
                        )

# merging sheets
all_dd.data <- all_dd.data_list %>%
  bind_rows() %>%
  mutate(across('Run_ID', as.numeric)) %>% 
  select(1:6, 'Linkage',  
         'AcceptedDroplets',	'PositiveDroplets', 
         'Threshold', 'MeanAmplitudeofPositives',	'MeanAmplitudeofNegatives', 'MeanAmplitudeTotal',
         'PoissonConfMax',	'PoissonConfMin', everything()) %>%  # put the important columns first
  mutate(Target_group = Target %>% 
           {case_when(str_detect(., 'N1|N2') ~ 'N1-N2',
                      str_detect(., 'N501Y|Del69') ~ str_c(variant_status, 'S_501-Del69,70', sep = ':'),
                      TRUE ~ 'Other')},
         .before = Linkage) %>% 
  
  # Make brackets of linkage
  mutate(Linkage.bracket = if_else(Linkage > 0, '> 0', '0') ) %>% 
  
  # remove BCoV or any other targets
  filter(!Target_group == 'Other',
          # remove controls
         !str_detect(assay_variable, samples_to_remove))

linkage_above.0 <- all_dd.data %>% 
  filter(Linkage > 0)

# plotting ----

# calling r markdown file
rmarkdown::render('Scripts_others/linkage_analysis plots.Rmd', output_file = str_c('./qPCR analysis/Extra graphs/', title_name, '.html'))


