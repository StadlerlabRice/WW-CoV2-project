# Plotting number of positive droplets for NTC's across multiple runs
# Date: 30/11/20

# Prelim ----

source('./general_functions.R') # Source the general_functions file

flpath.dat <- 'excel files/qPCR data dump_22-1-21.xlsx'

# Import data ----

# get sheet names in data dump local copy
sheets_in_dump <- excel_sheets(flpath.dat)

# filter ddPCR sheets
dd_sheet_names <- sheets_in_dump %>% .[str_detect(., 'dd.')] # Extract only ddPCR data with the dd. prefix

# getting a prototype sheet to fit the column types to outlier sheets 

dd_sheet_prototype_names <- dd_sheet_names[43:44]  # sheets_in_dump %>% .[str_detect(., 'dd.WW(61|79)')] # regular and wierd column types
# first_dd_sheet_name <- dd_sheet_names[1]
# first_dd_sheet <- read_xlsx(flpath.dat, first_dd_sheet_name)

col_types_prototypes <- dd_sheet_prototype_names %>%
  map( ~ read_xlsx(path = flpath.dat, sheet = .x)) %>%  # makes a list of the prototype sheets
  map_df( ~ map(., class)) # finds the class of the sheets
  
View(col_types_prototypes)
col_types_prototypes %>% map( ~ unique(.x) %>% ifelse(length(.) == 1, NA, .)) %>% view()
# col_types_prototypes %>% select(which(length(unique(.)) == 1)) %>% view()

## prototype regular sheet: dd.WW61; Prototype of alternate sheet : dd.WW79


# get all ddPCR sheets
all_dd.data_list <- map(dd_sheet_names, 
                        ~ read_xlsx(flpath.dat, sheet = .x) %>%
                          rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it
                          
                          # Changing problematic columns
                          rename(Concentration = any_of('Conc(copies/µL)'),
                                 AcceptedDroplets = any_of('Accepted Droplets'),
                                 Threshold = any_of('Threshold1'),
                                 MeanAmplitudeofPositives = any_of('MeanAmplitudeOfPositives'),
                                 MeanAmplitudeofNegatives = any_of('MeanAmplitudeOfNegatives')) %>%  # rename the column name - if exported from Quantasoft analysis Pro
                          mutate(across(any_of('Concentration'), as.numeric)) %>%  # Remove the NO CALLS and make it numeric column  
                          mutate_at('biological_replicates', ~str_replace_na(., '')) %>% 
                          mutate(across(matches('Total|Poisson|Mean|Ch|Ratio|Abundance|Linkage|CNV|Copies|Det'), as.numeric)) %>%  # convert ambiguous columns into numeric
                          mutate(across(where(is.list), as.character)) %>% # convert any stray lists into character
                          
                          mutate(across(Target, ~ str_replace_all(., c('BCoV.*' = 'BCoV', '/Baylor' = '')) ) ) %>% 
                          mutate('Run_ID' = str_extract(.x, '(?<=dd\\.WW)[:digit:]+'), .before = 1) # Extract the number of the ddPCR run
)

# Check data types

# types_of_all_data_columns <- all_dd.data_list %>% map_df( ~ map(., class)) %>%
#   mutate(Run_ID2 = map_chr(all_dd.data_list, ~ pull(., Run_ID) %>% unique), .before = 1)
# 
# types_differing <- types_of_all_data_columns %>%
#   map( ~ unique(.) %>%
#          {if(length(.) == 1) NULL
#            else .}
#   )
# 
# types_differing <- types_differing[-(which(sapply(types_differing,is.null),arr.ind=TRUE))]
# View(types_differing)


# merging sheets
all_dd.data <- all_dd.data_list %>%
  bind_rows() %>%
  mutate(across('Run_ID', as.numeric)) %>% 
  select(1:6, 'AcceptedDroplets',	'Positives', 
         'Threshold', 'MeanAmplitudeofPositives',	'MeanAmplitudeofNegatives', 'MeanAmplitudeTotal',
         'PoissonConfMax',	'PoissonConfMin', everything()) %>%  # put the important columns first
  mutate(Sample_type = if_else(str_detect(Sample_name, 'NTC') | str_detect(assay_variable, 'DI|BLANK|NTC'), 'negative', 'regular')) # classify NTCs vs regular samples


# write_csv(all_dd.data, path = 'excel files/Archive/ddpcr_rawdata_across-weeks.csv',  na = '')


# # shortcut data input ----
# 
# all_dd.data <- read_csv( 'excel files/Archive/ddpcr_rawdata_across-weeks.csv')

# Data filtering ----

# filter NTC
ntc_dd.data <- all_dd.data %>% 
  # select(Run_ID, Sample_name, Target, Positives, AcceptedDroplets, `Copy #`) %>% 
  filter(str_detect(Sample_name, 'NTC') | str_detect(assay_variable, 'DI|BLANK|NTC')) %>% 
  group_by(Run_ID, Target) %>% 
  mutate(mean_positive = mean(Positives))

# Plotting ----

# calling r markdown file
rmarkdown::render('ntc_droplets.Rmd', output_file = str_c('./qPCR analysis/Extra graphs/', 'NTC droplets', '.html'))

# 
# # testing 
# tst <- read_sheet(sheeturls$data_dump, sheet = 'dd.WW73_1109_N1N2')
# map_chr(tst, class) %>% view()
# view(tst)

