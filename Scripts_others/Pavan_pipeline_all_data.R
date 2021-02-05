# Formatting all data, recovery rates and filtration times for Pavan

# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file


# Input ----
weeks_to_read <- c(608 + 0:3 * 7, 706 + 0:3 * 7, 803 + 0:1 * 7) # all weeks till 810
sheets_to_read <- str_c(weeks_to_read, ' Rice'); sheets_to_read[str_detect(sheets_to_read, '706')] <- '706 Rice + Baylor'

# Extra categories to exclude from plotting (separate by | like this 'Vaccine|Troubleshooting')
extra_categories = 'Std|Vaccine|Control|Water|NTC|TR|Blank'

# Quant data ----
# Acquire all the pieces of the data : read saved raw qPCR results from a google sheet
list_rawqpcr <- map(sheets_to_read, ~ read_sheet(sheeturls$complete_data , sheet = .x) %>%  
                      mutate('Week' = str_match(.x, '[:digit:]*(?= Rice)')) %>%
                      rename('Target' = matches('Target'), 'Original Sample Volume' = matches('WW_vol|Original Sample Volume'), 'Ct' = matches('^CT', ignore.case = T)) %>% 
                      mutate_at('Target', ~str_replace_all(., c('.*N1.*' = 'SARS CoV-2 N1', '.*N2.*' = 'SARS CoV-2 N2')))
                    
) 

# Metadata ----


# Get volumes data from google sheet : "Sample registry"
cols_indicator <- str_c('-cc', rep('-', 17) %>% str_c(collapse = ''), 'c') # indicating data types in column : (c) Char or (-) skip
filtration_data_Rice <- read_sheet(sheeturls$sample_registry , sheet = 'Concentrated samples', range = 'A:U', col_types = cols_indicator) %>% 
  rename('Label_tube' = `Label on tube`, 
         'Biobot_id' = `Biobot/other ID`,
         Filtration_time_min = matches('filtration')) %>% 
  
  distinct() %>% # for removing repeated data in early stuff, before 608 (interferes with the merging of volumes for same bottles)
  mutate_at('Label_tube', ~str_remove_all(., " ")) %>% 
  mutate_at('Biobot_id', str_remove,  ' ') %>% 
  mutate_at('Filtration_time_min', ~hm(.x) %>% {hour(.) * 60 + minute(.)})


# Get influent parameters from excel sheet in box.com
cols_influent <- c('text','text', rep('numeric', 43)) # indicating data types in column
influent_parameters <- read_xlsx('Influent Parameters - TSS-NH3N-CBOD rev2.1' %>% 
                                  str_c('../../../Covid Tracking Project/COH Health Department/', ., '.xlsx'),
                                 col_types = cols_influent, 
                                 range = 'A1:AS43') %>% 
  rename(Measurement = `...1`) %>% fill(Measurement) %>% 
  pivot_longer(cols = -c(Measurement, Date), names_to = 'Facility') %>% 
  pivot_wider(names_from = Measurement) # WORK IN PROGRESS : fix clash with data types

# Attaching data ----


# Bind all wastewater quantification data
rawqpcr <- bind_rows(list_rawqpcr)
results_abs <- rawqpcr %>% filter(!str_detect(Facility, extra_categories)) %>%  # remove unnecessary data : BRSV and Baylor
  filter(!str_detect(Target, 'BRSV')) %>% 
  filter(!str_detect(Lab, 'B')) %>%
  
  # join with filtration time data
  mutate('Label_tube', str_remove_all(Tube_ID, " "), .keep = 'unused') %>% 
  left_join(filtration_data_Rice, by = 'Label_tube') %>% 
  
  # join with influent parameters
  mutate(Date = Week %>% str_extract('[:digit:]{3}') %>% str_replace('([:digit:])([:digit:]{2})', '\\1/\\2/2020') ) %>% 
  fuzzyjoin::regex_left_join(influent_parameters, by = c('Date', 'Facility')) %>% 
  
  mutate_at('Week', ~ as_factor(.)) %>% 
  mutate(Date = title_name %>% str_extract('[:digit:]{3}') %>% str_replace('([:digit:])([:digit:]{2})', '\\1/\\2/20'),
         Detection_Limit = if_else(str_detect(Target, 'N1|N2'), 330, 705) 
         )
  # mutate('Viral load per capita per day' = `Copies/l WW` * Flow.Rate.Liters.Per.Day / Population)

# Data check
{ggplot(filtration_data_Rice, aes(x = 1, y = Filtration_time_min, label = Label_tube)) + geom_jitter() + geom_violin(alpha = .3)} %>% plotly::ggplotly(tooltip = 'label')

# re-loading data to check for outliers in the time of filtration
large_cols_indicator <- str_c('-cc', rep('-', 15) %>% str_c(collapse = ''), 'ccc') # indicating data types in column : (c) Char or (-) skip
filtration_large <- read_sheet(sheeturls$sample_registry , sheet = 'Concentrated samples', range = 'A:U', col_types = large_cols_indicator) %>% 
  rename('Label_tube' = `Label on tube`, 
         'Biobot_id' = `Biobot/other ID`,
         Filtration_time_min = matches('filtration')) %>% 
  
  distinct() %>% # for removing repeated data in early stuff, before 608 (interferes with the merging of volumes for same bottles)
  mutate_at('Label_tube', ~str_remove_all(., " ")) %>% 
  mutate_at('Biobot_id', str_remove,  ' ') %>% 
  mutate_at('Filtration_time_min', ~hm(.x) %>% {hour(.) * 60 + minute(.)})

{ggplot(filtration_large, 
        aes(x = 1, y = Filtration_time_min, label = Label_tube, label12 = `Start time`, label13 = `End time` )) + 
    geom_jitter() + 
    geom_violin(alpha = .3) #+ 
    # geom_text_repel(data = filtration_large %>% filter(Filtration_time_min > 100))
  } %>%
  plotly::ggplotly(dynamicTicks = T, tooltip = c('label', 'label12', 'label13'))
