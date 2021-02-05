# ad-hoc filtering of more columns from the data dump for QC - posson confidence intervals, droplet counts, thresholds etc.
# Started with the methods paper-3 first but could be scaled to more things

# Prashant, 21/1/21

# Prelim ----

source('./general_functions.R') # Source the general_functions file

read_these_sheets <- c( 'dd.WW60_Conc Method 3-1 (Baylor)_N1N2',
                        'dd.WW61_Conc Method 3-2 Baylor_BCoV',
                        'dd.WW62_Conc Method 3-3 Baylor_N1N2_BCoV',
                        'WW85_Conc Method 3-1 Baylor_pMMoV_Std64',
                        'WW86_Conc Method 3-2 Baylor_pMMoV_Std65')

# Input ----

list_raw_quant_data <- map(read_these_sheets, 
                           ~ read_sheet(sheeturls$data_dump, sheet = .) %>% 
                             select(-any_of('Conc(copies/µL)'))) 

raw_quant_data <- bind_rows(list_raw_quant_data) %>% 
  select(1:6, 'Accepted Droplets',	'Positives', 
         'Threshold1', 'MeanAmplitudeOfPositives',	'MeanAmplitudeOfNegatives',
         'PoissonConfMax',	'PoissonConfMin', everything()) # put the important columns first

# View(raw_quant_data)

# processing ----

ntc_di_data <- raw_quant_data %>% 
  filter(str_detect(Sample_name, 'NTC') | str_detect(assay_variable, 'DI')) %>% 
  select(1:13) # only pull the important columns

write_sheet(ntc_di_data, sheet = 'NTC analysis',
            ss = 'https://docs.google.com/spreadsheets/d/17Qc_CchXsv_ZmP6XkXmewJxL57YZTL56n9dM-rUWBnc/edit#gid=1563853727')
