# Plotting number of positive droplets for NTC's across multiple runs
# Date: 30/11/20

# Import data ----

# get sheet names in data dump
sheets_in_dump <- sheet_names(ss = sheeturls$data_dump)

# filter ddPCR sheets
dd_sheet_names <- sheets_in_dump %>% .[str_detect(., 'dd.')] 

# get all ddPCR sheets
all_dd.data_list <- map(dd_sheet_names, ~ read_sheet(sheeturls$data_dump, sheet = .x) %>% 
                         rename(Sample_name = any_of('Sample Name')) %>% # if name is according to old convension, this will rename it 
                         select(-matches('Conc'), -biological_replicates) %>% 
                         mutate(across(where(is.list), as.character)) %>% 
                         mutate('Run_ID' = str_extract(.x, '(?<=dd\\.WW)[:digit:]+'), .before = 1)
)

all_dd.data <- map(all_dd.data_list, ~ select(.x, Run_ID, Sample_name, Target, Positives, any_of('AcceptedDroplets'), `Copy #`)) %>% 
  bind_rows()


# Data filtering ----

# filter NTC
ntc_dd.data <- all_dd.data %>% 
  select(Run_ID, Sample_name, Target, Positives, AcceptedDroplets, `Copy #`) %>% 
  filter(str_detect(Sample_name, 'NTC'))

# Plotting ----

# calling r markdown file
rmarkdown::render('ntc_droplets.Rmd', output_file = str_c('./qPCR analysis/Extra graphs/', 'NTC droplets', '.html'))

# 
# # testing 
# tst <- read_sheet(sheeturls$data_dump, sheet = 'dd.WW73_1109_N1N2')
# map_chr(tst, class) %>% view()
# view(tst)

