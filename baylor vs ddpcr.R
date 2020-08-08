# adhoc 713-715 baylor ddPCR vs baylor's own qPCR N1/N2

# load ddPCR data
dd <- read_sheet(sheeturls$wwtp_only_data, sheet = '713-715 baylor ddPCR') %>% 
  filter(Lab == 'B') %>% 
  mutate('Method' = 'R-ddPCR') %>% 
  rename(Copies_Per_Liter_WW = Copies_Per_Litre_WW)

# Load Rice-qPCR data
qpr <- read_sheet(sheeturls$wwtp_only_data, sheet = '713-715 Baylor_qPCR N1N2') %>% 
  filter(Lab == 'B') %>% 
  mutate('Method' = 'R-qPCR')

# load Baylor's excel files
q713 <- '../../../Covid Tracking Project/Rice and Baylor Combined Data/Baylor Data/7_13/' %>% 
  str_c('WWTP_713_Baylor_Results', '.csv') %>% 
  read_csv() %>% 
  group_by(WWTP, Target_Name) %>% 
  mutate('Method' = 'B-qPCR', WWTP_ID = str_c('713.', WWTP, '-', row_number()), .before = 1) %>% 
  filter(WWTP_ID %in% dd$WWTP_ID) %>% 
  ungroup()

q716 <- 'C:/Users/new/Box Sync/Covid Tracking Project/Rice and Baylor Combined Data/Baylor Data/7_16/' %>% 
  str_c('WWTP_716_Baylor_Results', '.csv') %>% 
  read_csv() %>% 
  group_by(WWTP, Target_Name) %>% 
  mutate('Method' = 'B-qPCR', WWTP_ID = str_c('715.', WWTP, '-', row_number()), .before = 1) %>% 
  filter(WWTP_ID %in% dd$WWTP_ID) %>% 
  ungroup()

# q merge
q.merge <- bind_rows(q713, q716) 
r.merge <- bind_rows(dd, qpr) %>% mutate_at('Sample_Type', as.character)

# head to head fight!
merge.data <- bind_rows(r.merge, q.merge) %>% 
  mutate(Week = str_extract(WWTP_ID, '[:digit:]*(?=.)'))


rmarkdown::render('baylor vs ddpcr.Rmd', output_file = '713-715 Baylor-Rice ddPCR, qPCR x2.html')




