# Purpose: Adhoc script to compare maxwell data to Qiagen in all possible ways

# addendum to maxwell comparisons.Rmd

# Load data ----

max.data <- read_sheet(sheeturls$complete_data, sheet = '824 Maxwell') %>% 
  mutate(RNA_extraction = 'Maxwell') 

qia.data <- read_sheet(sheeturls$complete_data, sheet = '824 Rice') %>%
  filter(WWTP %in% max.data$WWTP) %>% 
  mutate(RNA_extraction = 'Qiagen')

# combine data
comb.data <- rbind(max.data, qia.data) %>% 
  filter(!str_detect(Facility, 'Std|Vaccine|Control|NTC'))

# Put maxwell and qiagen side by side
wider_data <- comb.data %>% 
  select(WWTP, Facility, RNA_extraction, `Target Name`, `Copies/l WW`, `Recovery fraction`) %>% 
  group_by(WWTP, RNA_extraction, `Target Name`) %>%  mutate(index = row_number()) %>% 
  
  pivot_wider(names_from = 'RNA_extraction', values_from = c(`Copies/l WW`, `Recovery fraction`))