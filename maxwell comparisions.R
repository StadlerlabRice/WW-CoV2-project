# Purpose: Adhoc script to compare maxwell data to Qiagen in all possible ways

# addendum to maxwell comparisons.Rmd

# Shredding guide
shred_guide <- c('.*824.*' = 'minimal.shred', 
                 '.*825 [D-F].' = 'Zirconium_minimal', 
                 '.*825 [G-I].' = 'Biofilm kit_good', 
                 '.*825 [J-M].' = 'Isopropyl_medium')



# Load data ----

max.data <- read_sheet(sheeturls$complete_data, sheet = '824 Maxwell') %>% 
  mutate(RNA_extraction = 'Maxwell')

qia.data <- read_sheet(sheeturls$complete_data, sheet = '824 Rice') %>%
  filter(WWTP %in% max.data$WWTP) %>% 
  mutate(RNA_extraction = 'Qiagen')

# combine data ----
comb.data <- rbind(max.data, qia.data) %>% 
  filter(!str_detect(Facility, 'Std|Vaccine|Control|NTC')) %>% 
  mutate(Beads_shredding = str_replace_all(Tube_ID, shred_guide) %>% fct_relevel('Qiagen_good', 'Biofilm kit_good', 'Isopropyl_medium', 'Zirconium_minimal', 'minimal.shred'))


# Put maxwell and qiagen side by side
wider_data <- comb.data %>% 
  select(WWTP, Facility, RNA_extraction, `Target Name`, `Copies/l WW`, `Recovery fraction`, Beads_shredding) %>% 
  group_by(WWTP, RNA_extraction, `Target Name`) %>%  mutate(index = row_number()) %>% 
  
  pivot_wider(names_from = 'RNA_extraction', values_from = c(`Copies/l WW`, `Recovery fraction`))

# plotting ----

# calling r markdown file
rmarkdown::render('maxwell comparisions.Rmd', output_file = str_c('./qPCR analysis/', '824 maxwell comparisons', '.html'))
