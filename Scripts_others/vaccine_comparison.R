# Ad-hoc plotting vaccine vs boiled vaccine

# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file

# Load data ----
vaccine_data <- read_sheet(sheeturls$data_dump, sheet = "Vaccines") %>% 
  mutate(Method = str_replace_all(Sample_name, c('m.*' = 'Maxwell', 'Vaccine$' = 'Qiagen', 'Vaccineboil' = 'Lysate') ),
         Freeze = if_else(str_detect(Vaccine_ID, 'fridge'), '4C', 'Frozen'),
         Method = if_else(str_detect(Vaccine_ID, 'fridge'), 'Lysate-4C', Method),
         Vaccine_ID = str_extract(Vaccine_ID, '[:digit:]+')) %>% 
  
  filter(!str_detect(Run_ID, 'WW50|60|61'), # Remove outliers from the first boil quant (too high); redundant boil quants 60, 61 (too variable)
         Freeze != '4C') %>% # Remove outliers from fridge samples (not frozen - have lower quants)
  mutate(Copies_per_ul_stock = if_else(str_detect(Sample_name, 'boil'), `Copy #` * 5, `Copy #` * 5/2)) # account for dilutions



# Processing ----

vaccine_summary <- vaccine_data %>% 

  group_by(Method, Vaccine_ID) %>% 
  summarise(across(Copies_per_ul_stock, lst(mean, sd))) 

# vaccine_wide <- vaccine_summary %>% 
#   pivot_wider(values_from = starts_with('Copies'), names_from = Method)



# plotting  ----

plt1 <- ggplot(vaccine_summary, aes(Vaccine_ID, `Copies_per_ul_stock_mean`, colour = Method)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = `Copies_per_ul_stock_mean` - `Copies_per_ul_stock_sd`, 
                    ymax = `Copies_per_ul_stock_mean` + `Copies_per_ul_stock_sd` ), width = .1) + 
  
  geom_jitter(data = vaccine_data, aes(y = `Copies_per_ul_stock`, label = Run_ID), size = 1, alpha = .5, width = .2) + 
  ggtitle('Vaccine quantification with extraction methods')

plt1 %>% format_classic() %>% print()
plotly::ggplotly(plt1, dynamicTicks = T, tooltip = 'label')


# ggplot(vaccine_data %>% filter(str_detect(Method, 'Lysate')), aes(Vaccine_ID, CT, colour = Run_ID)) + geom_jitter(width = .2)

ggplot(vaccine_data %>% filter(str_detect(Method, 'Lysate')), aes(Vaccine_ID, Copies_per_ul_stock, colour = Run_ID, shape = Freeze)) + geom_jitter(width = .2) 
  # format_logscale_y(.) %>% 
  # plotly::ggplotly(., dynamicTicks = T)
