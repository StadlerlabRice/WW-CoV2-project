# Ad hoc plots for concentration methods paper
# Data distributed across sheets in an excel file

# Preliminaries ----
# file name
input_sheet <- 'Concentration methods paper-2'

# method_namer <- c('ha.*' = 'HA Filtration',
#                   '^e.*' = 'Elution',
#                   'uf.*' = 'Ultrafiltration',
#                   'de.*' = 'Direct Extraction') 
  
# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file


# Input file ----

all_data_input <- read_sheet(sheeturls$complete_data, sheet = input_sheet) %>% 
  rename(`Concentration method` = Concentration_method, 
         Target = `Target Name`,
         Fraction.recovered = `Recovery fraction`,
         Detection.limit = Detection_Limit,
         `Copies/L WW` = 'Copies/l WW') %>% # for compatibility with old plotting functions
  mutate(across(WWTP, ~ fct_relevel(.x, 'DI', after = Inf))) %>%  # bringing DI water control to the last position
  filter(!str_detect(Facility, 'Std|0|Vac'))

# filtering data ----

only_wwtps <- all_data_input %>% 
  filter(!str_detect(WWTP, 'DI|NTC')) # remove negative controls

# Summary and long_format ----

scatter_data_N_reco <- 

# 
# minimal_label_columns <- c('Target', 'WWTP', 'Concentration method')
# 
# # Extract minimal columns from processed data (good for running averages and std deviations for plotting)
# processed_minimal = list( raw.dat = all_data_input %>% 
#                             select(all_of(minimal_label_columns), where(is.numeric), -matches('vol')))
# # Group by all the text columns and calculate mean and standard deviation for biological replicates
# processed_minimal$summ.dat <- processed_minimal$raw.dat %>% 
#   group_by_at(all_of(minimal_label_columns)) %>% 
#   summarize_all(.funs = lst(mean, sd), na.rm = T)
# 
# # Convert the above minimal data into long format (convenient for plotting multiple data types on the same plot)
# long_processed_minimal <- processed_minimal %>% map(pivot_longer, cols = where(is.numeric),
#                                                     names_to = 'Measurement', values_to = 'value')                                                   
# long_processed_minimal$summ.dat %<>% separate(Measurement, into = c('Measurement','val'),"_") %>% 
#   pivot_wider(names_from = val, values_from = value) # Seperate mean and variance and group by variable of measurement


# Plots w shapes ----

aligned_shape_plt_N1 <- {all_data_input %>% 
    filter(str_detect(Target, 'N1')) %>% 
    ggplot(aes(WWTP, `Copies/L WW`, colour = `Concentration method`,  shape = WWTP)) + 
    geom_point() + 
    facet_grid(~`Concentration method`) +
    ylab('Genome copies/L wastewater') + ggtitle('SARS-CoV2 N1 across methods') + 
    scale_shape_manual(values = c(15,16,17,7,8,10,3))} %>% 
  format_logscale_y() %>%
  format_classic() %>% 
  print()

aligned_shape_plt_N2 <- {all_data_input %>% 
    filter(str_detect(Target, 'N2')) %>% 
    ggplot(aes(WWTP, `Copies/L WW`, colour = `Concentration method`,  shape = WWTP)) + 
    geom_point() +
    facet_grid(~`Concentration method`) +
    ylab('Genome copies/L wastewater') + ggtitle('SARS-CoV2 N2 across methods') + 
    scale_shape_manual(values = c(15,16,17,7,8,10,3))} %>% 
  format_logscale_y() %>%
  format_classic() %>% 
  print()

aligned_shape_plt_recovery <- {all_data_input %>% 
    ggplot(aes(WWTP, Fraction.recovered, colour = `Concentration method`, shape = WWTP)) + 
    geom_point() + 
    facet_grid(~`Concentration method`) +
    ylab('Fraction of surrogate virus recovered') + ggtitle('Surrogate recovery across methods') +
  scale_shape_manual(values = c(15,16,17,7,8,10,3))} %>% 
  format_classic(.) %>% 
  print()


aligned_shape_plt_pmmov <- {all_data_input %>% 
    filter(str_detect(Target, 'pMMoV')) %>% 
    ggplot(aes(WWTP, `Copies/L WW`, colour = `Concentration method`,  shape = WWTP)) + 
    geom_point() +
    facet_grid(~`Concentration method`) +
    ylab('Genome copies/L wastewater') + ggtitle('Internal control: Pepper-mild-mottle virus across methods') + 
    scale_shape_manual(values = c(15,16,17,7,8,10,3))} %>% 
  format_logscale_y() %>%
  format_classic() %>% 
  print()


scatter_N1_recovery <- only_wwtps %>% 
  

# Plot save ----
# (optional)

# plots-w-jitter
# ggsave('qPCR analysis/Methods paper/Methods_paper_N1.pdf', plot = plt.N1, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/Methods_paper_N2.pdf', plot = plt.N2, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/Methods_paper_Recovery.pdf', plot = plt.recovery, width = 8, height = 4)

# plots-w-shapes
save_plot <- function(plt.id, plt.name, plt.format = 'pdf', plt.width = 8, plt.height = 4)
{
  str_c('qPCR analysis/Methods paper/WWTP by shape/Methods_paper_', plt.id, '-aligned.', plt.format) %>% 
    ggsave(plot = plt.name, width = plt.width, height = plt.height)
  
}

save_plot('N1', aligned_shape_plt_N1)
save_plot('N2', aligned_shape_plt_N2)
save_plot('Recovery', aligned_shape_plt_recovery)
save_plot('pepper virus', aligned_shape_plt_pmmov)

# ggsave('qPCR analysis/Methods paper/WWTP by shape/Methods_paper_Recovery.N1-aligned.pdf', plot = aligned_shape_plt_N1, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/WWTP by shape/Methods_paper_Recovery.N2-aligned.pdf', plot = aligned_shape_plt_N2, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/WWTP by shape/Methods_paper_Recovery.Recovery-aligned.pdf', plot = aligned_shape_plt_recovery, width = 8, height = 4)
