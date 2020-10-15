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
         Fraction_recovered = `Recovery fraction`,
         `Copies/L WW` = 'Copies/l WW') %>% # for compatibility with old plotting functions
  mutate(across(WWTP, ~ fct_relevel(.x, 'DI', after = Inf))) %>%  # bringing DI water control to the last position
  filter(!str_detect(Facility, 'Std|0|Vac'))

# Join the N1N2 data with BCoV for doing scatter plot
# scatter_N_recovery <- all_data_input %>% 
#   pivot_longer() # work in progress



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
    ggplot(aes(WWTP, Fraction_recovered, colour = `Concentration method`, shape = WWTP)) + 
    geom_point() + 
    facet_grid(~`Concentration method`) +
    ylab('Fraction of surrogate virus recovered') + ggtitle('Surrogate recovery across methods') +
  scale_shape_manual(values = c(15,16,17,7,8,10,3))} %>% 
  format_classic(.) %>% 
  print()

# Plot save ----
# (optional)

# plots-w-jitter
# ggsave('qPCR analysis/Methods paper/Methods_paper_N1.pdf', plot = plt.N1, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/Methods_paper_N2.pdf', plot = plt.N2, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/Methods_paper_Recovery.pdf', plot = plt.recovery, width = 8, height = 4)

# plots-w-shapes
ggsave('qPCR analysis/Methods paper/WWTP by shape/Methods_paper_Recovery.N1-aligned.pdf', plot = aligned_shape_plt_N1, width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/WWTP by shape/Methods_paper_Recovery.N2-aligned.pdf', plot = aligned_shape_plt_N2, width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/WWTP by shape/Methods_paper_Recovery.Recovery-aligned.pdf', plot = aligned_shape_plt_recovery, width = 8, height = 4)
