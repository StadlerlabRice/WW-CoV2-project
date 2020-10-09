# Ad hoc plots for concentration methods paper
# Data distributed across sheets in an excel file

# Preliminaries ----
# file name
flpath <- 'qPCR analysis/Methods paper/Concentration Methods Data V2 New ddPCR data'

method_namer <- c('ha.*' = 'HA Filtration',
                  '^e.*' = 'Elution',
                  'uf.*' = 'Ultrafiltration',
                  'de.*' = 'Direct Extraction') 
  
# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file


# Input file ----

flpath_extension <- str_c(flpath,'.xlsx') # this completes the file path
read_these_sheets <- c('HA Filtration',
                       'Elution',
                       'Ultrafiltration',
                       'Direct Extraction'
                       )

xldata <- map_df(read_these_sheets, ~ read_xlsx(path = flpath_extension,
                                             sheet = .x,
                                             range = 'A2:N54') %>% 
                                    mutate(`Concentration method` = .x)
              ) %>% 
  select(Facility, WWTP, `Concentration method`, Target, `Copies/L WW`)

# BCoV data
bcov_data <- read_xlsx(path = flpath_extension,
                       sheet = 'EZ BCoV Data')


# Process data ----

N_data_to_plot <- xldata %>% 
  filter(!str_detect(WWTP, 'PT|NE')) %>% 
  mutate(across(Facility, ~ if_else(is.na(.x), 'Water', Facility))) %>% 
  mutate(across(WWTP, ~ fct_relevel(.x, 'DI', after = Inf))) %>% # bringing DI water control to the last position
  mutate(Sample_name = WWTP) # dummy column for the plotting function
         

N_data_list <- list(raw.dat = N_data_to_plot,
                  summ.dat = N_data_to_plot %>% 
                    group_by(across(-`Copies/L WW`)) %>% 
                    summarize_all(.funs = lst(mean, sd), na.rm = T))


# BCoV

BCoV_data_to_plot <- bcov_data %>% 
  filter(!str_detect(WWTP, 'PT|NE')) %>% 
  mutate(across(Facility, ~ if_else(is.na(.x), 'Water', Facility))) %>% 
  mutate(Target = 'BCoV') # dummy column, only necessary for plotting

bcov_data_list <- list(raw.dat = BCoV_data_to_plot,
                       summ.dat = BCoV_data_to_plot %>% 
                         group_by(across(-Fraction_recovered)) %>% 
                         summarize_all(.funs = lst(mean, sd), na.rm = T))



# Plots1 ----

plt.N1 <- {plot_mean_sd_jitter(.data_list = N_data_list,
                            long_format = F,
                            target_filter = 'N1',
                            x_var = `Concentration method`, y_var = `Copies/L WW`,
                            colour_var = `Concentration method`,
                            facet_var = `Concentration method`,
                            ascending_order = T,
                            ylabel = 'Genome copies/L Wastewater', title_text = 'SARS-CoV2 N1 across methods') + 
    theme(legend.position = 'top', axis.text.x = element_blank())} %>% 
  
  format_logscale_y(.) %>% 
  print()

plt.N2 <- {plot_mean_sd_jitter(.data_list = N_data_list,
                              long_format = F,
                              target_filter = 'N2',
                              x_var = `Concentration method`, y_var = `Copies/L WW`,
                              facet_var = WWTP, colour_var = `Concentration method`,
                              ylabel = 'Genome copies/L Wastewater', title_text = 'SARS-CoV2 N2 across methods')  + 
    theme(legend.position = 'top', axis.text.x = element_blank())} %>% 
  
  format_logscale_y(.) %>% 
  print()

plt.recovery <- {plot_mean_sd_jitter(.data_list = bcov_data_list,
                               long_format = F,
                               target_filter = '.*',
                               x_var = `Concentration method`, y_var = Fraction_recovered,
                               facet_var = WWTP, colour_var = `Concentration method`,
                               ylabel = 'Fraction of surrogate virus recovered', title_text = 'Surrogate recovery across methods')  + 
    theme(legend.position = 'top', axis.text.x = element_blank())} %>% 
  
  format_logscale_y(.) %>%
  print()

# Plots-kiara ----


shape_plt_N1 <- {N_data_to_plot %>% 
  filter(str_detect(Target, 'N1')) %>% 
  ggplot(aes(`Concentration method`, `Copies/L WW`, colour = `Concentration method`,  shape = WWTP)) + 
  geom_jitter() + 
  ylab('Genome copies/L wastewater') + ggtitle('SARS-CoV2 N1 across methods') + 
    scale_shape_manual(values = c(15,16,17,7,8,10,3))} %>% 
  format_logscale_y() %>%
  format_classic() %>% 
  print()

shape_plt_N2 <- {N_data_to_plot %>% 
    filter(str_detect(Target, 'N2')) %>% 
    ggplot(aes(`Concentration method`, `Copies/L WW`, colour = `Concentration method`,  shape = WWTP)) + 
    geom_jitter() + 
    ylab('Genome copies/L wastewater') + ggtitle('SARS-CoV2 N2 across methods') + 
    scale_shape_manual(values = c(15,16,17,7,8,10,3))} %>% 
  format_logscale_y() %>%
  format_classic() %>% 
  print()


shape_plt_recovery <- {BCoV_data_to_plot %>% 
  ggplot(aes(`Concentration method`, Fraction_recovered, colour = `Concentration method`, shape = WWTP)) + 
  geom_jitter() + 
  ylab('Fraction of surrogate virus recovered') + ggtitle('Surrogate recovery across methods')} %>% 
  format_classic(.) %>% 
  print()

# Data output ----
# (optional)

# plots-1
# ggsave('qPCR analysis/Methods paper/Methods_paper_N1.pdf', plot = plt.N1, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/Methods_paper_N2.pdf', plot = plt.N2, width = 8, height = 4)
# ggsave('qPCR analysis/Methods paper/Methods_paper_Recovery.pdf', plot = plt.recovery, width = 8, height = 4)

# plots-kiara
ggsave('qPCR analysis/Methods paper/Methods_paper_N1.pdf', plot = shape_plt_N1, width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/Methods_paper_N2.pdf', plot = shape_plt_N2, width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/Methods_paper_Recovery.pdf', plot = shape_plt_recovery, width = 8, height = 4)
