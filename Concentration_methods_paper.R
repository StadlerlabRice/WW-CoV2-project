# Ad hoc plots for concentration methods paper
# Data distributed across sheets in an excel file

# Preliminaries ----
# file name
flnm <- 'Archive/Concentration Methods Data V2 New ddPCR data'

method_namer <- c('ha.*' = 'HA Filtration',
                  '^e.*' = 'Elution',
                  'uf.*' = 'Ultrafiltration',
                  'de.*' = 'Direct Extraction') 
  
# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file


# Input file ----

flpath <- str_c('excel files/',flnm,'.xlsx') # this completes the file path
read_these_sheets <- c('HA Filtration',
                       'Elution',
                       'Ultrafiltration',
                       'Direct Extraction'
                       )

xldata <- map_df(read_these_sheets, ~ read_xlsx(path = flpath,
                                             sheet = .x,
                                             range = 'A2:N54') %>% 
                                    mutate(Concentration_method = .x)
              ) %>% 
  select(Facility, WWTP, Concentration_method, Target, `Copies/L WW`)

# BCoV data
bcov_data <- read_xlsx(path = flpath,
                       sheet = 'EZ BCoV Data')


# Process data ----

N_data_to_plot <- xldata %>% 
  filter(!str_detect(WWTP, 'PT|NE')) %>% 
  mutate(across(Facility, ~ if_else(is.na(.x), 'Water', Facility))) %>% 
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
                         group_by(across(-Percentage_recovered)) %>% 
                         summarize_all(.funs = lst(mean, sd), na.rm = T))



# Plots ----

plt.N1 <- {plot_mean_sd_jitter(.data_list = N_data_list,
                            long_format = F,
                            target_filter = 'N1',
                            x_var = Concentration_method, y_var = `Copies/L WW`,
                            colour_var = Concentration_method,
                            facet_var = Concentration_method,
                            ascending_order = T,
                            ylabel = 'Genome copies/L Wastewater', title_text = 'SARS-CoV2 N1 across methods') + 
    theme(legend.position = 'top', axis.text.x = element_blank())} %>% 
  
  format_logscale_y(.) %>% 
  print()

plt.N2 <- {plot_mean_sd_jitter(.data_list = N_data_list,
                              long_format = F,
                              target_filter = 'N2',
                              x_var = Concentration_method, y_var = `Copies/L WW`,
                              facet_var = WWTP, colour_var = Concentration_method,
                              ylabel = 'Genome copies/L Wastewater', title_text = 'SARS-CoV2 N2 across methods')  + 
    theme(legend.position = 'top', axis.text.x = element_blank())} %>% 
  
  format_logscale_y(.) %>% 
  print()

plt.recovery <- {plot_mean_sd_jitter(.data_list = bcov_data_list,
                               long_format = F,
                               target_filter = '.*',
                               x_var = Concentration_method, y_var = Percentage_recovered,
                               facet_var = WWTP, colour_var = Concentration_method,
                               ylabel = 'Fraction of surrogate virus recovered', title_text = 'Surrogate recovery across methods')  + 
    theme(legend.position = 'top', axis.text.x = element_blank())} %>% 
  
  format_logscale_y(.) %>%
  print()


# Data output ----
# (optional)
ggsave('qPCR analysis/Methods paper/Methods_paper_N1.png', plot = plt.N1, width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/Methods_paper_N2.png', plot = plt.N2, width = 8, height = 4)
ggsave('qPCR analysis/Methods paper/Methods_paper_Recovery.png', plot = plt.recovery, width = 8, height = 4)

