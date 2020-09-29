# Ad hoc plots for concentration methods paper
# Data distributed across sheets in an excel file

# Preliminaries ----
# file name
flnm <- 'Archive/Concentration Methods Data V2 New ddPCR data'
  
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

# Process data ----
data_to_plot <- xldata %>% 
  filter(!str_detect(WWTP, 'PT|NE')) %>% 
  mutate(across(Facility, ~ if_else(is.na(.x), 'Water', Facility))) %>% 
  mutate(Sample_name = WWTP) # dummy column for the plotting function
         

data_list <- list(raw.dat = data_to_plot,
                  summ.dat = data_to_plot %>% 
                    group_by(across(-`Copies/L WW`)) %>% 
                    summarize_all(.funs = lst(mean, sd), na.rm = T))

# Plots ----
plt1 <- plot_mean_sd_jitter(.data_list = data_list,
                            long_format = F,
                            measure_var = 'Recovered', sample_var = str_c(extra_categories, '|NTC|Vaccine'), exclude_sample = T, x_var = WWTP, ascending_order = T, ylabel = 'Genome copies/l Wastewater')


# Data output ----
# (optional)


