# Publication and Ad hoc plots for concentration methods paper
# Data distributed across sheets in an excel file

# Preliminaries ----
# file name
input_sheet <- 'Concentration methods paper-2'

# method_namer <- c('ha.*' = 'HA Filtration',
#                   '^e.*' = 'Elution',
#                   'uf.*' = 'Ultrafiltration',
#                   'de.*' = 'Direct Extraction') 

# Naming scheme for plot titles
title_namer <- c('N1' = 'SARS-CoV2 N1 across methods',
                 'N2' = 'SARS-CoV2 N2 across methods',
                 'BCoV' = 'Surrogate virus (BCoV) across methods',
                 'pMMoV' = 'Internal control: Pepper-mild-mottle virus across methods')  

# naming scheme for plot y axis labels
y_axis_namer <- c('Copies/L WW' = 'Genome copies/L wastewater',
                  'Copies/uL RNA' = 'Genome copies/uL RNA extract',
                  'Fraction.recovered' = 'Fraction of surrogate virus recovered')

# folder to save
sv.folder_namer <- c('Copies/L WW' = 'Copies ww',
                  'Copies/uL RNA' = 'Copies RNA',
                  'Fraction.recovered' = 'Recovery')


# Loading libraries, functions and user inputs
source('./general_functions.R') # Source the general_functions file

# Metadata ----

loq_data <- read_sheet('https://docs.google.com/spreadsheets/d/1_32AE3IkBRD3oGSYcYqZwknHZEHiGKtoy1zK5VVTzsI/edit#gid=463904425',
                       sheet = 'LOQ')

# Input file ----

all_data_input <- read_sheet(sheeturls$complete_data, sheet = input_sheet) %>% 
  rename(`Concentration method` = Concentration_method, 
         Target = `Target Name`,
         Fraction.recovered = `Recovery fraction`,
         Detection.limit = Detection_Limit,
         `Copies/L WW` = 'Copies/l WW', `Copies/uL RNA` = 'Copies/ul RNA') %>% # for compatibility with old plotting functions
  mutate(across(WWTP, ~ fct_relevel(.x, 'DI', after = Inf))) %>%  # bringing DI water control to the last position
  filter(!str_detect(Facility, 'Std|0|Vac')) %>% 
  
  # atach LOQs
  left_join(loq_data)

# filtering data ----

only_wwtps <- all_data_input %>% 
  filter(!str_detect(WWTP, 'DI|NTC')) # remove negative controls

# Removing direct extraction
no_direct_only_wwtps <- only_wwtps %>% 
  filter()

data_to_plot <- only_wwtps

# Summary and long_format ----

minimal_label_columns <- c('Target', 'WWTP', 'Concentration method')

# convert data to wide format - for plotting correlation/scatter plot
scatter_data_N_reco <- only_wwtps %>% 
  select(all_of(minimal_label_columns), Tube_ID, `Copies/L WW`, Fraction.recovered) %>% 
  pivot_wider(names_from = Target, values_from = c('Copies/L WW', Fraction.recovered))

wider_copies_data <- data_to_plot %>% 
  pivot_longer(cols = c(`Copies/uL RNA`, `Copies/L WW`), names_to = 'Copy data', values_to = 'Copies')

 
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

# Plotting functions ----

individual_plots <- function(.data_to_plot = data_to_plot, 
                             target_string = 'N1', 
                             y_var = `Copies/L WW`, 
                             plt.log = 'Y', 
                             facet.formula = as.formula(~`Concentration method`), 
                             plt.LOQ = 'yes', 
                             plt.save = 'yes',
                             plt.format = 'pdf', plt.width = 8, plt.height = 4)
{ 
  
  # preliminary conversions
  plt.title <- str_replace_all(target_string, title_namer) # title of plot
  plt.y_label <- substitute(y_var) %>% paste() %>%  # y axis label
    str_replace_all(y_axis_namer) 
  sv.folder <- substitute(y_var) %>% paste() %>%  # save folder
    str_replace_all(sv.folder_namer) 
  
  # Remove LOQ for recovery fraction and pMMoV
  if(str_detect(plt.y_label, 'Recovery') | str_detect(target_string, 'pMMoV|BCoV')) plt.LOQ <- 'no'
  # LOQ for copies/ul RNA
  if(str_detect(plt.y_label, 'RNA')) LOQ_var <- expr(LOQ_RNA) # place holder LOQ for ddPCR in copies/ul RNA
  else LOQ_var <- expr(LOQ)
 
  plt.alpha <- if_else(plt.LOQ == 'yes', 0.5, 1) 
    
  # Plotting
  plt <- {.data_to_plot %>% 
      filter(str_detect(Target, target_string)) %>% 
      
      ggplot(aes(WWTP, {{y_var}}, colour = `Concentration method`,  shape = WWTP)) + 
      geom_point(alpha = plt.alpha) + 
      facet_grid(facet.formula) +
      ylab(plt.y_label) + ggtitle(plt.title) + 
      scale_shape_manual(values = c(15,16,17,7,8,10,3)) +
      
      {if(plt.LOQ == 'yes') 
        list(geom_hline(aes(yintercept = {{LOQ_var}})),
             geom_point(data = . %>% filter({{y_var}} > {{LOQ_var}}) )
             # annotate(data = loq_data, geom = 'rect', xmin = -Inf, xmax = +Inf, ymin = -Inf, ymax = {{LOQ_var}}, alpha = .3),
             # annotate(data = loq_data, geom = 'text', x = Inf, y = {{LOQ_var}}, hjust = 'inside', label = 'Limit of Quantification')
        )
      } 
    
  } %>% 
    
    {if(plt.log == 'Y') format_logscale_y(.)} %>%
    
    format_classic() %>% 
    print()
  
  
  # Saving plot
  
  
  # work in progress, need to make sv.folder and plt.format as function inputs
  if(plt.save == 'yes') str_c('qPCR analysis/Methods paper/', sv.folder,  '/', target_string, '.', plt.format) %>%
      ggsave(width = plt.width, height = plt.height)
  
  return(plt)

}


# Plot save function ----

save_plot <- function(plt.id, plt.name, sv.folder = 'Copies ww', plt.format = 'pdf', plt.width = 8, plt.height = 4)
{
  str_c('qPCR analysis/Methods paper/', sv.folder,  '/', plt.id, '.', plt.format) %>% 
    ggsave(plot = plt.name, width = plt.width, height = plt.height)
  
}

# quick function for vectorization of making plots and saving
# leave it alone for now

# Plots ----

N_ww <- c('low' = 500, 'high' = 4e5)
N_RNA <- c('low' = .1, 'high' = 20)

rmarkdown::render('conc_methods_allfigs.rmd', 
                  output_file = str_c('./qPCR analysis/Methods paper/', 'all_figs_without DI,NTC' , '.html'))


# Plots with LOQ same scale ----

# Identify min and max per target: manually
data_to_plot %>% filter(str_detect(Target, 'N1|N2')) %>% 
  pull(`Copies/L WW`) %>% max()
