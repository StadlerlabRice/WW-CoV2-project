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
                 'BCoV' = 'Surrogate recovery across methods',
                 'pMMoV' = 'Internal control: Pepper-mild-mottle virus across methods')  

# naming scheme for plot y axis labels
y_axis_namer <- c('Copies/L WW' = 'Genome copies/L wastewater',
                  'Copies/uL RNA' = 'Genome copies/uL RNA extract',
                  'Fraction.recovered' = 'Fraction of surrogate virus recovered')

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

# convert data to wide cormat - for plotting correlation/scatter plot
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

individual_plots <- function(.data_to_plot = data_to_plot, target_string = 'N1', y_var = `Copies/L WW`, plt.log = 'Y', facet.formula = as.formula(~`Concentration method`), plt.LOD = 'no')
{ 
  plt.title <- str_replace_all(target_string, title_namer)
  plt.y_label <- substitute(y_var) %>% paste() %>% 
    str_replace_all(y_axis_namer)
  
  LOD <- 0.735 # place holder LOD for ddPCR in copies/ul RNA
  # if(str_detect(plt.y_label, 'Recovery') | str_detect(target_string, 'pMMoV')) plt.LOD <- 'no' 
  # else plt.LOD <- 'yes' # LOD is hardcoded in function call for now
 
  {.data_to_plot %>% 
      filter(str_detect(Target, target_string)) %>% 
      ggplot(aes(WWTP, {{y_var}}, colour = `Concentration method`,  shape = WWTP)) + 
      geom_point() + 
      facet_grid(facet.formula) +
      ylab(plt.y_label) + ggtitle(plt.title) + 
      scale_shape_manual(values = c(15,16,17,7,8,10,3)) +
      
      {if(plt.LOD == 'yes') 
        list(annotate(geom = 'rect', xmin = -Inf, xmax = +Inf, ymin = -Inf, ymax = LOD, alpha = .3),
            geom_hline(yintercept = LOD)#,
            # annotate(geom = 'text', x = Inf, y = LOD, label = 'Limit of detection')
            )
      } 
    
  } %>% 
    
    {if(plt.log == 'Y') format_logscale_y(.)} %>%
    
    format_classic() %>% 
    print()
}


# Plot save function ----

save_plot <- function(plt.id, plt.name, sv.folder = 'Copies ww', plt.format = 'pdf', plt.width = 8, plt.height = 4)
{
  str_c('qPCR analysis/Methods paper/', sv.folder,  '/', plt.id, '.', plt.format) %>% 
    ggsave(plot = plt.name, width = plt.width, height = plt.height)
  
}

# quick function for vectorization of making plots and saving
# leave it alone for now

# Plots w shapes ----

rmarkdown::render('conc_methods_allfigs.rmd', 
                  output_file = str_c('./qPCR analysis/Methods paper/', 'all_figs_without DI,NTC' , '.html'))


# Faceted plots with LOD ----

individual_plots(wider_copies_data %>% 
                   filter(str_detect(Target, 'SARS'), str_detect(`Copy data`, 'RNA')),
                 target_string = '.*', y_var = Copies, facet.formula = as.formula(Target ~ `Concentration method`), plt.LOD = 'yes')  
  # facet_grid(`Copy data` ~ Target, scales = 'free')

plt.LOD = 'yes'

data_to_plot %>% 
  filter(str_detect(Target, 'SARS')) %>% 
  
{ggplot(., aes(WWTP, `Copies/uL RNA`, colour = `Concentration method`, shape = WWTP)) + 
  geom_point() + 
  facet_grid(Target ~ `Concentration method`, scale = 'free_x') + 
    ylab(plt.y_label) + ggtitle(plt.title) + 
    scale_shape_manual(values = c(15,16,17,7,8,10,3)) +
    
    {if(plt.LOD == 'yes') 
     {annotate(geom = 'rect', xmin = -Inf, xmax = +Inf, ymin = -Inf, ymax = LOD) + 
        geom_hline(yintercept = LOD) + 
        annotate(geom = 'text', x = Inf, y = LOD)
      }
    } 
}
