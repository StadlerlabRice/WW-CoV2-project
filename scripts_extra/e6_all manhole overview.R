# Plotting all manhole samples from collated data sheet
# Prashant
# 7 April 2022

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file


title_name <- 'Overview Manholes_7-April-22'
  
# Load file
manhole_data <- read_xlsx('excel files/Wastewater Surveillance Manhole Report 03282022.xlsx')

# View data
# view(manhole_data)

# filter rice only data that is positive or inconclusive
manhole_rice.data <- 
  
  manhole_data %>%
  
  filter(Lab == 'Rice',
         `Current Result` != 'Negative') %>% 
  
  pivot_longer(cols = starts_with('Rep'),
               names_to = 'Replicate',
               values_to = 'Copies_per_litre')

# function to label on plots
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}


# make plot
plt.general <- 
  {ggplot(manhole_rice.data,
          aes(x = Replicate, y = Copies_per_litre,
              fill = `Facility Type`,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(outlier.shape = NA, alpha = .6) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(shape = 21,
                 alpha = 0.2, 
                 position = position_jitterdodge(dodge.width = .75, 
                                                 jitter.width = .12, jitter.height = 0)) + 
      # text labels for number of points
      stat_summary(fun.data = give.n, geom = 'text',
                   position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks() +  # tick marks to indicate log y axis
      
      ggtitle('Manhole data till 7th April, 2022',
               subtitle = 'Number of data points shown above the median line. Whiskers show 25% and 75% quartiles') +
    
      theme(legend.position = 'top')
    
    } %>% 
  
  format_logscale_y() %>%
  
  print()


# Save plots ----

ggsave(str_c('qPCR analysis/', title_name, '.png'),
       width = 10, height = 8)


# save plots into html file
  # calling r markdown file
rmarkdown::render('scripts_extra/e6_all manhole overview.Rmd', 
                  output_file = str_c('./qPCR analysis/', title_name, '.html'))

