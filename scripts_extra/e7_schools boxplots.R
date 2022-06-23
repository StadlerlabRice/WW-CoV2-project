# Plotting school samples by grade and enrollment from collated data sheet
# Prashant
# 11 April 2022

# Loading libraries, functions and user inputs
source('./0-general_functions_main.R') # Source the general_functions file


title_name <- 'Schools ordered by enrollment_7-April-22'

# Load files ----
all_data <- read_xlsx('excel files/boxplots_for_LS_7-4-22/Wastewater Surveillance manhole Report 03282022.xlsx')

# school_metadata <- read_xlsx('excel files/boxplots_for_LS_7-4-22/HISD_school_list_clean.xlsx') %>% 
#   
#   # join to get shortened name of the schools as listed in the data file
#   right_join(read_xlsx('excel files/boxplots_for_LS_7-4-22/Wastewater Schools in PCR testing program - SCHOOL ID - 02142022.xlsx'),
#              by = 'school_id')

# loading the custom generated file with school names and symbols
school_metadata <- read_csv('excel files/boxplots_for_LS_7-4-22/schools_metadata.csv') %>% 
  # reducing redundant columns -- Sharpstown International only one different : Elementary/Secondary and High school
  select(-Grade_Level) %>% rename(Grade_Level = Grade_Level_2)  # retain Grade_Level_2 and rename to Grade_Level

# View data
# view(school_data)


# polishing ----

# filter rice only data that is positive or inconclusive
school_rice.data <- 
  
  all_data %>%
  
  # Filter relevant data
  filter(Lab == 'Rice',             # select 'Rice' data
         `Current Result` != 'Negative',      # non negative
         `Facility Type` == 'School') %>%   # only from Schools
  
  # Attach school metadata
  left_join(school_metadata, by = 'Facility') %>% 
  
  # Order by enrollment
  arrange(Enrollment_as_Oct2019) %>% 
  mutate(across(c('Facility', 'Symbol'), # freeze the order of samples 
                ~ fct_inorder(.x))) %>% 
 
  # Collect all replicates and N1-N2 under the same column
  pivot_longer(cols = starts_with('Rep'),
               names_to = 'Replicate',
               values_to = 'Copies_per_litre') %>% 
  
  # order the grade levels for proper plotting
  mutate(across(Grade_Level, ~ fct_relevel(.x, 'Elementary', 'Middle', 'High School')))

# Average replicates and N1, N2
school_averaged <- 
  group_by(school_rice.data, across(-c(Copies_per_litre, Replicate))) %>% 
  summarize(across(Copies_per_litre, mean, na.rm = TRUE)) # takes the mean of stuff
  
  # group_by(Facility, Date) %>% 
  # mutate(across(Copies_per_litre, mean, na.rm = TRUE)) %>% 
  # select(-Replicate)
  

# function to label the number of data points on the plot
# Copied from stackoverflow : https://stackoverflow.com/a/31140454/9049673
give.n <- function(x){ 
  return(c(y = mean(x), label = length(x)))
}


# Plots ----

# make box and dot plot

# boxplot all ----

plt.general <- 
  {ggplot(school_rice.data,
          aes(x = Replicate, y = Copies_per_litre,
              fill = Grade_Level,
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
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      ggtitle('School data by grade : till 7th April, 2022',
              subtitle = 'Number of data points shown above the median line. Whiskers show 25% and 75% quartiles') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()


# Boxplot avg ----

# boxplot of average of replicates and targets
plt.avg <- 
  {ggplot(school_averaged,
          aes(x = 1, y = Copies_per_litre,
              fill = Grade_Level,
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
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      ggtitle('School data by grade : till 7th April, 2022',
              subtitle = 'Number of data points shown above the median line. Whiskers show 25% and 75% quartiles') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()

# by enrollment ----

# Each school - by enrollment 
plt.eachschoolenrollment <- 
  {ggplot(school_rice.data,
          aes(x = Enrollment_as_Oct2019, y = Copies_per_litre,
              fill = Replicate,
              label = Facility)) + 
      
      # boxplot
      # geom_boxplot(mapping = aes(colour = NA),
      #              outlier.shape = NA, alpha = .4) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(shape = 21,
                 alpha = 0.2, 
                 position = position_jitterdodge(dodge.width = .75, 
                                                 jitter.width = .12, jitter.height = 0)) + 
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      ggtitle('School data by enrollment : till 7th April, 2022',
              subtitle = 'Number of data points shown above the median line. Whiskers show 25% and 75% quartiles') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()


# by each school ----

# plot by each school - ordered by enrollment
plt.eachschool <- 
  {ggplot(school_averaged,
          aes(x = Copies_per_litre, y = Symbol,
              label = Facility)) + 
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.4) + 
      
      # annotations
      annotation_logticks(sides = 'b') +  # tick marks to indicate log y axis
      
      geom_text(aes(x = 3e2, label = Enrollment_as_Oct2019),
                colour = 'grey',
                show.legend = F) + 
      
      ggtitle('School data by enrollment : till 7th April, 2022',
              subtitle = 'School enrollment numbers as of Oct2019 are shown on the left') +
      ylab('School name (abbreviated)') +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_x() %>%
  
  print()


# Save plots ----

# ggsave(str_c('qPCR analysis/', title_name, '.png'), width = 6, height = 8)

ggsave(plot_as('Schools by grade_7-April-22'), plt.general, width = 5, height = 6)
ggsave(plot_as('Schools by grade-Avg_7-April-22'), plt.avg, width = 4, height = 6)
ggsave(plot_as('Schools_by school ordered by enrollment_7-April-22'), plt.eachschool, width = 6, height = 9)



# save plots into html file
# calling r markdown file -- Abandoned for now
rmarkdown::render('scripts_extra/e6_all school overview.Rmd', 
                  output_file = str_c('./qPCR analysis/', title_name, '.html'))

