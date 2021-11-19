
# Run 3-weekly..R first

# Lauren was interested in how the points compare to the LOD..

plt_lod <- plot_mean_sd_jitter(results_abs,
                    sample_filtering_var = extra_categories, exclude_sample = T,
                    target_filter_var = 'pMMoV',
                    x_var = Date, y_var = Copies_Per_Liter_WW, colour_var = Target_Name, 
                    ylabel = 'Genome copies/l Wastewater',
                    print_the_plot = FALSE) #+ 
  # connect the mean points iwth lines
  # geom_line(aes(group = Target_Name, y = mean_Copies_Per_Liter_WW)) + 
  # facet_wrap(facets = ~WWTP, scales = 'free_x')

plt_lod + 
  geom_col(data = results_abs %>% 
             select(Date, Detection_Limit, Target_Name) %>% 
             filter(Target_Name == 'pMMoV') %>% 
             unique,
           mapping =  aes(y = Detection_Limit),
           fill = 'grey',
           alpha = 0.5,
           # orientation = 'x',
           show.legend = FALSE)

# check if LOD is unique for each date
results_abs %>% 
  select(Date, Detection_Limit, Target_Name) %>% 
  filter(Target_Name == 'pMMoV') %>% 
  unique

# NO, not unique

# This creates problems with plotting LOD ..

ggsave(str_c('qPCR analysis/Extra graphs/LOD-', title_name, '.png'))
