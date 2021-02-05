# Ad-hoc plots for qPCR standard curves across freeze-thaws

# Easy way first : Load the slope and intercepts and run them

# Load slope_intercepts ----
std.data <- read_sheet(sheeturls$data_dump, sheet = 'Standard curves')

# filter BCoV std curves
std.bcov <- std.data %>% 
  filter(str_detect(Target, 'BCoV')) %>% 
  mutate(std.ID = str_replace_all(ID, 'Std([:digit:]*).*', '\\1') %>% as.numeric()) %>% 
  drop_na()


# plotting ----

# slope
std.bcov %>% 
  ggplot(aes(std.ID, Slope)) + 
  geom_point() + geom_line() + 
  ggtitle('BCoV standard curves: Slope across freeze thaws')

ggsave('qPCR analysis/Extra graphs/BCoV_standard_curve_evolution_slope.png')

# intercept
std.bcov %>% 
  ggplot(aes(std.ID, y_intercept)) + 
  geom_point() + geom_line() + 
  ggtitle('BCoV standard curves: Y_intercept across freeze thaws')

ggsave('qPCR analysis/Extra graphs/BCoV_standard_curve_evolution_yint.png')
