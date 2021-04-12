# Plots for qPCR standard curves across freeze-thaws
# Pull all std curve parameters from the data dump/Standard curves sheet and plot their parameters

# Easy way first : Load the slope and intercepts and run them

# Prelims
source('./general_functions.R') # Source the general_functions file

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

# Plot full curve ----
ggplot(mapping = aes(x = 3:5, y = 35 - 3 * 3:5)) +
  geom_point() +
  geom_abline(data = std.bcov, 
              aes(slope = Slope, intercept = y_intercept, colour = std.ID), 
              size = 1.5 #, alpha = .4
              ) +
  ggtitle('BCoV standard curves reconstructed: across freeze thaws',
          subtitle = 'No clear trend of increasing intercept with increasing Std.ID (time)') + 
  scale_colour_viridis_b()

ggsave('qPCR analysis/Extra graphs/BCoV_standard_curve_evolution_reconstructed.png')
