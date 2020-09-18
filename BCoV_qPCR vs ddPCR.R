# ad hoc - BCOV qPCR vs ddPCR

# load data ----
dddat <- read_sheet(sheeturls$complete_data, sheet = 'Triplex testing') %>% 
  filter(`Target Name` == 'BCoV2')

qdat <- read_sheet(sheeturls$complete_data, sheet = '907 Rice') %>% 
  filter(`Target Name` == 'BCoV_M') %>% 
  
  filter(Facility %in% dddat$Facility)
 
comb.dat <- bind_rows(dddat, qdat)

{ggplot(comb.dat, aes(x = WWTP, y = `Copies/l WW`, colour = `Target Name`)) + 
  geom_point() + annotate(geom = 'text', x = 'BW', y = 1e8, label = 'Maximum detection range')} %>%  
  format_logscale_y() %>% 
  print()

ggsave('qPCR analysis/BCoV qPCR vs ddPCR.png', height = 4, width = 6)
