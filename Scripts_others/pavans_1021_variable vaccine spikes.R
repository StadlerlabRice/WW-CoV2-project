# pavan 1021 filtering

# prelim ----
read_these_sheets <- c('dd.WW64_1019_BCoV', 'dd.WW66_1019_Manhole_N1N2_BCoV')

# data input ----
raw_dat <- map_df(read_these_sheets, ~ read_sheet(sheeturls$data_dump, sheet = .))

# data manipulation ----
fl.dat <- raw_dat %>% filter(str_detect(Sample_name, '1021'), !str_detect(assay_variable, 'DI')) %>% 
  mutate(across(assay_variable, as.numeric)) %>% 
  arrange(Target, assay_variable) %>% 
  select(Sample_name, assay_variable, `Copy #`, Positives, `Accepted Droplets`, everything()) %>% 
  view

{fl.dat %>% filter(str_detect(Target, 'BCoV')) %>% 
  ggplot(aes(assay_variable, `Copy #`)) +
    geom_point() + geom_line() +
    geom_point(mapping = aes(y = Positives/`Accepted Droplets`), colour = 'red') + geom_line(mapping = aes(y = Positives/`Accepted Droplets`), colour = 'red')
} %>% 
  format_logscale_y() %>% 
  print # %>% plotly::ggplotly(dynamicTicks = T)  

{fl.dat %>% filter(str_detect(Target, 'BCoV')) %>% 
  ggplot(aes(assay_variable, Positives)) +
  geom_point() + geom_line()} %>% 
  plotly::ggplotly(dynamicTicks = T)
