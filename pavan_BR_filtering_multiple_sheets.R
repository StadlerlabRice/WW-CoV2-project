
# get data from the complete data (1019 Rice) into presentable_data
presentable_data <- read_sheet(sheeturls$complete_data, sheet = '1019 Rice')

#                                                 Pavan's special runs   | The original processing for the same samples
d.pavan <- presentable_data %>% filter(str_detect(Facility, '102./.R.*') | str_detect(Tube_ID, '1020 (B|D|E|F)'))
  

d.interim <- d.pavan %>% 
  mutate(catcol = str_extract(WWTP, '.R'), .after = WWTP) %>% 
  
  mutate_cond(str_detect(Tube_ID, '1020 (B|D|E|F).$'), catcol  = str_replace(Tube_ID, '1020 (.)[:digit:]', '\\1R')) %>% 

  group_by(catcol) %>% 
  arrange(catcol, `Target Name`, Tube_ID, WWTP) %>% 
  nest()

map2(d.interim$data, d.interim$catcol, 
     ~ write_sheet(.x, sheet = .y, 
                   ss = 'https://docs.google.com/spreadsheets/d/1g6-Z0kpItuffidKEzjv-jvcTHzZPUQp9LxyTeKLngV4/edit#gid=0')
)

  
d.interim %>% unnest(cols = data) %>% 
  filter(str_detect(`Target Name`, 'BCoV')) %>% 
  write_sheet(sheet = 'Analysis',
              ss = 'https://docs.google.com/spreadsheets/d/1g6-Z0kpItuffidKEzjv-jvcTHzZPUQp9LxyTeKLngV4/edit#gid=0')

  