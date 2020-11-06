
# get data from the complete data (1019 Rice) into presentable_data
# presentable_data <- read_sheet(sheeturls$complete_data, sheet = '1019 Rice')

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


d.forplot <- d.interim %>% unnest(cols = c(data)) %>% 
  mutate(Phase = str_replace_all(WWTP, c('.*R$' = 'Residue', '.*RF$' = 'Filtered residue') ),
         Phase = if_else(str_detect(Facility, '/'), Phase, 'Supernatant'), .after = WWTP)


# Plots ---- 

# (this was processed in google sheets)
d1.input <- read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1g6-Z0kpItuffidKEzjv-jvcTHzZPUQp9LxyTeKLngV4/edit#gid=0', 
                         sheet = 'Correct Analysis N1, N2, BCoV') 

d1.forplot <- d1.input %>% drop_na(Facility) %>% 
  mutate(Site = str_replace(WWTP, '(.)R', '\\1'), .after = WWTP) %>% 
  
  mutate_cond(str_detect(Tube_ID, '1020 (B|D|E|F).$'), Site  = str_replace(Tube_ID, '1020 (.)[:digit:]', '\\1')) %>% 
  mutate(across(Category, ~ fct_relevel(., 'Supernatant', 'Residue') ) ) %>% 
  arrange(Category, `Target Name`, Site) %>% 
  mutate(serialnum = row_number(),.after = Tube_ID)

# plot1
d1.forplot %>% 
  ggplot(mapping = aes(x = serialnum, y = `% of Sample`, colour = Site)) + 
  geom_jitter(width = .2) + 
  facet_grid( ~ Category, scale = 'free_x') + 
  xlab(NULL) + ylab('Partitioning of virus (%)') +
  theme_bw() # change the plot look 

ggsave('qPCR analysis/Extra graphs/1020 Supernatent vs pellet.pdf', height = 4, width = 6)

# plot 2
d1.forplot %>% 
  ggplot(mapping = aes(x = Category, y = `% of Sample`, colour = Site)) + 
  geom_jitter(width = .2) + 
  facet_grid( ~ `Target Name`, scale = 'free_x') + 
  xlab(NULL) + ylab('Partitioning of virus (%)') +
  theme_bw() # change the plot look 

ggsave('qPCR analysis/Extra graphs/1020 Supernatent vs pellet_by target.pdf', height = 4, width = 6)

# plot 3
plt3 <- {d1.forplot %>% 
  ggplot(mapping = aes(x = Category, y = `Copies/l WW`, colour = Site)) + 
  geom_jitter(width = .2) + 
  facet_grid( ~ `Target Name`, scale = 'free_y') + 
  xlab(NULL) + 
  theme_bw() # change the plot look  %>% 
} %>% 
  format_logscale_y() %>% print()

ggsave('qPCR analysis/Extra graphs/Pavans 1020 residue/1020 sup vs residue_copiesperl ww_logscale_by target.pdf', height = 4, width = 6)


# plot 4
plt4 <- {d1.forplot %>% 
    ggplot(mapping = aes(x = `Target Name`, y = `Copies/l WW`, colour = Site)) + 
    geom_jitter(width = .5) + 
    facet_grid( ~ Category, scale = 'free_y') + 
    xlab(NULL) + 
    theme_bw() # change the plot look  %>% 
} %>% 
  format_logscale_y() %>% print()

ggsave('qPCR analysis/Extra graphs/Pavans 1020 residue/1020 sup vs residue_copiesperl ww_logscale.pdf', height = 4, width = 6)


# plot5
d1.forplot %>% 
  filter(str_detect(`Target Name`, 'BCoV')) %>% 
  ggplot(mapping = aes(x = Category, y = `Copies/l WW`, colour = Site)) + 
  geom_jitter(width = .2) + 
  facet_grid( ~ `Target Name`, scale = 'free_x') + 
  xlab(NULL) + #ylab('Partitioning of virus (%)') +
  ggtitle('SARS COV2 BCoV supernatant vs residue') +
  theme_bw() # change the plot look 

ggsave('qPCR analysis/Extra graphs/Pavans 1020 residue/1020 copies_L BCoV.pdf', height = 4, width = 6)

write_csv(d1.forplot, 'Pavans plotting data.csv', na = '')
