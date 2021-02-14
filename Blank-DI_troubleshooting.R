# investigating blanks

# prelims ----
# running general functions
source('C:/Users/new/Box Sync/Stadler lab/Data/COVID-qPCR work/general_functions.R')

rts <- c('dd.WW126_Schools+WWTP_N1N2', 'dd.WW127_Schools+WWTP_B117', 'dd.WW128_Schools_N1N2', 
         'dd.WW129_0210_Con+0209_Schools_Rerun', 'dd.WW130_0209_WWTP_B117Redo', 'dd.WW131_0209_WWTP_N1N2Redo')

# input ----

# Get and merge the two datasets from data dump
raw_quant_data <- map_dfr(rts, 
                          ~ read_sheet(sheeturls$data_dump, sheet = .x, range = 'A:L') %>%
                            filter(str_detect(assay_variable, regex('^DI|BLANK', ignore_case = TRUE)) | str_detect(Sample_name, 'NTC')) %>% 
                            mutate(run_ID = str_extract(.x, 'dd.WW([:digit:]*)'), .before = 1,
                                   across(where(is.list), as.character),
                                   across(biological_replicates, as.numeric),
                                   across(assay_variable, ~ str_replace(., '^0$', 'NTC')))
)

# simple processing ----
mid_quant_dat <- raw_quant_data %>% 
  mutate(across(assay_variable, ~ str_replace(.x, regex('blank', ignore_case = TRUE), 'BLANK')), # replace all versions of blank with BLANK
         run_num = str_match(run_ID, '[:digit:]+'), # Get all the digits
         catgry = if_else(run_num < 129 , 'First run', 'Re-run/clean')) # distinguish old from reruns

# plot ----

hzontal_jiggle <- position_jitter(height = 0, width = .3, seed = 1L) # standard jitter with reproducible randomness
# Source: https://github.com/tidyverse/ggplot2/pull/1996#issue-101459372

ggplot(mid_quant_dat, 
       aes(x = assay_variable, y = Positives, colour = run_ID)) + 
  geom_point(position = hzontal_jiggle) +
  
  # geom_text_repel(data = . %>% 
  #                   filter(str_detect(catgry, 'clean') & Positives > 0), 
  #                 aes(label = Positives),
  #                 position = hzontal_jiggle, box.padding = 1, # labelling points with the same jitter
  #                 show.legend = F) +
  
  facet_grid(~ catgry) # facetting for the category

ggsave('qPCR analysis/Extra graphs/dd.126-131_DI-blanks.png', width = 7, height = 4)
