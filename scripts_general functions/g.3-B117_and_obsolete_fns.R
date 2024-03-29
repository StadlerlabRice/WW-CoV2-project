
# Wastewater related ----

# Specific for B117 assay ddPCR: pivots the data wider, calculates percentage of variant vs total copies  
calculate_B117_percentage_variant <- function(.dat)
{
  # processing 
  # Select relevant columns, get the variant and WT side by side and calculate the variant/ all percentage and grab only necessary data
  # take this data and join it to the source data later
  
  text_cols <- c('Facility', 'WWTP', 'Date', 'Lab', 'Sample_ID','Target_Name', 'variant_status', 'Well Position', 'Filtered_WW_vol') # select constant columns
  # value_cols <- c('Copy #', 'AcceptedDroplets', 'Positives', 'Threshold') # all the value columns that change with threshold
  value_cols <- c('Copies_per_uL_RNA', 'Copies_Per_Liter_WW', 'PositiveDroplets')
  
  # target_fused_data <- .dat %>% 
  #   mutate(., across('Target_Name', ~ paste_without_NAs(., variant_status, .sep = "-"))) # add "-Variant" or "-all" to the Target_Name
  
  processed_data_with_percentage <- .dat %>% 
    select( all_of(text_cols), all_of(value_cols)) %>%  # select only important columns
    pivot_wider(names_from = variant_status, values_from = all_of(value_cols)) %>%  # put variant and all side by side
    
    mutate(percentage_variant = (`Copies_per_uL_RNA_Variant` / `Copies_per_uL_RNA_all` * 100) %>% round(2)) %>%  # calculate % of variant, round it off
    
    select(-any_of('variant_status')) %>% # remove column
    relocate(percentage_variant, .after = 'Target_Name')
    
    # below lines will produce stacked data for both variants so the B117 data is in the same format as the N1N2 data
    # mutate(across('Target_Name', ~ str_c(., '-Variant'))) %>% # add "-Variant" to the Target_Name
    # select(any_of(text_cols), percentage_variant) # exclude the raw Copy #s from selection
  
  # mrged_data <- left_join(target_fused_data, processed_data_with_percentage) 
  
}


# read in two sheets (WT and variant) for B117 assay and return fl -- old/obsolete

B117_read_in_files <- function()
  
{
    rps <- str_c(flnm, c('', '-variant') ) # Reading in two sheets, the second one has -variant attached
    
    fl <- map_dfr(rps,  # read the two files successively and attach them by row (df*r*)
                  ~ read_sheet(sheeturls$raw_ddpcr, sheet = .x) %>% 
                    mutate(variant_status = if_else(str_detect(.x, '-variant'),
                                                    'Variant',
                                                    'all'), .before = 1))
}



# Obsolete functions --------------------------------------------------

# Check for neighboring dates and merge them :: Obsolete
harmonize_week <- function(week_cols) 
{
  
  # Pick numeric entries in column (the rest will be restored as is)
  num_week <- week_cols %>% str_extract('[:digit:]{3,4}') %>%  as.numeric() %>% unique() %>% .[!is.na(.)]
  
  # Check for consecutive dates
  repl_week <- num_week %>% 
    str_c() %>% 
    str_replace('([:digit:]+)([:digit:]{2})', '\\1/\\2/20') %>% 
    mdy() %>%  # convert to dates
    map_dbl(., function(x) num_week[. %in% c(x, x-1)] %>% min()) %>% # make the min entry of consecutive dates
    as.character() %>% 
    set_names(nm = num_week)
  
  if(repl_week %>% is_empty() %>% {!.}) new_week_cols <- str_replace_all(week_cols, repl_week)
  else week_cols
  
}



# Obsolete
# Reads single column of data and converts into a 96 well plate format (Baylor Sample_names)
baylor_col_to_plate <- function(sheetnm)
{
  target <- 'BCoV' # appends to the Sample_name for direct pasting in qPCR template sheet
  flnm <- 'baylor_labels'
  registry_sheet <- 'https://docs.google.com/spreadsheets/d/1mJcCt1wMiOuBic6sRlBZJf8KSNu2y-B5PjzCUu7jPM8/edit#gid=1011717808'
  
  # bb_sheet <- c('Week 13 (7/6)')
  # 
  # # week_match <- flnm %>% str_extract('[:digit:]+(?=-|_)') 
  # 
  # 
  # biobot_url <- 'https://docs.google.com/spreadsheets/d/1ghb_GjTS4yMFbzb65NskAlm-2Gb5M4SNYi4FHE4YVyI/edit#gid=233791008' 
  # 
  # # Getting biobot names: named vector for backconversion
  # biobot_translator <- read_sheet(biobot_url, sheet = bb_sheet) %>% 
  #   rename('Biobot ID' = matches('Biobot|Comments', ignore.case = T), 
  #          'WWTP' = contains('SYMBOL', ignore.case = T), 
  #          'FACILITY NAME' = matches('FACILITY NAME', ignore.case = T)) %>%
  #   
  #   drop_na(WWTP) %>% 
  #   mutate('biobot_baylor' = str_replace(`Biobot ID`,'\\.', '_'), WWTP = as.character(WWTP)) %>%  
  #   mutate(bb_translator = set_names(biobot_baylor , WWTP)) %>% 
  #   pull(bb_translator)
  
  
  # Read sample sheet
  
  baylor_names <- str_c('excel files/Baylor/', flnm, '.xlsx') %>% 
    read_xlsx (sheet = sheetnm) %>% 
    rename('Sample' = matches('Site')) %>% 
    select('Well', 'Sample') %>% 
    
    mutate('row' = str_match(Well, '[:upper:]'), 'col' = str_match(Well, '[:digit:]+')) %>% 
    select(-Well) %>% 
    
    # correct names - put a dot before replicate number
    mutate_at('Sample', ~str_match(., '(^[:upper:]+|^[:digit:]+).*([:digit:])') %>% {str_c(.[,2], .[,3], sep = '.')}) %>%
    
    
    # substitute biobot ids in
    # mutate_at('Sample', str_replace_all , biobot_translator) %>% 
    
    # attach template name and week ID
    mutate_at('Sample', ~str_c(target, '-', str_extract(sheetnm, '[:digit:]*(?=-[:digit:])'), '_' , .))
  
  # Convert column to table - 96 well
  
  baylor_table <- baylor_names %>% 
    pivot_wider(names_from = 'col', values_from = 'Sample') %>% 
    rename('<>' = row) %>% 
    mutate('-' = '', .before = 1)
  
  View(baylor_table) # manually copy paste wherever
  
  # write to sample registry 96 well sheet
  sheet_append(registry_sheet, sheet = '96 well RNA', tibble(c('', '', ''))) # append 3 empty rows
  sheet_append(registry_sheet, sheet = '96 well RNA', baylor_table)
  
}
