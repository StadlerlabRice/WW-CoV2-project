# Experiments with case_when -- FAILS since each RHS is executed irrespective of the conditino
# https://community.rstudio.com/t/case-when-with-heterogeneous-date-formats/28465/2


tsttype <- function(.vec1, .vec2)
{
  
  list(.vec1, .vec2) %>% 
    map_chr(class) %>%  # get the data types of each vector
    
    {case_when(
      map_lgl(., ~ .x == 'character') %>% all ~ 
        'chars',
      
      map_lgl(., ~ .x == 'numeric') %>% all ~ 'numbers',
        
      map_lgl(., ~ .x == 'integer') %>% all ~ 'integer numbers',
      
      map_lgl(., ~ .x == 'Date') %>% all ~ 'dates',
      
      TRUE ~ 'type not recognized'
    
  ) }
  
}


tsttype(a1$Date, a2$Date)


list(1:5, 1:5) %>% map_chr(class) %>% 
  
  {case_when(
    map_lgl(., ~ .x == 'character') %>% all ~ 'chars',
    
    map_lgl(., ~ .x == 'numeric') %>% all ~ 'numbers',
    
    map_lgl(., ~ .x == 'integer') %>% all ~ 'integer numbers',    
    
    TRUE ~ 'type not recognized'
    
  ) }


tstdate <- function(.vec1, .vec2)
{
  list(.vec1, .vec2) %>% map_chr(class) %>% 
    {
      case_when(
        map_lgl(., ~ .x == 'Date') %>% all ~ abs(.vec1 - .vec2) > 5,
        
        map_lgl(., ~ .x == 'character') %>% all ~ str_detect(string = .vec2, pattern = .vec1), # first vector is the regex pattern
        
      
        TRUE ~ rep(F, length(.vec1)))
      
    }
}

tstdate(a1$Date, a2$Date)


list(a1$Date, a2$Date) %>% map_chr(class) %>% map_lgl(., ~ .x == 'character') %>% all


# A function for joining when dates are within a week apart, and when strings are regex matched
date_and_regex_match <- function(.vec1, .vec2)
{
  list(.vec1, .vec2) %>% 
    map_chr(class) %>%  # get the data types of each vector
    
    {case_when(
      
      # When both are dates
      map_lgl(., ~ .x == 'Date') %>% all ~ 
        abs(.vec1 - .vec2) < 5,
      
      # When both are characters
      map_lgl(., ~ .x == 'character') %>% all ~ 
        str_detect(string = .vec2, pattern = .vec1), # first vector is the regex pattern
      
      
      # When both are numbers
      map_lgl(., ~ str_detect(.x == 'numeric|integer')) %>% all ~ 
        .vec1 == .vec2,
      
      TRUE ~ rep(F, length(.vec1)) # if no match the all FALSE
      
    )}
  
}


date_and_regex_match(a1$Date, a2$Date)


# Fuzzyjoin stuff----

# Merge lift station to respective WWTP

# Need to create a custom function that regex matches strings, difference matches data within a week
# and pass it to fuzzy_left_join(match_fun = ..)
# IS this matching each column element by element or accounts for all possible permuations?


# Troubleshooting the fuzzyjoin with dummy data containing dates

# check dates match of LS and WWTP
LSd <- LS_data %>% 
  filter(str_detect(abbr_LS, 'ALIEF')) %>% 
  pull(Date) %>% 
  lubridate::ymd() %>% 
  unique()

WWd <- WWTP_data %>% 
  filter(str_detect(WWTP, 'UPPER BRAYS')) %>% 
  pull(Date) %>% 
  lubridate::ymd() %>% 
  unique()

# need to match within 2-3 days.. or within the same week
# divide a into two parts a1, a2 and join the first 3 dates to each of them..
a1 = filter(a, a3 == 'a') %>% bind_cols(Date = LSd[1:3])
a2 = filter(a, a3 == 'b') %>% bind_cols(Date = WWd[1:3])

LSd[1] - WWd[2]

fuzzyjoin::difference_left_join(a1, a2, by = c('Date') , max_dist = 2)
a3 <- mutate(a2, a3 = 'a')


fuzzyjoin::fuzzy_left_join(a1, a2, by = 'Date', match_fun = date_and_regex_match)