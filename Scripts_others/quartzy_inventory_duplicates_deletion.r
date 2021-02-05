
# Play with duplicates in inventory - quartzy

# load data ----

in.fl <- 'excel files/Archive/Stadler_Lab_Inventory_Oct_25_2020.xlsx'

al.dat <- excel_sheets(in.fl) %>% 
  set_names() %>% 
  map(read_xlsx, path = in.fl)


# flag duplicates as Y for deletion
fin.dat <- map(al.dat[-1], 
               ~ .x %>%  
                 group_by(`Item Name *`, Owner) %>% 
                 mutate(rep_count = row_number()) %>% 
                 mutate(`Delete? (Y/N)` = if_else(rep_count > 1, 'Y', 'N'))
)

map2(fin.dat, names(fin.dat), 
    ~ write_sheet(.x, sheet = .y, 
                  ss = 'https://docs.google.com/spreadsheets/d/1rl8ObTcDUCGtvEQKGltWeOHUjHmewMvUEvfX0pSMtY0/edit#gid=0')
)

# tst1 <- tst %>% 
#   select(-`Serial Number`) %>% 
#   unique()