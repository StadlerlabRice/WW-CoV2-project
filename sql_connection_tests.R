# Connecting to MySQL database for the LIMS system
# Author : Prashant
# Date: 24/March/21

# Prerequisites ----
library(tidyverse) ; # install 'keyring' package and store username password in it

# Load database ----
con <- DBI::dbConnect(RMariaDB::MariaDB(), 
                      host = "stadlerlab.com",
                      dbname = "stadlerlabLIMS",
                      user = keyring::key_list('stadlerlabLIMS')[1,2],
                      password = keyring::key_get('stadlerlabLIMS', 'stadlerRW'),
                      
                      timeout = 600
)

dbload_list <- db_list_tables(con) # load the list of tables

# add a table to the database
# copy_to(con, mtcars, 
#         temporary = TRUE, 
#         indexes = list(
#           c("year", "month", "day"), 
#           "carrier", 
#           "tailnum",
#           "dest"
#         )
# )

dbload <- tbl(con, 'concentration_table') %>% 
  tibble() 

RMariaDB::dbDisconnect(con)

