data <- data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
sheet_URL <- 'https://docs.google.com/spreadsheets/d/1ouk-kCJHERRhOMNP07lXfiC3aGB4wtWXpnYf5-b2CI4/edit#gid=0'
title_name <- 'david_testing2'
source('./general_functions.R')

rm(write_ok, sheet_dne)
check_ok_and_write(data, sheet_URL, title_name)