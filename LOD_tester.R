#' Delete when done. This tests the new LOD functions.

source('./general_functions.R')

sheet <- 'dd.WW91_1214_N1N2'

fl <- read_sheet(sheeturls$data_dump, sheet)

totalsheet <- complete_LOD_table(fl)

