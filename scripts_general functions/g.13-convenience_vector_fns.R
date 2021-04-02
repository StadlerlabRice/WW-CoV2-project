# g.13 vector convenience functions

# remove NA from a vector
remove_na_vectr <- function (x) x[!is.na(x)]

# remove NULL from a list
remove_null_lst <- function(x)  x[!sapply(x, is.null)]