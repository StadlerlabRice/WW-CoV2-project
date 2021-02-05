# adhoc plotting data from clipboard

# load data ----
infl <- read_tsv(clipboard(), col_names = F) # loading from clipboard

# view data
view(infl)

# plotting ----

ggplot(infl, aes(x = X3, y = X11, colour = as_factor(X10))) + 
  geom_point()

  

