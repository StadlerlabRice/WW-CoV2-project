# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Read the data (adjust the file path)
data <- read.csv("C:/Users/12282/Documents/Werk/022625_RESP_R2FluAB_comparison.csv")

# View the first few rows of your data
head(data)

data <- data[data$Copies_per_uL_R1 !=0,]

# Reshape the data to a longer format, where X and Y are in a single column
data_long <- data %>%
  pivot_longer(cols = c("Copies_per_uL_R1", "Copies_per_uL_R2"), 
               names_to = "Legend", 
               values_to = "Value")

# Now, let's create the grouped bar plot
p <- ggplot(data_long, aes(x = Target_Name, y = Value, fill = Legend, group = interaction(WWTP, Legend))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~WWTP, scales = "free_x") +  # Create a plot for each WWTP
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Copies/uL RESP vs R2 Primers", 
       x = "Target Name", 
       y = "Copies/uL") +
  theme_minimal()

print(p)

tiff('022625 FluA R2 Bar Graph.tiff', units="in", width=15, height=10, res=500, compression = 'lzw')
print(p)
dev.off()