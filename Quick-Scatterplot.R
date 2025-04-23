# Load necessary libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Read the data (adjust the file path)
data <- read.csv("C:/Users/12282/Documents/Werk/032425 FluAB R2 Comparison.csv")

# Create the scatter plot with different shapes for RNA and RNA2
p <- ggplot(data, aes(x = X, y = Y, color = WWTP)) +
  geom_point() +
  labs(title = "RESP Copies/uL (RNA) vs R2-RESP Copies/uL (RNA2)",
       x = "RESP Copies/uL (RNA)", 
       y = "R2-RESP Copies/uL (RNA2)",
       color = "WWTP") +
  theme_minimal() +
  scale_color_manual(values = brewer.pal(12, "Paired")) +  # Color scale for WWTP
  theme(legend.position = "top")  # Removed the extra comma here

# Check the plot output
print(p)

# Optional: Save the plot to a file (e.g., TIFF)
tiff("scatter_plot_with_x_and_y_separated.tiff", width = 8, height = 8, units = "in", res = 300)  # Adjust dimensions and resolution
print(p)  # Save the plot
dev.off()

# Optional: Interactive Plot with plotly
interactive_plot <- ggplotly(p, tooltip = "Target_Name")
interactive_plot  # Uncomment to view the interactive plot
