# Install and load the "zyp" package
install.packages("zyp")
install.packages("ggplot2")
library(zyp)
library(ggplot2)
library(scales)

# Sample data representing the proportion of papers including a specific code
adj <- c(0,
         0,
         0.1,
         0.090909091,
         0.125,
         0.076923077,
         0.076923077,
         0.266666667,
         0.076923077,
         0.166666667
 )

years <- 2015:2024

# Perform the Mann-Kendall Trend Test
result <- MannKendall(adj)

# Print the result
print(result)

# Calculate the trend line
trend_line <- predict(loess(adj ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = adj), color = "blue") +
  geom_line(aes(x = years, y = trend_line), color = "red") +
  labs(x = "Year", y = "Recognition Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 1), labels = label_percent()) +
  theme_classic()
