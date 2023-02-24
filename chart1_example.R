library(ggplot2)
library(dplyr)

data <- read.csv("/Users/katemu/Desktop/a3-spl-checkouts-kmutzet/datafile.csv", 
                 stringsAsFactor = FALSE)
num_checkouts_by_year <- data %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts))
x_values <- num_checkouts_by_year$CheckoutYear
y_values <- num_checkouts_by_year$Checkouts

color <- c("trend line" = "blue")
ggplot(num_checkouts_by_year) +
  geom_line(aes(x=x_values, y = y_values, color = "trend line")) + 
  scale_x_continuous(n.breaks = 11) +
  scale_color_manual(values = color, name = "Legend") +
  labs(title = "Number of Checked Out Books Over Time (2012-2023)",
                                                  x = "Year",
                                                  y = "Number of Check Outs")
