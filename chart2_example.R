library(ggplot2)
library(dplyr)

data <- read.csv("/Users/katemu/Desktop/a3-spl-checkouts-kmutzet/datafile.csv", 
                 stringsAsFactor = FALSE)
num_checkouts_by_year <- data %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts))
regular_books_by_year <- data %>%
  filter(MaterialType == "BOOK") %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts))
ebooks_by_year <- data %>%
  filter(MaterialType != "BOOK") %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts))
x_values <- num_checkouts_by_year$CheckoutYear
y_values_1 <- regular_books_by_year$Checkouts
y_values_2 <- ebooks_by_year$Checkouts
colornames <- c("regular books", "ebooks")
COLORS <- c("regular books" = "blue", "ebooks" = "red")

ggplot(num_checkouts_by_year) +
  geom_line(aes(x=x_values, y = y_values_1, color = "regular books")) + 
  geom_line(aes(x=x_values, y = y_values_2, color = "ebooks")) +
  scale_x_continuous(n.breaks = 11) +
  scale_color_manual(values = COLORS, name = "Colors") +
  labs(title = "Number of Checked Out Books Over Time (2012-2023)",
                                                  x = "Year",
                                                  y = "Number of Check Outs")