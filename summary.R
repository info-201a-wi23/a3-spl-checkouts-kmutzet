
library("dplyr")
library("ggplot2")

books_data <- 
  read.csv("/Users/katemu/Desktop/a3-spl-checkouts-kmutzet/datafile.csv", stringsAsFactor = FALSE)


#1. Average number of checkouts per item
avg_checkouts <- books_data %>%
  summarize(mean(Checkouts))
#2. Number of checkouts each year
num_checkouts_by_year <- books_data %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = sum(Checkouts))
#3. Number of Ebook Checkouts Year 2019
num_ebook_2019 <- books_data %>%
  filter(MaterialType != "BOOK") %>% 
  filter(CheckoutYear == 2019) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) %>% 
  pull(Checkouts)
#4. Number of regular book checkouts year 2019
num_book_2019 <- books_data %>%
  filter(MaterialType == "BOOK") %>% 
  filter(CheckoutYear == 2019) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) %>% 
  pull(Checkouts)
#5. Number of Ebook Checkouts Year 2020
num_ebook_2020 <- books_data %>%
  filter(MaterialType != "BOOK") %>% 
  filter(CheckoutYear == 2020) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) %>% 
  pull(Checkouts)
#6. Number of regular book checkouts year 2020
num_book_2020 <- books_data %>%
  filter(MaterialType == "BOOK") %>% 
  filter(CheckoutYear == 2020) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) %>% 
  pull(Checkouts)
#7. Number of Ebook Checkouts Year 2021
num_ebook_2021 <- books_data %>%
  filter(MaterialType != "BOOK") %>% 
  filter(CheckoutYear == 2021) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) %>% 
  pull(Checkouts)
#8. Number of regular book checkouts year 2021
num_book_2021 <- books_data %>%
  filter(MaterialType == "BOOK") %>% 
  filter(CheckoutYear == 2021) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) %>% 
  pull(Checkouts)
#9 Difference from 2019->2020 and then 2020-2021 for book numbers
difference_19_to_20b <- num_book_2020 - num_book_2019
difference_20_to_21b <- num_book_2021 - num_book_2020
#10 Difference from 2019->2020 and then 2020-2021 for ebook numbers
difference_19_to_20e <- num_ebook_2020 - num_ebook_2019
difference_20_to_21e <- num_ebook_2021 - num_ebook_2020
#11 What is the month with the most checkouts for ebooks in 2020?
max_ebook_2020 <- books_data %>%
  filter(MaterialType != "BOOK") %>% 
  filter(CheckoutYear == 2020) %>% 
  group_by(CheckoutMonth) %>% 
  summarize(Checkouts = sum(Checkouts))
max_ebook_2020 <- max_ebook_2020 %>% 
 filter(Checkouts == max(Checkouts)) %>% 
 pull(CheckoutMonth)
#12 Checkout numbers in 2020 by month
months_2020 <- books_data %>%
  filter(CheckoutYear == 2020) %>% 
  group_by(CheckoutMonth) %>% 
  summarize(Checkouts = sum(Checkouts))
#13 Checkout numbers in january 2021 (to see if the increase continued)
checkout_jan_2021 <- books_data %>%
  filter(CheckoutYear == 2021) %>% 
  group_by(CheckoutMonth) %>% 
  summarize(Checkouts = sum(Checkouts)) %>% 
  filter(CheckoutMonth == 1) %>% 
  pull(Checkouts)

