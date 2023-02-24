library(ggplot2)
library(dplyr)
library("stringr")

#This R file creates a Bar plot of the number of checkouts per series/titles

data <- read.csv("/Users/katemu/Desktop/a3-spl-checkouts-kmutzet/datafile.csv", 
                 stringsAsFactor = FALSE)

num_checkouts_by_title <- data %>%
  group_by(Title) %>%
  summarize(Checkouts = sum(Checkouts))

#Too many different title for the bar plot, made it by series instead
num_checkouts_by_title$Title <- tolower(num_checkouts_by_title$Title)
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "court")] <- "The Court Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "corte")] <- "The Court Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "blade")] <- "TOG Short Stories"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "pirate lord")] <- "TOG Short Stories"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "desert")] <- "TOG Short Stories"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "underworld")] <- "TOG Short Stories"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "empire")] <- "TOG Short Stories"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "throne of glass")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "kingdom of ash")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "fire")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "midnight")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "shadows")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "storms")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "dawn")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "cristal")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "medianoche")] <- "Throne of Glass Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "earth and blood")] <- "Crescent City Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "breath")] <- "Crescent City Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "city 1")] <- "Crescent City Series"
num_checkouts_by_title$Title[str_detect(num_checkouts_by_title$Title, 
                                        "catwoman")] <- "Catwoman book"

#Grouping titles again after renaming/filtering (condensing numbers)
num_checkouts_by_title <- num_checkouts_by_title %>%
 group_by(Title) %>%
 summarize(Checkouts = sum(Checkouts))

#Making plot
ggplot(num_checkouts_by_title) +
  geom_col(aes(x = Title, y = Checkouts, fill=Title)) +
  labs(title = "Different Titles/Series and Checkouts",
       x = "Titles", y = "# of Checkouts")
