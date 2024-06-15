PREDICTS1_sub <- PREDICTS_1 %>%
  group_by(Order) %>% mutate(Order = PREDICTS_1$Order) %>%
  summarise(frequency = table(Order)) 
library(dplyr)
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)
install.packages("RColorBrewer")
library(RColorBrewer)

# Subsetting the PREDICTS_1 data to just include species that most commonly carry zoonotic diseases
predicts_zoo <- dplyr::filter(PREDICTS_1, Order %in% c("Rodentia", "Chiroptera", "Soricomorpha", "Carnivora", "Artiodactyla", "Primates"))
predicts_zoo <- predicts_zoo %>% select(Order)

 ggplot(data.frame(predicts_zoo), aes(x=Order, fill = Order)) +
  geom_bar(width = 0.7) + scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#a6d854", "#8da0cb", "#e78ac3", "#ffd92f")) +
  labs(title = "Predicts", x = "Order", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
  

# Doing the same with PREDICTS_2 data
predicts2_zoo <- dplyr::filter(PREDICTS_2, Order %in% c("Rodentia", "Chiroptera", "Soricomorpha", "Carnivora", "Artiodactyla", "Primates"))
predicts2_zoo <- predicts2_zoo %>% select(Order)

 ggplot(data.frame(predicts2_zoo), aes(x=Order, fill = Order)) +
  geom_bar(width = 0.7) + scale_fill_brewer(palette = "Set2") +
  labs(title = "Predicts Supplementary", x = "Order", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

library(taxize)
help(sapply)

# Attempting some code to get an order column for MCDB 
MCDB_names <- MCDB_subset %>% select(Family)
MCDB_names <- distinct(MCDB_names)
MCDB_names <- na.omit(MCDB_names)

ordernames <- sapply(MCDB_names, tax_name, get = "order", USE.NAMES = F)
install.packages("tidyverse")

library(tidyverse)
orders <- t(ordernames)

orders$order <- as.list(orders$order)
as.list(orders$order)

order <- data.frame(orders[[3]])

allnames <- MCDB_subset %>% select(Family)
MCDB_orders <- bind_cols(MCDB_names, order)
MCDB_orders <- merge(allnames, MCDB_orders, by = "Family", all = FALSE)
names(MCDB_orders)[names(MCDB_orders) == "orders..3.."] <- "Order"

MCDB_zoo <- dplyr::filter(MCDB_orders, Order %in% c("Rodentia", "Chiroptera", "Soricomorpha", "Carnivora", "Artiodactyla", "Primates"))

MCDB_plot <- ggplot(data.frame(MCDB_zoo), aes(x=Order, fill = Order)) +
  geom_bar(width = 0.7) + scale_fill_brewer(palette = "Set2") +
  labs(title = "Mammal Communities", x = "Order", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

install.packages("gridExtra")
library(gridExtra)

grid.arrange(predicts1_plot, predicts2_plot, MCDB_plot, ncol = 2)



