PREDICTS1_sub <- PREDICTS_1 %>%
  group_by(Order) %>% mutate(Order = PREDICTS_1$Order) %>%
  summarise(frequency = table(Order)) 
library(dplyr)
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)
install.packages("RColorBrewer")
library(RColorBrewer)

# ----------------------------MAKING BARPLOTS---------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------------------------------

#------------------------------------------------ MAKING MAPS -------------------------------------------------------

#Making a Dataframe with just MCDB sites that only have 1 geolocation
MCDB_sites_same <- MCDB_subset %>% select(Reference, Latitude, Longitude)

MCDB_i<- MCDB_sites_same %>%
  group_by(Reference) %>%
  mutate(All_Same = all(Latitude == first(Latitude) & Longitude == first(Longitude))) %>%
  ungroup()

MCDB_i <-MCDB_i %>%
  filter(!All_Same)

rm(sites_same)

library(maps)

# Making a map with all the distinct data sets with one geolocation
map("world", fill = TRUE, col = "lightblue", bg = "white")
  points(MCDB_i$Longitude, MCDB_i$Latitude, col = "#1b9e77", pch = 16, cex = 0.5)
  points(PREDICTS_1$Longitude, PREDICTS_1$Latitude, col = "#7570b3", pch = 16, cex = 0.5)
  points(PREDICTS_2$Longitude, PREDICTS_2$Latitude, col = "#e7298a", pch = 16, cex = 0.5)

# ---------------------------------------------------------------------------------------------------
  
write.csv(MCDB_i,"~/Desktop/DATABASES/data_modified/MCDB_i.csv", row.names = FALSE)
  
4640/31044
14.9 %
unique(MCDB_subset$Best_guess_binomial)
unique(PREDICTS_1$Best_guess_binomial)
unique(PREDICTS_2$Best_guess_binomial)

setdiff(MCDB_subset$Best_guess_binomial, PREDICTS_1$Best_guess_binomial)


566 

setdiff(PREDICTS_2$Best_guess_binomial, PREDICTS_1$Best_guess_binomial)

22

setdiff(PREDICTS_1$Best_guess_binomial, union(MCDB_subset$Best_guess_binomial, PREDICTS_2$Best_guess_binomial))

566+22+408

406
588/562
566+22

588+408


sites_subset <- all_sites[sample(nrow(all_sites), 5), ]
sites_sub1 <- head(all_sites, 10000)

unique_sites <- dd3 %>% select(`Site_ID/SSS`, Longitude, Latitude)
unique_sites <- unique_sites %>% group_by(Longitude, Latitude) %>%
  slice(1) %>%
  ungroup()


write.csv(unique_sites,"~/Desktop/DATABASES/data_modified/unique_sites.csv", row.names = FALSE)
