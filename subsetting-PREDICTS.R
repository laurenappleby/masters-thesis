# PREDICTS subset
install.packages("devtools")
library(devtools)
install_github("timnewbold/predicts-demo",subdir="predictsFunctions")
library(predictsFunctions)

# dependencies
library(yarg)
library(cat)
library(Hmisc)
library(taxize)
library(ggplot2)
library(countrycode)
library(plyr)
library(dplyr)

# ------------------------ CODE FOR THE SPECIES SUBSETTING IN PREDICTS -------------------------

# predicts data (both original and supplemental data)
PREDICTS_rds = readRDS("./data/predicts/predicts_database.Rds")
PREDICTS_supp = readRDS("./data/predicts/predicts_database.Rds")

PREDICTS_merged <- bind_rows(PREDICTS_rds, PREDICTS_supp)

# correct for sampling effort
# rescales sampling effort within studies (SS) relative to maximum (each value = se/max(se)), then corrects diversity metric for effort
cat('Correcting for sampling effort\n')
PREDICTS_species = CorrectSamplingEffort(PREDICTS_merged)

# merge sites that were sampled close together 
cat('Merging sites\n')
PREDICTS_species = MergeSites(PREDICTS_merged)

# remove taxa (e.g. plants) that are not relevant 
PREDICTS_species$Best_guess_binomial = as.vector(PREDICTS_species$Best_guess_binomial)
PREDICTS_sub = PREDICTS_species[ PREDICTS_species$Best_guess_binomial != "" & PREDICTS_species$Phylum %in% c("Chordata", "Arthropoda", "Gastropoda", "Mollusca"), ]

# standardise species names to facilitate taxonomic matching (creates binomial and genus columns)
PREDICTS_sub$bgb = tolower(PREDICTS_sub$Best_guess_binomial)
PREDICTS_sub$bgg = unlist(lapply(strsplit(PREDICTS_sub$bgb, " "), function(x) return(x[1])))

# code countries as ISO2c 
PREDICTS_sub$Country_ISO2C = countrycode(as.vector(PREDICTS_sub$Country), origin="country.name", destination="iso2c")

# remove records with no coordinates (n=866)
nrow(PREDICTS_sub[ is.na(PREDICTS_sub$Longitude) | is.na(PREDICTS_sub$Latitude), ])
PREDICTS_sub = PREDICTS_sub[ !is.na(PREDICTS_sub$Longitude) & !is.na(PREDICTS_sub$Latitude), ]

# Subsetting to just mammals:
class_counts <- table(PREDICTS_sub$Class)
num_mammalias <- class_counts["Mammalia"]
# There are 32885 mammal records
PREDICTS_mammalia <- PREDICTS_sub[PREDICTS_sub$Class == "Mammalia", ]

# SAVING THE FILE 
saveRDS(PREDICTS_mammalia, file = "PREDICTS_mammalia.rds")

folder_path <- "~/Desktop/DATABASES/data_modified"
file_path <- file.path(folder_path, "PREDICTS_mammalia.rds")
saveRDS(PREDICTS_mammalia, file = file_path)