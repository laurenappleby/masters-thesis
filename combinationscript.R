# Script for Combining PREDICTS and Mammal Communities
# Combining the PREDICTS and MCDB data into one Data Frame:

library(dplyr)

# First subsetting the PREDICTS database to just keep the relevant columns
PREDICTS_subset <- PREDICTS_mammalia %>% select(Reference , Diversity_metric_type , 
                                                Diversity_metric_is_effort_sensitive , SSS , SSB, 
                                                Sampling_effort , Habitat_as_described , Longitude , 
                                                Latitude , Country , Ecoregion , Biome , Order , Family , 
                                                Genus , Species , Best_guess_binomial , Measurement, 
                                                Sample_start_earliest)

# Merging the mammal communities data
MCDB_sites_trapping <- merge(MCDB_sites, MCDB_trapping, by = "Site_ID", all = FALSE)
MCDB_communities_species <- merge(MCDB_communities, MCDB_species, by = "Species_ID", all = FALSE)

MCDB_subset <- left_join(MCDB_communities_species, MCDB_sites_trapping, by = c("Site_ID", "Initial_year"))
MCDB_subset <- MCDB_subset %>% arrange(Site_ID)

MCDB_subset$Best_guess_binomial = paste(MCDB_subset$Genus, MCDB_subset$Species, sep=" ")

MCDB_subset <- MCDB_subset %>%
  group_by(`Site_ID/SSS`) %>%
  mutate(Species_richness = n_distinct(Best_guess_binomial))

# Subsetting the mammal communities data
MCDB_subset <- MCDB_subset %>% select(Site_ID , Initial_year , Presence_only , Abundance , Family , Genus , 
                                      Species , Country , Latitude , Longitude , Habitat_description , Trap_nights ,
                                      Reference_ID , Species_richness)

# Changing the Presence Only column from binary 0 and 1 to character Abundance/Occurrence
MCDB_subset$Presence_only[MCDB_subset$Presence_only == 0] <- "Abundance"
MCDB_subset$Presence_only[MCDB_subset$Presence_only == 1] <- "Occurrence"

# Changing the NULL to 1 (For occurrence data binary 0/1 for present or absent)
MCDB_subset$Presence_only[MCDB_subset$Presence_only == "NULL"] <- 1

# Changing types of data so the columns can match up 
PREDICTS_subset$Sampling_effort <- as.numeric(PREDICTS_subset$Sampling_effort)
MCDB_subset$Sampling_effort <- as.numeric(MCDB_subset$Sampling_effort)
as.numeric(PREDICTS_subset$Sampling_effort)
as.numeric(MCDB_subset$Sampling_effort)

MCDB_subset$Longitude <- as.numeric(MCDB_subset$Longitude)
MCDB_subset$Latitude <- as.numeric(MCDB_subset$Latitude)
as.numeric(MCDB_subset$Longitude)
as.numeric(MCDB_subset$Latitude)

MCDB_subset$`Site_ID/SSS` <- as.factor(MCDB_subset$`Site_ID/SSS`)
as.factor(MCDB_subset$`Site_ID/SSS`)

PREDICTS_subset$Initial_year <- as.character(PREDICTS_subset$Initial_year)
as.character(PREDICTS_subset$Initial_year)

MCDB_subset$Abundance <- as.double(MCDB_subset$Abundance)
as.double(MCDB_subset$Abundance)

#Renaming columns so they match up
names(MCDB_subset)[names(MCDB_subset) == "Reference_ID"] <- "Reference"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Habitat_as_described"] <- "Habitat_description"
names(MCDB_subset)[names(MCDB_subset) == "Trap_nights"] <- "Sampling_effort"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Sample_start_earliest"] <- "Initial_year"
names(MCDB_subset)[names(MCDB_subset) == "Site_ID"] <- "Site_ID/SSS"
names(PREDICTS_subset)[names(PREDICTS_subset) == "SSS"] <- "Site_ID/SSS"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Total_abundance"] <- "Site_abundance"
names(MCDB_subset)[names(MCDB_subset) == "Presence_only"] <- "Diversity_metric_type"
names(MCDB_subset)[names(MCDB_subset) == "Abundance"] <- "Measurement"

#Putting them together
dd <- bind_rows(PREDICTS_subset, MCDB_subset)

# Adding Diversity Metric TRUE/FALSE to the MCDB data within the combined frame
dd$Diversity_metric_is_effort_sensitive[dd$Diversity_metric_type == "Occurrence"] <- FALSE
dd$Diversity_metric_is_effort_sensitive[dd$Diversity_metric_type == "Abundance"] <- TRUE

# Rescaling Sampling Effort adding column for max effort
dd$Sampling_effort[dd$Diversity_metric_type == "Occurrence"] <- 1

dd <- dd %>%
  group_by(Reference) %>%
  mutate(max_effort = max(Sampling_effort))

#adding column for effort rescaled
dd <- dd %>%
  mutate(effort_rescaled = Sampling_effort / max_effort)

#adding column for rescaling factor
dd$rescaling_factor = 1/dd$effort_rescaled

#adding a column for plain abundance 
dd <- dd %>% 
  mutate(Abundance = Measurement)
dd$Abundance[dd$Diversity_metric_type == "Occurrence"] <- NA

#adding a column for rescaled abundance
dd$Abundance_rescaled = dd$rescaling_factor * dd$Measurement

# Removing sites that don't have long/lat information and sites with invalid longitude information (17)
dd <- dd[complete.cases(dd$Longitude, dd$Latitude), ]

invalid_lon <- dd$Longitude < -180 | dd$Longitude > 180
invalid_rows <- invalid_lon
dd <- dd[!invalid_rows, ]




