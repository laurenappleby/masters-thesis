# Script for Combining PREDICTS and Mammal Communities
# Combining the PREDICTS and MCDB data into one Data Frame:

library(dplyr)

# First subsetting the PREDICTS database to just keep the relevant columns
PREDICTS_subset <- PREDICTS_mammalia %>% select(Source_ID , Reference , Diversity_metric_type , 
                                                Diversity_metric_is_effort_sensitive , SSS , SSB, 
                                                Sampling_effort , Habitat_as_described , Longitude , 
                                                Latitude , Country , Ecoregion , Biome , Order , Family , 
                                                Genus , Species , Best_guess_binomial , Measurement, 
                                                Sample_start_earliest , Total_abundance)

# Merging the mammal communities data
MCDB_sites_trapping <- merge(MCDB_sites, MCDB_trapping, by = "Site_ID", all = FALSE)
MCDB_communities_species <- merge(MCDB_communities, MCDB_species, by = "Species_ID", all = FALSE)

MCDB_subset <- left_join(MCDB_communities_species, MCDB_sites_trapping, by = c("Site_ID", "Initial_year"))
MCDB_subset <- MCDB_subset %>% arrange(Site_ID)

# Subsetting the mammal communities data
MCDB_subset <- MCDB_subset %>% select(Site_ID , Initial_year , Presence_only , Abundance , Family , Genus , 
                                      Species , Country , Latitude , Longitude , Habitat_description , Trap_nights ,
                                      Reference_ID)

#Renaming variables so they bind in the same column
names(MCDB_subset)[names(MCDB_subset) == "Reference_ID"] <- "Reference"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Habitat_as_described"] <- "Habitat_description"
names(MCDB_subset)[names(MCDB_subset) == "Trap_nights"] <- "Sampling_effort"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Sample_start_earliest"] <- "Initial_year"
names(MCDB_subset)[names(MCDB_subset) == "Site_ID"] <- "Site_ID/SSS"
names(PREDICTS_subset)[names(PREDICTS_subset) == "SSS"] <- "Site_ID/SSS"
names(PREDICTS_subset)[names(PREDICTS_subset) == "Total_abundance"] <- "Abundance"

# Putting them together
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

dd <- bind_rows(PREDICTS_subset, MCDB_subset)

# Rescaling Sampling Effort adding column for max effort
dd <- dd %>%
  group_by(Reference) %>%
  mutate(max_effort = max(Sampling_effort))

#adding column for effort rescaled
dd <- dd %>%
  mutate(effort_rescaled = Sampling_effort / max_effort)

#adding column for rescaling factor
dd$rescaling_factor = 1/dd$effort_rescaled

#adding column for abundance corrected
dd <- dd %>% 
  mutate(abundance_corrected = Abundance * rescaling_factor)

# Removing sites that don't have long/lat information and sites with invalid longitude information (17)
dd <- dd[complete.cases(dd$Longitude, dd$Latitude), ]

invalid_lon <- dd$Longitude < -180 | dd$Longitude > 180
invalid_rows <- invalid_lon
dd <- dd[!invalid_rows, ]

#adding a line to test where my script is

