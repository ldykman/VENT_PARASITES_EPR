# This file tidies fish dissection data from Palmyra Atoll contributed by John McLaughlin 2020
# Created March 25, 2021
# March 14 2022
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

library(tidyr)
library(dplyr)

# None required

# SETTING WORKING DIRECTORY

path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

# IMPORTING PALMYRA FISH DATA SET

palmyra.fish <- read.csv(paste(path, "Data", "32_Fish_Parasite.csv", sep = "/"), header=TRUE)

# RENAMING COLUMNS FOR CONSISTENCY

colnames(palmyra.fish)[colnames(palmyra.fish) == "Fish_ID"] <- "HOST_ID"
palmyra.fish$HOST_LENGTH_TOTAL_MM <- palmyra.fish$Total_Length_cm*10
colnames(palmyra.fish)[colnames(palmyra.fish) == "Host_Name"] <- "HOST_SPECIES"
colnames(palmyra.fish)[colnames(palmyra.fish) == "Total_Weight_g"] <- "HOST_WEIGHT_WET_G"
colnames(palmyra.fish)[colnames(palmyra.fish) == "Parasite_Group"] <- "PARASITE_GROUP_COMMON_NAME"
colnames(palmyra.fish)[colnames(palmyra.fish) == "Parasite_Stage"] <- "PARASITE_LIFE_STAGE"
colnames(palmyra.fish)[colnames(palmyra.fish) == "Intensity"] <- "PARASITE_COUNT"
colnames(palmyra.fish)[colnames(palmyra.fish) == "Parasite_Strategy"] <- "PARASITE_CONSUMER_STRATEGY"
colnames(palmyra.fish)[colnames(palmyra.fish) == "Parasite_Name"] <- "PARASITE_SPECIES_NAME"

palmyra.fish$ECOSYSTEM <- c(rep("Atoll lagoon sandflat", dim(palmyra.fish)[1]))
palmyra.fish$HOST_GROUP_COMMON_NAME <- c(rep("Fish", dim(palmyra.fish)[1]))
palmyra.fish$DISSECTION <- c(rep("Full", dim(palmyra.fish)[1]))
palmyra.fish$COLLECTION_SITE <- c(rep(NA, dim(palmyra.fish)[1]))
palmyra.fish$COLLECTION_DATE <- c(rep(NA, dim(palmyra.fish)[1]))
palmyra.fish$DISSECTOR <- c(rep(NA, dim(palmyra.fish)[1]))
palmyra.fish$DISSECTION_DATE <- c(rep(NA, dim(palmyra.fish)[1]))
palmyra.fish$COLLECTOR <- c(rep(NA, dim(palmyra.fish)[1]))
palmyra.fish$HOST_TISSUE <- c(rep(NA, dim(palmyra.fish)[1]))
palmyra.fish$HOST_SEX <- c(rep(NA, dim(palmyra.fish)[1]))
palmyra.fish$NOTES <- c(rep(NA, dim(palmyra.fish)[1]))

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(palmyra.fish$PARASITE_SPECIES_NAME))
length(unique(palmyra.fish$HOST_SPECIES))
length(unique(palmyra.fish$HOST_ID))

# Parasite species: 72
# Host species: 39
# Host individuals: 646

# CHANGING PARASITE IDS THAT ARE BLANK OR NA TO "NONE" AND CHANGING COUNTS TO ZERO

palmyra.fish$PARASITE_SPECIES_NAME[palmyra.fish$PARASITE_SPECIES_NAME %in% c(NA, "", " ")] <- "NONE"
palmyra.fish$PARASITE_COUNT[palmyra.fish$PARASITE_SPECIES_NAME == "NONE" | is.na(palmyra.fish$PARASITE_GROUP_COMMON_NAME)] <- 0
palmyra.fish$PARASITE_COUNT <- as.numeric(palmyra.fish$PARASITE_COUNT)

palmyra.fish$PARASITE_GROUP_COMMON_NAME[palmyra.fish$PARASITE_COUNT == 0] <- NA
palmyra.fish$PARASITE_SPECIES_NAME[palmyra.fish$PARASITE_COUNT == 0] <- "NONE"
palmyra.fish$PARASITE_SPECIES_NAME[palmyra.fish$PARASITE_SPECIES_NAME == "Benedenia_hawaiensis"] <- "Benedenia_hawaiiensis"
palmyra.fish$PARASITE_CONSUMER_STRATEGY[palmyra.fish$PARASITE_CONSUMER_STRATEGY == "Trophically_Trasmitted_Macroparasite"] <- "Trophically_Transmitted_Macroparasite"

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(palmyra.fish$PARASITE_SPECIES_NAME))
length(unique(palmyra.fish$HOST_SPECIES))
length(unique(palmyra.fish$HOST_ID))

# Parasite species: 73
# Host species: 39
# Host individuals: 646

# SAVING A CSV FILE

filename = paste0("TIDY_Parasites_Palmyra_Fish_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(palmyra.fish, paste(path, "Output", filename, sep = "/"))
