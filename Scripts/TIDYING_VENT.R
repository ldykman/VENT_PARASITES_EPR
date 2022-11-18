# This file tidies the EPR parasite data set by Dykman et al. 2022
# Created Oct 13, 2020
# Modified Feb 15, 2021
# Modified June 10, 2021
# Modified and annotated Jan 14 2021
# Modified Mar 14 2022
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

# None required

# SETTING WORKING DIRECTORY

# Re-name to your working directory
path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

# LOADING VENT DATA SET

vent.data <- read.csv(paste(path, "Data", "PARASITES_VENT_DISSECTION.csv", sep = "/"), header=TRUE)

# REMOVING ROWS AND COLUMNS THAT HAVE NO DATA

ind.col <- apply(vent.data, 2, function(x) all(is.na(x)))
vent.data <- vent.data[, !ind.col]

ind.row <- apply(vent.data, 1, function(x) all(is.na(x)))
vent.data <- vent.data[!ind.row,]

# CHANGING THE CLASS OF SOME COLUMNS
# May throw warnings NAs introduced by coercion - this is okay

vent.data$COLLECTION_DATE <- as.Date(vent.data$COLLECTION_DATE, format = "%Y-%m-%d")
vent.data$COLLECTION_LATITUDE <- as.numeric(as.character(vent.data$COLLECTION_LATITUDE))
vent.data$COLLECTION_LONGITUDE <- as.numeric(as.character(vent.data$COLLECTION_LONGITUDE))
vent.data$COLLECTION_DEPTH <- as.numeric(as.character(vent.data$COLLECTION_DEPTH))
vent.data$DISSECTION_DATE <- as.Date(vent.data$DISSECTION_DATE, format = "%Y-%m-%d")
vent.data$HOST_LENGTH_TOTAL_MM <- as.numeric(vent.data$HOST_LENGTH_TOTAL_MM)
vent.data$HOST_LENGTH_STANDARD_MM <- as.numeric(vent.data$HOST_LENGTH_STANDARD_MM)
vent.data$HOST_WIDTH_MM <- as.numeric(vent.data$HOST_WIDTH_MM)
vent.data$HOST_WEIGHT_WET_G <- as.numeric(vent.data$HOST_WEIGHT_WET_G)
vent.data$PARASITE_LENGTH_MICRON <- as.numeric(vent.data$PARASITE_LENGTH_MICRON)
vent.data$PARASITE_WIDTH_MICRON <- as.numeric(vent.data$PARASITE_WIDTH_MICRON)
vent.data$PARASITE_COUNT <- as.numeric(vent.data$PARASITE_COUNT)
vent.data$NOTES <- paste(vent.data$HOST_NOTES, vent.data$PARASITE_NOTES, sep="")

str(vent.data) # Check that the class of columns updated correctly

# REMOVING OTHER SAMPLES THAT DO NOT MEET CRITERIA OF STUDY

vent.data <- vent.data[!vent.data$COLLECTION_SITE %in% c("K Vent?", "PBR"),] # Removing southern vents outside 9 50N vent field (PBR and K vent) 

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(vent.data$PARASITE_SPECIES_NAME))
length(unique(vent.data$HOST_SPECIES))
length(unique(vent.data$HOST_ID))

# Parasite species: 46
# Host species: 52
# Host individuals: 2216

# Retaining only morphogroups with parasite consumer strategies as defined in Lafferty & Kuris 2002

unique(vent.data$PARASITE_CONSUMER_STRATEGY)
parasite.consumer.strategies <- c("Trophically_Transmitted_Macroparasite", "Macroparasite", "Castrator")

# For consumer strategies not being retained, changing PARASITE_GROUP_COMMON_NAME and PARASITE_MORPHOGROUP_NAME to NA and PARASITE_COUNT to 0

vent.data$PARASITE_MORPHOGROUP_NAME[!vent.data$PARASITE_CONSUMER_STRATEGY %in% parasite.consumer.strategies] <- NA
vent.data$PARASITE_SPECIES_NAME[!vent.data$PARASITE_CONSUMER_STRATEGY %in% parasite.consumer.strategies] <- NA
vent.data$PARASITE_COUNT[!vent.data$PARASITE_CONSUMER_STRATEGY %in% parasite.consumer.strategies] <- 0
vent.data$PARASITE_GROUP_COMMON_NAME[!vent.data$PARASITE_CONSUMER_STRATEGY %in% parasite.consumer.strategies] <- NA

# ONLY RETAINING CONFIRMED METAZOAN PARASITE TAXONOMIC GROUPS FOR ANALYSIS

unique(vent.data$PARASITE_GROUP_COMMON_NAME)
parasite.groups.to.retain <- c("Acanthocephala", "Cestoda", "Copepoda", "Monogenea", "Nematoda", "Rhizocephala", "Trematoda") # These are the taxa examined in Dykman et al. 2022

# For parasite taxonomic groups not being retained, changing PARASITE_GROUP_COMMON_NAME and PARASITE_MORPHOGROUP_NAME to NA and PARASITE_COUNT to 0

vent.data$PARASITE_MORPHOGROUP_NAME[!vent.data$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.retain] <- NA
vent.data$PARASITE_SPECIES_NAME[!vent.data$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.retain] <- NA
vent.data$PARASITE_COUNT[!vent.data$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.retain] <- 0
vent.data$PARASITE_GROUP_COMMON_NAME[!vent.data$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.retain] <- NA

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(vent.data$PARASITE_SPECIES_NAME))
length(unique(vent.data$HOST_SPECIES))
length(unique(vent.data$HOST_ID))

# Parasite morphogroups: 28
# Host species: 52
# Host individuals: 2216

vent.data$PARASITE_SPECIES_NAME[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PARASITE_SPECIES_NAME_AphiaID[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PARASITE_SPECIES_NAME_LSID[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PARASITE_PHOTOS[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PATHOLOGY[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PARASITE_SAMPLES[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PARASITE_LENGTH_MICRON[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PARASITE_WIDTH_MICRON[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA
vent.data$PARASITE_CONSUMER_STRATEGY[is.na(vent.data$PARASITE_MORPHOGROUP_NAME)] <- NA

# CHANGING PARASITE IDS THAT ARE BLANK TO "NONE" AND CHANGING COUNTS TO ZERO

vent.data$PARASITE_COUNT <- as.numeric(vent.data$PARASITE_COUNT)
vent.data$PARASITE_MORPHOGROUP_NAME <- as.character(vent.data$PARASITE_MORPHOGROUP_NAME)
vent.data$PARASITE_MORPHOGROUP_NAME[vent.data$PARASITE_MORPHOGROUP_NAME %in% c(NA, "", " ")] <- "NONE"
vent.data$PARASITE_SPECIES_NAME[vent.data$PARASITE_SPECIES_NAME %in% c(NA, "", " ")] <- "NONE"
vent.data$PARASITE_COUNT[is.na(vent.data$PARASITE_MORPHOGROUP_NAME) | is.na(vent.data$PARASITE_GROUP_COMMON_NAME)] <- 0
vent.data$PARASITE_GROUP_COMMON_NAME[vent.data$PARASITE_COUNT == 0] <- NA
vent.data$PARASITE_MORPHOGROUP_NAME[vent.data$PARASITE_COUNT == 0] <- "NONE"
vent.data$PARASITE_SPECIES_NAME[vent.data$PARASITE_COUNT == 0] <- "NONE"

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(vent.data$PARASITE_SPECIES_NAME))
length(unique(vent.data$HOST_SPECIES))
length(unique(vent.data$HOST_ID))

# Parasite species: 28
# Host species: 52
# Host individuals: 2216

# ADDING ECOSYSTEM TYPE

vent.data$ECOSYSTEM <- c(rep("Vent", dim(vent.data)[1]))

# WRITING A CSV OF EPR PARASITE DATA THAT HAS BEEN CLEANED AND EDITED BY THIS SCRIPT

filename = paste0("TIDY_Parasites_Vent_", format(Sys.Date(), "%Y-%m-%d"), ".csv")

write.csv(vent.data, paste(path, "Output", filename, sep = "/"))
