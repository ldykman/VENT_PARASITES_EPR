# This file tidies invertebrate parthenitae dissection data from Palmyra Atoll contributed by John McLaughlin 2021
# Created July 7, 2021
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

# None required

# SETTING WORKING DIRECTORY

path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

# INSTALLING PACKAGES

library(tidyr)

# IMPORTING PALMYRA INVERT DATA SET

palmyra.parth <- read.csv(paste(path, "Data", "Palmyra_Parth_FN_Jan_21_2021.csv", sep = "/"), header=TRUE)

# RENAMING COLUMNS FOR CONSISTENCY

colnames(palmyra.parth)[colnames(palmyra.parth) == "Host_ID"] <- "HOST_ID"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Host_Species"] <- "HOST_SPECIES"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Host_Phylum"] <- "HOST_GROUP_COMMON_NAME"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Parasite_Stage"] <- "PARASITE_LIFE_STAGE"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Collection_Date"] <- "COLLECTION_DATE"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Flat"] <- "COLLECTION_SITE"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Length_mm"] <- "HOST_LENGTH_TOTAL_MM"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Weight_with_shell"] <- "HOST_WEIGHT_WET_G"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Parasite_Species"] <- "PARASITE_SPECIES_NAME"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Parasite_Class"] <- "PARASITE_GROUP_COMMON_NAME"
colnames(palmyra.parth)[colnames(palmyra.parth) == "Intensity"] <- "PARASITE_COUNT"

palmyra.parth$ECOSYSTEM <- c(rep("Atoll lagoon sandflat", dim(palmyra.parth)[1]))
palmyra.parth$HOST_TISSUE <- c(rep("gonad", dim(palmyra.parth)[1]))
palmyra.parth$PARASITE_CONSUMER_STRATEGY <- c(rep("Castrator", dim(palmyra.parth)[1]))
palmyra.parth$COLLECTION_DATE <- as.Date(palmyra.parth$COLLECTION_DATE, format = "%d-%b-%y")
palmyra.parth$COLLECTOR <- c(rep(NA, dim(palmyra.parth)[1]))
palmyra.parth$DISSECTOR <- c(rep(NA, dim(palmyra.parth)[1]))
palmyra.parth$DISSECTION_DATE <- c(rep(NA, dim(palmyra.parth)[1]))
palmyra.parth$NOTES <- c(rep(NA, dim(palmyra.parth)[1]))
palmyra.parth$HOST_SEX <- c(rep(NA, dim(palmyra.parth)[1]))

# INITIAL

length(unique(palmyra.parth$PARASITE_SPECIES_NAME))
length(unique(palmyra.parth$HOST_SPECIES))
length(unique(palmyra.parth$HOST_ID))

# Parasite species: 12
# Host species: 19
# Host individuals: 1658

palmyra.full <- palmyra.parth[palmyra.parth$Dissection == "Full",]
palmyra.int <- palmyra.parth[palmyra.parth$Dissection == "First_Intermediate_Host_Only",]

# WHICH HOSTS WERE FULL DISSECTED AND NOT PARTHENITAE INSPECTED
unique(palmyra.full$HOST_SPECIES)[!unique(palmyra.full$HOST_SPECIES) %in% unique(palmyra.int$HOST_SPECIES)]

# WHICH HOSTS WERE PARTHENITAE INSPECTED BUT NOT FULL DISSECTED
unique(palmyra.int$HOST_SPECIES)[!unique(palmyra.int$HOST_SPECIES) %in% unique(palmyra.full$HOST_SPECIES)]

host.species.to.remove <- c("Little_Brown_Snail", "Nerita_signata")

palmyra.parth <- palmyra.parth[!palmyra.parth$HOST_SPECIES %in% host.species.to.remove,]

palmyra.parth$PARASITE_SPECIES_NAME[palmyra.parth$PARASITE_COUNT == 0] <- "NONE"
palmyra.parth$PARASITE_GROUP_COMMON_NAME[palmyra.parth$PARASITE_COUNT == 0] <- "NONE"
palmyra.parth$PARASITE_CONSUMER_STRATEGY[palmyra.parth$PARASITE_COUNT == 0] <- NA
palmyra.parth$PARASITE_LIFE_STAGE[palmyra.parth$PARASITE_COUNT == 0] <- NA
palmyra.parth$HOST_TISSUE[palmyra.parth$PARASITE_COUNT == 0] <- NA

# FINAL

length(unique(palmyra.parth$PARASITE_SPECIES_NAME))
length(unique(palmyra.parth$HOST_SPECIES))
length(unique(palmyra.parth$HOST_ID))

# Parasite species: 13
# Host species: 17
# Host individuals: 1588

# SAVING A CSV FILE

filename = paste0("TIDY_Parasites_Palmyra_Invert_Parth_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(palmyra.parth, paste(path, "Output", filename, sep = "/"))
