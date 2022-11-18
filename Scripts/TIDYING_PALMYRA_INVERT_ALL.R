# This file tidies invertebrate dissection data from Palmyra Atoll contributed by John McLaughlin 2020
# Created March 25, 2021
# Modified Mar 14 2022
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

# None required

# IMPORTING PALMYRA INVERT DATA SET

palmyra.invert <- read.csv(paste(path, "Data", "Palmyra_Invert_Parasite_FN_Apr_2021.csv", sep = "/"), header=TRUE)

# CHANGING CERTAIN COLUMN NAMES

colnames(palmyra.invert)[colnames(palmyra.invert) == "Host_Species"] <- "HOST_SPECIES"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Host_Order"] <- "HOST_GROUP_COMMON_NAME"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Host_ID"] <- "HOST_ID"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Collection_Date"] <- "COLLECTION_DATE"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Flat"] <- "COLLECTION_SITE"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Length_mm"] <- "HOST_LENGTH_TOTAL_MM"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Parasite_Species"] <- "PARASITE_SPECIES_NAME"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Collection_Date"] <- "COLLECTION_DATE"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Intensity"] <- "PARASITE_COUNT"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Parasite_Class"] <- "PARASITE_GROUP_COMMON_NAME"
colnames(palmyra.invert)[colnames(palmyra.invert) == "Parasite_Stage"] <- "PARASITE_LIFE_STAGE"

palmyra.invert$COLLECTION_DATE <- as.Date(palmyra.invert$COLLECTION_DATE, format = "%d-%b-%y")
palmyra.invert$ECOSYSTEM <- c(rep("Atoll lagoon sandflat", dim(palmyra.invert)[1]))
palmyra.invert$DISSECTION <- c(rep("Full", dim(palmyra.invert)[1]))
palmyra.invert$DISSECTOR <- c(rep(NA, dim(palmyra.invert)[1]))
palmyra.invert$DISSECTION_DATE <- c(rep(NA, dim(palmyra.invert)[1]))
palmyra.invert$COLLECTOR <- c(rep(NA, dim(palmyra.invert)[1]))
palmyra.invert$HOST_TISSUE <- c(rep(NA, dim(palmyra.invert)[1]))
palmyra.invert$HOST_WEIGHT_WET_G <- c(rep(NA, dim(palmyra.invert)[1]))
palmyra.invert$HOST_SEX <- c(rep(NA, dim(palmyra.invert)[1]))
palmyra.invert$PARASITE_CONSUMER_STRATEGY <- c(rep(NA, dim(palmyra.invert)[1]))
palmyra.invert$NOTES <- c(rep(NA, dim(palmyra.invert)[1]))

# WERE ANY OF THE SAMPLES SUBSAMPLED?

# For all hosts in this dataset, dissection = Full

unique(palmyra.invert$Dissection)

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(palmyra.invert$PARASITE_SPECIES_NAME))
length(unique(palmyra.invert$HOST_SPECIES))
length(unique(palmyra.invert$HOST_ID))

# Parasite species: 14
# Host species: 43
# Host individuals: 1882

# REMOVING CERTAIN HOST GROUPS

host.groups.to.remove <- c("Holothuroidea", "Anthozoa", "Insecta", "Phoronida", "Enteropneusta")

palmyra.invert <- palmyra.invert[!palmyra.invert$HOST_GROUP_COMMON_NAME %in% host.groups.to.remove,]

# AFTER SUBSETTING

length(unique(palmyra.invert$PARASITE_SPECIES_NAME))
length(unique(palmyra.invert$HOST_SPECIES))
length(unique(palmyra.invert$HOST_ID))

# Parasite species: 14
# Host species: 32
# Host individuals: 1201

unique(palmyra.invert$PARASITE_SPECIES_NAME)

# REMOVING PARASITE GROUPS AND IDS WITH COUNT ZERO

palmyra.invert$PARASITE_COUNT <- as.numeric(palmyra.invert$PARASITE_COUNT)
palmyra.invert$PARASITE_GROUP_COMMON_NAME[palmyra.invert$PARASITE_COUNT == 0] <- NA
palmyra.invert$PARASITE_SPECIES_NAME[palmyra.invert$PARASITE_COUNT == 0] <- "NONE"

# AFTER SUBSETTING

length(unique(palmyra.invert$PARASITE_SPECIES_NAME))
length(unique(palmyra.invert$HOST_SPECIES))
length(unique(palmyra.invert$HOST_ID))

# Parasite species: 12
# Host species: 32
# Host individuals: 1201

unique(palmyra.invert$PARASITE_GROUP_COMMON_NAME)

# AFTER SUBSETTING

length(unique(palmyra.invert$PARASITE_SPECIES_NAME))
length(unique(palmyra.invert$HOST_SPECIES))
length(unique(palmyra.invert$HOST_ID))

# Parasite species: 12
# Host species: 32
# Host individuals: 1190

# SAVING A CSV FILE

filename = paste0("TIDY_Parasites_Palmyra_Invert_All_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(palmyra.invert, paste(path, "Output", filename, sep = "/"))
