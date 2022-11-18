# This file compiles data sets from hydrothermal vents, estuaries, and tropical intertidal mudflat.
# Created July 9, 2020
# Modified June 10, 2021
# Modified March 14 2022
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

#install.packages("dplyr")
#install.packages("fossil")
#install.packages("reshape")
#install.packages("SpadeR")

library(dplyr)
library(fossil)
library(reshape)
library(SpadeR)

# SETTING WORKING DIRECTORY

# Re-name to your working directory
path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

path_in <- paste(path, "Output", sep = "/")

# IMPORTING TIDY DATA FROM THE THREE ECOSYSTEMS

# Host groups examined will be Fish, Mollusca, Crustacea, and Polychaeta. These must be standard across data sets, and examined to assure they do not include any host species outside the scope of this study, for example insects or terrestrial species. All other phyla must be removed.
# Be sure to re-name filenames below to include the most current date. This allows you to choose which version to analyze.

kelp <- read.csv(paste(path_in, "TIDY_Parasites_Kelp_2022-09-05.csv", sep = "/"), header=TRUE)
palmyra.fish <- read.csv(paste(path_in, "TIDY_Parasites_Palmyra_Fish_2022-09-05.csv", sep = "/"), header=TRUE)
palmyra.invert <- read.csv(paste(path_in, "TIDY_Parasites_Palmyra_Invert_All_2022-09-05.csv", sep = "/"), header=TRUE)
palmyra.parth <- read.csv(paste(path_in, "TIDY_Parasites_Palmyra_Invert_Parth_2022-09-05.csv", sep = "/"), header=TRUE)
vent <- read.csv(paste(path_in, "TIDY_Parasites_Vent_2022-11-03.csv", sep = "/"), header=TRUE)

# SUBSETTING COLUMNS FOR ALL DATA SETS

column.list <- c("ECOSYSTEM", "COLLECTION_SITE", "COLLECTOR", "COLLECTION_DATE", "DISSECTOR", "DISSECTION_DATE", "HOST_GROUP_COMMON_NAME", "HOST_SPECIES", "HOST_ID", "HOST_LENGTH_TOTAL_MM", "HOST_WEIGHT_WET_G", "HOST_SEX", "HOST_TISSUE", "PARASITE_SPECIES_NAME", "PARASITE_GROUP_COMMON_NAME", "PARASITE_LIFE_STAGE", "PARASITE_CONSUMER_STRATEGY", "PARASITE_COUNT", "NOTES")

kelp.subset <- kelp[column.list]
palmyra.fish.subset <- palmyra.fish[column.list]
palmyra.invert.subset <- palmyra.invert[column.list]
palmyra.parth.subset <- palmyra.parth[column.list]
vent.subset <- vent[column.list]

# BINDING TOGETHER FINAL DATA SET

data.all <- rbind(kelp.subset, palmyra.fish.subset, palmyra.invert.subset, palmyra.parth.subset, vent.subset)

# CONVERTING SOME COLUMN TYPES

data.all$COLLECTION_DATE <- as.Date(data.all$COLLECTION_DATE, format = "%Y-%m-%d")
data.all$DISSECTION_DATE <- as.Date(data.all$DISSECTION_DATE, format = "%Y-%m-%d")
data.all$ECOSYSTEM <- factor(data.all$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

# STANDARDIZING HOST GROUPS

data.all$HOST_GROUP_COMMON_NAME[data.all$HOST_GROUP_COMMON_NAME %in% c("fish", "Fish")] <- "Fish"
data.all$HOST_GROUP_COMMON_NAME[data.all$HOST_GROUP_COMMON_NAME %in% c("Decapoda", "Caprellidae", "Malacostraca", "Crustacea", "Isopoda")] <- "Crustacean"
data.all$HOST_GROUP_COMMON_NAME[data.all$HOST_GROUP_COMMON_NAME %in% c("Gastropoda", "gastropoda", "Bivalvia", "Mollusca")] <- "Mollusk"
data.all$HOST_GROUP_COMMON_NAME[data.all$HOST_GROUP_COMMON_NAME %in% c("Polychaeta")] <- "Polychaete"

# UNCOMMENT THIS SECTION TO SUBSET HOSTS WITH SAMPLE SIZE >= 10

host.table <- data.all %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME, HOST_SPECIES, HOST_ID, PARASITE_GROUP_COMMON_NAME, PARASITE_SPECIES_NAME, PARASITE_COUNT) %>%
  tally()

host.table$PARASITE_GROUP_COMMON_NAME[is.na(host.table$PARASITE_GROUP_COMMON_NAME)] <- "NONE"

hosts.examined <- host.table %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME, HOST_SPECIES, HOST_ID) %>%
  tally()

hosts.examined <- hosts.examined %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME, HOST_SPECIES) %>%
  tally()

species.to.retain <- unique(hosts.examined[hosts.examined$n >= 10,]$HOST_SPECIES)
data.all <- data.all[data.all$HOST_SPECIES %in% species.to.retain,]

# REMOVING NON-PARASITIC CONSUMER STRATEGIES

strategies.to.remove <- c("Commensal", "Free-living", "Egg predator", "Micropredator", "Pathogen", "Predator")

data.all$PARASITE_SPECIES_NAME[data.all$PARASITE_CONSUMER_STRATEGY %in% strategies.to.remove] <- "NONE"
data.all$PARASITE_COUNT[data.all$PARASITE_CONSUMER_STRATEGY %in% strategies.to.remove] <- 0
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_CONSUMER_STRATEGY %in% strategies.to.remove] <- "NONE"
data.all$HOST_TISSUE[data.all$PARASITE_CONSUMER_STRATEGY %in% strategies.to.remove] <- NA
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_CONSUMER_STRATEGY %in% strategies.to.remove] <- "NONE"
data.all$PARASITE_CONSUMER_STRATEGY[data.all$PARASITE_CONSUMER_STRATEGY %in% strategies.to.remove] <- NA

# STANDARDIZING METADATA FOR PARASITE COUNTS THAT ARE ZERO

data.all$PARASITE_SPECIES_NAME[data.all$PARASITE_COUNT == 0] <- "NONE"
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_COUNT == 0] <- "NONE"
data.all$HOST_TISSUE[data.all$PARASITE_COUNT == 0] <- NA
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_COUNT == 0] <- "NONE"
data.all$PARASITE_CONSUMER_STRATEGY[data.all$PARASITE_COUNT == 0] <- NA

# STANDARDIZING PARASITE GROUP COMMON NAMES

data.all$PARASITE_GROUP_COMMON_NAME <- as.character(data.all$PARASITE_GROUP_COMMON_NAME)

data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Acanthocephalan")] <- "Acanthocephala"
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Trematode", "Digenea", "digenea", "Digenean", "Aspidogastrea")] <- "Trematoda"
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% c("cestoda", "Cestode")] <- "Cestoda"
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% c("nematoda", "Nematode", "Chromadorea")] <- "Nematoda"
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Copepod")] <- "Copepoda"
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Isopod")] <- "Isopoda"
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Monogene")] <- "Monogenea"

# CHECKING INFO

length(unique(data.all$PARASITE_SPECIES_NAME))
length(unique(data.all$HOST_SPECIES))
length(unique(data.all$HOST_ID))

# Parasite ID: 319
# Host Species: 215
# Host ID: 5943

# REMOVING PARASITE GROUPS

parasite.groups.to.remove <- c("Nemertea", "Polychaeta", "Turbellaria", "Malacostraca", "Conoidasida", "Ciliophora", "Amphipoda", "Acari", "Bacterial cyst", 
                               "Bivalvia", "Brachyura", "Bryozoan", "Cirripedia", "Coccidia", "Decapoda", "Gregarina", "Myxozoa", "myxozoa", "Ostracoda", "pathology", "Unknown", "unknown",
                               "Myxozoan", "Oomycete", "Ciliate")

data.all$PARASITE_SPECIES_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.remove] <- "NONE"
data.all$PARASITE_COUNT[data.all$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.remove] <- 0
data.all$HOST_TISSUE[data.all$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.remove] <- NA
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.remove] <- "NONE"
data.all$PARASITE_CONSUMER_STRATEGY[data.all$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.remove] <- NA
data.all$PARASITE_GROUP_COMMON_NAME[data.all$PARASITE_GROUP_COMMON_NAME %in% parasite.groups.to.remove] <- "NONE"

# CHECKIGN INFO AGAIN

length(unique(data.all$PARASITE_SPECIES_NAME))
length(unique(data.all$HOST_SPECIES))
length(unique(data.all$HOST_ID))

# Parasite ID: 307
# Host Species: 215
# Host ID: 5943

# CLEANING SOME LIFE STAGE NAMES

data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Cestoda" & data.all$PARASITE_LIFE_STAGE == "Juvenile"] <- "Metacestode"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("Cystacanth", "cystacanths", "cystacanth")] <- "Cystacanth"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("Metacestode", "metacestodes", "mestacestoda", "metacestode ", "metacestode", 
                                                       "Metacestode ", "Metacestoda", "plerocercoid", "Plerocercoid", "plerceroid", "plerceroid ")] <- "Metacestode"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("Metacercaria", "Metacercariae", "Trematode metacercaria", "Torticaecum_Type", "Monilicaecum_Type", "metacercaria", "cyst")] <- "Metacercaria"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("Redia", "Trematode sporocyst", "Sporocyst", "sporocyst")] <- "Sporocyst"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("na", "unknown", "")] <- NA
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("chalimus", "larva", "Larvae")] <- "Larva"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("adult female", "adults", "adult", "adult ", "fertilized externa", "Adult")] <- "Adult"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_LIFE_STAGE %in% c("juvenile", "juvenile ", "juveniles", "Juvenile", "immature adult", "adult, immature")] <- "Adult, immature"

# EDITING SOME LIFE STAGES

data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Acanthocephala" & data.all$PARASITE_LIFE_STAGE == "Metacestode"] <- "Cystacanth"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Cestoda" & data.all$PARASITE_LIFE_STAGE == "Cystacanth"] <- "Metacestode"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Cestoda" & is.na(data.all$PARASITE_LIFE_STAGE)] <- "Metacestode"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Copepoda" & is.na(data.all$PARASITE_LIFE_STAGE)] <- "Adult"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Monogenea" & is.na(data.all$PARASITE_LIFE_STAGE)] <- "Adult"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Nematoda" & data.all$PARASITE_LIFE_STAGE %in% c("Larva", "Adult, immature")] <- "Larva"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Nematoda" & data.all$PARASITE_LIFE_STAGE == "Metacercaria"] <- "Larva"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Nematoda" & is.na(data.all$PARASITE_LIFE_STAGE)] <- "Adult"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME == "Trematoda" & data.all$PARASITE_LIFE_STAGE == "Cystacanth"] <- "Metacercaria"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Isopoda", "Trematoda") & data.all$PARASITE_LIFE_STAGE %in% c("Adult, immature", "Juvenile")] <- "Adult, immature"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_SPECIES_NAME == "Dollfustrema.californiae" & is.na(data.all$PARASITE_LIFE_STAGE)] <- "Metacercaria"
data.all$PARASITE_LIFE_STAGE[data.all$PARASITE_SPECIES_NAME == "Neozoogonus.californicus" & is.na(data.all$PARASITE_LIFE_STAGE)] <- "Adult"

life.stages.test <- data.all %>%
  group_by(PARASITE_GROUP_COMMON_NAME, PARASITE_LIFE_STAGE) %>%
  tally()

# ADDING LIFE CYCLE DATA

data.all$PARASITE_LIFE_CYCLE <- c(rep("NONE", dim(data.all)[1]))
data.all$PARASITE_LIFE_CYCLE[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Acanthocephala", "Cestoda", "Nematoda", "Trematoda")] <- "Indirect"
data.all$PARASITE_LIFE_CYCLE[data.all$PARASITE_GROUP_COMMON_NAME %in% c("Copepoda", "Isopoda", "Monogenea", "Rhizocephala")] <- "Direct"

# CHANGING SOME SPECIFIC LIFE CYCLES BASED ON KNOWN LIFE CYCLES IN LITERATURE

data.all$PARASITE_LIFE_CYCLE[data.all$PARASITE_SPECIES_NAME %in% c("Peniculus.fistula", "Peniculus.sp.")] <- "Indirect"
data.all$PARASITE_LIFE_CYCLE[data.all$PARASITE_SPECIES_NAME %in% c("Capillariidae_sp_1")] <- "Direct"

data.all$GROUP_STAGE <- paste(data.all$PARASITE_GROUP_COMMON_NAME, data.all$PARASITE_LIFE_STAGE, sep = ", ")

# MAKING HOST GROUP CATEGORY TO SEPARATE FISH FROM INVERTEBRATES

data.all$HOST_GROUP_COMMON_NAME_SIMPLE <- c(rep(NA, dim(data.all)[1]))
data.all$HOST_GROUP_COMMON_NAME_SIMPLE[data.all$HOST_GROUP_COMMON_NAME == "Fish"] <- "Fish"
data.all$HOST_GROUP_COMMON_NAME_SIMPLE[data.all$HOST_GROUP_COMMON_NAME %in% c("Crustacean", "Mollusk", "Polychaete")] <- "Invertebrate"

# WRITING A CSV OF EITHER THE FULL DATASET OR SUBSET DATA

filename1 = paste0("TIDY_DATA_ALL_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(data.all, paste(path, "Output", filename1, sep = "/"))

# CREATING A TABLE WITH THE NUMBER OF HOSTS EXAMINED

host.table <- data.all %>%
  group_by(HOST_ID, HOST_SPECIES, HOST_GROUP_COMMON_NAME, ECOSYSTEM) %>%
  tally()

hosts.examined <- host.table %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME, HOST_SPECIES) %>%
  tally()

host.species <- hosts.examined %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME, n) %>%
  tally()

# Calculating total and estimated parasite richness

# FIGURE 1: PARASITE SPECIES ACCUMULATION CURVES FOR EACH HOST SPECIES IN EACH HOST GROUP

# CREATING A SIMPLIFIED DATA TABLE WITH HOST DISSECTION DATA

hosts.examined <- data.all %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME, HOST_SPECIES, HOST_ID) %>%
  tally()

hosts.examined <- hosts.examined %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME, HOST_SPECIES) %>%
  tally()

host.species <- hosts.examined %>%
  group_by(ECOSYSTEM, HOST_GROUP_COMMON_NAME) %>%
  tally()

host.species$ECO_GROUP <- paste(host.species$ECOSYSTEM, host.species$HOST_GROUP_COMMON_NAME, sep = "_")

spp.to.remove <- c("Anisakidae gen..NJ.MO10", "Anisakidae gen..unknown.nematodes", "Anisakis.sp. A", "Cestoda.C.MET.MO2", "DIG.MO10.DIG.MO10", "DIG.MO3.DIG.MO3",
                   "DIG.MO9.DIG.MO9", "Trematoda.DIGEJUV", "Digene.unknown", "META.MO16.META.MO16", "Metacercaria.sp. I", "Metacercaria.UNK.MO11", "Metacercaria.unknown",
                   "Metacestode.CMET.MO1", "Metacestode.CMET.MO3", "Metacestode.unknown", "Trematoda.METAUNK", "Monogene.unknown", "NEM.AB2.NEM.AB2", "Nematoda.unknown",
                   "NJ.MO4.sp.", "Strong_Legs_Copepod", "Tetraphyllid metacestode.TTRM.MO6", "HOST_ID", "NONE", "HOST_SPECIES")

# CREATING A METADATA TABLE FOR THE FOUR ECOSYSTEMS

host.count.ind <- host.table %>%
  group_by(HOST_GROUP_COMMON_NAME, ECOSYSTEM) %>%
  tally()

host.count.spp <- host.table %>%
  group_by(HOST_GROUP_COMMON_NAME, ECOSYSTEM, HOST_SPECIES) %>%
  tally()

host.count.spp <- host.count.spp %>%
  group_by(HOST_GROUP_COMMON_NAME, ECOSYSTEM) %>%
  tally()

meta.table <- cbind(host.count.spp, host.count.ind$n)

colnames(meta.table) <- c("HOST_GROUP_COMMON_NAME", "ECOSYSTEM", "HOST_SPECIES", "HOST_INDIVIDUALS")

filename3 = paste0("MERGED_Ecosystem_Metadata_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(meta.table, paste(path, "Output", filename3, sep = "/"))

# PARASITE LIFE STAGE COMPOSITION

psite.spp.per.host.group.table <- aggregate(data.all$PARASITE_COUNT, by = list(data.all$PARASITE_SPECIES_NAME, data.all$ECOSYSTEM, data.all$HOST_GROUP_COMMON_NAME_SIMPLE, data.all$PARASITE_GROUP_COMMON_NAME, data.all$PARASITE_LIFE_STAGE, data.all$PARASITE_LIFE_CYCLE), FUN=sum)
colnames(psite.spp.per.host.group.table) <- c("PARASITE_SPECIES_NAME", "ECOSYSTEM", "HOST_GROUP_COMMON_NAME_SIMPLE", "PARASITE_GROUP_COMMON_NAME", "PARASITE_LIFE_STAGE", "PARASITE_LIFE_CYCLE", "PARASITE_COUNT")

psite.spp.per.host.group.table <- psite.spp.per.host.group.table[!psite.spp.per.host.group.table$PARASITE_LIFE_STAGE %in% c("NONE"),]

psite.spp.per.host.group.table <- psite.spp.per.host.group.table[with(psite.spp.per.host.group.table, order(ECOSYSTEM, HOST_GROUP_COMMON_NAME_SIMPLE, PARASITE_LIFE_CYCLE, PARASITE_LIFE_STAGE, PARASITE_GROUP_COMMON_NAME)),]

# HERE I CREATE A CSV FILE TO FILL IN INFORMATION ON PARASITE LIFE CYCLES AS I RESEARCH THEM

filename2 <- paste0("MERGED_Parasite_Species_LifeCycle_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(psite.spp.per.host.group.table, paste(path, "Output", filename2, sep = "/"))
