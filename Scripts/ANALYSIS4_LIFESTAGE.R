# This script analyzes the relative proportions of species with direct and indirect life cycles.
# June 16, 2021
# Modified November 4, 2021
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("tidyverse")

library(ggplot2)
library(reshape)
library(tidyverse)

# SETTING WORKING DIRECTORY

path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

# IMPORTING DATA

input_file = paste0("TIDY_DATA_ALL_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
data.all <- read.csv(paste(path, "Output", input_file, sep = "/"), header=TRUE)

data.all$COLLECTION_DATE <- as.Date(data.all$COLLECTION_DATE, format = "%Y-%m-%d")

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
                   "DIG.MO9.DIG.MO9", "Trematoda.DIGEJUV", "Trematoda.DIGEUNK", "Digene.unknown", "META.MO16.META.MO16", "Metacercaria.sp. I", "Metacercaria.UNK.MO11", "Metacercaria.unknown",
                   "Metacestode.CMET.MO1", "Metacestode.CMET.MO3", "Metacestode.unknown", "Trematoda.METAUNK", "Monogene.unknown", "NEM.AB2.NEM.AB2", "Nematoda.unknown",
                   "NJ.MO4.sp.", "Strong_Legs_Copepod", "Tetraphyllid metacestode.TTRM.MO6", "HOST_ID", "NONE", "HOST_SPECIES")

stages.to.remove <- c("META.MO5.OXCA.1185.5.", "Metacercaria.sp. B", "Metacercaria.sp. C", "Metacercaria.sp. D", "Mesobathylebouria.SPOR01")

# DEFINING COLORS
stagecolors <- hcl.colors(8, "Green-Brown")[c(2,3,4,5,6,7)]
names(stagecolors) <- c("Adult", "Cystacanth", "Larva", "Metacercaria", "Metacestode", "Sporocyst")

# PARASITE LIFE STAGE COMPOSITION

psite.spp.stage.table <- aggregate(data.all$PARASITE_COUNT, by = list(data.all$PARASITE_SPECIES_NAME, data.all$ECOSYSTEM, data.all$HOST_GROUP_COMMON_NAME_SIMPLE, data.all$PARASITE_LIFE_STAGE), FUN=sum)
colnames(psite.spp.stage.table) <- c("PARASITE_SPECIES_NAME", "ECOSYSTEM", "HOST_GROUP_COMMON_NAME_SIMPLE", "PARASITE_LIFE_STAGE", "PARASITE_COUNT")

psite.spp.stage.table <- psite.spp.stage.table[!psite.spp.stage.table$PARASITE_SPECIES_NAME %in% spp.to.remove,]

psite.spp.stage.table$PARASITE_LIFE_STAGE[psite.spp.stage.table$PARASITE_LIFE_STAGE == "Adult, immature"] <- "Adult" # For simplicity considering adult stages that are immature as adults.

psite.spp.stage.summary <- aggregate(psite.spp.stage.table$PARASITE_COUNT, 
                                     by = list(psite.spp.stage.table$ECOSYSTEM, 
                                               psite.spp.stage.table$HOST_GROUP_COMMON_NAME_SIMPLE,
                                               psite.spp.stage.table$PARASITE_LIFE_STAGE), function(x) sum(x != 0))

colnames(psite.spp.stage.summary) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME_SIMPLE", "PARASITE_LIFE_STAGE", "PARASITE_SP_COUNT")

stage.comp.fish <- psite.spp.stage.summary[psite.spp.stage.summary$HOST_GROUP_COMMON_NAME_SIMPLE == "Fish",]
stage.comp.invert <- psite.spp.stage.summary[psite.spp.stage.summary$HOST_GROUP_COMMON_NAME_SIMPLE == "Invertebrate",]

# Chi Squared Test for Fish Hosts

chisq.stage.table.fish <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.fish)
chisq.stage.fish <- chisq.test(chisq.stage.table.fish)
fisher.stage.fish <- fisher.test(chisq.stage.table.fish)
count.stage.fish <- as.data.frame(chisq.stage.table.fish)
count.stage.fish$ECOSYSTEM <- factor(count.stage.fish$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

stage.comp.1 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.fish[stage.comp.fish$ECOSYSTEM %in% c("Kelp forest", "Atoll lagoon sandflat"),])
chisq.stage.comp1 <- chisq.test(stage.comp.1) # p = 0.0015
fisher.stage.comp1 <- fisher.test(stage.comp.1) # p = 0.0014

stage.comp.2 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.fish[stage.comp.fish$ECOSYSTEM %in% c("Kelp forest", "Vent"),])
chisq.stage.comp2 <- chisq.test(stage.comp.2) # p = 0.065
fisher.stage.comp2 <- fisher.test(stage.comp.2) # p = 0.111

stage.comp.3 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.fish[stage.comp.fish$ECOSYSTEM %in% c("Atoll lagoon sandflat", "Vent"),])
chisq.stage.comp3 <- chisq.test(stage.comp.3) # p = 0.037
fisher.stage.comp3 <- fisher.test(stage.comp.3) # p = 0.013

# Chi Squared Test for Invert Hosts

chisq.stage.table.invert <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.invert)
chisq.stage.invert <- chisq.test(chisq.stage.table.invert)
fisher.stage.invert <- fisher.test(chisq.stage.table.invert)
count.stage.invert <- as.data.frame(chisq.stage.table.invert)
count.stage.invert$ECOSYSTEM <- factor(count.stage.invert$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

stage.comp.1 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.invert[stage.comp.invert$ECOSYSTEM %in% c("Kelp forest", "Atoll lagoon sandflat"),])
chisq.stage.comp1 <- chisq.test(stage.comp.1) # p = 0.0078
fisher.stage.comp1 <- fisher.test(stage.comp.1) # p = 0.00073

stage.comp.2 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.invert[stage.comp.invert$ECOSYSTEM %in% c("Kelp forest", "Vent"),])
chisq.stage.comp2 <- chisq.test(stage.comp.2) # p = 0.313
fisher.stage.comp2 <- fisher.test(stage.comp.2) # p = 0.32

stage.comp.3 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_STAGE, stage.comp.invert[stage.comp.invert$ECOSYSTEM %in% c("Atoll lagoon sandflat", "Vent"),])
chisq.stage.comp3 <- chisq.test(stage.comp.3) # p = 0.021
fisher.stage.comp3 <- fisher.test(stage.comp.3) # p = 0.0074

# Stacked barplot for parasite life stages in fish hosts

ggsave(paste(path, "Figures", paste0("Figure_EPR_Analysis_Composition_Stage_Fish_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(count.stage.fish, aes(fill=PARASITE_LIFE_STAGE, y=Freq, x=ECOSYSTEM)) +
         geom_bar(position = "fill", stat="identity", width=0.6, color = "black") +
         xlab("Ecosystem") +
         ylab("Morphogroup Count") +
         scale_fill_manual("Life Stage", values = stagecolors) +
         ggtitle("Parasite Life Stages in Fish Hosts") +
         theme(text = element_text(size = 18, colour = "black"),
               plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
               axis.text.x = element_text(color = "gray48"),
               #legend.title = element_text(size = 22), # Change legend title font size
               #legend.text = element_text(size = 18),
               axis.title.x = element_text(vjust = -0.6),
               axis.ticks.x=element_blank(),
               axis.text.y = element_text(colour = "black"),
               panel.grid.major = element_line(color = "white"),
               panel.grid.minor = element_line(color = "white"),
               panel.background = element_rect(fill = "transparent"),
               legend.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 5, width = 8)

# Stacked barplot for parasite life stages in invertebrate hosts

ggsave(paste(path, "Figures", paste0("Figure_EPR_Analysis_Composition_Stage_Invert_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(count.stage.invert, aes(fill=PARASITE_LIFE_STAGE, y=Freq, x=ECOSYSTEM)) +
         geom_bar(position = "fill", stat="identity", width=0.6, color = "black") +
         xlab("Ecosystem") +
         ylab("Morphogroup Count") +
         scale_fill_manual("Life Stage", values = stagecolors) +
         ggtitle("Parasite Life Stages in Invertebrate Hosts") +
         theme(text = element_text(size = 18, colour = "black"),
               plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
               axis.text.x = element_text(color = "gray48"),
               axis.title.x = element_text(vjust = -0.6),
               axis.ticks.x=element_blank(),
               axis.text.y = element_text(colour = "black"),
               panel.grid.major = element_line(color = "white"),
               panel.grid.minor = element_line(color = "white"),
               panel.background = element_rect(fill = "transparent"),
               legend.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height = 5, width = 8)
