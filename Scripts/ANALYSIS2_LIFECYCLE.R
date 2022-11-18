# This script analyzes the relative proportions of species with direct and indirect life cycles.
# June 16, 2021
# Modified November 4, 2021
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

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
cyclecolors <- hcl.colors(10, "Green-Brown")[c(1,7)]
names(cyclecolors) <- c("Direct", "Indirect")

# LIFE CYCLE ANALYSIS

psite.spp.cycle.table <- aggregate(data.all$PARASITE_COUNT, by = list(data.all$PARASITE_SPECIES_NAME, data.all$ECOSYSTEM, data.all$HOST_GROUP_COMMON_NAME_SIMPLE, data.all$PARASITE_LIFE_CYCLE), FUN=sum)
colnames(psite.spp.cycle.table) <- c("PARASITE_SPECIES_NAME", "ECOSYSTEM", "HOST_GROUP_COMMON_NAME_SIMPLE", "PARASITE_LIFE_CYCLE", "PARASITE_COUNT")

psite.spp.cycle.table <- psite.spp.cycle.table[!psite.spp.cycle.table$PARASITE_SPECIES_NAME %in% c(spp.to.remove, stages.to.remove),] # Removes potentially redundant morophogroups in fish hosts

psite.spp.cycle.summary <- aggregate(psite.spp.cycle.table$PARASITE_COUNT, 
                                     by = list(psite.spp.cycle.table$ECOSYSTEM, 
                                               psite.spp.cycle.table$HOST_GROUP_COMMON_NAME_SIMPLE,
                                               psite.spp.cycle.table$PARASITE_LIFE_CYCLE), function(x) sum(x != 0))

colnames(psite.spp.cycle.summary) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME_SIMPLE", "PARASITE_LIFE_CYCLE", "PARASITE_SP_COUNT")

cycle.comp.fish <- psite.spp.cycle.summary[psite.spp.cycle.summary$HOST_GROUP_COMMON_NAME_SIMPLE == "Fish",]
cycle.comp.invert <- psite.spp.cycle.summary[psite.spp.cycle.summary$HOST_GROUP_COMMON_NAME_SIMPLE == "Invertebrate",]

# Chi Squared Test for Fish Hosts

chisq.cycle.table.fish <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_CYCLE, cycle.comp.fish)
chisq.cycle.fish <- chisq.test(chisq.cycle.table.fish) # 0.02
fisher.cycle.fish <- fisher.test(chisq.cycle.table.fish) # 0.022
count.cycle.fish <- as.data.frame(chisq.cycle.table.fish)
count.cycle.fish$ECOSYSTEM <- factor(count.cycle.fish$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

cycle.comp.1 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_CYCLE, cycle.comp.fish[cycle.comp.fish$ECOSYSTEM %in% c("Kelp forest", "Atoll lagoon sandflat"),])
chisq.cycle.comp1 <- chisq.test(cycle.comp.1) # Almost significant p = 0.07

cycle.comp.2 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_CYCLE, cycle.comp.fish[cycle.comp.fish$ECOSYSTEM %in% c("Kelp forest", "Vent"),])
chisq.cycle.comp2 <- chisq.test(cycle.comp.2) # Not significant p = 0.31
fisher.cycle.comp2 <- fisher.test(cycle.comp.2) # 0.28

cycle.comp.3 <- xtabs(PARASITE_SP_COUNT ~ ECOSYSTEM + PARASITE_LIFE_CYCLE, cycle.comp.fish[cycle.comp.fish$ECOSYSTEM %in% c("Atoll lagoon sandflat", "Vent"),])
chisq.cycle.comp3 <- chisq.test(cycle.comp.3) # Significant p = 0.04
fisher.cycle.comp3 <- fisher.test(cycle.comp.3) # 0.026

# Chi Squared Test for Invert Hosts

chisq.cycle.table.invert <- xtabs(PARASITE_SP_COUNT ~ PARASITE_LIFE_CYCLE + ECOSYSTEM, cycle.comp.invert)
chisq.cycle.invert <- chisq.test(chisq.cycle.table.invert) # Not significant p = 0.21
fisher.cycle.invert <- fisher.test(chisq.cycle.table.invert) # p = 0.14
count.cycle.invert <- as.data.frame(chisq.cycle.table.invert)
count.cycle.invert$ECOSYSTEM <- factor(count.cycle.invert$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

# Stacked barplot for parasite life cycles in fish hosts

ggsave(paste(path, "Figures", paste0("Figure_EPR_Analysis_Composition_Cycle_Fish_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(count.cycle.fish, aes(fill=PARASITE_LIFE_CYCLE, y=Freq, x=ECOSYSTEM)) +
         geom_bar(position = "fill", stat="identity", width=0.6, color = "black") +
         xlab("Ecosystem") +
         ylab("Morphogroup Count") +
         scale_fill_manual("Life Cycle", values = cyclecolors) +
         ggtitle("Parasite Life Cycles in Fish Hosts") +
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

# Stacked barplot for parasite life cycles in invertebrate hosts

ggsave(paste(path, "Figures", paste0("Figure_EPR_Analysis_Composition_Cycle_Invert_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(count.cycle.invert, aes(fill=PARASITE_LIFE_CYCLE, y=Freq, x=ECOSYSTEM)) +
         geom_bar(position = "fill", stat="identity", width=0.6, color = "black") +
         xlab("Ecosystem") +
         ylab("Morphogroup Count") +
         scale_fill_manual("Life Stage", values = cyclecolors) +
         ggtitle("Parasite Life Cycles in Invertebrate Hosts") +
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
