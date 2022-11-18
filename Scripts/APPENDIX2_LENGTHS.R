# This script plots parasite abundance and richness by host lengths.
# Created March 19 2022
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

# DEFINING ECOSYSTEM COLORS

ecocolors <- hcl.colors(5, "Geyser")[c(5,4,1)]
names(ecocolors) <- c("Kelp forest", "Atoll lagoon sandflat", "Vent")

# ANALYSIS 4: PLOTTING PARASITE ABUNDANCE AND DIVERSITY BY HOST LENGTHS

data.length <- aggregate(data.all$PARASITE_COUNT, by = list(data.all$ECOSYSTEM, data.all$HOST_GROUP_COMMON_NAME, data.all$HOST_SPECIES, data.all$HOST_ID, data.all$HOST_LENGTH_TOTAL_MM), FUN=sum)
colnames(data.length) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "HOST_SPECIES", "HOST_ID", "HOST_LENGTH", "PARASITE_COUNT")

psite.spp.per.host.table <- aggregate(data.all$PARASITE_COUNT, by = list(data.all$PARASITE_SPECIES_NAME, data.all$ECOSYSTEM, data.all$HOST_GROUP_COMMON_NAME, data.all$HOST_ID, data.all$PARASITE_GROUP_COMMON_NAME), FUN=sum)
colnames(psite.spp.per.host.table) <- c("PARASITE_SPECIES", "ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "HOST_ID", "PARASITE_GROUP", "PARASITE_COUNT")

psite.spp.per.host.summary <- aggregate(psite.spp.per.host.table$PARASITE_COUNT, by = list(psite.spp.per.host.table$ECOSYSTEM, psite.spp.per.host.table$HOST_GROUP_COMMON_NAME, psite.spp.per.host.table$HOST_ID), function(x) sum(x != 0))
colnames(psite.spp.per.host.summary) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "HOST_ID", "PARASITE_SP_COUNT")

data.length <- merge(data.length, psite.spp.per.host.summary[c("HOST_ID", "PARASITE_SP_COUNT")], by = "HOST_ID")
data.length$PARASITE_COUNT <- log(data.length$PARASITE_COUNT + 1) # Log + 1 transform abundance data.

data.length.mean <- aggregate(data.length$HOST_LENGTH, by = list(data.length$HOST_SPECIES), FUN=mean)
colnames(data.length.mean) <- c("HOST_SPECIES", "HOST_LENGTH_MEAN")
data.length.sd <- aggregate(data.length$HOST_LENGTH, by = list(data.length$HOST_SPECIES), FUN=sd)
colnames(data.length.sd) <- c("HOST_SPECIES", "HOST_LENGTH_SD")
data.richness.mean <- aggregate(data.length$PARASITE_SP_COUNT, by = list(data.length$HOST_SPECIES), FUN=mean)
colnames(data.richness.mean) <- c("HOST_SPECIES", "PARASITE_RICHNESS_MEAN")
data.richness.sd <- aggregate(data.length$PARASITE_SP_COUNT, by = list(data.length$HOST_SPECIES), FUN=sd)
colnames(data.richness.sd) <- c("HOST_SPECIES", "PARASITE_RICHNESS_SD")

data.count.mean <- aggregate(data.length$PARASITE_COUNT, by = list(data.length$HOST_SPECIES), FUN=mean)
colnames(data.count.mean) <- c("HOST_SPECIES", "PARASITE_COUNT_MEAN")

data.count.sd <- aggregate(data.length$PARASITE_COUNT, by = list(data.length$HOST_SPECIES), FUN=sd)
colnames(data.count.sd) <- c("HOST_SPECIES", "PARASITE_COUNT_SD")

hosts.examined <- merge(hosts.examined, data.length.mean, by = "HOST_SPECIES")
hosts.examined <- merge(hosts.examined, data.length.sd, by = "HOST_SPECIES")
hosts.examined <- merge(hosts.examined, data.richness.mean, by = "HOST_SPECIES")
hosts.examined <- merge(hosts.examined, data.richness.sd, by = "HOST_SPECIES")
hosts.examined <- merge(hosts.examined, data.count.mean, by = "HOST_SPECIES")
hosts.examined <- merge(hosts.examined, data.count.sd, by = "HOST_SPECIES")

hosts.examined$ECOSYSTEM <- factor(hosts.examined$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

# COUNTS BY HOST LENGTH

ggsave(paste(path, "Figures", paste0("Figure_Supplement_Length_Counts_All_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(hosts.examined, aes(x=HOST_LENGTH_MEAN, y=PARASITE_COUNT_MEAN, colour=ECOSYSTEM)) +
         geom_point(size = 3, alpha=0.5, inherit.aes = TRUE) +
         geom_errorbar(aes(x = HOST_LENGTH_MEAN, ymin = PARASITE_COUNT_MEAN - PARASITE_COUNT_SD, ymax = PARASITE_COUNT_MEAN + PARASITE_COUNT_SD), size = 0.4, width=10, alpha=0.6) +
         geom_errorbarh(aes(xmin = HOST_LENGTH_MEAN - HOST_LENGTH_SD, xmax = HOST_LENGTH_MEAN + HOST_LENGTH_SD, y = PARASITE_COUNT_MEAN), size = 0.4, height=0.5, alpha=0.6) +
         scale_fill_manual("Ecosystem", values = ecocolors) +
         scale_color_manual("Ecosystem", values = ecocolors) +
         xlab("Host Length (mm)") +
         ylab("Parasite count") +
         labs(color='Ecosystem') +
         ggtitle("Parasite counts by host length") + # Setting the title
         theme_bw() +
         facet_grid(rows = vars(HOST_GROUP_COMMON_NAME)) +
         theme(text=element_text(size=12), aspect.ratio=0.5, panel.border = element_rect(colour="black"), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
               legend.title = element_text(size=12), #change legend title font size
               legend.text = element_text(size=10)), height=6, width=6)# Setting the theme

# RICHNESS BY HOST LENGTH

ggsave(paste(path, "Figures", paste0("Figure_Supplement_Length_Richness_All_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(hosts.examined, aes(x=HOST_LENGTH_MEAN, y=PARASITE_RICHNESS_MEAN, colour=ECOSYSTEM)) +
         geom_point(size = 3, alpha=0.5, inherit.aes = TRUE) +
         geom_errorbar(aes(x = HOST_LENGTH_MEAN, ymin = PARASITE_RICHNESS_MEAN - PARASITE_RICHNESS_SD, ymax = PARASITE_RICHNESS_MEAN + PARASITE_RICHNESS_SD), size = 0.4, width=10, alpha=0.6) +
         geom_errorbarh(aes(xmin = HOST_LENGTH_MEAN - HOST_LENGTH_SD, xmax = HOST_LENGTH_MEAN + HOST_LENGTH_SD, y = PARASITE_RICHNESS_MEAN), size = 0.4, height=0.5, alpha=0.6) +
         scale_fill_manual("Ecosystem", values = ecocolors) +
         scale_color_manual("Ecosystem", values = ecocolors) +
         xlab("Host Length (mm)") +
         ylab("Parasite richness") +
         labs(color='Ecosystem') +
         ggtitle("Parasite richness by host length") + # Setting the title
         theme_bw() +
         facet_grid(rows = vars(HOST_GROUP_COMMON_NAME)) +
         theme(text=element_text(size=12), aspect.ratio=0.5, panel.border = element_rect(colour="black"), panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
               legend.title = element_text(size=12), #change legend title font size
               legend.text = element_text(size=10)), height=6, width=6)# Setting the theme
