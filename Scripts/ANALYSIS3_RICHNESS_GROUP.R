# This script produces species accumulation curves for the major parasite groups in fish hosts.
# June 16, 2021
# Modified November 4, 2021
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

# library(ggplot2)
# library(reshape)
# library(tidyverse)
# library(lme4)
# library(barplot3d)
# library(AICcmodavg)
# library(fossil)
# library(wesanderson)
# library(vegan)
# library(plotrix)
# library(gplots)

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

spp.to.remove <- c("Anisakidae gen..NJ.MO10", "Anisakidae gen..unknown.nematodes", "Anisakis.sp. A", "Cestoda.C.MET.MO2", "DIG.MO10.DIG.MO10", "DIG.MO3.DIG.MO3",
                   "DIG.MO9.DIG.MO9", "Trematoda.DIGEJUV", "Trematoda.DIGEUNK", "Digene.unknown", "META.MO16.META.MO16", "Metacercaria.sp. I", "Metacercaria.UNK.MO11", "Metacercaria.unknown",
                   "Metacestode.CMET.MO1", "Metacestode.CMET.MO3", "Metacestode.unknown", "Trematoda.METAUNK", "Monogene.unknown", "NEM.AB2.NEM.AB2", "Nematoda.unknown",
                   "NJ.MO4.sp.", "Strong_Legs_Copepod", "Tetraphyllid metacestode.TTRM.MO6", "HOST_ID", "NONE", "HOST_SPECIES")

stages.to.remove <- c("META.MO5.OXCA.1185.5.", "Metacercaria.sp. B", "Metacercaria.sp. C", "Metacercaria.sp. D", "Mesobathylebouria.SPOR01")

# DEFINING ECOSYSTEM COLORS

ecocolors <- hcl.colors(5, "Geyser")[c(5,4,1)]
names(ecocolors) <- c("Kelp forest", "Atoll lagoon sandflat", "Vent")

# GENERATING ACCUMULATION CURVES FOR PARASITE GROUPS OF FISH

final.data.table <- as.data.frame(matrix(nrow = 0, ncol = 5))

ecosystems <- unique(data.all$ECOSYSTEM)
groups.host <- unique(data.all$HOST_GROUP_COMMON_NAME)
groups.psite <- c("Acanthocephala", "Cestoda", "Copepoda", "Isopoda", "Monogenea", "Nematoda", "Rhizocephala", "Trematoda")

for (ecosystem in ecosystems) {
  
  for (group.h in groups.host) {
    
    for (group.p in groups.psite) {
      
      data.subset <- data.all[data.all$ECOSYSTEM == ecosystem & data.all$HOST_GROUP_COMMON_NAME == group.h,]
      data.subset$PARASITE_COUNT[!data.subset$PARASITE_GROUP_COMMON_NAME == group.p] <- 0
      data.subset$PARASITE_SPECIES_NAME[!data.subset$PARASITE_GROUP_COMMON_NAME == group.p] <- "NONE"
      data.subset$PARASITE_LIFE_CYCLE[!data.subset$PARASITE_GROUP_COMMON_NAME == group.p] <- "NONE"
      psite.spp.table <- cast(data.subset, HOST_SPECIES ~ PARASITE_SPECIES_NAME, value = "PARASITE_COUNT", fun.aggregate = sum)
      rownames(psite.spp.table) <- psite.spp.table$HOST_SPECIES
      psite.spp.table <- psite.spp.table[!colnames(psite.spp.table) %in% c(spp.to.remove, stages.to.remove)]
      
      L <- colSums(psite.spp.table != 0)
      H <- as.integer(length(rownames(psite.spp.table)))
      h_list = c(1:length(rownames(psite.spp.table)))
      
      results <- c(0)
      
      for (h in h_list) {
        
        p.hat <- 0
        
        for (Lj in L) {
          
          p.hat <- p.hat + (1 - choose((H-Lj), h)/choose(H, h))
          
        }
        
        results <- c(results, p.hat)
        
      }
      
      results.data.frame <- cbind(rep(ecosystem, length(results)), rep(group.h, length(results)), rep(group.p, length(results)), c(0:(length(results)-1)), results)
      final.data.table <- rbind(final.data.table, results.data.frame)
      
    }
    
  }
  
}

colnames(final.data.table) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "PARASITE_GROUP_COMMON_NAME", "HOST_RICHNESS", "ESTIMATED_PARASITE_RICHNESS")

final.data.table$HOST_RICHNESS <- as.numeric(as.character(final.data.table$HOST_RICHNESS))
final.data.table$ESTIMATED_PARASITE_RICHNESS <- as.numeric(as.character(final.data.table$ESTIMATED_PARASITE_RICHNESS))
final.data.table$ECOSYSTEM <- factor(final.data.table$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

# Plotting major parasite group diversity

# Acanthocephala

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Acanthocephala_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[final.data.table$PARASITE_GROUP_COMMON_NAME == "Acanthocephala" & final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=ESTIMATED_PARASITE_RICHNESS, colour=ECOSYSTEM))
       + geom_line(size = 5, alpha = 1.0)
       + ylim(0,15)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + theme_bw() + theme(text = element_text(size = 20),
                            #legend.position = "none",
                            panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.key.width = unit(1, 'cm'), #change legend key width
                            legend.title = element_text(size=16),
                            legend.text = element_text(size=14),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Acanthocephala")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(1,1,1,1), "cm")), height = 4, width = 4.5)

# Cestoda

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Cestoda_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[final.data.table$PARASITE_GROUP_COMMON_NAME == "Cestoda" & final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=ESTIMATED_PARASITE_RICHNESS, colour=ECOSYSTEM))
       + geom_line(size = 5, alpha = 1.0)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + ylim(0, 15)
       + theme_bw() + theme(text = element_text(size = 20),
                            legend.position = "none",
                            panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.key.width = unit(1, 'cm'), #change legend key width
                            legend.title = element_text(size=12),
                            legend.text = element_text(size=8),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Cestoda")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(1,1,1,1), "cm")), height = 4, width = 4.5)

# Copepoda

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Copepoda_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[final.data.table$PARASITE_GROUP_COMMON_NAME == "Copepoda" & final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=ESTIMATED_PARASITE_RICHNESS, colour=ECOSYSTEM))
       + geom_line(size = 5, alpha = 1.0)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + ylim(0, 15)
       + theme_bw() + theme(text = element_text(size = 20),
                            legend.position = "none",
                            panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.key.width = unit(1, 'cm'), #change legend key width
                            legend.title = element_text(size=12),
                            legend.text = element_text(size=8),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Copepoda")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(1,1,1,1), "cm")), height = 4, width = 4.5)

# Monogenea

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Monogenea_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[final.data.table$PARASITE_GROUP_COMMON_NAME == "Monogenea" & final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=ESTIMATED_PARASITE_RICHNESS, colour=ECOSYSTEM))
       + geom_line(size = 5, alpha = 1.0)
       + ylim(0, 15)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + theme_bw() + theme(text = element_text(size = 20),
                            legend.position = "none",
                            panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.key.width = unit(1, 'cm'), #change legend key width
                            legend.title = element_text(size=12),
                            legend.text = element_text(size=8),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Monogenea")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(1,1,1,1), "cm")), height = 4, width = 4.5)

# Nematoda

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Nematoda_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[final.data.table$PARASITE_GROUP_COMMON_NAME == "Nematoda" & final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=ESTIMATED_PARASITE_RICHNESS, colour=ECOSYSTEM))
       + geom_line(size = 5, alpha = 1.0)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + theme_bw() + theme(text = element_text(size = 20),
                            legend.position = "none",
                            panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.key.width = unit(1, 'cm'), #change legend key width
                            legend.title = element_text(size=12),
                            legend.text = element_text(size=8),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Nematoda")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(1,1,1,1), "cm")), height = 4, width = 4.5)

# Trematoda

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Trematoda_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[final.data.table$PARASITE_GROUP_COMMON_NAME == "Trematoda" & final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=ESTIMATED_PARASITE_RICHNESS, colour=ECOSYSTEM))
       + geom_line(size = 5, alpha = 1.0)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + theme_bw() + theme(text = element_text(size = 20),
                            legend.position = "none",
                            panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                            legend.key.width = unit(1, 'cm'), #change legend key width
                            legend.title = element_text(size=12),
                            legend.text = element_text(size=8),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Trematoda")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(1,1,1,1), "cm")), height = 4, width = 4.5)
