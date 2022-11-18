# This script analyzes overall parasite richness comparing three ecosystems: kelp forest, Palmyra Atoll lagoon sandflat, and vent.
# Created Mar 14 2022
# Modified Sept 5 2022
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

# install.packages("fossil")
# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("tidyverse")

library(fossil)
library(ggplot2)
library(reshape)
library(tidyverse)

# SETTING WORKING DIRECTORY

# Re-name to your working directory
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
                   "DIG.MO9.DIG.MO9", "Trematoda.DIGEJUV", "Trematoda.DIGEUNK", "Biospeedotrema.DIGEUNK", "Digene.unknown", "META.MO16.META.MO16", "Metacercaria.sp. I", "Metacercaria.UNK.MO11", "Metacercaria.unknown",
                   "Metacestode.CMET.MO1", "Metacestode.CMET.MO3", "Metacestode.unknown", "Trematoda.METAUNK", "Monogene.unknown", "NEM.AB2.NEM.AB2", "Nematoda.unknown",
                   "NJ.MO4.sp.", "Strong_Legs_Copepod", "Tetraphyllid metacestode.TTRM.MO6", "HOST_ID", "NONE", "HOST_SPECIES")

stages.to.remove <- c("META.MO5.OXCA.1185.5.", "Metacercaria.sp. B", "Metacercaria.sp. C", "Metacercaria.sp. D", "Mesobathylebouria.SPOR01")

# DEFINING ECOSYSTEM COLORS

ecocolors <- hcl.colors(5, "Geyser")[c(5,4,1)]
names(ecocolors) <- c("Kelp forest", "Atoll lagoon sandflat", "Vent")

################################################

# PARASITE SPECIES PER HOST SPECIES RAW DATA

psite.spp.per.host.spp.table <- aggregate(data.all$PARASITE_COUNT, by = list(data.all$PARASITE_SPECIES_NAME, data.all$ECOSYSTEM, data.all$HOST_GROUP_COMMON_NAME, data.all$HOST_SPECIES, data.all$PARASITE_GROUP_COMMON_NAME), FUN=sum)
colnames(psite.spp.per.host.spp.table) <- c("PARASITE_SPECIES_NAME", "ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "HOST_SPECIES", "PARASITE_GROUP_COMMON_NAME", "PARASITE_COUNT")

psite.spp.per.host.spp.table$PARASITE_COUNT[psite.spp.per.host.spp.table$PARASITE_SPECIES_NAME %in% spp.to.remove] <- 0

psite.spp.per.host.spp.summary <- aggregate(psite.spp.per.host.spp.table$PARASITE_COUNT, by = list(psite.spp.per.host.spp.table$ECOSYSTEM, psite.spp.per.host.spp.table$HOST_GROUP_COMMON_NAME, psite.spp.per.host.spp.table$HOST_SPECIES), function(x) sum(x != 0))
colnames(psite.spp.per.host.spp.summary) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "HOST_SPECIES", "PARASITE_RICHNESS")

psite.spp.per.host.spp.summary$ECOSYSTEM <- factor(psite.spp.per.host.spp.summary$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

psite.spp.raw.mean <- aggregate(psite.spp.per.host.spp.summary$PARASITE_RICHNESS, by = list(psite.spp.per.host.spp.summary$ECOSYSTEM, psite.spp.per.host.spp.summary$HOST_GROUP_COMMON_NAME), FUN=mean)
colnames(psite.spp.raw.mean) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "PARASITE_RICHNESS_MEAN")
psite.spp.raw.mean$ECO_GROUP <- paste(psite.spp.raw.mean$ECOSYSTEM, psite.spp.raw.mean$HOST_GROUP_COMMON_NAME, sep = "_")

psite.spp.raw.sd <- aggregate(psite.spp.per.host.spp.summary$PARASITE_RICHNESS, by = list(psite.spp.per.host.spp.summary$ECOSYSTEM, psite.spp.per.host.spp.summary$HOST_GROUP_COMMON_NAME), FUN=sd)
colnames(psite.spp.raw.sd) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "PARASITE_RICHNESS_SD")
psite.spp.raw.sd$ECO_GROUP <- paste(psite.spp.raw.sd$ECOSYSTEM, psite.spp.raw.sd$HOST_GROUP_COMMON_NAME, sep = "_")

psite.spp.raw.table <- merge(psite.spp.raw.mean, psite.spp.raw.sd[c("ECO_GROUP", "PARASITE_RICHNESS_SD")], by = "ECO_GROUP")
psite.spp.raw.table <- merge(psite.spp.raw.table, host.species[c("ECO_GROUP", "n")], by = "ECO_GROUP")
psite.spp.raw.table$SQRT_N <- sqrt(psite.spp.raw.table$n)
psite.spp.raw.table$STD_ERR <- psite.spp.raw.table$PARASITE_RICHNESS_SD / psite.spp.raw.table$SQRT_N

# ANALYSIS 1: PARASITE CHAO2 RICHNESS ESTIMATE PER HOST SPECIES

final.data.table <- as.data.frame(matrix(nrow=0, ncol=4))

groups <- unique(data.all$HOST_GROUP_COMMON_NAME)
ecosystems <- unique(data.all$ECOSYSTEM)

for (ecosystem in ecosystems) {

  for (group in groups) {

    print(c(ecosystem, group))

    data.subset <- data.all[data.all$ECOSYSTEM == ecosystem & data.all$HOST_GROUP_COMMON_NAME == group,]

    species <- unique(data.subset$HOST_SPECIES)

    for (spec in species) {

      print(spec)

      data.subset.2 <- data.subset[data.subset$HOST_SPECIES == spec,]
      psite.spp.table <- cast(data.subset.2, HOST_ID ~ PARASITE_SPECIES_NAME, value = "PARASITE_COUNT", fun.aggregate = sum)
      rownames(psite.spp.table) <- psite.spp.table$HOST_ID
      psite.spp.table <- psite.spp.table[!colnames(psite.spp.table) %in% spp.to.remove]

      if (dim(psite.spp.table)[2] == 0) {

        chao <- 0
        
      }

      else if (dim(psite.spp.table)[2] == 1) {

        chao <- 1

      }

      else {

        chao <- chao2(psite.spp.table, taxa.row=FALSE)

      }

      final.data.table <- rbind(final.data.table, c(ecosystem, group, spec, chao))
    }
  }
}

colnames(final.data.table) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "HOST_SPECIES", "CHAO2")

final.data.table$CHAO2 <- as.numeric(as.character(final.data.table$CHAO2))
final.data.table$ECOSYSTEM <- factor(final.data.table$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))
final.data.table$HOST_SPECIES <- factor(final.data.table$HOST_SPECIES, levels = sort(unique(final.data.table$HOST_SPECIES)))

chao.spp.raw.mean <- aggregate(final.data.table$CHAO2, by = list(final.data.table$ECOSYSTEM, final.data.table$HOST_GROUP_COMMON_NAME), FUN=mean)
colnames(chao.spp.raw.mean) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "PARASITE_RICHNESS_MEAN")
chao.spp.raw.mean$ECO_GROUP <- paste(chao.spp.raw.mean$ECOSYSTEM, chao.spp.raw.mean$HOST_GROUP_COMMON_NAME, sep = "_")

chao.spp.raw.sd <- aggregate(final.data.table$CHAO2, by = list(final.data.table$ECOSYSTEM, final.data.table$HOST_GROUP_COMMON_NAME), FUN=sd)
colnames(chao.spp.raw.sd) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "PARASITE_RICHNESS_SD")
chao.spp.raw.sd$ECO_GROUP <- paste(chao.spp.raw.sd$ECOSYSTEM, chao.spp.raw.sd$HOST_GROUP_COMMON_NAME, sep = "_")

chao.spp.raw.table <- merge(chao.spp.raw.mean, chao.spp.raw.sd[c("ECO_GROUP", "PARASITE_RICHNESS_SD")], by = "ECO_GROUP")
chao.spp.raw.table <- merge(chao.spp.raw.table, host.species[c("ECO_GROUP", "n")], by = "ECO_GROUP")
chao.spp.raw.table$SQRT_N <- sqrt(chao.spp.raw.table$n)
chao.spp.raw.table$STD_ERR <- chao.spp.raw.table$PARASITE_RICHNESS_SD / chao.spp.raw.table$SQRT_N

# Getting average length measurement for host species

data.subset <- data.all[!is.na(data.all$HOST_LENGTH_TOTAL_MM),]
data.length.mean <- aggregate(data.subset$HOST_LENGTH_TOTAL_MM, by = list(data.subset$HOST_SPECIES), FUN=mean)
colnames(data.length.mean) <- c("HOST_SPECIES", "HOST_LENGTH_MEAN")

# Species with no size data were assigned based on average or single reported length for species from online sources.

data.length.mean <- rbind(data.length.mean, c("Megathura crenulata", 120.0))
data.length.mean <- rbind(data.length.mean, c("Alia carinata", 8.0))
data.length.mean <- rbind(data.length.mean, c("Spirorbinae gen. sp.", 5.0))
data.length.mean <- rbind(data.length.mean, c("Uca_tetragonon", 19.0))
data.length.mean <- rbind(data.length.mean, c("Tellina_crucigera", 50.0))
data.length.mean <- rbind(data.length.mean, c("Malacoceros_sp_1", 78.0))

# Megathura crenulata 12 cm (120 mm)
# Alia carinata 8 mm
# Spirorbinae 5 mm
# Uca tetragonon 18.6 mm
# Tellina crucigera 50 mm
# Malacoceros 78 mm

final.data.table <- merge(final.data.table, data.length.mean, by = "HOST_SPECIES")

filename2 = paste0("STATISTICS_Data_Table_Chao2_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(final.data.table, paste(path, "Output", filename2, sep = "/"))

# PARASITE SPECIES PER HOST SPECIES CHAO2

ggsave(paste(path, "Figures", paste0("Figure_Richness_Barplot_Chao2_HostSpecies_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(chao.spp.raw.table, aes(fill = ECOSYSTEM, x=HOST_GROUP_COMMON_NAME, y=PARASITE_RICHNESS_MEAN)) +
         geom_bar(stat = "identity", position = position_dodge(), color = "black") +
         geom_errorbar(aes(x = HOST_GROUP_COMMON_NAME, 
                           ymin = ifelse(PARASITE_RICHNESS_MEAN - STD_ERR <= 0, 0, PARASITE_RICHNESS_MEAN - STD_ERR), 
                           ymax = PARASITE_RICHNESS_MEAN + STD_ERR), size = 0.5, width = 0.4, stat = "identity", position = position_dodge(0.9))+ #, size = 0.2, width=0.2, alpha=0.6) +# Setting the theme
         scale_fill_manual("Ecosystem", values = ecocolors) +
         scale_color_manual("Ecosystem", values = ecocolors) +
         xlab("Host Group") +
         ylab("Parasite Richness") +
         ggtitle("Parasite Richness Per Host Species") +
         theme_bw() +
         labs(fill = "Ecosystem") +
         labs(color = "Ecosystem") +
         ylim(c(0,30))+
         theme(text=element_text(size = 26),
               legend.position = c(0.8, 0.75),
               aspect.ratio=0.6,
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.title = element_text(size=18),
               legend.text = element_text(size=16),
               panel.background = element_rect(fill = "transparent", colour = NA),
               plot.background = element_rect(fill = "transparent", colour = NA)), height=6, width=8) # Setting the theme

#################################################

### PARASITE SPECIES PER HOST SPECIES ACCUMULATION CURVES

# GENERATE SPECIES ACCUMULATION CURVES FOR FISH AND INVERTEBRATES

# Calculating conservative estimates for species diversity. This removes any parasite groups that might pass from one host to another within fish or invertebrates.

final.data.table <- as.data.frame(matrix(nrow=0,ncol=4))

ecosystems <- unique(data.all$ECOSYSTEM)
groups <- unique(data.all$HOST_GROUP_COMMON_NAME_SIMPLE)

for (ecosystem in ecosystems) {
  
  for (group in groups) {
    
    data.subset <- data.all[data.all$ECOSYSTEM == ecosystem & data.all$HOST_GROUP_COMMON_NAME_SIMPLE == group,]
    psite.spp.table <- cast(data.subset, HOST_SPECIES~PARASITE_SPECIES_NAME, value = "PARASITE_COUNT", fun.aggregate = sum)
    rownames(psite.spp.table) <- psite.spp.table$HOST_SPECIES
    psite.spp.table <- psite.spp.table[!colnames(psite.spp.table) %in% c(spp.to.remove, stages.to.remove)] # In future change this back to fish.spp.to.remove
    
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
    
    results.data.frame <- cbind(rep(ecosystem, length(results)), rep(group, length(results)), c(0:(length(results)-1)), results)
    final.data.table <- rbind(final.data.table, results.data.frame)
    
  }
  
}

colnames(final.data.table) <- c("ECOSYSTEM", "HOST_GROUP_COMMON_NAME", "HOST_RICHNESS", "PARASITE_RICHNESS")

final.data.table$HOST_RICHNESS <- as.numeric(as.character(final.data.table$HOST_RICHNESS))
final.data.table$PARASITE_RICHNESS <- as.numeric(as.character(final.data.table$PARASITE_RICHNESS))
final.data.table$AGG <- paste(final.data.table$ECOSYSTEM, final.data.table$HOST_GROUP_COMMON_NAME, final.data.table$HOST_RICHNESS, sep = "_")

final.data.table$ECOSYSTEM <- factor(final.data.table$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))

# Plotting Curves for Fish Hosts

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Fish_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=PARASITE_RICHNESS, color=ECOSYSTEM, fill = ECOSYSTEM))
       + geom_line(size = 6)
       + scale_fill_manual("Ecosystem", values = ecocolors)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + theme_bw() + theme(text = element_text(size = 28),
                            legend.position = "none",
                            panel.border = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.line = element_line(colour = "black"),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Fish")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(2,2,2,2), "cm")), height = 6, width = 7)

# Plotting Curves for Invertebrate Hosts

ggsave(paste(path, "Figures", paste0("Figure_Richness_Curves_Invert_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
       ggplot(final.data.table[!final.data.table$HOST_GROUP_COMMON_NAME == "Fish",], aes(x=HOST_RICHNESS, y=PARASITE_RICHNESS, color=ECOSYSTEM, fill = ECOSYSTEM))
       + geom_line(size = 6)
       + scale_fill_manual("Ecosystem", values = ecocolors)
       + scale_color_manual("Ecosystem", values = ecocolors)
       + theme_bw() + theme(text = element_text(size = 28),
                            legend.position = "none",
                            panel.border = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.line = element_line(colour = "black"),
                            panel.background = element_rect(fill = "transparent", colour = NA),
                            plot.background = element_rect(fill = "transparent", colour = NA))
       + ggtitle("Invertebrates")
       + xlab("Host Richness") + ylab("Parasite Richness") + theme(plot.margin = unit(c(2,2,2,2), "cm")), height=6, width=7)
