### PARASITES REPORTED IN LITERATURE
# This script tidies and analyzes spreadsheet from vent parasite reported in the literature as of May 2022.

# Lauren Dykman
# Created May 20, 2021
# Modified April 26, 2022

rm(list=ls())

# INSTALLING PACKAGES

#install.packages(c("reshape", "tidyverse", "dplyr"))

library(reshape)
library(tidyverse)
library(dplyr)

# SETTING WORKING DIRECTORY

path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

# UPLOADING DATA

data.literature <- read.csv(paste(path, "Data", "PARASITES_VENT_LITERATURE.csv", sep = "/"), header=TRUE)

data.literature$COLLECTION_YEAR[data.literature$COLLECTION_YEAR == "NR"] <- data.literature$CITATION_YEAR[data.literature$COLLECTION_YEAR == "NR"] # If collection year was not reported use publication year instead.

data.literature <- data.literature[data.literature$PARASITE_CONSUMER_STRATEGY %in% c("macroparasite"),]

table.year <- data.literature %>%
  group_by(COLLECTION_YEAR, PARASITE_GROUP_COMMON_NAME, PARASITE_SPECIES_PUBLICATION_ID) %>%
  tally()

table.year.summary <- table.year %>%
  group_by(COLLECTION_YEAR, PARASITE_GROUP_COMMON_NAME) %>%
  tally()

# Plotting cumulative species discovery over years

final.data.table <- as.data.frame(matrix(nrow=0,ncol=7))
final.data.table <- rbind(final.data.table, c(1975, rep(0, 6)))

sp.count.table <- cast(table.year.summary, COLLECTION_YEAR ~ PARASITE_GROUP_COMMON_NAME, value = "n")
sp.count.table[is.na(sp.count.table)] <- 0
sp.count.table <- as.data.frame(lapply(sp.count.table, as.numeric))

colnames(final.data.table) <- colnames(sp.count.table)

for (i in c(1:dim(sp.count.table)[1])) {
  
  running.sum <- colSums(rbind(final.data.table[i,2:7], sp.count.table[i,2:7]))
  
  final.data.table <- rbind(final.data.table, c(sp.count.table[i,1], running.sum))
  
}

rownames(final.data.table) <- final.data.table$COLLECTION_YEAR
final.data.t <- t(final.data.table[-1])

data.to.plot <- melt(final.data.t)
colnames(data.to.plot) <- c("PARASITE_GROUP_COMMON_NAME", "COLLECTION_YEAR", "n")

# DEFINING COLORS

n <- length(unique(data.to.plot$PARASITE_GROUP_COMMON_NAME))

groupcolors <- hcl.colors(n, "Geyser")
names(groupcolors) <- c(unique(data.to.plot$PARASITE_GROUP_COMMON_NAME))

# Plot

ggsave(paste(path, "Figures", paste0("Figure_Literature_Spp_Discovery_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"), sep = "/"),
  ggplot(data.to.plot, aes(x=COLLECTION_YEAR, y=n, fill=PARASITE_GROUP_COMMON_NAME)) + 
    geom_area(alpha=0.8, size=0.8, colour="black") +
    scale_x_continuous(breaks = seq(1970, 2005, by = 5)) +
    scale_fill_manual("Ecosystem", values = groupcolors) +
    scale_color_manual("Ecosystem", values = groupcolors) +
    xlab("Year") + 
    ylab("Cumulative Parasite Species") +
    ggtitle("Published Records of Vent Parasite Species") +
    theme(text = element_text(size = 26, colour = "black"),
          axis.line = element_line(colour = "grey50"),
          legend.position = c(.2, .7),
          panel.grid.major = element_line(color = NULL),
          panel.grid.minor = element_line(color = NULL),
          panel.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)), height = 8, width = 11)
