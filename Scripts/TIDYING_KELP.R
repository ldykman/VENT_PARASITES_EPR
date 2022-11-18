# This file tidies dissection data from kelp forests in Santa Barbara from Morton et al. 2021
# Created March 25, 2021
# Modified March 14 2022
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

# None required

# SETTING WORKING DIRECTORY

path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

# KELP FOREST DATA SET

kelp.dissection <- read.csv(paste(path, "Data", "8_Dissection_data.csv", sep = "/"), header=TRUE)

# CHANGING COLUMN NAMES FOR CONSISTENCY

colnames(kelp.dissection)[colnames(kelp.dissection) == "Collection.Date"] <- "COLLECTION_DATE"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Dissection.Date"] <- "DISSECTION_DATE"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Dissected.By"] <- "DISSECTOR"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Site.Name"] <- "COLLECTION_SITE"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Host.Group"] <- "HOST_GROUP_COMMON_NAME"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Full.Host.ID"] <- "HOST_ID"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Host.length_mm"] <- "HOST_LENGTH_TOTAL_MM"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Host.weight_g"] <- "HOST_WEIGHT_WET_G"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Psite_Group"] <- "PARASITE_GROUP_COMMON_NAME"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Parasite.life.stage"] <- "PARASITE_LIFE_STAGE"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Parasite.count"] <- "PARASITE_COUNT"
colnames(kelp.dissection)[colnames(kelp.dissection) == "host.sex"] <- "HOST_SEX"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Tissue"] <- "HOST_TISSUE"
colnames(kelp.dissection)[colnames(kelp.dissection) == "Notes"] <- "NOTES"

kelp.dissection$ECOSYSTEM <- c(rep("Kelp forest", dim(kelp.dissection)[1]))
kelp.dissection$COLLECTOR <- c(rep(NA, dim(kelp.dissection)[1]))
kelp.dissection$COLLECTOR <- c(rep(NA, dim(kelp.dissection)[1]))
kelp.dissection$PARASITE_CONSUMER_STRATEGY <- c(rep(NA, dim(kelp.dissection)[1]))
kelp.dissection$COLLECTION_DATE <- as.Date(kelp.dissection$COLLECTION_DATE, format = "%m/%d/%y")

kelp.dissection$PARASITE_SPECIES_NAME <- paste(kelp.dissection$Final.ID.Parasite.Genus, kelp.dissection$Final.ID.Parasite.species, sep=".")
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$Parasite.count == 0] <- "NONE"
kelp.dissection$HOST_SPECIES <- paste(kelp.dissection$Host.Genus, kelp.dissection$Host.Sp., sep=" ")

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(kelp.dissection$PARASITE_SPECIES_NAME))
length(unique(kelp.dissection$HOST_SPECIES))
length(unique(kelp.dissection$HOST_ID))

# Parasite species: 331
# Host species: 119
# Host individuals: 1716

# RETAINING MAINLAND SITES

sites.to.keep <- c("Mohawk", "Naples", "UCSB seawater intake", "Goleta Bay/ Pier",
                    "Rich's IV", "Unnamed, Near Arroyo Burro", "Arroyo Burro", "Ellwood Mesa")

kelp.dissection <- kelp.dissection[kelp.dissection$COLLECTION_SITE %in% sites.to.keep,]

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(kelp.dissection$PARASITE_SPECIES_NAME))
length(unique(kelp.dissection$HOST_SPECIES))
length(unique(kelp.dissection$HOST_ID))

# Parasite species: 215
# Host species: 88
# Host individuals: 930

# REMOVING CERTAIN HOST GROUPS AND RENAMING OTHERS

kelp.dissection <- kelp.dissection[!kelp.dissection$HOST_GROUP_COMMON_NAME %in% c("Ophiuroidea", "Echinoderm", "Anemone"),]

# REMOVING IDS OF DEAD PARASITES

unique(kelp.dissection$PARASITE_SPECIES_NAME)

parasites.to.remove <- c("Dead.unknown", "Dead.unknown.nematodes", "unknown.debris", "Nematoda.dead juvenile", "Monogene egg.MONE.MO1", "Copepoda.unknown", "Acanthocephala.UNK.MO1")

# Copepoda.unknown notes say it was only a copepod egg sac so removed.

kelp.dissection$PARASITE_COUNT[kelp.dissection$PARASITE_SPECIES_NAME %in% parasites.to.remove 
                              | kelp.dissection$PARASITE_GROUP_COMMON_NAME == "None found"] <- 0

kelp.dissection$PARASITE_GROUP_COMMON_NAME[kelp.dissection$PARASITE_COUNT == 0 
                              | kelp.dissection$PARASITE_GROUP_COMMON_NAME == "None found"] <- NA

kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_COUNT == 0
                              | kelp.dissection$PARASITE_SPECIES_NAME == "None found.None found"] <- "NONE"

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(kelp.dissection$PARASITE_SPECIES_NAME))
length(unique(kelp.dissection$HOST_SPECIES))
length(unique(kelp.dissection$HOST_ID))

# Parasite species: 203
# Host species: 82
# Host individuals: 852

# SETTING PARASITE COUNT COLUMN TO NUMERIC

kelp.dissection$PARASITE_COUNT <- as.numeric(kelp.dissection$PARASITE_COUNT)

# REMOVING PARASITES WITH COUNT VALUES OF NA

# This removed Nematoda.Unknown.nematodes from Crassedoma giganteum and Urastoma.cyprinae from Mytilus californianus and Maxwellia gemma

kelp.dissection <- kelp.dissection[!is.na(kelp.dissection$PARASITE_COUNT),]

# CORRECTING CAPITALIZATION ERRORS

kelp.dissection$HOST_SPECIES[kelp.dissection$HOST_SPECIES == "paralabrax clathratus"] <- "Paralabrax clathratus"
kelp.dissection$HOST_SPECIES[kelp.dissection$HOST_SPECIES == "semicossyphus pulcher"] <- "Semicossyphus pulcher"
kelp.dissection$HOST_SPECIES[kelp.dissection$HOST_SPECIES == "Sebastes goodei "] <- "Sebastes goodei"
kelp.dissection$HOST_SPECIES[kelp.dissection$HOST_SPECIES == "Heterodontus  francisci"] <- "Heterodontus francisci"
kelp.dissection$HOST_SPECIES[kelp.dissection$HOST_SPECIES == "Flabellina Iodinea"] <- "Flabellina iodinea"
kelp.dissection$HOST_SPECIES[kelp.dissection$HOST_SPECIES == "mitra idae"] <- "Mitra idae"
kelp.dissection$HOST_SPECIES[kelp.dissection$HOST_SPECIES == "Ocinebrium  subangulato"] <- "Ocinebrium subangulato"

kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Cystidicolidae gen..sp. ", "Cystidicolidae gen. .sp. ")] <- "Cystidicolidae gen. sp."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Gnathia.stevensii"] <- "Gnathia.steveni"
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Philometra.sp. ")] <- "Philometra.sp."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Acuaridae gen..sp.")] <- "Acuariidae gen..sp."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Dichlyne.sp. A")] <- "Dichelyne.sp. A"
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Monorchidae.sp.unk.", "unk. Monorchidae.sp.")] <- "Monorchiidae.sp.unk."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Iphitime.sp. ")] <- "Iphitime.sp."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Bomolochus.cuneatus ")] <- "Bomolochus.cuneatus"
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Caligus .sp. A ")] <- "Caligus .sp. A"
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Peniculus.sp.", "Peniculus.sp ", "Peniculus.sp. ")] <- "Peniculus.sp."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Caligidae gen. .spp. ")] <- "Caligidae gen. spp."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Cemocotyle.sp. ")] <- "Cemocotyle.sp."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Caligus .COP.MO7")] <- "Caligus .COP.MO7"
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Hatschekia.sp. ")] <- "Hatschekia.sp."
kelp.dissection$PARASITE_GROUP_COMMON_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Gnathia.steveni" & kelp.dissection$PARASITE_GROUP_COMMON_NAME == "Copepoda"] <- "Isopoda"
#kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "unk. Monorchidae.sp."] <- "Monorchidae.sp.unk."
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Cystacanth.AC.MO2"] <- "Cystacanth.CA.MO2"
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Meta.MO9.Meta.MO9"] <- "META.MO9.META.MO9"

# MERGING SOME MORPHOGROUP NAMES IN AGREEMENT WITH NODE LIST FROM KELP DATA SET IN CONSULTATION WITH DANA MORTON 2022-04-11

# From D Morton: this was initally assigned the same ID as Lernaeopodidae gen.sp. A and Lernaeopodidae gen.sp. B (all were named COP.MO19 at first. Upon reviewing the photos for the other two, I realized the other two were different species, but I could not locate the photos for this specimen even though the data sheet indicated they exist. I suspect it's the same as Lernaeopodidae gen.sp. A based off the hosts (OXPI and SEAT), whereas sp. B came from CHPU. I would exclude this one since we can't prove it's unique.
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Lernaeopodidae gen.COP.MO19"] <- "Lernaeopodidae gen.sp. A"

# From D Morton: Include with name Andracantha phalacrocoracis
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Andracantha.gravida"] <- "Andracantha.phalacrocoracis"

# Name change was a synonym
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Corynosoma.obtuscens"] <- "Corynosoma.australe" 

# From D Morton: suspected to be the same as Trypanorhynch metacestode.sp. E, both entered as Lacistorhynchus dollfusi in food web. 
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Trypanorhynch metacestode.sp. F"] <- "Trypanorhynch metacestode.sp. E" 

# From D Morton: was aggregated with Trypanorhynch metacestode.sp. C, but name didn't get updated. (both are found in SESE and only 1 individual of TRPM.MO11 was found in 1 host)
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Trypanorhynch metacestode.TRPM.MO11"] <- "Trypanorhynch metacestode.sp. C" 

# From D Morton: this is Hysterothylacium.aduncum.875.5 (stage 5 is the paratenic/J3 stage). Stage 3 would be in the invert. 
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Hysterothylacium.spp."] <- "Hysterothylacium.aduncum"

# From D Morton: this was aggregated with Parafilaroides.decorus.910.5 (juvenile stage, adult is in seals)
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Parafilaroides.sp."] <- "Parafilaroides.decorus"

# From D Morton: this was assigned to Pseudoterranova.decipiens.908.5
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Pseudoterranova.sp."] <- "Pseudoterranova.decipiens"

# From D Morton: include but group with Metacercaria.sp.D (Metacercaria.sp.D.742.5)
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "META.MO9.META.MO9"] <- "Metacercaria.sp. D"

# From D Morton: I assigned these links to Galactosomum.humbargari.643.5, not 100% on ID. 
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Metacercaria.sp. E"] <- "Galactosomum.humbargari"

# From D Morton: Pellamyzon.abitionis.754.1 is a synonym of P sebastodis.
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Pellamyzon.sebastodis"] <- "Pellamyzon.abitionis"

# From D Morton: Confirm these links are from KEKE, those should be to Dollfusiella.litocephala.551.4 (larval stage of this tape worm). 
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Dollfusiella.sp. 1"] <- "Dollfusiella.litocephala"

# From D Morton: confirm this was found in COCA, if so this is Trypanorhynchunk.unk..611.4
kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "Trypanorhynch metacestode."] <- "Trypanorhynchunk.unk."

kelp.dissection$PARASITE_SPECIES_NAME[kelp.dissection$PARASITE_SPECIES_NAME == "metacercaria.UNK.MO11"] <- "Metacercaria.UNK.MO11"

# CHECKING NUMBER OF HOSTS AND PARASITES

length(unique(kelp.dissection$PARASITE_SPECIES_NAME))
length(unique(kelp.dissection$HOST_SPECIES))
length(unique(kelp.dissection$HOST_ID))

# Parasite species: 190
# Host species: 78
# Host individuals: 845

# CHANGING SOME STRATEGY INFORMATION

kelp.dissection$PARASITE_CONSUMER_STRATEGY[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Caligidae gen. spp.", "Caligus .COP.MO7", "Caligus .sp. A", "Lepeophtheirus.sp. A")] <- "Micropredator"
kelp.dissection$PARASITE_CONSUMER_STRATEGY[kelp.dissection$PARASITE_SPECIES_NAME %in% c("Porcellidae.unknown")] <- "Predator"

tissues.to.double <- c("gill", "pectoral fin under skin ", "gill filament ", "gills",
                       "ovary", "pectoral fin", "gill/branchial chamber", "gills (attatched)",
                       "gonad", "kidney", "gonads", "Gills ", "Gill", "eye muscle", "eye",
                       "gill filaments ", "Gonad", "gonad ","gills ", "gill filmament",
                       "kidney ", "gill fillament")

idx = which(kelp.dissection$HOST_TISSUE %in% tissues.to.double)
kelp.dissection$PARASITE_COUNT[idx] <- kelp.dissection$PARASITE_COUNT[idx] * 2

# REMOVING ELASMOBRANCH HOSTS

sharks <- c("Myliobatis californica", "Platyrhinoidis triseriata", "Rhinobatos.productus", "Torpedo californica",
            "Cephaloscyllium.ventriosum", "Heterodontus.francisci", "Mustelus henlei", "Notorynchus cepedianus",
            "Squatina californica", "Triakis semifasciata") # 78 hosts before

kelp.dissection <- kelp.dissection[!kelp.dissection$HOST_SPECIES %in% sharks,]

# SAVING A CSV FILE

filename = paste0("TIDY_Parasites_Kelp_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(kelp.dissection, paste(path, "Output", filename, sep = "/"))
