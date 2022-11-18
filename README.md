# README

## Scripts to produce the analyses and figures in the manuscript “Parasite Diversity in an Isolated, Disturbed Marine Ecosystem” submitted (November 2022)

### Lauren Dykman, 2022
### MIT-WHOI Joint Program in Oceanography/Applied Ocean Sciences & Engineering

This GitHub repository contains scripts for tidying and merging data sets and running analyses in the manuscript “Parasite Diversity in an Isolated, Disturbed Marine Ecosystem” submitted (November 2022). These analyses compare parasite diversity and prevalence at disturbed deep-sea hydrothermal vents (Dykman et al. 2022) to marine ecosystems that are island-like but undisturbed (Palmyra Atoll lagoon sandflats) (Vidal-Martínez et al. 2012, Vidal-Martínez et al. 2017, McLaughlin 2018, González-Solís et al. 2019, Soler-Jiménez et al. 2019) and well-connected but moderately disturbed (Santa Barbara kelp forests) (Morton et al. 2021).

These scripts are set up to work with a file structure containing four folders.

**Data**: contains input data.\
**Figures**: a folder where figures produced by the analysis will be output.\
**Output**: a folder where intermediate data tables produced by the analysis will be output.\
**Scripts**: a folder containing the scripts.

### Software Version Numbers

R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"\
Copyright (C) 2022 The R Foundation for Statistical Computing\
Platform: x86_64-apple-darwin17.0 (64-bit)

# Step 1: Run data tidying scripts for each ecosystem

Data tidying scripts 1) rename columns for consistency, 2) remove host groups and species from analyses that do not meet the criteria of the study, 3) remove some parasite morphogroups that are not considered for example dead parasites or unknowns, 4) rename some host and parasite groups/species for consistency between datasets or to correct spelling errors, 5) format data to be consistent between datasets. Each script should be run to produce a  tidy output file to be used in analyses. 

**IMPORTANT:** All scripts should have the same working directory and should be in one folder entitled “Scripts”.

## TIDYING_VENT.R

**Input**: Data > PARASITES_VENT.csv

This data table is published open access online in BCO-DMO as:

Dykman, L., Mullineaux, L., Tepolt, C., Kuris, A. (2022) Dissection data for metazoan parasites and other symbionts from vent-endemic host species collected from the 9°50’N deep-sea hydrothermal vent field. Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2022-08-24 [if applicable, indicate subset used]. [http://lod.bco-dmo.org/id/dataset/879118](http://lod.bco-dmo.org/id/dataset/879118) [access date].

**Output**: Output > TIDY_Parasites_Vent_YYYY-MM-DD.csv

## TIDYING_KELP.R

**Input**: Data > 8_Dissection_data.csv

This analysis uses the dissection data table (8_Dissection_data.csv) available open access online in:

Morton DN, Antonino CY, Broughton FJ, Dykman LN, Kuris AM, Lafferty KD. (2021) A food web including parasites for kelp forests of the Santa Barbara Channel, California. Sci Data. Apr 8;8(1):99. doi: 10.1038/s41597-021-00880-4.

**Output**: Output > TIDY_Parasites_Kelp_YYYY-MM-DD.csv 

## TIDYING_PALMYRA_FISH.R

**Input**: Data > 32_Fish_Parasite.csv

Palmyra dissection data sheets were shared by Dr. John McLaughlin, UC Santa Barbara, on 2019-11-21. These are the dissection data tables in an in-prep data paper (unpublished as of 2022-11-16). Subsets of the species list from the  dissection data files have been published in Vidal-Martínez et al. (2012), Vidal-Martínez et al. (2017), McLaughlin (2018), González-Solís et al. (2019), Soler-Jiménez et al. (2019).

**Output**: Output > TIDY_Parasites_Palmyra_Fish_YYYY-MM-DD.csv

## TIDYING_PALMYRA_INVERT.R

**Input**: Data > Palmyra_Invert_Parasite_FN_Apr_2021.csv

Palmyra dissection data sheets were shared by Dr. John McLaughlin, UC Santa Barbara, on 2019-11-21. These are the raw dissection data tables in an in-prep data paper (unpublished as of 2022-11-16). Subsets of the species list from the raw dissection data files have been published in Vidal-Martínez et al. (2012), Vidal-Martínez et al. (2017), McLaughlin (2018), González-Solís et al. (2019), Soler-Jiménez et al. (2019):

**Output**: Output > TIDY_Parasites_Palmyra_Invert_All_YYYY-MM-DD.csv

## TIDYING_PALMYRA_INVERT_PARTHENITAE.R

**Input**: Data > Palmyra_Parth_FN_Jan_21_2021.csv

Palmyra dissection data sheets were shared by Dr. John McLaughlin, UC Santa Barbara, on 2019-11-21. These are the raw dissection data tables in an in-prep data paper (unpublished as of 2022-11-16). Subsets of the species list from the raw dissection data files have been published in Vidal-Martínez et al. (2012), Vidal-Martínez et al. (2017), McLaughlin (2018), González-Solís et al. (2019), Soler-Jiménez et al. (2019):

**Output**: Output > TIDY_Parasites_Palmyra_Invert_Parth_YYYY-MM-DD.csv

# Step 2: Run Data Set Merging Script

## MERGING.R

**Input**:\
•	Output > TIDY_Parasites_Vent_YYYY-MM-DD.csv\
•	Output > TIDY_Parasites_Kelp_YYYY-MM-DD.csv\
•	Output > TIDY_Parasites_Palmyra_Fish_YYYY-MM-DD.csv\
•	Output > TIDY_Parasites_Palmyra_Invert_All_YYYY-MM-DD.csv\
•	Output > TIDY_Parasites_Palmyra_Invert_Parth_YYYY-MM-DD.csv

Binds data sets together, retaining only column names necessary for analysis. Removes host species with sample size < 10 individuals. Outputs a merged dataset with species from the three ecosystems that is used in subsequent analyses and statistics. This script also outputs three data tables with metadata on parasites and host species counts in the three ecosystems.

**Outputs**:\
•	Output > TIDY_DATA_ALL_YYYY-MM-DD.csv\
•	Output > MERGED_HostCounts_YYYY-MM-DD.csv\
•	Output > MERGED_Metadata_YYYY-MM-DD.csv\
•	Output > MERGED_SppMinMax_YYYY-MM-DD.csv

# Step 3: Run Analysis and Statistics Scripts 

## ANALYSIS1_RICHNESS_OVERALL.R
	
**Input**: Output > TIDY_DATA_ALL_YYYY-MM-DD.csv
	
This script runs analyses of parasite richness per host species (Fig. 3a) and parasite richness as a function of host species richness (Figs. 3b&c). Outputs include a figure of mean estimated (Chao2) parasite richness per host species in each ecosystem and host group (Fig. 3a), and species accumulation curves of parasite richness as a function of host richness for fish hosts (Fig. 3b) and invertebrate hosts (Fig. 3c). This script also outputs an intermediate table that will be used in later statistical analyses.

**Outputs:**\
•	Figures > Figure_Richness_Barplot_Chao2_HostSpecies_YYYY-MM-DD.pdf\
•	Figures > Figure_Richness_Curves_Fish_YYYY-MM-DD.pdf\
•	Figures > Figure_Richness_Curves_Invert_YYYY-MM-DD.pdf\
•	Output > STATISTICS_Data_Table_Chao2_YYYY-MM-DD.csv

## ANALYSIS1_STATISTICS.R

**Input**: Output > STATISTICS_Data_Table_Chao2_YYYY-MM-DD.csv

This script runs a rank ANCOVA statistical test with an ordered alternative using data produced in the previous script ANALYSIS1_RICHNESS_OVERALL.R (Fig. 3a in manuscript). Outputs a table of randomization results. The p-value and R-squared value are returned in the script but not saved as an output file.

**Outputs**: Output > STATISTICS_ORDERED_Randomizations_YYYY-MM-DD.csv

## ANALYSIS2_LIFECYCLE.R
	
**Input**: Output > TIDY_DATA_ALL_YYYY-MM-DD.csv

Analyzes the proportion of parasite species with direct life cycles (one-host) versus indirect life cycles (multi-host) in fish hosts (Fig. 4a) and invertebrate hosts (Fig. 4b) in the three ecosystems. Runs chi-squared analyses to test for significant differences in the proportion of parasite species in the two life cycles between ecosystems. Saves the two figures in the manuscript as output figures. Results of chi-squared statistical tests are returned in the script but not saved as an output file. Life cycle assignments and rationale with citations are available as a data table in BCO-DMO:

Dykman, L. (2022a) Parasite taxonomic and life cycle information from literature. Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2022-08-25 [if applicable, indicate subset used]. [http://lod.bco-dmo.org/id/dataset/879253](http://lod.bco-dmo.org/id/dataset/879253) [access date]

**Output:**\
•	Figures > Figure_EPR_Analysis_Composition_Cycle_Fish_YYYY-MM-DD.pdf\
•	Figures > Figure_EPR_Analysis_Composition_Cycle_Invert_YYYY-MM-DD.pdf

## ANALYSIS3_RICHNESS_GROUP.R

**Input**: Output > TIDY_DATA_ALL_YYYY-MM-DD.csv

Generates species accumulation curves for individual parasite taxa in fish hosts in the three ecosystems. Produces six output figures that make up the panels in Fig. 5.

**Output:**\
•	Figures > Figure_Richness_Curves_Acanthocephala_YYYY-MM-DD.pdf\
•	Figures > Figure_Richness_Curves_Cestoda_YYYY-MM-DD.pdf\
•	Figures > Figure_Richness_Curves_Copepoda_YYYY-MM-DD.pdf\
•	Figures > Figure_Richness_Curves_Monogenea_YYYY-MM-DD.pdf\
•	Figures > Figure_Richness_Curves_Nematoda_YYYY-MM-DD.pdf\
•	Figures > Figure_Richness_Curves_Trematoda_YYYY-MM-DD.pdf

## ANALYSIS4_LIFESTAGE.R

**Input**: Output > TIDY_DATA_ALL_YYYY-MM-DD.csv

Analyzes the proportion of parasite morphogroups in different life stages in fish hosts (Fig. 6a) and invertebrate hosts (Fig. 6b) in the three ecosystems. Runs chi-squared analyses to test for significant differences in the proportion of morphogroups in different life stages between ecosystems. Saves the two figures in the manuscript as output figures. Results of chi-squared statistical tests are returned in the script but not saved as an output file.

**Output**:\
•	Figures > Figure_EPR_Analysis_Composition_Stage_Fish_YYYY-MM-DD.pdf\
•	Figures > Figure_EPR_Analysis_Composition_Stage_Invert_YYYY-MM-DD.pdf

## APPENDIX1_EPR_LITERATURE.R
	
**Input**: Output > PARASITES_VENT_LITERATURE.csv

Produces a stacked composition barplot showing the number of parasite species in different taxa reported in published literature per year. The table of vent parasite species reported in published literature is published in BCO-DMO as:

Dykman, L. (2022b) Records and metadata for deep-sea hydrothermal vent parasite, egg predator, and micropredator species reported in published literature. Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2022-08-25 [if applicable, indicate subset used]. [http://lod.bco-dmo.org/id/dataset/879266](http://lod.bco-dmo.org/id/dataset/879266)[access date].

**Output**: Figures > Figure_Literature_Spp_Discovery_YYYY-MM-DD.pdf

## APPENDIX2_LENGTHS.R

**Input**: Output > TIDY_DATA_ALL_YYYY-MM-DD.csv

Plots mean parasite species richness and parasite counts per host species as a function of host species length. These output figures form the two panels in Figure S4.4 published in the supplementary material of the manuscript.

**Outputs:**\
•	Figures > Figure_Supplement_Length_Counts_All_YYYY-MM-DD.pdf\
•	Figures > Figure_Supplement_Length_Richness_All_YYYY-MM-DD.pdf

# REFERENCES

Dykman, L., Mullineaux, L., Tepolt, C., Kuris, A. (2022) Dissection data for metazoan parasites and other symbionts from vent-endemic host species collected from the 9°50’N deep-sea hydrothermal vent field. Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2022-08-24 [if applicable, indicate subset used]. [http://lod.bco-dmo.org/id/dataset/879118](http://lod.bco-dmo.org/id/dataset/879118) [access date]

Dykman, L. (2022a) Parasite taxonomic and life cycle information from literature. Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2022-08-25 [if applicable, indicate subset used]. [http://lod.bco-dmo.org/id/dataset/879253](http://lod.bco-dmo.org/id/dataset/879253) [access date]

Dykman, L. (2022b) Records and metadata for deep-sea hydrothermal vent parasite, egg predator, and micropredator species reported in published literature. Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 1) Version Date 2022-08-25 [if applicable, indicate subset used]. [http://lod.bco-dmo.org/id/dataset/879266](http://lod.bco-dmo.org/id/dataset/879266) [access date]

González-Solís, D., Soler-Jiménez, L.C., Aguirre-Macedo, M.L., McLaughlin, J.P., Shaw, J.C., James, A. K., ... & Vidal-Martínez, V.M. (2019). Parasitic nematodes of marine fishes from Palmyra Atoll, East Indo-Pacific, including a new species of Spinitectus (Nematoda, Cystidicolidae). ZooKeys, 892, 1.

McLaughlin, J.P. (2018). The food web for the sand flats at Palmyra Atoll. University of California, Santa Barbara.
Morton D.N., Antonino C.Y., Broughton F.J., Dykman L.N., Kuris A.M., Lafferty K.D. (2021) A food web including parasites for kelp forests of the Santa Barbara Channel, California. Sci Data. Apr 8;8(1):99. doi: 10.1038/s41597-021-00880-4. 

Soler-Jiménez, L.C., Morales-Serna, F.N., Aguirre-Macedo, M.L., McLaughlin, J.P., Jaramillo, A.G., Shaw, J.C., ... & Vidal-Martínez, V.M. (2019). Parasitic copepods (Crustacea, Hexanauplia) on fishes from the lagoon flats of Palmyra Atoll, Central Pacific. ZooKeys, 833, 85.

Vidal-Martínez, V.M., Aguirre-Macedo, M.L., McLaughlin, J.P., Hechinger, R.F., Jaramillo, A.G., Shaw, J. C., ... & Lafferty, K. D. (2012). Digenean metacercariae of fishes from the lagoon flats of Palmyra Atoll, Eastern Indo-Pacific. Journal of Helminthology, 86(4), 493-509.

Vidal-Martínez, V.M., Soler-Jiménez, L.C., Aguirre-Macedo, M.L., Mclaughlin, J., Jaramillo, A.G., Shaw, J.C., ... & Lafferty, K.D. (2017). Monogenea of fishes from the lagoon flats of Palmyra Atoll in the Central Pacific. ZooKeys, (713), 1.





