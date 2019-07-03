# opioid_costs

Replication Materials for Brill and Ganz (2018)

Index:
The zip file contains three folders:
1.	R
2.	data
3.	out
4.	opioid_map

### R
The “R” folder contains all of the R code required to replicate all figures in the paper. R-3.3.3 was used to produce the paper; however, any recent version of R should reproduce the output faithfully.

### data
The “data” folder stores the raw data used to generate the model. Information on data sources is available in the Technical Appendix of the paper.

### out
The “out” folder is a repository for the output for the tables in the text. Once you run the Rmd file, new spreadsheets will be created that output the state and county results.

### opioid_map
The “opioid_map” folder contains code for creating an interactive shiny map displaying per-capita opioid costs.

## Replication Instructions:
The following packages must be installed on your machine prior to running the code: knitr, mosaic, dplyr, readr, readxl, tidyr, stringr, choroplethr, choroplethrMaps, sas7bdat, tidycensus, lme4, glmmTMB, RColorBrewer.

Open and knit “opioid_distributional_analysis.Rmd”. If you knit to word, then the maps will be easily extractable from the resulting output, which will be saved in the “R” folder. In addition, the data used to create the tables in the paper are outputted to the “out” folder.
