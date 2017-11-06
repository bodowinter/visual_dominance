## Bodo Winter
## July 27, 2017
## Preprocessing COHA:

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(tidyverse)
library(stringr)

## Load data:

setwd('/Volumes/Macintosh HD/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/COHA/')

## Get all file names:

all_files <- list.files()

## Loop through and extract:

xdata <- c()
for (i in seq_along(all_files)) {
	this_df <- read_csv(all_files[i])
	these_counts <- this_df %>% group_by(Word, Register) %>% count()
	these_counts$Year <- as.numeric(str_extract(all_files[i], '[0-9]+'))
	xdata <- bind_rows(xdata,
		these_counts)
	}

## Write to file:

setwd('/Users/winterb/Research/senses_sensory_modalities/viberg/brandnew_analysis/data/')
write_csv(xdata, 'COHA_summary.csv')



