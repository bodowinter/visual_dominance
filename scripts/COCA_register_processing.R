## Bodo Winter
## July 23, 2017
## Processing register data in COCA

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(tidyverse)
library(stringr)

## Load data:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/COCA/')
academic <- read_csv('academic.csv')
magazine <- read_csv('magazine.csv')
news <- read_csv('news.csv')
spoken <- read_csv('spoken.csv')
fiction <- read_csv('fiction.csv')

## Names of files:

regs <- c('spoken', 'academic', 'news', 'magazine', 'fiction')

## Rename files:

for (i in seq_along(regs)) {
	this_df <- get(regs[i])
	this_df <- rename(this_df,
		Word = X1, Lemma = X2, POS = X3)
	assign(regs[i], this_df)
	}

## Load in adjs/nouns/verbs:

adjs <- read_csv('lynott_connell_2009_adj_norms.csv')
nouns <- read_csv('lynott_connell_2013_noun_norms.csv')
verbs <- read_csv('winter_2016_verb_norms.csv')

##  Merge:

adjs <- select(adjs, Word:ModalityExclusivity)
verbs <- select(verbs, -RandomSet, -N)
xdata <- bind_rows(adjs, verbs, nouns)


##------------------------------------------------------------------
## Process POS-specific frequencies:
##------------------------------------------------------------------

## Add POS info:

xdata$POS <- c(rep('Adj', nrow(adjs)),
	rep('Verb', nrow(verbs)),
	rep('Noun', nrow(nouns)))

## Add empty columns with the respective frequencies:

myM <- as_tibble(matrix(rep(NA, 5 * nrow(xdata)), ncol = 5))
colnames(myM) <- regs
xdata <- bind_cols(xdata, myM)

## Loop through words and extract the respective frequencies:

for (i in 1:nrow(xdata)) {
	this_word <- xdata[i, ]$Word
	for (j in seq_along(regs)) {
		this_reg <- get(regs[j])
		
		this_subs <- filter(this_reg,
			Word == this_word)
		
		if (xdata[i, ]$POS == 'Adj') {
			this_subs <- filter(this_subs,
				str_detect(POS, 'jj'))
			}

		if (xdata[i, ]$POS == 'Verb') {
			this_subs <- filter(this_subs,
				str_detect(POS, 'vv'))
			}

		if (xdata[i, ]$POS == 'Noun') {
			this_subs <- filter(this_subs,
				str_detect(POS, 'nn'))
			}
		
		xdata[i, regs[j]] <- nrow(this_subs)
		}
	if (i %% 50 == 0) cat(str_c('Processing word ... ', i, '\n'))
	}

## Write to file:

write_csv(xdata, 'COCA_registers.csv')


##------------------------------------------------------------------
## Process POS-specific non-specific frequencies:
##------------------------------------------------------------------

## Combine into data frame:

xdata <- bind_rows(adjs, verbs, nouns)

## Add empty column:

xdata$COCA <- rep(NA, nrow(xdata))

## Put all register files into one:

COCA_all <- bind_rows(spoken, academic,
	news, magazine, fiction)

## Loop through words and extract the respective frequencies:

for (i in 1:nrow(xdata)) {
	this_word <- xdata[i, ]$Word
	xdata[i, ]$COCA <- nrow(filter(COCA_all, Word == this_word))
	if (i %% 50 == 0) cat(str_c('Processing word ... ', i, '\n'))
	}

## Put into file:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/frequency_lists/')
write_csv(select(xdata, Word, COCA), 'COCA.csv')

