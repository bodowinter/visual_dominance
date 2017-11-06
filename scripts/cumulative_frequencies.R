## Bodo Winter
## June 8, 2017; Adapted for visual dominance July 23, 2017
## Look at frequencies across different corpora:

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Libraries:

library(MASS)
library(png)
library(tidyverse)
library(stringr)

## Load perceptual data:

setwd('/Volumes/Macintosh HD/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/')
adjs <- read_csv('lynott_connell_2009_adj_norms.csv')
nouns <- read_csv('lynott_connell_2013_noun_norms.csv')
verbs <- read_csv('winter_2016_verb_norms.csv')

strik <- read_csv('strik_lievers_2015.csv')

## Combine:

xdata <- bind_rows(dplyr::select(adjs, -PropertyBritish),
	nouns, dplyr::select(verbs, -RandomSet, -N))

## Add POS information:

xdata$POS <- c(rep('Adj', nrow(adjs)),
	rep('Noun', nrow(nouns)),
	rep('Verb', nrow(verbs)))

## Load in frequency datasets:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/frequency_lists/')
SUBTLEX <- read_csv('SUBTLEX_US.csv')
ELP <- read_csv('ELP.csv')
BLP <- read_csv('BLP.csv')
COCA <- read_csv('COCA.csv')

## Rename BLP Word column for consistency with function:

BLP <- rename(BLP, Word = spelling)

## Load in all the frequencies:

xdata$FreqSUBTL <- SUBTLEX[match(xdata$Word, SUBTLEX$Word), ]$FREQcount
xdata$FreqKF <- ELP[match(xdata$Word, ELP$Word), ]$Freq_KF
xdata$FreqHAL <- ELP[match(xdata$Word, ELP$Word), ]$Freq_HAL
xdata$FreqCELEX <- BLP[match(xdata$Word, BLP$Word), ]$celex.frequency
xdata$FreqBNC <- BLP[match(xdata$Word, BLP$Word), ]$bnc.frequency
xdata$FreqSUBTL_UK <- BLP[match(xdata$Word, BLP$Word), ]$subtlex.frequency
xdata$FreqCOCA <- COCA[match(xdata$Word, COCA$Word), ]$COCA

## Load all the frequencies into Strik Lievers (2015) dataset:

strik$FreqSUBTL <- SUBTLEX[match(strik$Word, SUBTLEX$Word), ]$FREQcount
strik$FreqKF <- ELP[match(strik$Word, ELP$Word), ]$Freq_KF
strik$FreqHAL <- ELP[match(strik$Word, ELP$Word), ]$Freq_HAL
strik$FreqCELEX <- BLP[match(strik$Word, BLP$Word), ]$celex.frequency
strik$FreqBNC <- BLP[match(strik$Word, BLP$Word), ]$bnc.frequency
strik$FreqSUBTL_UK <- BLP[match(strik$Word, BLP$Word), ]$subtlex.frequency
strik$FreqCOCA <- COCA[match(strik$Word, COCA$Word), ]$COCA



##------------------------------------------------------------------
## Stacked bar plot of all data:
##------------------------------------------------------------------

## Rename:

xdata <- rename(xdata,
	Modality = DominantModality)

## Make a table:

print(xtab <- xdata %>% group_by(Modality) %>%
	summarize(SUBTLEX = sum(FreqSUBTL, na.rm = T),
		COCA = sum(FreqCOCA, na.rm = T),
		KuceraFrancis = sum(FreqKF, na.rm = T),
		HAL = sum(FreqHAL, na.rm = T),
		CELEX = sum(FreqCELEX, na.rm = T),
		BNC = sum(FreqBNC, na.rm = T),
		SUBTLEX_UK = sum(FreqSUBTL_UK, na.rm = T)))

## Make different registers to rows:

xtab <- xtab %>%
	gather(Corpus, Frequency, -Modality) %>%
	spread(Modality, Frequency)

## Source empty plot function:

source('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/scripts/emptyplot.R')

## Order the table rows according to how displayed:

xtab <- xtab[c(6, 3, 4, 5, 7, 2, 1), ]

## Get names for corpora to be displayed:

corpora <- xtab$Corpus

## Make a barplot of this for

xtab <- as.matrix(xtab[, -1])
xtab <- xtab[, c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')]
xprops <- prop.table(xtab, 1)

## Check these values:

xprops

## Cumulative proportion:

xprops.cum <- t(apply(xprops, MARGIN = 1, cumsum))
xprops.cum <- cbind(rep(0, 7), xprops.cum[, ])

## Define colors:

mycols <- c('#f37058', '#efbe1b', '#f79038', '#425fac', '#30b77d')

## Load in images:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/figures/')
s1 <- readPNG('sight.png')
s2 <- readPNG('sound.png')
s3 <- readPNG('touch.png')
s4 <- readPNG('taste.png')
s5 <- readPNG('smell.png')

## Define vector of names:

sense_names <- str_c('s', 1:5)

## Define y-factor:

yfac <- 0.1

## General plot:

quartz('', 8, 6)
par(mai = c(1, 0, 0.5, 0.25), omi = rep(0, 4))
emptyplot(xlim = c(-0.3, 1), ylim = c(-0.5, nrow(xtab) + 0.5))

## Plot axis:

axis(side = 1, at = seq(0, 1, 0.25),
	lwd = 2, lwd.ticks = 2, cex.axis = 1.25, font =2)
text(side = 1, x = 0.5, y = -2.2,
	labels = 'Proportion', font = 2,
	cex = 2.2, xpd = NA)

## Plot senses:

text(xpd = NA,
	x = -0.15, y = nrow(xtab) + 0.75, font = 2, cex = 2,
	labels = 'Order:')
for (i in 1:5) {
		rasterImage(get(sense_names[i]),
				xleft = ((1:5 / 10) - 0.1)[i] - 0.034,
				xright = ((1:5 / 10) - 0.1)[i] + 0.034,
				ybottom = nrow(xtab) + 0.2, ytop = nrow(xtab) + 0.85, xpd = NA)				
	}

## Loop through rectangle and plot:

for (i in 1:nrow(xtab)) {
	text(y = i - 0.5, x = -0.02,
		corpora[nrow(xtab) + 1 - i], font = 2, cex = 1.5, adj = 1)
	
	for (j in 1:5) {
		rect(ybottom = nrow(xtab) - i + yfac, ytop = nrow(xtab) - i + 1 - yfac,
				xleft = xprops.cum[i, j], xright = xprops.cum[i, j + 1],
				col = mycols[j])
		}
	}



##------------------------------------------------------------------
## Additional checks, works for different POS and also highly exclusive adjectives?
##------------------------------------------------------------------

## Make a table of adjectives only:

print(xtab <- xdata %>% filter(POS == 'Adj') %>%
	group_by(Modality) %>%
	summarize(SUBTLEX = sum(FreqSUBTL, na.rm = T),
		COCA = sum(FreqCOCA, na.rm = T),
		KuceraFrancis = sum(FreqKF, na.rm = T),
		HAL = sum(FreqHAL, na.rm = T),
		CELEX = sum(FreqCELEX, na.rm = T),
		BNC = sum(FreqBNC, na.rm = T),
		SUBTLEX_UK = sum(FreqSUBTL_UK, na.rm = T)))

## Make different registers to rows:

xtab <- xtab %>%
	gather(Corpus, Frequency, -Modality) %>%
	spread(Modality, Frequency)

## Make a table of verbs only:

print(xtab <- xdata %>% filter(POS == 'Noun') %>%
	group_by(Modality) %>%
	summarize(SUBTLEX = sum(FreqSUBTL, na.rm = T),
		COCA = sum(FreqCOCA, na.rm = T),
		KuceraFrancis = sum(FreqKF, na.rm = T),
		HAL = sum(FreqHAL, na.rm = T),
		CELEX = sum(FreqCELEX, na.rm = T),
		BNC = sum(FreqBNC, na.rm = T),
		SUBTLEX_UK = sum(FreqSUBTL_UK, na.rm = T)))

## Make different registers to rows:

print(xtab <- xtab %>%
	gather(Corpus, Frequency, -Modality) %>%
	spread(Modality, Frequency))

## Define modality names:

mymods <- c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')

## Take the 10 most exclusive ones per modality for adjectives:

new_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(xdata, POS == 'Adj',
		Modality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	new_excl <- bind_rows(new_excl, this_df[1:10, ])
	}

## Make a table of these:

print(xtab <- new_excl %>% 
	group_by(Modality) %>%
	summarize(SUBTLEX = sum(FreqSUBTL, na.rm = T),
		COCA = sum(FreqCOCA, na.rm = T),
		KuceraFrancis = sum(FreqKF, na.rm = T),
		HAL = sum(FreqHAL, na.rm = T),
		CELEX = sum(FreqCELEX, na.rm = T),
		BNC = sum(FreqBNC, na.rm = T),
		SUBTLEX_UK = sum(FreqSUBTL_UK, na.rm = T)))

## Make different registers to rows:

print(xtab <- xtab %>%
	gather(Corpus, Frequency, -Modality) %>%
	spread(Modality, Frequency))

## Make a stacked bar plot of this:


## Order the table rows according to how displayed:

xtab <- xtab[c(6, 3, 4, 5, 7, 2, 1), ]

## Get names for corpora to be displayed:

corpora <- xtab$Corpus

## Make a barplot of this for

xtab <- as.matrix(xtab[, -1])
xtab <- xtab[, c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')]
xprops <- prop.table(xtab, 1)

## Cumulative proportion:

xprops.cum <- t(apply(xprops, MARGIN = 1, cumsum))
xprops.cum <- cbind(rep(0, 7), xprops.cum[, ])

## Define y-factor:

yfac <- 0.1

## General plot:

quartz('', 8, 6)
par(mai = c(1, 0, 0.5, 0.25), omi = rep(0, 4))
emptyplot(xlim = c(-0.3, 1), ylim = c(-0.5, nrow(xtab) + 0.5))

## Plot axis:

axis(side = 1, at = seq(0, 1, 0.25),
	lwd = 2, lwd.ticks = 2, cex.axis = 1.25, font =2)
text(side = 1, x = 0.5, y = -2.2,
	labels = 'Proportion', font = 2,
	cex = 2.2, xpd = NA)

## Plot senses:

text(xpd = NA,
	x = -0.15, y = nrow(xtab) + 0.75, font = 2, cex = 2,
	labels = 'Order:')
for (i in 1:5) {
		rasterImage(get(sense_names[i]),
				xleft = ((1:5 / 10) - 0.1)[i] - 0.034,
				xright = ((1:5 / 10) - 0.1)[i] + 0.034,
				ybottom = nrow(xtab) + 0.2, ytop = nrow(xtab) + 0.85, xpd = NA)				
	}

## Loop through rectangle and plot:

for (i in 1:nrow(xtab)) {
	text(y = i - 0.5, x = -0.02,
		corpora[nrow(xtab) + 1 - i], font = 2, cex = 1.5, adj = 1)
	
	for (j in 1:5) {
		rect(ybottom = nrow(xtab) - i + yfac, ytop = nrow(xtab) - i + 1 - yfac,
				xleft = xprops.cum[i, j], xright = xprops.cum[i, j + 1],
				col = mycols[j])
		}
	}

			### EVEN STRONGER

## Check probabilities:

xprops



##------------------------------------------------------------------
## Stacked bar plot of Strik Lievers (2015) data for triangulation:
##------------------------------------------------------------------

## Make a table:

print(xtab <- strik %>% 
	filter(Instrument == 'no') %>%
	group_by(Modality) %>%
	summarize(SUBTLEX = sum(FreqSUBTL, na.rm = T),
		COCA = sum(FreqCOCA, na.rm = T),
		KuceraFrancis = sum(FreqKF, na.rm = T),
		HAL = sum(FreqHAL, na.rm = T),
		CELEX = sum(FreqCELEX, na.rm = T),
		BNC = sum(FreqBNC, na.rm = T),
		SUBTLEX_UK = sum(FreqSUBTL_UK, na.rm = T)))

## Make different registers to rows:

xtab <- xtab %>%
	gather(Corpus, Frequency, -Modality) %>%
	spread(Modality, Frequency)

## Order the table rows according to how displayed:

xtab <- xtab[c(6, 3, 4, 5, 7, 2, 1), ]

## Get names for corpora to be displayed:

corpora <- xtab$Corpus

## Make a barplot of this for

xtab <- as.matrix(xtab[, -1])
xtab <- xtab[, c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')]
xprops <- prop.table(xtab, 1)

## Cumulative proportion:

xprops.cum <- t(apply(xprops, MARGIN = 1, cumsum))
xprops.cum <- cbind(rep(0, 7), xprops.cum[, ])

## Define y-factor:

yfac <- 0.1

## General plot:

quartz('', 8, 6)
par(mai = c(1, 0, 0.5, 0.25), omi = rep(0, 4))
emptyplot(xlim = c(-0.3, 1), ylim = c(-0.5, nrow(xtab) + 0.5))

## Plot axis:

axis(side = 1, at = seq(0, 1, 0.25),
	lwd = 2, lwd.ticks = 2, cex.axis = 1.25, font =2)
text(side = 1, x = 0.5, y = -2.2,
	labels = 'Proportion', font = 2,
	cex = 2.2, xpd = NA)

## Plot senses:

text(xpd = NA,
	x = -0.15, y = nrow(xtab) + 0.75, font = 2, cex = 2,
	labels = 'Order:')
for (i in 1:5) {
		rasterImage(get(sense_names[i]),
				xleft = ((1:5 / 10) - 0.1)[i] - 0.034,
				xright = ((1:5 / 10) - 0.1)[i] + 0.034,
				ybottom = nrow(xtab) + 0.2, ytop = nrow(xtab) + 0.9, xpd = NA)				
	}

## Loop through rectangle and plot:

for (i in 1:nrow(xtab)) {
	text(y = i - 0.5, x = -0.02,
		corpora[nrow(xtab) + 1 - i], font = 2, cex = 1.5, adj = 1)
	
	for (j in 1:5) {
		rect(ybottom = nrow(xtab) - i + yfac, ytop = nrow(xtab) - i + 1 - yfac,
				xleft = xprops.cum[i, j], xright = xprops.cum[i, j + 1],
				col = mycols[j])
		}
	}




##------------------------------------------------------------------
## Inferential statistics:
##------------------------------------------------------------------

## Z-score each variable:

xdata <- mutate(xdata,
	Vis_z = (VisualStrengthMean - mean(VisualStrengthMean)) / sd(VisualStrengthMean),
	Hap_z = (HapticStrengthMean - mean(HapticStrengthMean)) / sd(HapticStrengthMean),
	Aud_z = (AuditoryStrengthMean - mean(AuditoryStrengthMean)) / sd(AuditoryStrengthMean),
	Gus_z = (GustatoryStrengthMean - mean(GustatoryStrengthMean)) / sd(GustatoryStrengthMean),
	Olf_z = (OlfactoryStrengthMean - mean(OlfactoryStrengthMean)) / sd(OlfactoryStrengthMean))


##------------------------------------------------------------------
## Create exclusive datasets:
##------------------------------------------------------------------

## Add POS info:

xdata$POS <- c(rep('Adj', nrow(adjs)),
	rep('Noun', nrow(nouns)),
	rep('Verb', nrow(verbs)))

## Define modality names:

mymods <- c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')

## Take the 10 most exclusive ones per modality for adjectives:

adj_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(xdata, POS == 'Adj',
		Modality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	adj_excl <- bind_rows(adj_excl, this_df[1:10, ])
	}

## Take the 10 most exclusive ones per modality for verbs:

verb_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(xdata, POS == 'Verb',
		Modality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	verb_excl <- bind_rows(verb_excl, this_df[1:10, ])
	}

## For nouns the same analysis is not possible because there's not 10 for each modality
## Take the 10 most exclusive ones per modality for nouns, if there are enough:

noun_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(xdata, POS == 'Noun',
		Modality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	if (nrow(this_df) > 10) {
		noun_excl <- bind_rows(noun_excl, this_df[1:10, ])
		} else {
			noun_excl <- bind_rows(noun_excl, this_df)
			}
	}

## Combine exclusives:

xdata_excl <- bind_rows(adj_excl, noun_excl, verb_excl)

## Z-score each variable within the exclusive dataset:

xdata_excl <- mutate(xdata_excl,
	Vis_z = (VisualStrengthMean - mean(VisualStrengthMean)) / sd(VisualStrengthMean),
	Hap_z = (HapticStrengthMean - mean(HapticStrengthMean)) / sd(HapticStrengthMean),
	Aud_z = (AuditoryStrengthMean - mean(AuditoryStrengthMean)) / sd(AuditoryStrengthMean),
	Gus_z = (GustatoryStrengthMean - mean(GustatoryStrengthMean)) / sd(GustatoryStrengthMean),
	Olf_z = (OlfactoryStrengthMean - mean(OlfactoryStrengthMean)) / sd(OlfactoryStrengthMean))

## Make separate negative binomial models per register:

summary(xmdl.SUBTLEX <- glm.nb(FreqSUBTL ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata_excl))
summary(xmdl.COCA <- glm.nb(FreqCOCA ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata_excl))
summary(xmdl.HAL <- glm.nb(FreqHAL ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata_excl))
summary(xmdl.KF <- glm.nb(FreqKF ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata_excl))
summary(xmdl.SUBTL_UK <- glm.nb(FreqSUBTL_UK ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata_excl))
summary(xmdl.CELEX <- glm.nb(FreqCELEX ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata_excl))
summary(xmdl.BNC <- glm.nb(FreqBNC ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata_excl))



