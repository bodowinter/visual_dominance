## Bodo Winter
## July 28, 2017
## Look at average frequencies across different corpora:

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Libraries:

library(stringr)
library(MASS)
library(pscl)
library(png)
library(car)
library(tidyverse)

## Get my emptyplot() function:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/scripts/')
source('emptyplot.R')

## Load perceptual data:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/')
adjs <- read_csv('lynott_connell_2009_adj_norms.csv')
nouns <- read_csv('lynott_connell_2013_noun_norms.csv')
verbs <- read_csv('winter_2016_verb_norms.csv')

strik <- read_csv('strik_lievers_2015.csv')

## Combine:

xdata <- bind_rows(select(adjs, -PropertyBritish),
	nouns, select(verbs, -RandomSet, -N))

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
## Create exclusive datasets:
##------------------------------------------------------------------

## Define modality names:

mymods <- c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')

## Take the 10 most exclusive ones per modality for adjectives:

adj_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(xdata, POS == 'Adj',
		DominantModality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	adj_excl <- bind_rows(adj_excl, this_df[1:10, ])
	}

## Take the 10 most exclusive ones per modality for verbs:

verb_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(xdata, POS == 'Verb',
		DominantModality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	verb_excl <- bind_rows(verb_excl, this_df[1:10, ])
	}

## For nouns the same analysis is not possible because there's not 10 for each modality
## Take the 10 most exclusive ones per modality for nouns, if there are enough:

noun_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(xdata, POS == 'Noun',
		DominantModality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	if (nrow(this_df) > 10) {
		noun_excl <- bind_rows(noun_excl, this_df[1:10, ])
		} else {
			noun_excl <- bind_rows(noun_excl, this_df)
			}
	}

## Check relationship with exclusivity:

summary(glm.nb(FreqSUBTL ~ ModalityExclusivity, xdata))

## Combine exclusives:

xdata_excl <- bind_rows(adj_excl, noun_excl, verb_excl)



##------------------------------------------------------------------
## Descriptive averages:
##------------------------------------------------------------------

## Look at averages per category, adjectives:

adj_excl %>% group_by(DominantModality) %>%
	summarize(FreqSUBTL = mean(FreqSUBTL, na.rm = T),
		FreqKF = mean(FreqSUBTL, na.rm = T),
		FreqHAL = mean(FreqHAL, na.rm = T),
		FreqCELEX = mean(mean(FreqCELEX, na.rm = T)),
		FreqBNC = mean(FreqBNC, na.rm = T),
		FreqSUBTL_UK = mean(FreqSUBTL_UK, na.rm = T),
		FreqCOCA = mean(FreqCOCA, na.rm = T)) %>%
	arrange(desc(FreqSUBTL)) %>%
	mutate(FreqSUBTL = round(FreqSUBTL))

## Look at averages per category, verbs:

verb_excl %>% group_by(DominantModality) %>%
	summarize(FreqSUBTL = mean(FreqSUBTL, na.rm = T),
		FreqKF = mean(FreqSUBTL, na.rm = T),
		FreqHAL = mean(FreqHAL, na.rm = T),
		FreqCELEX = mean(mean(FreqCELEX, na.rm = T)),
		FreqBNC = mean(FreqBNC, na.rm = T),
		FreqSUBTL_UK = mean(FreqSUBTL_UK, na.rm = T),
		FreqCOCA = mean(FreqCOCA, na.rm = T)) %>%
	arrange(desc(FreqSUBTL)) %>%
	mutate(FreqSUBTL = round(FreqSUBTL))

## Look at averages per category, nouns:

noun_excl %>% group_by(DominantModality) %>%
	summarize(FreqSUBTL = mean(FreqSUBTL, na.rm = T),
		FreqKF = mean(FreqSUBTL, na.rm = T),
		FreqHAL = mean(FreqHAL, na.rm = T),
		FreqCELEX = mean(mean(FreqCELEX, na.rm = T)),
		FreqBNC = mean(FreqBNC, na.rm = T),
		FreqSUBTL_UK = mean(FreqSUBTL_UK, na.rm = T),
		FreqCOCA = mean(FreqCOCA, na.rm = T)) %>%
	arrange(desc(FreqSUBTL)) %>%
	mutate(FreqSUBTL = round(FreqSUBTL))

## Look at averages per category, across all of them:

xdata_excl %>% group_by(DominantModality) %>%
	summarize(FreqSUBTL = mean(FreqSUBTL, na.rm = T),
		FreqKF = mean(FreqSUBTL, na.rm = T),
		FreqHAL = mean(FreqHAL, na.rm = T),
		FreqCELEX = mean(mean(FreqCELEX, na.rm = T)),
		FreqBNC = mean(FreqBNC, na.rm = T),
		FreqSUBTL_UK = mean(FreqSUBTL_UK, na.rm = T),
		FreqCOCA = mean(FreqCOCA, na.rm = T)) %>%
	arrange(desc(FreqSUBTL)) %>%
	mutate(FreqSUBTL = round(FreqSUBTL))



##------------------------------------------------------------------
## Model frequency, separate models for each modality (collinearity!), full data:
##------------------------------------------------------------------

## Z-score:

xdata_excl <- mutate(xdata_excl,
	Vis_z = VisualStrengthMean - mean(VisualStrengthMean),
	Vis_z = Vis_z / sd(Vis_z),
	Hap_z = HapticStrengthMean - mean(HapticStrengthMean),
	Hap_z = Hap_z / sd(Hap_z),	
	Aud_z = AuditoryStrengthMean - mean(AuditoryStrengthMean),
	Aud_z = Aud_z / sd(Aud_z),
	Olf_z = OlfactoryStrengthMean - mean(OlfactoryStrengthMean),
	Olf_z = Olf_z / sd(Olf_z),
	Gus_z = GustatoryStrengthMean - mean(GustatoryStrengthMean),
	Gus_z = Gus_z / sd(Gus_z))

## Conjoined analysis:

SUBTLEX.combined <- glm.nb(FreqSUBTL ~ Vis_z +
	Aud_z + Gus_z +
	Hap_z + Olf_z,
	data = xdata_excl)
summary(SUBTLEX.combined)

## Check overdispersion:

odTest(SUBTLEX.combined)

## Variance inflation factors:

vif(SUBTLEX.combined)

## ALL:
## Model for 10 most exclusive adjectives:

SUBTLEX.vis <- glm.nb(FreqSUBTL ~ Vis_z,
	data = xdata_excl)
summary(SUBTLEX.vis)

SUBTLEX.aud <- glm.nb(FreqSUBTL ~ Aud_z,
	data = xdata_excl)
summary(SUBTLEX.aud)

SUBTLEX.hap <- glm.nb(FreqSUBTL ~ Hap_z,
	data = xdata_excl)
summary(SUBTLEX.hap)

SUBTLEX.gus <- glm.nb(FreqSUBTL ~ Gus_z,
	data = xdata_excl)
summary(SUBTLEX.gus)

SUBTLEX.olf <- glm.nb(FreqSUBTL ~ Olf_z,
	data = xdata_excl)
summary(SUBTLEX.olf)



##------------------------------------------------------------------
## Same plot, combined model (reported in paper):
##------------------------------------------------------------------

## Position factor for modalities:

xfac <- 0

## New modality identifiers:

mymods_z <- c('Vis_z', 'Aud_z', 'Hap_z', 'Gus_z', 'Olf_z')

## Make plots:

quartz('', 9, 6)
par(mai = rep(0.25, 4),
	omi = c(0.5, 1.5, 0.5, 0.25))
## Plot 1, adjectives:
emptyplot(xlim = c(0, 5.5), ylim = c(-1, 1))
mtext(side = 2, text = 'Frequency Coefficient', line = 4.7,
	font = 2, cex = 1.9)
axis(side = 2, at = seq(-1, 1, 0.25),
	las = 2, lwd = 2, font = 2, cex.axis = 1.25)
abline(h = 0, lty = 2, lwd = 2)
xsum <- summary(SUBTLEX.combined)$coefficients
for (i in 1:5) {

	xcoef <- xsum[mymods_z[i], 1]
	xse <- xsum[mymods_z[i], 2]

	rect(xleft = i - 0.2, i + 0.2, ybottom = 0, ytop = xcoef,
		col = mycols[i])

	segments(x0 = i, y0 = xcoef - 1.96 * xse, y1 = xcoef + 1.96 * xse,
		lwd = 1, xpd = NA)
	
	# arrows(x0 = i, y0 = xcoef - 1.96 * xse, y1 = xcoef + 1.96 * xse,
		# lwd = 2, angle = 90, code = 3, length = 0.1, col = mycols[i])
	# # points(x = i, y = xcoef,
		# # pch = 15, cex = 2, col = mycols[i])
	rasterImage(image = get(sense_names[i]),
		xleft = i - 0.19 - xfac, xright = i + 0.19 - xfac,
		ybottom = -0.9 - 0.1, ytop = -0.9 + 0.1, xpd = NA)
	}



##------------------------------------------------------------------
## Test the same thing for Strik Lievers (2015):
##------------------------------------------------------------------

## Look at averages per category, nouns:

strik %>% group_by(Modality) %>%
	filter(POS == 'Noun') %>%
	summarize(FreqSUBTL = mean(FreqSUBTL, na.rm = T),
		FreqKF = mean(FreqSUBTL, na.rm = T),
		FreqHAL = mean(FreqHAL, na.rm = T),
		FreqCELEX = mean(mean(FreqCELEX, na.rm = T)),
		FreqBNC = mean(FreqBNC, na.rm = T),
		FreqSUBTL_UK = mean(FreqSUBTL_UK, na.rm = T),
		FreqCOCA = mean(FreqCOCA, na.rm = T)) %>%
	arrange(desc(FreqSUBTL))

## Look at averages per category, adjectives:

strik %>% group_by(Modality) %>%
	filter(POS == 'Adj') %>%
	summarize(FreqSUBTL = mean(FreqSUBTL, na.rm = T),
		FreqKF = mean(FreqSUBTL, na.rm = T),
		FreqHAL = mean(FreqHAL, na.rm = T),
		FreqCELEX = mean(mean(FreqCELEX, na.rm = T)),
		FreqBNC = mean(FreqBNC, na.rm = T),
		FreqSUBTL_UK = mean(FreqSUBTL_UK, na.rm = T)) %>%
	arrange(desc(FreqSUBTL))		# no COCA because not extracted

## Look at averages per category, verbs:

strik %>% group_by(Modality) %>%
	filter(POS == 'Verb') %>%
	summarize(FreqSUBTL = mean(FreqSUBTL, na.rm = T),
		FreqKF = mean(FreqSUBTL, na.rm = T),
		FreqHAL = mean(FreqHAL, na.rm = T),
		FreqCELEX = mean(mean(FreqCELEX, na.rm = T)),
		FreqBNC = mean(FreqBNC, na.rm = T),
		FreqSUBTL_UK = mean(FreqSUBTL_UK, na.rm = T)) %>%
	arrange(desc(FreqSUBTL))		# no COCA because not extracted

