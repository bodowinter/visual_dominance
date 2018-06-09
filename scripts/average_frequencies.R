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
## Addressing reviewer 2's concern:
##------------------------------------------------------------------

## Reviewer 2 asked about whether the way the word lists were constructed
## could affect the main frequency result. To assess this, we can look at
## the verbs, for which we have both a random and a non-random subset
## (see description in Winter, 2016):

verb_excl$RandomSet <- verbs[match(verb_excl$Word, verbs$Word), ]$RandomSet
summary(glm.nb(FreqSUBTL ~ VisualStrengthMean,
	data = filter(verb_excl, RandomSet == 'yes')))	# check random subset
		# only 4 words
summary(glm.nb(FreqSUBTL ~ VisualStrengthMean,
	data = filter(verb_excl, RandomSet == 'no')))	# check non-random subset
summary(glm.nb(FreqSUBTL ~ VisualStrengthMean * RandomSet,
	data = verb_excl))	# check interaction

## Also, check for verbs and nouns together (both randomly sampled):

verbs_random <- filter(verbs, RandomSet == 'yes') %>%
	dplyr::select(-RandomSet, -N)
NV <- bind_rows(nouns, verbs_random)
NV$FreqSUBTL <- SUBTLEX[match(NV$Word, SUBTLEX$Word), ]$FREQcount
NV <- mutate(NV, Vis_c = scale(VisualStrengthMean, scale = FALSE),
	Excl_c = scale(ModalityExclusivity, scale = FALSE))
summary(glm.nb(FreqSUBTL ~ Vis_c * Excl_c,
	data = NV))	# more visually exclusive words are more frequent
# however, no main effect



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
## Reviewer 1 requested adding POS as a control variable
## (there is not enough data to fit the interaction between POS and all strength variables):

SUBTLEX.combined <- glm.nb(FreqSUBTL ~ Vis_z +
	Aud_z + Gus_z +
	Hap_z + Olf_z + POS,
	data = xdata_excl)
summary(SUBTLEX.combined)

## Check overdispersion:

odTest(SUBTLEX.combined)

## Variance inflation factors:

vif(SUBTLEX.combined)

## Check whether there's an interaction with POS for vision:
## Requested by reviewer 1:

SUBTLEX_POS <- glm.nb(FreqSUBTL ~ Vis_z * POS,
	data = xdata_excl)
summary(SUBTLEX_POS)
SUBTLEX_POS_no_int <- glm.nb(FreqSUBTL ~ Vis_z + POS,
	data = xdata_excl)
SUBTLEX_POS_no_vis <- glm.nb(FreqSUBTL ~ POS,
	data = xdata_excl)
anova(SUBTLEX_POS_no_vis,
	SUBTLEX_POS_no_int, test = 'Chisq')	# main effect
anova(SUBTLEX_POS_no_int,
	SUBTLEX_POS, test = 'Chisq')	# interaction

## ALL separate (sanity check):
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

## Addressing reviewer 1's concern, biasing words:

dfbeta_vis <- numeric(nrow(xdata_excl))
leave_one_out_p_vis <- numeric(nrow(xdata_excl))

dfbeta_hap <- numeric(nrow(xdata_excl))
leave_one_out_p_hap <- numeric(nrow(xdata_excl))

for (i in 1:nrow(xdata_excl)) {
	this_mdl <- glm.nb(FreqSUBTL ~ Vis_z +
		Aud_z + Gus_z +
		Hap_z + Olf_z,
		data = xdata_excl[-i, ])
	dfbeta_vis[i] <- summary(this_mdl)$coefficients['Vis_z', 'Estimate']
	leave_one_out_p_vis[i] <- summary(this_mdl)$coefficients['Vis_z', 'Pr(>|z|)']

	dfbeta_hap[i] <- summary(this_mdl)$coefficients['Hap_z', 'Estimate']
	leave_one_out_p_hap[i] <- summary(this_mdl)$coefficients['Hap_z', 'Pr(>|z|)']
	}
hist(dfbeta_vis)
hist(dfbeta_hap)
range(dfbeta_vis)
range(dfbeta_hap)
any(leave_one_out_p_vis > 0.05)
any(leave_one_out_p_hap > 0.05)
leave_one_out_p_hap[leave_one_out_p_hap > 0.05]
xdata_excl[(leave_one_out_p_hap > 0.05), ]

## Addressing reviewer 1's concern, the role of POS:

summary(xmdl_comb <- glm.nb(FreqSUBTL ~ Vis_z +
	Aud_z + Gus_z +
	Hap_z + Olf_z + POS,
	data = xdata_excl))


##------------------------------------------------------------------
## Same plot, combined model (reported in paper):
##------------------------------------------------------------------

## Define colors and modalities:

mycols <- c('#f37058', '#efbe1b', '#f79038', '#425fac', '#30b77d')
mymods <- c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')

## Load in images:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/figures/')
s1 <- readPNG('sight.png')
s2 <- readPNG('sound.png')
s3 <- readPNG('touch.png')
s4 <- readPNG('taste.png')
s5 <- readPNG('smell.png')

## Define vector of names:

sense_names <- str_c('s', 1:5)

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

