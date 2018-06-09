## Bodo Winter
## July 27, 2017
## Analyzing COHA:

## New script that includes GAMs after reviewer suggestions

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(tidyverse)
library(stringr)
library(png)
library(mgcv)

## Load data:

setwd('/Volumes/Macintosh HD/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/')

## Source empty plot function:

source('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/scripts/emptyplot.R')

## Get all file names:

COHA <- read_csv('COHA_summary_new_lemmatized.csv')

## Load modality norm data:

setwd('/Users/winterb/Research/senses_sensory_modalities/viberg/brandnew_analysis/data/')
adjs <- read_csv('lynott_connell_2009_adj_norms.csv')
nouns <- read_csv('lynott_connell_2013_noun_norms.csv')
verbs <- read_csv('winter_2016_verb_norms.csv')

## Combine different modality norm datasets:

xdata <- bind_rows(dplyr::select(adjs, -PropertyBritish),
	nouns, dplyr::select(verbs, -RandomSet, -N))

## Add data source identifier:

xdata$POS <- c(rep('Adj', nrow(adjs)),
	rep('Noun', nrow(nouns)),
	rep('Verb', nrow(verbs)))

## Rename modality column:

xdata <- rename(xdata,
	Modality = DominantModality)


##------------------------------------------------------------------
## Process COHA data:
##------------------------------------------------------------------

## Extract first few letters of POS tag (main POS):

COHA <- mutate(COHA,
	POS2 = str_sub(POS, 1, 2))

## Get only adjectives, nouns and verbs and change those labels:

COHA <- filter(COHA,
	POS2 %in% c('vv', 'nn', 'jj')) %>%
	mutate(MainPOS = ifelse(POS2 == 'vv', 'Verb', 'Noun'),
		MainPOS = ifelse(POS2 == 'jj', 'Adj', MainPOS))

## Make all NA's in the Year column into 2000's (that's what they are):

COHA[is.na(COHA$Year), ]$Year <- '2000'

## Load in the lemmatized data (adjectives hand-checked) and loop through it to get POS-specific freqs:
## if that lemma exists as its own, it's preferred; so crying = lemma and crying (form) with lemma "cry" for adjective will be the "crying" lemma

all_lems <- read_csv('all_words_lemmatized_new.csv')
all_lems$Modality <- xdata$Modality
all_lems$ModalityExclusivity <- xdata$ModalityExclusivity

## Build empty data frame to be filled with data:

all_lems$Freq <- 0		# NAs are true 0's
COHA_lem <- c()
for (i in seq_along(unique(COHA$Year))) {
	all_lems$Year <- unique(COHA$Year)[i] 
	COHA_lem <- bind_rows(COHA_lem, all_lems)
	}
COHA_lem <- arrange(COHA_lem, Word, POS, Year) %>%
	mutate(ID = str_c(Lemma, ':', POS, ':', Year))

## Now loop through and get all the data:

for (i in 1:nrow(all_lems)) {
	w <- all_lems[i, ]$Lemma
	this_POS <- all_lems[i, ]$POS
	if (!(w %in% c('rippled', 'rippling'))) {
		df <- filter(COHA, Lemma == w, MainPOS == this_POS) %>%
			group_by(Lemma, MainPOS, Year) %>% summarise(n = sum(n)) %>%
			mutate(ID = str_c(Lemma, ':', MainPOS, ':', Year))
		COHA_lem[match(df$ID, COHA_lem$ID), ]$Freq <- df$n
		}
	if (i %% 50 == 0) cat(str_c(i, '\n'))
	}

## Fix rippled and rippling:

rippled <- filter(COHA, Word == 'rippled', MainPOS == 'Adj') %>%
	group_by(Word, Lemma, MainPOS, Year) %>% summarise(n = sum(n))
rippling <- filter(COHA, Word == 'rippling', MainPOS == 'Adj') %>%
	group_by(Word, Lemma, MainPOS, Year) %>% summarise(n = sum(n))
for (i in 1:nrow(rippled)) {
	COHA_lem[COHA_lem$POS == 'Adj' & COHA_lem$Word == 'rippled' & COHA_lem$Year == rippled[i,]$Year, ]$Freq <- rippled[i, ]$n
	COHA_lem[COHA_lem$POS == 'Adj' & COHA_lem$Word == 'rippling' & COHA_lem$Year == rippling[i,]$Year, ]$Freq <- rippling[i, ]$n
	}



##------------------------------------------------------------------
## Get only the 10 most exclusives per POS per modality:
##------------------------------------------------------------------

## Define modality names:

mymods <- c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')

## Take the 10 most exclusive ones per modality for adjectives:

adj_excl <- c()
for (i in seq_along(mymods)) {
	this_mod <- mymods[i]
	this_df <- filter(adjs, 
		DominantModality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))[1:10, ]
	this_df <- filter(COHA_lem, Word %in% this_df$Word, POS == 'Adj')
	adj_excl <- bind_rows(adj_excl, this_df)
	}

## Take the 10 most exclusive ones per modality for verbs:

verb_excl <- c()
for (i in seq_along(mymods)) {
	this_mod <- mymods[i]
	this_df <- filter(verbs, 
		DominantModality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))[1:10, ]
	this_df <- filter(COHA_lem, Word %in% this_df$Word, POS == 'Verb')
	verb_excl <- bind_rows(verb_excl, this_df)
	}

## For nouns the same analysis is not possible because there's not 10 for each modality
## Take the 10 most exclusive ones per modality for nouns, if there are enough:

noun_excl <- c()
for (i in seq_along(mymods)) {
	this_mod <- mymods[i]
	this_df <- filter(nouns,
		DominantModality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))[1:10, ]
	this_df <- filter(this_df, !is.na(Word))	# for taste and smell
	this_df <- filter(COHA_lem, Word %in% this_df$Word, POS == 'Noun')
	noun_excl <- bind_rows(noun_excl, this_df)
	}

## Combine exclusives:

xdata_excl <- bind_rows(adj_excl, verb_excl, noun_excl)



##------------------------------------------------------------------
## Compute relative frequencies:
##------------------------------------------------------------------

## Get totals:

COHA_totals <- COHA_lem %>% group_by(Year) %>% summarise(Total = sum(Freq))
excl_totals <- xdata_excl %>% group_by(Year) %>% summarise(Total = sum(Freq))

## Merge that back into the data frames:

COHA_lem <- left_join(COHA_lem, COHA_totals)
xdata_excl <- left_join(xdata_excl, excl_totals)

## Compute relative frequencies &  make year numeric:

COHA_lem <- mutate(COHA_lem,
	RelFreq = Freq / Total,
	Year = as.numeric(Year))
xdata_excl <- mutate(xdata_excl,
	RelFreq = Freq / Total,
	Year = as.numeric(Year))

## Make a plot of full descriptive data (modality and POS):

COHA_lem %>%
	group_by(Modality, POS, Year) %>%
	summarize(RelFreqMean = mean(RelFreq)) %>% ggplot(aes(x = Year, y = RelFreqMean, col = Modality)) +
	geom_line() + facet_wrap(~POS)	# odd noun results is because of "breath" and "air" (and only 2 words)
xdata_excl %>%
	group_by(Modality, POS, Year) %>%
	summarize(RelFreqMean = mean(RelFreq)) %>% ggplot(aes(x = Year, y = RelFreqMean, col = Modality)) +
	geom_line() + facet_wrap(~POS)



##------------------------------------------------------------------
## Calculate simple linear trends for slopes:
##------------------------------------------------------------------

## Create unique identifiers (for those that have multiple POS):

COHA_lem <- mutate(COHA_lem,
	WordID = str_c(Word, ':', POS))
xdata_excl <- mutate(xdata_excl,
	WordID = str_c(Word, ':', POS))

## Calculate slopes for each word, including verbs and nouns:

all_WordIDS <- unique(COHA_lem$WordID)
all_slopes <- NA

for (i in seq_along(all_WordIDS)) {
	df <- filter(COHA_lem, WordID == all_WordIDS[i])
	all_slopes[i] <- coef(lm(RelFreq ~ Year, data = df))[2]
	if (i %% 50 == 0) cat(str_c('Processing ... ', i, '\n'))
	}

## Make this into a data frame:

slopes <- tibble(WordID = all_WordIDS, Slope = all_slopes,
	Modality = COHA_lem[match(all_WordIDS, COHA_lem$WordID), ]$Modality)

## Calculate slopes for each word, including verbs and nouns:

excl_WordIDS <- unique(xdata_excl$WordID)
excl_slopes <- NA

for (i in seq_along(excl_WordIDS)) {
	df <- filter(xdata_excl, WordID == excl_WordIDS[i])
	excl_slopes[i] <- coef(lm(RelFreq ~ Year, data = df))[2]
	if (i %% 50 == 0) cat(str_c('Processing ... ', i, '\n'))
	}

## Make this into a data frame:

excl_slopes <- tibble(WordID = excl_WordIDS, Slope = excl_slopes,
	Modality = xdata_excl[match(excl_WordIDS, xdata_excl$WordID), ]$Modality)

## Get POS info:

slopes <- mutate(slopes,
	POS = str_extract(WordID, 'Adj|Noun|Verb'))
excl_slopes <- mutate(excl_slopes,
	POS = str_extract(WordID, 'Adj|Noun|Verb'))

## Check the ones that have increased/decreased the most:

arrange(slopes, Slope) %>% dplyr::select(WordID, Modality, Slope)
arrange(slopes, desc(Slope)) %>% dplyr::select(WordID, Modality, Slope)
arrange(excl_slopes, Slope) %>% dplyr::select(WordID, Modality, Slope)
arrange(excl_slopes, desc(Slope)) %>% dplyr::select(WordID, Modality, Slope)

## Get means per modality:

slopes %>% group_by(POS, Modality) %>%
	summarize(Slope = mean(Slope, na.rm = TRUE)) %>%
	mutate(Slope = Slope * 1000) %>% 
	arrange(POS, desc(Slope))
excl_slopes %>% group_by(POS, Modality) %>%
	summarize(Slope = mean(Slope, na.rm = TRUE)) %>%
	mutate(Slope = Slope * 1000) %>% 
	arrange(POS, desc(Slope))

## Make a model of this:

summary(lm(Slope ~ Modality, data = slopes))
anova(lm(Slope ~ Modality, data = slopes))
summary(lm(Slope ~ Modality, data = excl_slopes))
anova(lm(Slope ~ Modality, data = excl_slopes))

## With POS interactions:

summary(lm(Slope ~ Modality * POS, data = slopes))
anova(lm(Slope ~ Modality * POS, data = slopes))
summary(lm(Slope ~ Modality * POS, data = excl_slopes))
anova(lm(Slope ~ Modality * POS, data = excl_slopes))

## Make a plot of this:

slopes %>% ggplot(aes(x = Modality, y = Slope, fill = Modality)) +
	geom_boxplot()
excl_slopes %>% ggplot(aes(x = Modality, y = Slope, fill = Modality)) +
	geom_boxplot()
excl_slopes %>% ggplot(aes(x = Modality, y = Slope, fill = Modality)) +
	geom_boxplot() + facet_wrap(~POS)

## Basically shows no remarkable changes across this time.




##------------------------------------------------------------------
## Make a plot:
##------------------------------------------------------------------

## Define colors and modalities:

mycols <- c('#f37058', '#efbe1b', '#f79038', '#425fac', '#30b77d')
mymods <- c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')

## Load in other images:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/figures/')
s1 <- readPNG('sight.png')
s2 <- readPNG('sound.png')
s3 <- readPNG('touch.png')
s4 <- readPNG('taste.png')
s5 <- readPNG('smell.png')

## Define vector of names:

sense_names <- str_c('s', 1:5)

## Get averages and SDs:
# (exchange with COHA_sum for alternative analysis)

xmeans <- xdata_excl %>% group_by(Year, Modality) %>%
	summarize(RelFreqMean = mean(RelFreq),
		RelFreqSD = sd(RelFreq)) %>%
	arrange(Modality, Year)

## To compute 95% confidence interval of the mean per word, we need N words per year:

year_counts <- xdata_excl %>% group_by(Year, Modality) %>% count()
xmeans <- left_join(xmeans, year_counts)

## Compute 95% confidence interval around the mean:

xmeans <- mutate(xmeans,
	SE = RelFreqSD / sqrt(n),
	UB = RelFreqMean + 1.96 * SE,
	LB = RelFreqMean - 1.96 * SE,
	UB_SE = RelFreqMean + SE,
	LB_SE = RelFreqMean - SE)

## Get transparent version of red:

myred <- t(col2rgb('#f37058', alpha = FALSE)) / 255
myred <- as.vector(myred)

vis_only <- filter(xmeans, Modality == 'Visual')

## Make the plot:

quartz('', 9, 6)
par(mai = c(1.5, 1, 0.5, 1.8))
emptyplot(xlim = c(1800, 2000), ylim = c(0, 0.03))
axis(side = 1, at = seq(1800, 2000, 20),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.15)
mtext(side = 1, line = 3.5,
	text = 'Decade', font = 2, cex = 2.2)
axis(side = 4, at = seq(0, 0.03, 0.01), las = 2,
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25)
mtext(side = 4, line = 4.8,
	text = 'Relative frequency', font = 2, cex = 1.8)
# with(vis_only,
	# polygon(x = c(Year, rev(Year)), y = c(LB_SE, rev(UB_SE)),
		# border = FALSE, col = rgb(myred[1], myred[2], myred[3], alpha = 0.4)))
for (i in seq_along(mycols)){
	this_mod <- mymods[i]
	this_df <- filter(xmeans, Modality == this_mod)
	with(this_df,
		points(Year, RelFreqMean, col = mycols[i], lwd = 5, type = 'l'))

	rasterImage(image = get(sense_names[i]),
		xleft = 1800 - 7 - 1, xright = 1800 + 7 - 1,
		ybottom = filter(xmeans, Year == 1810,
			Modality == this_mod)$RelFreqMean - (0.005 * 0.3),
		ytop = filter(xmeans, Year == 1810,
			Modality == this_mod)$RelFreqMean + (0.005 * 0.3),
			xpd = NA)
	}



##------------------------------------------------------------------
## Model this with GAMs:
##------------------------------------------------------------------

## For GAM, make into factors:

COHA_lem <- mutate(COHA_lem,
	Modality = as.factor(Modality),
	Word = as.factor(Word))
xdata_excl <- mutate(xdata_excl,
	Modality = as.factor(Modality),
	Word = as.factor(Word))

## For GAM, down-sample number of years to ease computation, COHA_sum:

COHA_lem <- mutate(COHA_lem,
	Year20 = cut(Year, breaks = seq(1820, 2000, 20)))
COHA_lem[is.na(COHA_lem$Year20), ]$Year20 <- '(1.82e+03,1.84e+03]'	# first 30 years together
COHA_lem <- mutate(COHA_lem,
	Year20step = as.numeric(Year20))

## For GAM, down-sample number of years to ease computation, COHA_excl_sum:

xdata_excl <- mutate(xdata_excl,
	Year20 = cut(Year, breaks = seq(1820, 2000, 20)))
xdata_excl[is.na(xdata_excl$Year20), ]$Year20 <- '(1.82e+03,1.84e+03]'	# first 30 years together
xdata_excl <- mutate(xdata_excl,
	Year20step = as.numeric(Year20))

## And make them into visual versus non-visual since that's the main comparison:

COHA_lem <- mutate(COHA_lem,
	VisOrNot = ifelse(Modality == 'Visual', 'Visual', 'Other'),
	VisOrNot = as.factor(VisOrNot))
xdata_excl <- mutate(xdata_excl,
	VisOrNot = ifelse(Modality == 'Visual', 'Visual', 'Other'),
	VisOrNot = as.factor(VisOrNot))

## Fit GAMs, vision vs. rest:

xmdl_excl <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = xdata_excl)
xmdl_excl_sum <- summary(xmdl_excl)
xmdl <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_lem)
xmdl_sum <- summary(xmdl)
save(xmdl_excl, xmdl_excl_sum, xmdl, xmdl_sum,
	file = 'GAM_vision_versus_rest.RData')

## Fit GAMs, for all modalities:

xmdl_excl_all_mods <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality + POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = xdata_excl)
xmdl_excl_all_mods_sum <- summary(xmdl_excl_all_mods)
xmdl_all_mods <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality + POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_lem)
xmdl_all_mods_sum <- summary(xmdl_all_mods)
save(xmdl_excl_all_mods, xmdl_excl_all_mods_sum, xmdl_all_mods, xmdl_all_mods_sum,
	file = 'GAM_all_modalities.RData')

## Separate for POS:

xmdl_excl_noun <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(xdata_excl, POS == 'Noun'))
xmdl_excl_noun_sum <- summary(xmdl_excl_noun)
xmdl_excl_verb <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(xdata_excl, POS == 'Verb'))
xmdl_excl_verb_sum <- summary(xmdl_excl_verb)
xmdl_excl_adj <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(xdata_excl, POS == 'Adj'))
xmdl_excl_adj_sum <- summary(xmdl_excl_adj)
save(xmdl_excl_noun, xmdl_excl_noun_sum, xmdl_excl_verb,
	xmdl_excl_verb_sum, xmdl_excl_adj, xmdl_excl_adj_sum, file = 'GAM_exclusive_byPOS.RData')

## Plot these models:

plot_smooth(xmdl_excl_noun, view = 'Year20step',
	cond = list(Modality = 'Visual'), rm.ranef = FALSE)
plot_smooth(xmdl_excl_verb, view = 'Year20step',
	cond = list(Modality = 'Visual'), rm.ranef = FALSE)
plot_smooth(xmdl_excl_adj, view = 'Year20step',
	cond = list(Modality = 'Visual'), rm.ranef = FALSE)

## Separate for POS, Vis vs Non-Vis:

xmdl_excl_noun_vis <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(xdata_excl, POS == 'Noun'))
xmdl_excl_noun_sum_vis <- summary(xmdl_excl_noun_vis)
xmdl_excl_verb_vis <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(xdata_excl, POS == 'Verb'))
xmdl_excl_verb_sum_vis <- summary(xmdl_excl_verb_vis)
xmdl_excl_adj_vis <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(xdata_excl, POS == 'Adj'))
xmdl_excl_adj_sum_vis <- summary(xmdl_excl_adj_vis)
save(xmdl_excl_noun_vis, xmdl_excl_noun_sum_vis, xmdl_excl_verb_vis,
	xmdl_excl_verb_sum_vis, xmdl_excl_adj_vis, xmdl_excl_adj_sum_vis, file = 'GAM_exclusive_byPOS_VisVsNonVis.RData')

## Fit GAM for vis versus rest, with POS interaction:

xmdl_excl_int <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot * POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = xdata_excl)
xmdl_excl_int_sum <- summary(xmdl_excl_int)
xmdl_int <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot * POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_lem)
xmdl_int_sum <- summary(xmdl_int)
save(xmdl_excl_int, xmdl_excl_int_sum, xmdl_int, xmdl_int_sum,
	file = 'GAM_vision_versus_rest_withPOSinteraction.RData')

## Fit GAMs, for all modalities with POS interaction:

xmdl_excl_all_mods_int <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality * POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = xdata_excl)
xmdl_excl_all_mods_int_sum <- summary(xmdl_excl_all_mods_int)
xmdl_all_mods_int <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality * POS + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_lem)
xmdl_all_mods_int_sum <- summary(xmdl_all_mods_int)
save(xmdl_excl_all_mods_int, xmdl_excl_all_mods_int_sum, xmdl_all_mods_int, xmdl_all_mods_int_sum,
	file = 'GAM_all_modalities_withPOSinteraction.RData')

## Visualize:

library(itsadug)
par(mfrow = c(1, 2), cex = 1.1)
plot_smooth(xmdl, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), ylim = c(0, 0.003), rm.ranef = FALSE)
plot_smooth(xmdl, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), ylim = c(0, 0.003), rm.ranef = FALSE, add = TRUE, col = 'darkred')

## Visualize, exclusives:

plot_smooth(xmdl_excl, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), ylim = c(0, 0.04), rm.ranef = FALSE)
plot_smooth(xmdl_excl, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), ylim = c(0, 0.04), rm.ranef = FALSE, add = TRUE, col = 'darkred')

## Extract data for a publication-ready plot:

full_other <- plot_smooth(xmdl, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
full_vis <- plot_smooth(xmdl, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv
excl_other <- plot_smooth(xmdl_excl, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
excl_vis <- plot_smooth(xmdl_excl, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv

## Get transparent version of red:

myred <- t(col2rgb('#f37058', alpha = FALSE)) / 255
myred <- as.vector(myred)

## Load in images:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/figures/')

## Make a double plot of this:

quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.75, 0.5, 1), mai = c(0.5, 0.5, 0.5, 0.5))
# Plot 1
emptyplot(xlim = c(1, 9), ylim = c(0, 0.002), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(full_other$Year20step, rev(full_other$Year20step)),
	c(full_other$ll, rev(full_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(full_other$Year20step, full_other$fit, type = 'l', lwd = 3)
# Visual line
polygon(c(full_vis$Year20step, rev(full_vis$Year20step)),
	c(full_vis$ll, rev(full_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(full_vis$Year20step, full_vis$fit, type = 'l', lwd = 3, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = full_vis[15, ]$fit - 0.00015,
	ytop = full_vis[15, ]$fit + 0.00015,
		xpd = NA)
# Axes
axis(side = 2, at = seq(0, 0.002, 0.0005),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
axis(side = 1, at = 1:9, labels = seq(1820, 1980, 20),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
mtext(side = 2, text = 'Relative frequency', line = 5.2, font = 2, cex = 1.5)
mtext(side = 3, text = '(a) All words', line = 1.5, font = 2, cex = 1.6)
# Plot 2
emptyplot(xlim = c(1, 9), ylim = c(0, 0.03), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(excl_other$Year20step, rev(excl_other$Year20step)),
	c(excl_other$ll, rev(excl_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(excl_other$Year20step, excl_other$fit, type = 'l', lwd = 3)
# Visual line
polygon(c(excl_vis$Year20step, rev(excl_vis$Year20step)),
	c(excl_vis$ll, rev(excl_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(excl_vis$Year20step, excl_vis$fit, type = 'l', lwd = 3, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = excl_vis[15, ]$fit - (0.075 * 0.03),
	ytop = excl_vis[15, ]$fit + (0.075 * 0.03),
		xpd = NA)
# Axes
axis(side = 1, at = 1:9, labels = seq(1820, 1980, 20),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
axis(side = 4, at = seq(0, 0.03, 0.01),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
mtext(side = 4, text = 'Relative frequency', line = 4.5, font = 2, cex = 1.5)
mtext(side = 3, text = '(b) 10 most exclusive words', line = 1.5, font = 2, cex = 1.6)

