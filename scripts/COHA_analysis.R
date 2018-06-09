## Bodo Winter
## July 27, 2017
## Analyzing COHA:

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

## 

## Get counts by this:

COHA <- COHA %>% group_by(Word, MainPOS) %>%
	summarize(n = sum(n))

## Combine with COHA:

COHA_excl <- left_join(COHA, dplyr::select(xdata_excl, Word:Modality, POS))
COHA <- left_join(COHA, dplyr::select(xdata, Word:Modality, POS))

## Get rid of non-exclusives for the COHA_excl file:

COHA_excl <- filter(COHA_excl,
	!is.na(Modality))

## Get rid of registers:

COHA_excl <- COHA_excl %>% group_by(Word, Year, Modality, POS) %>%
	summarize(Freq = sum(n))
COHA <- COHA %>% group_by(Word, Year, Modality, POS) %>%
	summarize(Freq = sum(n))

## Add year total N:

COHA_excl_sum <- COHA_excl %>% group_by(Year) %>%
	summarize(Total = sum(Freq)) %>% right_join(COHA_excl)
COHA_sum <- COHA %>% group_by(Year) %>%
	summarize(Total = sum(Freq)) %>% right_join(COHA)

## Compute relative frequency:

COHA_excl_sum <- mutate(COHA_excl_sum, RelFreq = Freq / Total)
COHA_sum <- mutate(COHA_sum, RelFreq = Freq / Total)

## Get rid of nouns and verbs for the analysis (basic pattern also works for them):

# COHA_excl_sum <- filter(COHA_excl_sum,
	# POS == 'Verb')
# COHA_sum <- filter(COHA_sum,
	# POS == 'Verb')

## Make a plot:

COHA_sum %>%
	group_by(Modality, Year) %>%
	summarize(RelFreqMean = mean(RelFreq)) %>% ggplot(aes(x = Year, y = RelFreqMean, col = Modality)) +
	geom_line()
COHA_excl_sum %>%
	group_by(Modality, Year) %>%
	summarize(RelFreqMean = mean(RelFreq)) %>% ggplot(aes(x = Year, y = RelFreqMean, col = Modality)) +
	geom_line()

## This is where I did visual exploration of POS and register differences
## Except for haptic > visual for verbs because of 'get' etc. classified as touch
## No striking patterns, visual dominance all along



##------------------------------------------------------------------
## Calculate simple linear trends for slopes:
##------------------------------------------------------------------

## Plot a few individual lines for exploration:

## SWEET, PURPLE, SHINY, FRAGRANT
these_words <- c('fragrant', 'aromatic', 'pungent',
	'stinky', 'perfumed', 'reeking', 'scented', 'smelly')

filter(COHA_sum, Word %in% these_words) %>%
	ggplot(aes(x = Year, y = RelFreq, col = Word)) +
	geom_line()

## Calculate slopes for each word, including verbs and nouns:

xdata$Slope <- NA
xdata$Slope_excl <- NA
for (i in 1:nrow(xdata)) {
	this_df <- filter(COHA_sum,
		Word == xdata[i, ]$Word)
	if (nrow(this_df) > 0) {
		xdata[i, ]$Slope <- coef(lm(RelFreq ~ Year, data = this_df))[2]
		}
	this_df <- filter(COHA_excl_sum,
		Word == xdata[i, ]$Word)
	if (nrow(this_df) > 0) {
		xdata[i, ]$Slope_excl <- coef(lm(RelFreq ~ Year, data = this_df))[2]
		}
	if (i %% 50 == 0) cat(str_c('Processing ... ', i, '\n'))
	}

## Check the ones that have increased/decreased the most:

arrange(xdata, Slope) %>% dplyr::select(Word, Modality, Slope)
arrange(xdata, desc(Slope)) %>% dplyr::select(Word, Modality, Slope)

## Get means per slope:

xdata %>% group_by(POS, Modality) %>%
	summarize(Slope = mean(Slope, na.rm = TRUE)) %>%
	mutate(Slope = Slope * 1000) %>% 
	arrange(POS, desc(Slope))
xdata %>% group_by(POS, Modality) %>%
	summarize(Slope_excl = mean(Slope_excl, na.rm = TRUE)) %>%
	mutate(Slope_excl = Slope_excl * 1000) %>% 
	arrange(POS, desc(Slope_excl))

## Make a model of this:

summary(lm(Slope ~ Modality, data = xdata))
anova(lm(Slope ~ Modality, data = xdata))
summary(lm(Slope_excl ~ Modality, data = xdata))
anova(lm(Slope_excl ~ Modality, data = xdata))

## With POS interactions:

summary(lm(Slope ~ Modality * POS, data = xdata))
anova(lm(Slope ~ Modality * POS, data = xdata))
summary(lm(Slope_excl ~ Modality * POS, data = xdata))
anova(lm(Slope_excl ~ Modality * POS, data = xdata))

## Make a plot of this:

xdata %>% ggplot(aes(x = Modality, y = Slope, fill = Modality)) +
	geom_boxplot()
xdata %>% ggplot(aes(x = Modality, y = Slope_excl, fill = Modality)) +
	geom_boxplot()

## Basically shows no remarkable changes across this time.

## Check the average relative frequency across the modality:

## Calculate slopes for each word, including verbs and nouns:

xdata$Avg <- NA
xdata$Avg_excl <- NA
for (i in 1:nrow(xdata)) {
	this_df <- filter(COHA_sum,
		Word == xdata[i, ]$Word)
	if (nrow(this_df) > 0) {
		xdata[i, ]$Avg <- mean(this_df$RelFreq)
		}
	this_df <- filter(COHA_excl_sum,
		Word == xdata[i, ]$Word)
	if (nrow(this_df) > 0) {
		xdata[i, ]$Avg_excl <- mean(this_df$RelFreq)
		}
	if (i %% 50 == 0) cat(str_c('Processing ... ', i, '\n'))
	}

## Check:

summary(xmdl.avg <- lm(Avg ~ Modality, data = xdata))
summary(xmdl.avg_excl <- lm(Avg_excl ~ Modality, data = xdata))
anova(xmdl.avg)
anova(xmdl.avg_excl)

## Check with POS interaction:

summary(xmdl.avg <- lm(Avg ~ Modality * POS, data = xdata))
summary(xmdl.avg_excl <- lm(Avg_excl ~ Modality * POS, data = xdata))
anova(xmdl.avg)
anova(xmdl.avg_excl)



##------------------------------------------------------------------
## Model this with GAM:
##------------------------------------------------------------------

## For GAM, make into factors:

COHA_sum <- mutate(COHA_sum,
	Modality = as.factor(Modality),
	Word = as.factor(Word))
COHA_excl_sum <- mutate(COHA_excl_sum,
	Modality = as.factor(Modality),
	Word = as.factor(Word))

## For GAM, down-sample number of years to ease computation, COHA_sum:

COHA_sum <- mutate(COHA_sum,
	Year20 = cut(Year, breaks = seq(1820, 2000, 20)))
COHA_sum[is.na(COHA_sum$Year20), ]$Year20 <- '(1.82e+03,1.84e+03]'	# first 30 years together
COHA_sum <- mutate(COHA_sum,
	Year20step = as.numeric(Year20))

## For GAM, down-sample number of years to ease computation, COHA_excl_sum:

COHA_excl_sum <- mutate(COHA_excl_sum,
	Year20 = cut(Year, breaks = seq(1820, 2000, 20)))
COHA_excl_sum[is.na(COHA_excl_sum$Year20), ]$Year20 <- '(1.82e+03,1.84e+03]'	# first 30 years together
COHA_excl_sum <- mutate(COHA_excl_sum,
	Year20step = as.numeric(Year20))

## And make them into visual versus non-visual since that's the main comparison:

COHA_sum <- mutate(COHA_sum,
	VisOrNot = ifelse(Modality == 'Visual', 'Visual', 'Other'),
	VisOrNot = as.factor(VisOrNot))
COHA_excl_sum <- mutate(COHA_excl_sum,
	VisOrNot = ifelse(Modality == 'Visual', 'Visual', 'Other'),
	VisOrNot = as.factor(VisOrNot))

## Fit GAMs for all:

xmdl_excl <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_excl_sum)
save(xmdl_excl, file = 'GAM_COHA_excl_sum_all.RData')
xmdl <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_sum)
save(xmdl, file = 'GAM_COHA_sum_all.RData')

## Fit GAMs for all, for all modalities:

xmdl_excl_all_mods <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_excl_sum)
save(xmdl_excl_all_mods, file = 'GAM_COHA_excl_sum_all.RData')
xmdl_all_mods <- bam(RelFreq ~ s(Year20step, by = Modality, k = 5) +
	Modality + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_sum)
save(xmdl_all_mods, file = 'GAM_COHA_sum_all_all_mods.RData')

## Visualize:

library(itsadug)
par(mfrow = c(1, 2), cex = 1.1)
plot_smooth(xmdl, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), ylim = c(0, 0.002), rm.ranef = FALSE)
plot_smooth(xmdl, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), ylim = c(0, 0.002), rm.ranef = FALSE, add = TRUE)

## Visualize, exclusives:

par(mfrow = c(1, 2), cex = 1.1)
plot_smooth(xmdl_excl, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), ylim = c(0, 0.03), rm.ranef = FALSE)
plot_smooth(xmdl_excl, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), ylim = c(0, 0.03), rm.ranef = FALSE, add = TRUE)

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
s1 <- readPNG('sight.png')

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

## Adjectives only, GAMs:

xmdl_excl_adj <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(COHA_excl_sum, POS == 'Adj'))
save(xmdl_excl_adj, file = 'GAM_COHA_excl_sum_adj.RData')
xmdl_adj <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(COHA_sum, POS == 'Adj'))
save(xmdl, file = 'GAM_COHA_sum_adj.RData')

## Nouns only, GAMs:

xmdl_excl_noun <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(COHA_excl_sum, POS == 'Noun'))
save(xmdl_excl_noun, file = 'GAM_COHA_excl_sum_noun.RData')
xmdl_noun <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(COHA_sum, POS == 'Noun'))
save(xmdl, file = 'GAM_COHA_sum_noun.RData')

## verbs only, GAMs:

xmdl_excl_verb <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(COHA_excl_sum, POS == 'Verb'))
save(xmdl_excl_verb, file = 'GAM_COHA_excl_sum_verb.RData')
xmdl_verb <- bam(RelFreq ~ s(Year20step, by = VisOrNot, k = 5) +
	VisOrNot + 
	s(Year20step, Word, bs = 'fs', k = 5, m = 1),
	data = filter(COHA_sum, POS == 'Verb'))
save(xmdl, file = 'GAM_COHA_sum_verb.RData')

## Extract data for a publication-ready plot:

adj_other <- plot_smooth(xmdl_adj, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
adj_vis <- plot_smooth(xmdl_adj, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv
adj_excl_other <- plot_smooth(xmdl_excl_adj, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
adj_excl_vis <- plot_smooth(xmdl_excl_adj, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv

noun_other <- plot_smooth(xmdl_noun, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
noun_vis <- plot_smooth(xmdl_noun, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv
noun_excl_other <- plot_smooth(xmdl_excl_noun, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
noun_excl_vis <- plot_smooth(xmdl_excl_noun, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv

verb_other <- plot_smooth(xmdl_verb, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
verb_vis <- plot_smooth(xmdl_verb, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv
verb_excl_other <- plot_smooth(xmdl_excl_verb, view = 'Year20step',
	cond = list(VisOrNot = 'Other'), rm.ranef = TRUE)$fv
verb_excl_vis <- plot_smooth(xmdl_excl_verb, view = 'Year20step',
	cond = list(VisOrNot = 'Visual'), rm.ranef = TRUE)$fv

## Make a double plot of this, only adjectives:

quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.75, 0.5, 1), mai = c(0.5, 0.5, 0.5, 0.5))
# Plot 1
emptyplot(xlim = c(1, 9), ylim = c(0, 0.002), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(adj_other$Year20step, rev(adj_other$Year20step)),
	c(adj_other$ll, rev(adj_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(adj_other$Year20step, adj_other$fit, type = 'l', lwd = 3)
# Visual line
polygon(c(adj_vis$Year20step, rev(adj_vis$Year20step)),
	c(adj_vis$ll, rev(adj_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(adj_vis$Year20step, adj_vis$fit, type = 'l', lwd = 3, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = adj_vis[15, ]$fit - 0.00015,
	ytop = adj_vis[15, ]$fit + 0.00015,
		xpd = NA)
# Axes
axis(side = 2, at = seq(0, 0.002, 0.0005),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
axis(side = 1, at = 1:9, labels = seq(1820, 1980, 20),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
mtext(side = 2, text = 'Relative frequency', line = 5.2, font = 2, cex = 1.5)
mtext(side = 3, text = '(a) All adjectives', line = 1.5, font = 2, cex = 1.6)
# Plot 2
emptyplot(xlim = c(1, 9), ylim = c(0, 0.03), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(adj_excl_other$Year20step, rev(adj_excl_other$Year20step)),
	c(adj_excl_other$ll, rev(adj_excl_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(adj_excl_other$Year20step, adj_excl_other$fit, type = 'l', lwd = 3)
# Visual line
polygon(c(adj_excl_vis$Year20step, rev(adj_excl_vis$Year20step)),
	c(adj_excl_vis$ll, rev(adj_excl_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(adj_excl_vis$Year20step, adj_excl_vis$fit, type = 'l', lwd = 3, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = adj_excl_vis[15, ]$fit - (0.075 * 0.03),
	ytop = adj_excl_vis[15, ]$fit + (0.075 * 0.03),
		xpd = NA)
# Axes
axis(side = 1, at = 1:9, labels = seq(1820, 1980, 20),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
axis(side = 4, at = seq(0, 0.03, 0.01),
	font = 2, cex.axis = 1.25, las = 2, lwd = 2)
mtext(side = 4, text = 'Relative frequency', line = 4.5, font = 2, cex = 1.5)
mtext(side = 3, text = '(b) 10 most exclusive adjectives', line = 1.5, font = 2, cex = 1.6)

## -------------------------------- GAM plot matrix:
## Plot matrix for adjectives, nouns and verbs:

quartz('', 13, 11)
par(mfrow = c(3, 2), mai = c(0.25, 0.25, 0.1, 0.25), omi = c(1, 2, 0.5, 1.25))
# Adjectives: --------------------------------------------------------------------
# Plot 1
emptyplot(xlim = c(1, 9), ylim = c(0, 0.0015), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(adj_other$Year20step, rev(adj_other$Year20step)),
	c(adj_other$ll, rev(adj_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(adj_other$Year20step, adj_other$fit, type = 'l', lwd = 4, lty = 2)
# Visual line
polygon(c(adj_vis$Year20step, rev(adj_vis$Year20step)),
	c(adj_vis$ll, rev(adj_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(adj_vis$Year20step, adj_vis$fit, type = 'l', lwd = 4, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = adj_vis[15, ]$fit - (0.075 * 0.0015),
	ytop = adj_vis[15, ]$fit + (0.075 * 0.0015),
		xpd = NA)
# Axes
axis(side = 2, at = seq(0, 0.0015, 0.0005),
	font = 2, cex.axis = 1.35, las = 2, lwd = 2)
axis(side = 1, at = 1:9, labels = FALSE, las = 2, lwd = 2)
mtext(side = 2, text = 'Adj', line = 6.5, font = 2, cex = 2.5, las = 2)
mtext(side = 3, text = 'All words', line = 1.5, font = 2, cex = 2.5)
# Plot 2
emptyplot(xlim = c(1, 9), ylim = c(0, 0.04), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(adj_excl_other$Year20step, rev(adj_excl_other$Year20step)),
	c(adj_excl_other$ll, rev(adj_excl_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(adj_excl_other$Year20step, adj_excl_other$fit, type = 'l', lwd = 4, lty = 2)
# Visual line
polygon(c(adj_excl_vis$Year20step, rev(adj_excl_vis$Year20step)),
	c(adj_excl_vis$ll, rev(adj_excl_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(adj_excl_vis$Year20step, adj_excl_vis$fit, type = 'l', lwd = 4, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = adj_excl_vis[15, ]$fit - (0.075 * 0.04),
	ytop = adj_excl_vis[15, ]$fit + (0.075 * 0.04),
		xpd = NA)
# Axes
mtext(side = 3, text = '10 most exclusive words', line = 1.5, font = 2, cex = 2.5)
axis(side = 1, at = 1:9, labels = FALSE, las = 2, lwd = 2)
axis(side = 4, at = seq(0, 0.04, 0.01),
	font = 2, cex.axis = 1.35, las = 2, lwd = 2)
mtext(side = 4, text = 'Rel. Frequency', line = 5.2, font = 2, cex = 1.5)
# Nouns: --------------------------------------------------------------------
# Plot 1
emptyplot(xlim = c(1, 9), ylim = c(0, 0.004), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(noun_other$Year20step, rev(noun_other$Year20step)),
	c(noun_other$ll, rev(noun_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(noun_other$Year20step, noun_other$fit, type = 'l', lwd = 4, lty = 2)
# Visual line
polygon(c(noun_vis$Year20step, rev(noun_vis$Year20step)),
	c(noun_vis$ll, rev(noun_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(noun_vis$Year20step, noun_vis$fit, type = 'l', lwd = 4, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = noun_vis[15, ]$fit - (0.075 * 0.004),
	ytop = noun_vis[15, ]$fit + (0.075 * 0.004),
		xpd = NA)
# Axes
axis(side = 2, at = seq(0, 0.004, 0.001),
	font = 2, cex.axis = 1.35, las = 2, lwd = 2)
axis(side = 1, at = 1:9, labels = FALSE, las = 2, lwd = 2)
mtext(side = 2, text = 'Noun', line = 6.5, font = 2, cex = 2.5, las = 2)
# Plot 2
emptyplot(xlim = c(1, 9), ylim = c(0, 0.04), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(noun_excl_other$Year20step, rev(noun_excl_other$Year20step)),
	c(noun_excl_other$ll, rev(noun_excl_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(noun_excl_other$Year20step, noun_excl_other$fit, type = 'l', lwd = 4, lty = 2)
# Visual line
polygon(c(noun_excl_vis$Year20step, rev(noun_excl_vis$Year20step)),
	c(noun_excl_vis$ll, rev(noun_excl_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(noun_excl_vis$Year20step, noun_excl_vis$fit, type = 'l', lwd = 4, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = noun_excl_vis[15, ]$fit - (0.075 * 0.04),
	ytop = noun_excl_vis[15, ]$fit + (0.075 * 0.04),
		xpd = NA)
# Axes
axis(side = 4, at = seq(0, 0.04, 0.01),
	font = 2, cex.axis = 1.35, las = 2, lwd = 2)
axis(side = 1, at = 1:9, labels = FALSE, las = 2, lwd = 2)
mtext(side = 4, text = 'Rel. Frequency', line = 5.2, font = 2, cex = 1.5)
# Verbs: --------------------------------------------------------------------
# Plot 1
emptyplot(xlim = c(1, 9), ylim = c(0, 0.004), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(verb_other$Year20step, rev(verb_other$Year20step)),
	c(verb_other$ll, rev(verb_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(verb_other$Year20step, verb_other$fit, type = 'l', lwd = 4, lty = 2)
# Visual line
polygon(c(verb_vis$Year20step, rev(verb_vis$Year20step)),
	c(verb_vis$ll, rev(verb_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(verb_vis$Year20step, verb_vis$fit, type = 'l', lwd = 4, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = verb_vis[15, ]$fit - (0.075 * 0.004),
	ytop = verb_vis[15, ]$fit + (0.075 * 0.004),
		xpd = NA)
# Axes
axis(side = 2, at = seq(0, 0.004, 0.001),
	font = 2, cex.axis = 1.35, las = 2, lwd = 2)
axis(side = 1, at = 1:9, labels = seq(1820, 1980, 20),
	font = 2, cex.axis = 1.6, las = 2, lwd = 2)
mtext(side = 2, text = 'Verb', line = 6.5, font = 2, cex = 2.5, las = 2)
mtext(side = 1, text = 'Year', line = 6.5, font = 2, cex = 2.5)
# Plot 2
emptyplot(xlim = c(1, 9), ylim = c(0, 0.08), xaxs = 'i', yaxs = 'i')
# Other line
polygon(c(verb_excl_other$Year20step, rev(verb_excl_other$Year20step)),
	c(verb_excl_other$ll, rev(verb_excl_other$ul)), col = rgb(0, 0, 0, 0.3), border = FALSE)
points(verb_excl_other$Year20step, verb_excl_other$fit, type = 'l', lwd = 4, lty = 2)
# Visual line
polygon(c(verb_excl_vis$Year20step, rev(verb_excl_vis$Year20step)),
	c(verb_excl_vis$ll, rev(verb_excl_vis$ul)), col = rgb(myred[1], myred[2], myred[3], 0.3), border = FALSE)
points(verb_excl_vis$Year20step, verb_excl_vis$fit, type = 'l', lwd = 4, col = '#f37058')
rasterImage(image = s1,
	xleft = 5 - 0.4, xright = 5 + 0.4,
	ybottom = verb_excl_vis[15, ]$fit - (0.075 * 0.08),
	ytop = verb_excl_vis[15, ]$fit + (0.075 * 0.08),
		xpd = NA)
# Axes
axis(side = 1, at = 1:9, labels = seq(1820, 1980, 20),
	font = 2, cex.axis = 1.6, las = 2, lwd = 2)
axis(side = 4, at = seq(0, 0.08, 0.02),
	font = 2, cex.axis = 1.35, las = 2, lwd = 2)
mtext(side = 4, text = 'Rel. Frequency', line = 5.2, font = 2, cex = 1.5)
mtext(side = 1, text = 'Year', line = 6.5, font = 2, cex = 2.5)

## Summarize all GAMs:

print(xmdl.sum <- summary(xmdl))
print(xmdl_excl.sum <- summary(xmdl_excl))

print(xmdl_all_mods.sum <- summary(xmdl_all_mods))
print(xmdl_excl_all_mods.sum <- summary(xmdl_excl_all_mods))

print(xmdl_adj.sum <- summary(xmdl_adj))
print(xmdl_excl_adj.sum <- summary(xmdl_excl_adj))

print(xmdl_noun.sum <- summary(xmdl_noun))
print(xmdl_excl_noun.sum <- summary(xmdl_excl_noun))

print(xmdl_verb.sum <- summary(xmdl_verb))
print(xmdl_excl_verb.sum <- summary(xmdl_excl_verb))

save.image('entire_session.RData')



##------------------------------------------------------------------
## Make a plot:
##------------------------------------------------------------------

## Define colors and modalities:

mycols <- c('#f37058', '#efbe1b', '#f79038', '#425fac', '#30b77d')
mymods <- c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')

## Load in other images:

s2 <- readPNG('sound.png')
s3 <- readPNG('touch.png')
s4 <- readPNG('taste.png')
s5 <- readPNG('smell.png')

## Define vector of names:

sense_names <- str_c('s', 1:5)

## Get averages and SDs:
# (exchange with COHA_sum for alternative analysis)

xmeans <- COHA_excl_sum %>% group_by(Year, Modality) %>%
	summarize(RelFreqMean = mean(RelFreq),
		RelFreqSD = sd(RelFreq)) %>%
	arrange(Modality, Year)



##------------------------------------------------------------------
## A less crowded plot:
##------------------------------------------------------------------

## To compute 95% confidence interval of the mean per word, we need N words per year:

year_counts <- COHA_excl_sum %>% group_by(Year, Modality) %>% count()
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
## Analyze 10 most exclusives:
##------------------------------------------------------------------

## Take the 10 most exclusive ones per modality:

new_excl <- c()
for (i in 1:5) {
	this_mod <- mymods[i]
	this_df <- filter(adjs, DominantModality == this_mod)
	this_df <- arrange(this_df, desc(ModalityExclusivity))
	new_excl <- bind_rows(new_excl, this_df[1:10, ])
	}

## Check these over time:

COHA_sum_new_excl <- filter(COHA_sum,
	Word %in% pull(new_excl, Word))

## Get averages and SDs:

COHA_sum_new_excl_sum <- COHA_sum_new_excl %>%
	group_by(Year, Modality) %>% summarize(RelFreq = mean(RelFreq))
COHA_sum_new_excl_sum %>% ggplot(aes(x = Year, y = RelFreq, col = Modality)) +
	geom_line()

