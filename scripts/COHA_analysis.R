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
# library(mgcv)

## Load data:

setwd('/Volumes/Macintosh HD/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/')

## Source empty plot function:

source('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/scripts/emptyplot.R')

## Get all file names:

COHA <- read_csv('COHA_summary.csv')

## Load modality norm data:

setwd('/Users/winterb/Research/senses_sensory_modalities/viberg/brandnew_analysis/data/')
adjs <- read_csv('lynott_connell_2009_adj_norms.csv')
nouns <- read_csv('lynott_connell_2013_noun_norms.csv')
verbs <- read_csv('winter_2016_verb_norms.csv')

## Get the most exclusive ones:

adjs_excl <- filter(adjs,
	ModalityExclusivity > quantile(ModalityExclusivity, 0.8))
nouns_excl <- filter(nouns,
	ModalityExclusivity > quantile(ModalityExclusivity, 0.8))
verbs_excl <- filter(verbs,
	ModalityExclusivity > quantile(ModalityExclusivity, 0.8))

## Combine different modality norm datasets:

xdata <- bind_rows(dplyr::select(adjs, -PropertyBritish),
	nouns, dplyr::select(verbs, -RandomSet, -N))
xdata_excl <- bind_rows(dplyr::select(adjs_excl, -PropertyBritish),
	nouns_excl, dplyr::select(verbs_excl, -RandomSet, -N))

## Add data source identifier:

xdata$POS <- c(rep('Adj', nrow(adjs)),
	rep('Noun', nrow(nouns)),
	rep('Verb', nrow(verbs)))
xdata_excl$POS <- c(rep('Adj', nrow(adjs_excl)),
	rep('Noun', nrow(nouns_excl)),
	rep('Verb', nrow(verbs_excl)))

## Rename modality column:

xdata <- rename(xdata,
	Modality = DominantModality)
xdata_excl <- rename(xdata_excl,
	Modality = DominantModality)

## Combine with COHA:

COHA_excl <- left_join(COHA, dplyr::select(xdata_excl, Word:Modality, POS))
COHA <- left_join(COHA, dplyr::select(xdata, Word:Modality, POS))

## Get rid of non-exclusives for the COHA_excl file:

COHA_excl <- filter(COHA_excl,
	!is.na(Modality))

## Add year total N:

COHA_excl_sum <- COHA_excl %>% group_by(Year) %>%
	summarize(Total = sum(n)) %>% right_join(COHA_excl)
COHA_sum <- COHA %>% group_by(Year) %>%
	summarize(Total = sum(n)) %>% right_join(COHA)

## Summarize for plotting:

COHA_plot <- COHA_sum %>%		# change _excl
	group_by(Modality, Year, Total) %>%
	summarize(n = sum(n)) %>%
	mutate(RelFreq = n / Total)

## Make a quick plot of this:

COHA_plot %>% ggplot(aes(x = Year, y = RelFreq, col = Modality)) +
	geom_line()		# + facet_wrap(~Register)

## This is where I did visual exploration of POS and register differences
## Except for haptic > visual for verbs because of 'get' etc. classified as touch
## No striking patterns, visual dominance all along


##------------------------------------------------------------------
## Model this with GAM:
##------------------------------------------------------------------

## Get rid of register info:

COHA_word <- COHA_sum %>%
	group_by(Year, Word, Modality, POS) %>%
	summarize(n = sum(n))
COHA_excl_word <- COHA_excl_sum %>%
	group_by(Year, Word, Modality, POS) %>%
	summarize(n = sum(n))

## Get rid of nouns and verbs for this analysis (they are problematic,
## but it also works with them):

COHA_word <- filter(COHA_word,
	POS == 'Adj')
COHA_excl_word <- filter(COHA_excl_word,
	POS == 'Adj')

## Get relative frequencies:

COHA_word <- COHA_word %>%
	group_by(Year) %>% summarize(N = sum(n)) %>%
	left_join(COHA_word) %>%
	mutate(RelFreq = n / N)
COHA_excl_word <- COHA_excl_word %>%
	group_by(Year) %>% summarize(N = sum(n)) %>%
	left_join(COHA_excl_word) %>%
	mutate(RelFreq = n / N)

## Plot a few individual lines for exploration:

## SWEET, PURPLE, SHINY, FRAGRANT
these_words <- c('fragrant', 'aromatic', 'pungent',
	'stinky', 'perfumed', 'reeking', 'scented', 'smelly')

filter(COHA_word, Word %in% these_words) %>%
	ggplot(aes(x = Year, y = RelFreq, col = Word)) +
	geom_line()

## Calculate slopes for each word, including verbs and nouns:

adjs$Slope <- NA
for (i in 1:nrow(adjs)) {
	this_df <- filter(COHA_word,
		Word == COHA_word[i, ]$Word)
	if (nrow(this_df) > 0) {
		adjs[i, ]$Slope <- coef(lm(RelFreq ~ Year, data = this_df))[2]
		}
	if (i %% 50 == 0) cat(str_c('Processing ... ', i, '\n'))
	}

## Check the ones that have increased/decreased the most:

arrange(adjs, Slope) %>% dplyr::select(Word, DominantModality, Slope)
arrange(adjs, desc(Slope)) %>% dplyr::select(Word, DominantModality, Slope)

## Get means per slope:

adjs %>% group_by(DominantModality) %>%
	summarize(Slope = mean(Slope)) %>%
	mutate(Slope = Slope * 1000) %>% 
	arrange(desc(Slope))

## Make a model of this:

summary(lm(Slope ~ DominantModality, data = adjs))
anova(lm(Slope ~ DominantModality, data = adjs))

## Make a plot of this:

adjs %>% ggplot(aes(x = DominantModality, y = Slope, fill = DominantModality)) +
	geom_boxplot()

## For GAM, make into factors:

COHA_word$Modality <- as.factor(COHA_word$Modality)
COHA_word$Word <- as.factor(COHA_word$Word)

## Fit a GAM:

# # xmdl <- bam(RelFreq ~ s(Year, by = Modality, k = 5) +
	# Modality +
	# s(Year, Word, bs = 'fs', k = 5, m = 1),
	# data = COHA_word)

# ## Fit a GAM without the interaction:

# xmdl.noint <- bam(RelFreq ~ s(Year, k = 10) +
	# Modality +
	# s(Year, Word, bs = 'fs', k = 5, m = 1),
	# data = COHA_word)

# ## Compare using likelihood ratio test:

# anova(xmdl.noint, xmdl, test = 'Chisq')

# ## Compare using information-theoretic criteria:

# AIC(xmdl.noint); AIC(xmdl)
# BIC(xmdl.noint); BIC(xmdl)
# library(qpcR)
# evidence(xmdl.noint, xmdl)


##------------------------------------------------------------------
## Make a plot:
##------------------------------------------------------------------

## Source empty plot function:

source('/Users/winterb/Research/senses_sensory_modalities/viberg/brandnew_analysis/scripts/emptyplot.R')

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

## Get averages and SDs:

xmeans <- aggregate(RelFreq ~ Year * Modality,
	FUN = mean, data = COHA_word)
xsds <- aggregate(RelFreq ~ Year * Modality,
	FUN = sd, data = COHA_word) %>% rename(RelFreqSD = RelFreq)
all_ns <- COHA_word %>% group_by(Modality, Year) %>%
	count() %>% ungroup()
xall <- as_tibble(bind_cols(xmeans,
	dplyr::select(xsds, RelFreqSD), dplyr::select(all_ns, nn)))

## Compute confidence intervals:

xall <- mutate(xall,
	SE = RelFreqSD / sqrt(nn),
	UB = RelFreq + 1.96 * SE,
	LB = RelFreq - 1.96 * SE)

## Make the plot:

quartz('', 11, 5)
par(mfrow = c(1, 2), mai = c(0.1, 0.1, 0.1, 0.3), omi = c(1, 1.5, 0.5, 1.5))
# Plot 1:
emptyplot(xlim = c(1800, 2000), ylim = c(0, 0.025))
axis(side = 1, at = seq(1800, 2000, 20),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.15)
mtext(side = 1, line = 3.5,
	text = 'Decade', font = 2, cex = 2.2)
mtext(side = 2, line = 4.8,
	text = 'Relative frequency', font = 2, cex = 1.8)
axis(side = 2, at = seq(0, 0.025, 0.005), las = 2,
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25)
with(filter(COHA_word, Word == 'sweet'),
	points(Year, RelFreq, col = mycols[4], lwd = 3, type = 'l'))
with(filter(COHA_word, Word == 'red'),
	points(Year, RelFreq, col = mycols[1], lwd = 3, type = 'l'))
with(filter(COHA_word, Word == 'fragrant'),
	points(Year, RelFreq, col = mycols[5], lwd = 3, type = 'l'))

rasterImage(s1,
	xleft = mean(seq(1810, 2000, 10)) - 10,
	xright = mean(seq(1810, 2000, 10)) + 10,
	ybottom = mean(filter(COHA_word, Word == 'red')$RelFreq) - 0.00138,
	ytop = mean(filter(COHA_word, Word == 'red')$RelFreq) + 0.00138)
text(x = mean(seq(1810, 2000, 10)) - 20,
	y = mean(filter(COHA_word, Word == 'red')$RelFreq) + 0.00138,
	col = mycols[1], font = 2, labels = '"red"', cex = 1.25)

rasterImage(s4,
	xleft = mean(seq(1810, 2000, 10)) - 10,
	xright = mean(seq(1810, 2000, 10)) + 10,
	ybottom = mean(filter(COHA_word, Word == 'sweet')$RelFreq) - 0.001328 - 0.002,
	ytop = mean(filter(COHA_word, Word == 'sweet')$RelFreq) + 0.001328 - 0.002)
text(x = mean(seq(1810, 2000, 10)) + 20,
	y = mean(filter(COHA_word, Word == 'sweet')$RelFreq),
	col = mycols[4], font = 2, labels = '"sweet"', cex = 1.25)

rasterImage(s5,
	xleft = mean(seq(1810, 2000, 10)) - 10,
	xright = mean(seq(1810, 2000, 10)) + 10,
	ybottom = mean(filter(COHA_word, Word == 'fragrant')$RelFreq) - 0.001328,
	ytop = mean(filter(COHA_word, Word == 'fragrant')$RelFreq) + 0.001328)
text(x = mean(seq(1810, 2000, 10)) + 20,
	y = mean(filter(COHA_word, Word == 'fragrant')$RelFreq) + 0.00138 + 0.0005,
	col = mycols[5], font = 2, labels = '"fragrant"', cex = 1.25)

# Plot 2:
emptyplot(xlim = c(1800, 2000), ylim = c(0, 0.005))
axis(side = 1, at = seq(1800, 2000, 20),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.15)
mtext(side = 1, line = 3.5,
	text = 'Decade', font = 2, cex = 2.2)
for (i in seq_along(mycols)){
	this_mod <- mymods[i]
	this_df <- filter(xall, Modality == this_mod)
	# if (i == 1){
		# with(this_df,
			# polygon(c(Year, rev(Year)), c(UB, rev(LB)),
				# col = rgb(0, 0, 0, 0.5), border = NA))
		# }
	with(this_df,
		points(Year, RelFreq, col = mycols[i], lwd = 3, type = 'l'))
	}

rasterImage(s1,
	xleft = mean(seq(1810, 2000, 10)) - 10,
	xright = mean(seq(1810, 2000, 10)) + 10,
	ybottom = mean(filter(xall, Modality == 'Visual')$RelFreq) - 0.00026 - 0.0002,
	ytop = mean(filter(xall, Modality == 'Visual')$RelFreq) + 0.00026 - 0.0002)

rasterImage(s3,
	xleft = mean(seq(1810, 2000, 10)) - 10,
	xright = mean(seq(1810, 2000, 10)) + 10,
	ybottom = mean(filter(xall, Modality == 'Haptic')$RelFreq) - 0.00026 - 0.0002,
	ytop = mean(filter(xall, Modality == 'Haptic')$RelFreq) + 0.00026 - 0.0002)

axis(side = 4, at = seq(0, 0.005, 0.001), las = 2,
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25)


##------------------------------------------------------------------
## A less crowded plot:
##------------------------------------------------------------------

## Make the plot:

quartz('', 9, 6)
par(mai = c(1.5, 1.5, 0.5, 1.8))
emptyplot(xlim = c(1800, 2000), ylim = c(0, 0.008))
axis(side = 1, at = seq(1800, 2000, 20),
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.15)
mtext(side = 1, line = 3.5,
	text = 'Decade', font = 2, cex = 2.2)
axis(side = 4, at = seq(0, 0.008, 0.002), las = 2,
	lwd.ticks = 2, lwd = 2, font = 2, cex.axis = 1.25)
mtext(side = 4, line = 4.8,
	text = 'Relative frequency', font = 2, cex = 1.8)
for (i in seq_along(mycols)){
	this_mod <- mymods[i]
	this_df <- filter(xall, Modality == this_mod)
	with(this_df,
		points(Year, RelFreq, col = mycols[i], lwd = 5, type = 'l'))

	rasterImage(image = get(sense_names[i]),
		xleft = 1800 - 7 - 1, xright = 1800 + 7 - 1,
		ybottom = filter(xall, Year == 1810,
			Modality == this_mod)$RelFreq - 0.00038 - 0.0002 + 0.00015,
		ytop = filter(xall, Year == 1810,
			Modality == this_mod)$RelFreq + 0.00038 - 0.0002 + 0.00015,
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

COHA_word_new_excl <- filter(COHA_word,
	Word %in% pull(new_excl, Word))

## Get averages and SDs:

COHA_word_new_excl_sum <- COHA_word_new_excl %>%
	group_by(Year, Modality) %>% summarize(RelFreq = mean(RelFreq))
COHA_word_new_excl_sum %>% ggplot(aes(x = Year, y = RelFreq, col = Modality)) +
	geom_line()

