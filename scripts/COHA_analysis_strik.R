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

## Get all file names:

COHA <- read_csv('COHA_summary_strik.csv')

## Load modality norm data:

setwd('/Users/winterb/Research/senses_sensory_modalities/viberg/brandnew_analysis/data/')
xdata <- read_csv('strik_lievers_2015.csv')

## Get rid of instruments:

xdata <- xdata %>% filter(Instrument == 'no')

## Combine with COHA and get rid of instruments there:

COHA <- left_join(COHA,
	dplyr::select(xdata, Word, Modality, POS)) %>%
	filter(!is.na(Modality))

## Add year total N:

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

## Get relative frequencies:

COHA_word <- COHA_word %>%
	group_by(Year) %>% summarize(N = sum(n)) %>%
	left_join(COHA_word) %>%
	mutate(RelFreq = n / N)

## Calculate slopes for each word, including verbs and nouns:

xdata$Slope <- NA
for (i in 1:nrow(xdata)) {
	this_df <- filter(COHA_word,
		Word == COHA_word[i, ]$Word)
	if (nrow(this_df) > 0) {
		xdata[i, ]$Slope <- coef(lm(RelFreq ~ Year, data = this_df))[2]
		}
	if (i %% 50 == 0) cat(str_c('Processing ... ', i, '\n'))
	}

## Check the ones that have increased/decreased the most:

arrange(xdata, Slope) %>% dplyr::select(Word, Modality, Slope)
arrange(xdata, desc(Slope)) %>% dplyr::select(Word, Modality, Slope)

## Get means per slope:

xdata %>% group_by(Modality) %>%
	summarize(Slope = mean(Slope, na.rm = T)) %>%
	mutate(Slope = Slope * 1000) %>% 
	arrange(desc(Slope))

## Make a model of this:

summary(lm(Slope ~ Modality, data = xdata))
anova(lm(Slope ~ Modality, data = xdata))

## Make a plot of this:

xdata %>% ggplot(aes(x = Modality, y = Slope, fill = Modality)) +
	geom_boxplot()

## For GAM, make into factors:

COHA_word$Modality <- as.factor(COHA_word$Modality)
COHA_word$Word <- as.factor(COHA_word$Word)

## Center the year variable:

COHA_word <- mutate(COHA_word,
	Year_c = Year - mean(Year))

## Fit a GAM:

xmdl <- bam(RelFreq ~ s(Year_c, by = Modality, k = 5) +
	Modality +
	Year_c +
	s(Year_c, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_word)

## Fit a GAM without the interaction:

xmdl.noint <- bam(RelFreq ~ s(Year_c, k = 5) +
	Modality +
	Year_c +
	s(Year_c, Word, bs = 'fs', k = 5, m = 1),
	data = COHA_word)

## Compare using likelihood ratio test:

anova(xmdl.noint, xmdl, test = 'Chisq')

## Compare using information-theoretic criteria:

AIC(xmdl.noint); AIC(xmdl)
BIC(xmdl.noint); BIC(xmdl)
library(qpcR)
evidence(xmdl.noint, xmdl)



