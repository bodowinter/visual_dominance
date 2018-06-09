## Bodo Winter
## July 23, 2017
## Processing register data in COCA

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(MASS)
library(stringr)
library(png)
library(tidyverse)

## Load data:

setwd('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/')
xdata <- read_csv('COCA_registers.csv')

## Names of files:

regs <- c('spoken', 'academic', 'news', 'magazine', 'fiction')
regs <- sort(regs)

## Compute log frequencies:

myM <- as_tibble(apply(xdata[, regs], 2,
	FUN = function(x) log10(x + 1)))
colnames(myM) <- str_c('Log', str_to_title(regs))
xdata <- bind_cols(xdata, myM)



##------------------------------------------------------------------
## Get a subset of highly indicative adjectives:
##------------------------------------------------------------------

## Filter POS:

adjs <- filter(xdata,
	POS == 'Adj')

## For each modality, find the 10% most exclusive words:

adjs_excl <- c()
for (i in 1:5) {
	this_mod <- unique(adjs$DominantModality)[i]
	this_df <- filter(adjs, DominantModality == this_mod)
	
	## Take 10 topmost exclusives:
	
	this_df <- arrange(this_df, desc(ModalityExclusivity))[1:10, ]
	
	## Append to main dataset:
	
	adjs_excl <- bind_rows(adjs_excl, this_df)
	}



##------------------------------------------------------------------
## Stacked bar plot:
##------------------------------------------------------------------

## Make a table:

print(xtab <- adjs_excl %>% group_by(DominantModality) %>%
	summarize(spoken = sum(spoken),
		academic = sum(academic),
		news = sum(news),
		magazine = sum(magazine),
		fiction = sum(fiction)))

## Make different registers to rows:

xtab <- xtab %>%
	gather(Register, Frequency, -DominantModality) %>%
	spread(DominantModality, Frequency)

## Source empty plot function:

source('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/scripts/emptyplot.R')
		
## Make a barplot of this for

xtab <- as.matrix(xtab[, -1])
xtab <- xtab[, c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')]
xprops <- prop.table(xtab, 1)

## Cumulative proportion:

xprops.cum <- t(apply(xprops, MARGIN = 1, cumsum))
xprops.cum <- cbind(rep(0, 5), xprops.cum[, ])

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
emptyplot(xlim = c(-0.3, 1), ylim = c(0, nrow(xtab) + 0.5))

## Plot axis:

axis(side = 1, at = seq(0, 1, 0.25),
	lwd = 2, lwd.ticks = 2, cex.axis = 1.25, font =2)
text(side = 1, x = 0.5, y = -1.2,
	labels = 'Proportion', font = 2,
	cex = 2.2, xpd = NA)

## Plot senses:

# text(xpd = NA,
	# x = -0.15, y = 5.45, font = 2, cex = 2,
	# labels = 'Order:')
for (i in 1:5) {
		rasterImage(get(sense_names[i]),
				xleft = ((1:5 / 10) - 0.1)[i] - 0.034,
				xright = ((1:5 / 10) - 0.1)[i] + 0.034,
				ybottom = 5.2, ytop = 5.7, xpd = NA)				
	}

## Loop through rectangle and plot:

for (i in 1:5) {
	text(y = i - 0.5, x = -0.02,
		regs[6 - i], font = 2, cex = 1.5, adj = 1)
	
	for (j in 1:5) {
		rect(ybottom = 5 - i + yfac, ytop = 5 - i + 1 - yfac,
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

## Make separate negative binomial models per register:

xmdl.academic <- glm.nb(academic ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata)
xmdl.spoken <- glm.nb(spoken ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata)
xmdl.magazine <- glm.nb(magazine ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata)
xmdl.fiction <- glm.nb(fiction ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata)
xmdl.news <- glm.nb(news ~ Vis_z + Hap_z + Aud_z + Gus_z + Olf_z,
	data = xdata)

## Inspect:

summary(xmdl.academic)
summary(xmdl.spoken)
summary(xmdl.news)
summary(xmdl.magazine)
summary(xmdl.fiction)

