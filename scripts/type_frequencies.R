## Bodo Winter
## July 23, 2017
## Processing type counts per POS

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(tidyverse)
library(stringr)
library(png)

## Load data:

setwd('/Volumes/Macintosh HD/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/data/')
adjs <- read_csv('lynott_connell_2009_adj_norms.csv')
nouns <- read_csv('lynott_connell_2013_noun_norms.csv')
verbs <- read_csv('winter_2016_verb_norms.csv')

## Get the most exclusive ones:

# adjs_excl <- filter(adjs,
	# ModalityExclusivity > quantile(ModalityExclusivity, 0.8))
# nouns_excl <- filter(nouns,
	# ModalityExclusivity > quantile(ModalityExclusivity, 0.8))
# verbs_excl <- filter(verbs,
	# ModalityExclusivity > quantile(ModalityExclusivity, 0.8))

## Combine:

xdata <- bind_rows(dplyr::select(adjs, -PropertyBritish),
	nouns, dplyr::select(verbs, -RandomSet, -N))
# xdata_excl <- bind_rows(dplyr::select(adjs_excl, -PropertyBritish),
	# nouns_excl, dplyr::select(verbs_excl, -RandomSet, -N))

## Add POS information:

xdata$POS <- c(rep('Adj', nrow(adjs)),
	rep('Noun', nrow(nouns)),
	rep('Verb', nrow(verbs)))
xdata_excl$POS <- c(rep('Adj', nrow(adjs_excl)),
	rep('Noun', nrow(nouns_excl)),
	rep('Verb', nrow(verbs_excl)))



##------------------------------------------------------------------
## Look at exclusives:
##------------------------------------------------------------------

## Make a table:

print(xtab <- xdata %>% group_by(DominantModality) %>%
	count(POS))

## Make different registers to rows:

xtab <- xtab %>% spread(DominantModality, n, fill = 0)

## Re-order:

xtab <- xtab[, c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')]
# xtab <- xtab[, c('Visual', 'Auditory', 'Haptic', 'Olfactory')]

## Get proportions:

prop.table(as.matrix(xtab), margin = 1)



##------------------------------------------------------------------
## Stacked bar plot:
##------------------------------------------------------------------

## Make a table:

print(xtab <- xdata %>% group_by(DominantModality) %>%
	count(POS))		# switch to xdata_excl if necessary

## Make different registers to rows:

xtab <- xtab %>% spread(DominantModality, n, fill = 0)

## Re-order:

if (ncol(xtab) == 6) {
	xtab <- xtab[, c('Visual', 'Auditory', 'Haptic', 'Gustatory', 'Olfactory')]
	}
# if (ncol(xtab) == 5) {
	# xtab <- xtab[ , c('Visual', 'Auditory', 'Haptic', 'Olfactory')]
	# }
print(xtab)

## Perform a Chi-Square test on this:

print(xchisq <- chisq.test(xtab[1, ], simulate.p.value = F))	# use = T for small tables
round(xchisq$stdres, 2)

print(xchisq <- chisq.test(xtab[2, ], simulate.p.value = F))	# use = T for small tables
round(xchisq$stdres, 2)

print(xchisq <- chisq.test(xtab[3, ], simulate.p.value = F))	# use = T for small tables
round(xchisq$stdres, 2)

## Source empty plot function:

source('/Users/winterb/Research/senses_sensory_modalities/visual_dominance/analysis/scripts/emptyplot.R')
		
## Make a table of this for barplotting:

xtab <- as.matrix(xtab)
xprops <- prop.table(xtab, 1)

## Cumulative proportion:

xprops.cum <- t(apply(xprops, MARGIN = 1, cumsum))
xprops.cum <- cbind(rep(0, nrow(xtab)), xprops.cum)

## Define colors:

mycols <- c('#f37058', '#efbe1b', '#f79038', '#425fac', '#30b77d')
if (ncol(xtab) < 5) {
	mycols <- mycols[-4]
	}

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
par(mai = c(1, 0, 0.75, 0), omi = rep(0, 4))
emptyplot(xlim = c(-0.3, 1), ylim = c(0, nrow(xtab) + 0.5))

## Plot axis:

axis(side = 1, at = seq(0, 1, 0.25),
	lwd = 2, lwd.ticks = 2, cex.axis = 1.25, font =2)
text(x = 0.5, y = -0.8,
	labels = 'Proportion', font = 2,
	cex = 2.2, xpd = NA)

## Plot senses:

# text(xpd = NA,
	# x = -0.15, y = 3.45, font = 2, cex = 2,
	# labels = 'Order:')
for (i in 1:5) {
		rasterImage(get(sense_names[i]),
				xleft = ((1:5 / 10) - 0.1)[i] - 0.034,
				xright = ((1:5 / 10) - 0.1)[i] + 0.034,
				ybottom = 3.21, ytop = 3.55, xpd = NA)				
	}

## Loop through rectangle and plot:

for (i in 1:nrow(xtab)) {
	text(y = i - 0.5, x = -0.02,
		c('Verb', 'Noun', 'Adj')[i], font = 2, cex = 1.5, adj = 1)
	text(y = i - 0.75, x = -0.02,
		c('(N = 300)', '(N = 400)', '(N = 423)')[i], font = 2, cex = 1.05, adj = 1)
	
	for (j in 1:ncol(xtab)) {
		rect(ybottom = nrow(xtab) - i + yfac, ytop = nrow(xtab) - i + 1 - yfac,
				xleft = xprops.cum[i, j], xright = xprops.cum[i, j + 1],
				col = mycols[j])
		}
	}

