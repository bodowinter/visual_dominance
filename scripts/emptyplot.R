## Bodo Winter
## July 22, 2017
## Empty plot function

## Function for empty plot:

emptyplot <- function(xlim, ylim, ...) {
	plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n',
	xlab = '', ylab = '',
	xlim = xlim, ylim = ylim, bty = 'n', ...)
	}