
DrawCanvas <- function(sample, title, x_label, x_min, x_max){
# Draws a blank canvas for a histogram or density plot.
#
# Args:
#   sample: a numeric vector
#   title: a character string
#   x_label: a character string
#   x_min: a numeric value
#   x_max: a numeric value
#
# Returns:
#   A blank canvas for a histogram or density plot.
#
# Raises:
#   An error if sample is not a numeric vector, if title is
# not a character string, if x_label is not a character string,
# if x_min is not a numeric value, or if x_max is not a numeric value.

    if(!is.numeric(sample))
        stop("sample must be a numeric vector.")
    if(!is.character(title))
        stop("title must be a character string.")
    if(!is.character(x_label))
        stop("x_label must be a character string.")
    if(!is.numeric(x_min))
        stop("x_min must be a numeric value.")
    if(!is.numeric(x_max))
        stop("x_max must be a numeric value.")
    if (missing(x_min)) { x_min <- min(sample) - 3}
    if (missing(x_max)) { x_max <- max(sample) + 3}
    plot(NULL, xlim = c(x_min, x_max),ylim = c(0,max(table(sample)) * 2.3),
    xlab = x_label, ylab = 'Frequency', main = title)
}

PlotHistogram <- function(sample, width, title='Histogram',
                 x_label = 'Education',x_min,x_max){
# Draws a histogram of a numeric vector with a specified bin width.
# Also draws a red vertical line for each observation.
#
# Args:
#   sample: a numeric vector
#   width: a numeric value
#   title: a character string
#   x_label: a character string
#   x_min: a numeric value
#   x_max: a numeric value
#
# Returns:
#   A histogram of a numeric vector.
#
# Raises:
#   An error if sample is not a numeric vector,
# if width is not a numeric value,
# if title is not a character string,
# if x_label is not a character string,
# if x_min is not a numeric value,
# or if x_max is not a numeric value.

    if (!is.numeric(sample))
        stop("sample must be a numeric vector.")
    if (!is.numeric(width))
        stop("width must be a numeric value.")
    if (!is.character(title))
        stop("title must be a character string.")
    if (!is.character(x_label))
        stop("x_label must be a character string.")
    if (!is.numeric(x_min))
        stop("x_min must be a numeric value.")
    if (!is.numeric(x_max))
        stop("x_max must be a numeric value.")
    if (missing(x_min)) { x_min <- min(sample) - 3}
    if (missing(x_max)) { x_max <- max(sample) + 3}
    DrawCanvas(sample, title, x_label, x_min, x_max)
    bins <- seq(min(sample)-2, max(sample)+2, by = width) # define bins
    for (i in 2:length(bins)){ # loop over bins
        x <- bins[i-1]
        y <- bins[i]
        count <- sum(sample >= x & sample < y) # count number of obs. in bin
        rect(x, 0, y, count, col = 'lightblue') # draw rectangle
        ss = sample[sample >= x & sample < y] # get observations in bin
        ns = length(ss)
        if (ns >=1){
            for (j in 1:ns){ # loop over observations in bin
                segments(x0=ss[j],y0=0,x1=ss[j],y1=0.2*(sum(ss==ss[j])),
                col='red',lty=1,lwd=5) # draw vertical line
            }
        }
    }
}

EstimateCrudeDensity <- function(data, width){
# Computes crude binomial density estimates for a numeric vector.
#
# Args:
#   data: a numeric vector
#   width: a numeric value
#
# Returns:
#   A matrix with two columns: the first column contains
# the bin centers and the second column contains the binomial density estimates.
#
# Raises:
#   An error if data is not a numeric vector or if width is not a numeric value.

    if (!is.numeric(data))
        stop("data must be a numeric vector.")
    if (!is.numeric(width))
        stop("width must be a numeric value.")
    h <- 1/(length(data) * width)
    # define bin centers
    x <- seq(min(data) - width / 2, max(data) + width / 2, by = width / 2)
    d <- rep(0,length(x))
    for (i in 1:length(x)){ # loop over bin centers
        # compute binomial density estimate
        d[i] <- sum(data > x[i] - width / 2 & data <= x[i] + width / 2) * h
    }
    return(cbind(x,d))
}


LastRowWithSameDensity <- function(cbd, i){
# Computes the index of the last row in a matrix with the same
#density value as the i-th row.
#
# Args:
#   cbd: a matrix with two columns, where the first column contains
# the bin centers and the second column contains the binomial density estimates.
#   i: an integer value representing the index of the current row in the matrix.
#
# Returns:
#   An integer value representing the index of the last row in the matrix
# with the same density value as the i-th row.
#
# Raises:
#   An error if cbd is not a matrix or if i is not an integer value.

    if (!is.matrix(cbd))
        stop("cbd must be a matrix.")
    if (!is.integer(i))
        stop("i must be an integer value.")
    j <- i
    while (cbd[j, 2] == cbd[i, 2]){
        j <- j + 1
    }
    return(j)
}

PlotCrudeDensity <- function(cbd,title = 'Density plot',x_label,x_min,x_max){
# Plots a density plot with a step function.
#
# Args:
#   cbd: a matrix with two columns, where the first column contains the bin centers and the second column contains the binomial density estimates. Output of crude_binomial_density_estimates.
#   title: a character string
    if (missing(mn)) { mn <- min(cbd[, 1]) - 2}
    if (missing(mx)) { mx <- min(cbd[, 1]) + 2}
    if (missing(lx)) { lx <- 'Variable'}
    plot(NULL, xlim = c(mn, mx), ylim = c(0, max(cbd[, 2]) * 1.25),
    xlab = x_label, ylab = 'Density', main = title)
    i <- 1
    while (i < nrow(cbd)) {
        ta <- cbd[i,1]
        th <- cbd[i,2]
        k = LastRowWithSameDensity(cbd,i)
        tb <- cbd[k,1]
        # horizontal segment
        segments(x0 = ta, y0 = th, x1 = tb, y1 = th, col = 'cornflowerblue', lty = 1, lwd = 3)
        # vertical segments
        segments(x0 = tb, y0 = th, x1 = tb, y1 = cbd[k,2], col = 'cornflowerblue', lty = 1, lwd = 3)
        i <- k
    }
}
