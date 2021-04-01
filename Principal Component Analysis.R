
# ORDINATIONS: PRINCIPAL COMPONENT ANALYSIS

# Load packages

if (!require(rgl)) {
  install.packages("rgl")
}
library(rgl)

# Visualizing how ordinations work using
# an example

# Principal Component Analysis (PCA):
# The simplest of all ordinations

# Note: PCA assumes that relationships
# between variables are linear, and
# therefore it is almost never
# appropriate for ecological data!
# (I'm just using PCA to explain how
# ordination works.)

set.seed(1)
x <- 1:100 + rnorm(100, 5, 5)
y <- x + rnorm(100, 10, 20)
z <- y + rnorm(100, 1, 10)
M <- cbind(x, y, z)
pca <- prcomp(M, center = T, scale. = F)

# Here's our data plotted on a 3-
# dimensional graph. Each dimension
# represents one of the measured
# variables. Perhaps 'x' represents
# the amount of soil carbon, 'y'
# represents the amount of soil
# nitrogen, and 'z' represents the
# amount of soil phosphorus (I just
# made this up). Each point is a plot
# where you took these measurements
# from.

open3d()
plot3d(M, xlab = "", ylab = "", zlab = "")
title3d(xlab = "Carbon", ylab = "Nitrogen", zlab = "Phosphorus")

# Our first job is to look for the
# axis where there is the most
# variability in our data. Let's rotate
# this graph until we find it.

# We found it. Now, let's draw a line
# through it.

PC1 <- rbind(pca$center + min(pca$x[, 1]) * pca$rotation[, 1], pca$center + max(pca$x[, 1]) * pca$rotation[, 1])
colnames(PC1) = c("x", "y", "z")
segments3d(PC1, col = 1 , lwd = 2)

# Our next job is to find the axis where
# there is the second-most variability in
# our data. We have a constraint now,
# though: this second axis MUST be
# perpendicular ('orthoganal') to the first
# axis.

# Let's rotate the graph again to find this
# second axis.

# We found it - now, let's draw a different
# line through this axis.

PC2 <- rbind(pca$center + min(pca$x[, 2]) * pca$rotation[, 2], pca$center + max(pca$x[, 2]) * pca$rotation[, 2])
colnames(PC2) = c("x", "y", "z")
segments3d(PC2, col = 2 , lwd = 2)

# Since this is a 3-dimensional graph, we
# should be able to find one more principal
# component axis. There will be the same
# number of princiupal component axes that
# there are dimensions.

PC3 <- rbind(pca$center + min(pca$x[, 3]) * pca$rotation[, 3], pca$center + max(pca$x[, 3]) * pca$rotation[, 3])
colnames(PC3) = c("x", "y", "z")
segments3d(PC3, col = 3 , lwd = 2)

# Now that we've found all of the principal
# component axes, we are going to make a
# new plot. This new plot will plot our data
# on new axes - instead of using our original
# axes, which represented soil elements, we
# are going to use the first two principal
# component axes as our x and y axes.

# This is the plot we get if we simply take
# a snapshot of what our 3-D graph looked
# like when we rotated it such that PCA axis
# 1 was the horizontal axis and PCA axis 2
# was the vertical axis:

par(mar = c(5, 4, 4, 2) + 0.1)
plot(pca$x[, 1],pca$x[, 2], type = 'n', axes = F, xlab = "", ylab = "")
axis_limits <- c(min(par('usr')[c(1, 3)]), max(par('usr')[c(2, 4)]))
plot(pca$x[, 1], pca$x[, 2], xlim = axis_limits, ylim = axis_limits, xlab = "Principal Component Axis 1", ylab = "Principal Component Axis 2", main = "Principal Component Analysis")

# This is the plot we get if we re-scale
# our axes:

plot(pca$x[, 1],pca$x[, 2], xlab = "Principal Component Axis 1", ylab = "Principal Component Axis 2", main = "Principal Component Analysis\nWith Rescaled Axes")

# Take note - now, it seems like both PCA
# axes have similar amounts of variability,
# but we know that this is NOT the case
# based on the previous plot! We can also
# tell by looking at the ranges of values
# on each axis.

# We can also re-scale PCA axes so that
# instead of using absolute distances,
# we can see how many standard deviations
# apart data points are:

pca <- prcomp(M, center = T, scale. = T)
plot(pca$x[, 1], pca$x[, 2], xlab = "Principal Component Axis 1", ylab = "Principal Component Axis 2", main = "Principal Component Analysis")

# Important: note that the relative importance
# of axis 1 and axis 2 that you could clearly
# see from the first plot is less easy-to-see
# in the second plot. Keep this in mind when
# you're looking at the correlations between
# species scores or environmental variables
# with ordination axes - things that are highly-
# correlated with axes that don't explain a lot
# of variation are not that important, even if
# the correlations are strong, because the axis
# doesn't explain a lot of variability in the
# first place!

# Final note: this example was 3-dimensional,
# but the same exact principles apply for
# higher-dimensional data, too. It's difficult
# to visualize higher-dimensional data, so 2-
# and 3-dimensional examples are best to start
# with.
