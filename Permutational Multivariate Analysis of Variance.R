
# PERMUTATIONAL MULTIVARIATE ANALYSIS
# OF VARIANCE (PERMANOVA)

# Load packages

if (!require (rgl)) {
  install.packages("rgl")
}
library (rgl)
if (!require (vegan)) {
  install.packages("vegan")
}
library (vegan)

# Visualizing how PERMANOVA works

set.seed(1)
x1 <- seq(40, 50, by = 0.5) + rnorm(21, 10, 5)
y1 <- x1 + rnorm(21, 10, 5)
z1 <- y1 + rnorm(21, 10, 5)
x2 <- seq(40, 50, by = 0.5) + rnorm(21, 10, 5)
y2 <- x2 - rnorm(21, 8, 5)
z2 <- y2 - rnorm(21, 10, 5)
x3 <- seq(60, 70, by = 0.5) + rnorm(21, 12, 5)
y3 <- x3 + rnorm(21, 13, 5)
z3 <- y3 + rnorm(21, 15, 5)
group <- rep(1:3, each = 21)
x <- c(x1, x2, x3)
y <- c(y1, y2, y3)
z <- c(z1, z2, z3)
df1 <- data.frame(group, x, y, z)
centroids1 <- data.frame(group = 1:3, x = c(mean(x1), mean(x2), mean(x3)), y = c(mean(y1), mean(y2), mean(y3)), z = c(mean(z1), mean(z2), mean(z3)))

# These groups appear to be quite different -
# points within groups are clustered around
# group centroids, and centroids are separated
# by quite a bit of space

open3d()

# Expand this new window before
# running the next two lines of code

plot3d(df1$x, df1$y, df1$z, col = df1$group, xlab = "", ylab = "", zlab = "")
pch3d(centroids1$x, centroids1$y, centroids1$z, color = centroids1$group, pch = 10)

# These groups don't appear to be different
# because the within-group variability is
# much greater then the between-group
# variability

group <- rep(1:3, 21)
x <- c(x1, x2, x3)
y <- c(y1, y2, y3)
z <- c(z1, z2, z3)
df2 <- data.frame(group, x, y, z)
x_centers <- aggregate(df2$x, by = list(df2$group), mean)
colnames(x_centers) <- c("group", "x")
y_centers <- aggregate(df2$y, by = list(df2$group), mean)
colnames(y_centers) <- c("group", "y")
z_centers <- aggregate(df2$z, by = list(df2$group), mean)
colnames(z_centers) <- c("group", "z")
centroids2 <- merge(merge(x_centers, y_centers, by = "group"), z_centers, by = "group")
open3d()

# Expand this new window before
# running the next two lines of code

plot3d(df2$x, df2$y, df2$z, col = df2$group, xlab = "", ylab = "", zlab = "")
pch3d(centroids2$x, centroids2$y, centroids2$z, color = centroids2$group, pch = 10, cex = 15)

# How PERMANOVA works:

# 1. We want to know if between-group
# variability is greater than within-
# group variability. If it is, then we
# can be confident that groups are
# different.

# 2. We accomplish this by using
# permutations of our data. This is done
# randomly.

# What's permuted?

# Here is our data again:

View(df1)

# Observations within groups are represented
# by rows, as you can see.

# Therefore, rows are permuted so that
# observations are shuffled between groups
# randomly. Each time a random permutation
# is done, this comparison of between-
# group variability to within-group
# variability is performed again.

# When the data are permuted, the points in
# our multidimensional space don't change.
# The only thing that changes are the colors
# of the points (or, in other words, the
# groups that the points belong to).

# The pseudo-F statistic is the ratio between
# the between-group variability to the within-
# group variability. If the ratio is large,
# between-group variability is bigger than
# within-group variability, and the data are
# like the example from the first plot. If the
# ratio is small, within-group variability is
# bigger than between-group variability, and
# the data look like the example from the
# second plot.

# After you've done 999 (or however many
# you want to do) permutations, PERMANOVA
# looks to see how many different permutations
# give you a pseudo-F statistic greater than
# the pseudo-F statistic from your original,
# un-permuted data. If there are only a small
# number of pseudo-F statistics from permuted
# data smaller than the pseudo-F statistic
# from your original, unpermuted data, then
# PERMANOVA says that your groups are different.
# If there are a lot of pseudo-F statistics
# from permuted data that are bigger than your
# original, un-permuted data, then PERMANOVA
# says that your groups aren't different.

# Ideally, we'd calculate a pseudo-F
# statistic for every single permutation of
# our data to use in the calculation of our
# final p value. In practice, though, this
# is unrealistic, since the number of
# permutations of our data are equal to the
# number of rows in our data frame factorial.
# For a data frame with 5 rows, there are
# 120 possible permutations, but for bigger
# data frames, things quickly get out of
# hand - for a data frame with a mere 12
# rows, there are 479001600 (i.e., 12
# factorial) possible permutations.
# Therefore, we usually have R perform 999
# permutations, selected at random.

# P(n, r) = n! / (n - r)!

# If n = r, then

# P(n, r) = n!

factorial(5)
factorial(12)

# Let's perform a PERMANOVA on some
# made-up data.

Replication <- rep(1:4, each = 15)
Plot <- rep(1:15, 4)
Nitrogen_Rate <- rep(seq(0, 200, by = 50), each = 3)
Soil_Type <- c("Clayey Loam", "Loamy Clay", "Sandy Loam")
Data_Frame <- data.frame(Replication, Plot, Nitrogen_Rate, Soil_Type)
Bacteria <- rnorm(60, 20, 2)
Bacteria <- ifelse(Bacteria < 0, 0, Bacteria)
Fungi <- rnorm(60, 15, 3)
Fungi <- ifelse(Fungi < 0, 0, Fungi)
Nematodes <- rnorm(60, 10, 4)
Nematodes <- ifelse(Nematodes < 0, 0, Nematodes)
Species_Matrix <- as.matrix(data.frame(Bacteria, Fungi, Nematodes))
All_Data <- cbind(Data_Frame, Species_Matrix)

# Here is some of our made-up data:

head(All_Data)

# Let's perform a PERMANOVA on these
# data.

adonis(Species_Matrix ~ Data_Frame$Nitrogen_Rate * Data_Frame$Soil_Type, distance = "bray", permutations = 999)

# The above experiment was a
# completely randomized design, which
# is the simplest of all experimental
# design types.

# PERMANOVAs can handle all sorts of
# hierarchical data structures, but to
# my knowledge, the functions people
# have written to perform PERMANOVA in
# R can't really handle data structures
# that have more than one level of
# nesting. Let's at least do an example
# of when there is one level of nesting.

# By the way, 'nesting' is a term used
# to describe when experimental units
# are, well, nested. For example, you
# may be making measurements on several
# individual ears of corn on the same
# corn plant, and you may be looking at
# several corn plants in a given field,
# and you may have several fields in
# your study. In this example, corn plant
# is nested within field, and ear of corn
# is nested within corn plant. Since each
# individual corn plant is not replicated
# in each field - specific, unique corn
# plants only appear in the field they
# were planted in - corn plant is nested
# within field. The same logic can be
# used to explain why ear of corn is
# nested within corn plant. This is an
# example of subsampling, and in order to
# calculate F statistics correctly, you
# would average corn plant values and use
# those averages as the error term when
# looking for differences between fields,
# and you would average individual ear of
# corn values and use those averages as
# the error term when looking for
# differences between corn plants.

# Anyway, here's an example of a simple
# nested design using `adonis()` (first
# we'll slightly modify out made-up
# data so that it's blocked and not
# merely replicated):

Block <- rep(1:4, each = 15)
Data_Frame <- data.frame(Block, Plot, Nitrogen_Rate, Soil_Type)
adonis(Species_Matrix ~ Data_Frame$Nitrogen_Rate * Data_Frame$Soil_Type, distance = "bray", permutations = 999, strata = Data_Frame$Block)

# The `strata` argument is used to
# constrain how permutations are
# carried out. In this case, rows
# are only permuted within blocks,
# and never between blocks. Since
# we want to partition some of the
# overall variance into the block
# effect, this technique is valid.
