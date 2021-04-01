
# DISTANCE MEASURES

# Load packages

if (!require(rgl)) {
  install.packages("rgl")
}
library(rgl)

# Euclidean Distance

# Euclidean distance is the absolute
# distance and it's calculated using
# the Pythagorean theorem. For 3-
# dimensional data, the formula is
# an extension of the 2-dimensional
# formula:

# Distance = sqrt((x ^ 2) + (y ^ 2) + (z ^ 2))

x <- c(2, 2, 4)
y <- c(3, 1, 2)
z <- c(2, 3, 4)
plot3d(0, xlim = c(0, 5), ylim = c(0, 5), zlim = c(0, 5)); title3d(main = expression(bold("Euclidean Distances"))); pch3d(x, y, z, pch = 16, col = "red"); lines3d(x[c(1:length(x), 1)], y[c(1:length(y), 1)], z[c(1:length(z), 1)]); text3d(x, y, z, paste("Point", 1:3)); distances <- round(sqrt((x ^ 2) + (y ^ 2) + (z ^ 2)), 2); legend3d("bottom", title = expression(bold("Distances")), legend = c(paste(c("Point 1 to Point 2:", "Point 1 to Point 3:", "Point 2 to Point 3:"), as.character(distances))), cex = 1.75)

# Euclidean distances are often not
# appropriate for species abundance
# data because all zeros aren't
# created equal.

# Here's why I make that claim:

Plant_1_Abundance <- rnorm(20, 100, 15)
Plant_2_Abundance <- rnorm(20, 300, 50)
Plant_3_Abundance <- rnorm(20, 150, 15)
x_limits <- c(min(c(min(density(Plant_1_Abundance)$x), min(density(Plant_2_Abundance)$x), min(density(Plant_3_Abundance)$x))), max(c(max(density(Plant_1_Abundance)$x), max(density(Plant_2_Abundance)$x), max(density(Plant_3_Abundance)$x))))
y_limits <- c(0, max(c(max(density(Plant_1_Abundance)$y), max(density(Plant_2_Abundance)$y), max(density(Plant_3_Abundance)$y))))
par(mar = c(10, 4, 4, 2))
plot(0, type = "n", xlim = x_limits, ylim = y_limits, xlab = "Elevation (m)", ylab = "Frequency", main = "Frequencies of Three Plant Species\nAcross an Elevation Gradient")
lines(density(Plant_1_Abundance)$x, density(Plant_1_Abundance)$y, col = 1)
lines(density(Plant_2_Abundance)$x, density(Plant_2_Abundance)$y, col = 2)
lines(density(Plant_3_Abundance)$x, density(Plant_3_Abundance)$y, col = 3)
legend("bottom", inset = c(0, -0.75), title = "Species", legend = 1:3, col = 1:3, lty = 1, xpd = T, horiz = T)

# There are no occurrences of species
# two at 100 m or at 500 m. Does that
# mean that plots from 100 m and 500
# m are similar? No! (Although if you
# used Euclidean distances, your
# statistical analysis would tell you
# that they are similar.)

# Bray-Curtis Distance

# This is one of several distance
# measures that does not deem rows
# to be similar when they both have
# 0s in the same column.

# I'll just go through one example:

Plots <- paste("Plot", 1:4, sep = "_")
Species_1 <- c(3, 4, 1, 2)
Species_2 <- c(10, 0, 1, 3)
Species_3 <- c(2, 2, 5, 7)
matrix_1 <- cbind(Species_1, Species_2, Species_3)
rownames(matrix_1) <- Plots
matrix_1

# Plots 1 and 2 have 3 of the first
# species in common, 0 of the second
# species in common, and 2 of the
# third species in common.

# Plots 1 and 2 have a total of 15
# and 7 individuals in them,
# respectively.

# Their Bray-Curtis dissimilarity
# is 0.524:

1 - ((2 * (3 + 0 + 2)) / (15 + 6))

# We can calculate Bray-Curtis
# dissimilarity for all pairwise
# plot comparisons at once and
# create a distance matrix:

lower_triangle <- NULL
k <- 1
for (i in 1:(nrow(matrix_1) - 1)) {
  for (j in (i + 1):nrow(matrix_1)) {
    y <- NULL
    for(x in 1:ncol(matrix_1)) {
      y[x] <- min(matrix_1[i, x], matrix_1[j, x])
    }
    numerator <- sum(y)
    denominator <- sum(c(matrix_1[i, ], matrix_1[j, ]))
    lower_triangle[k] <- 1 - ((2 * numerator) / denominator)
    k <- k + 1
  }
}
distance_matrix <- matrix(0, nrow = nrow(matrix_1), ncol = nrow(matrix_1))
distance_matrix[lower.tri(distance_matrix)] <- lower_triangle
distance_matrix[upper.tri(distance_matrix)] <- t(distance_matrix)[upper.tri(t(distance_matrix))]
rownames(distance_matrix) <- colnames(distance_matrix) <- rownames(matrix_1)
distance_matrix

# Jaccard distance is very, very
# similar to Bray-Curtis distance
# and can pretty much be used
# interchangeably.

# Presence-Absence Distance Measures

# I would strongly advise against
# using Beal's smoothing for
# presence-absence data. It's
# essentially a band-aid fix for a
# problem that is easily solved by
# using a more relevant distance
# measure.

# 1 = present; 0 = absent

# There are several great choices
# for distance metrics when you've
# got presence-absence data.
# Sorensen's distance is one of
# them. It's calculated much like
# Jaccard distance is calculated.
# Another is Sokal-Michener
# distance.

# Sokal-Michener Distance

# This is actually a similarity
# measure, not a distance or
# dissimilarity measure. You can
# always subtract your similarity
# measure from 1 to obtain a
# dissimilarity measure, and the
# same goes for calculating a
# similarity measure from a
# dissimilarity measure. (This
# doesn't work for Euclidean
# distances because they aren't
# restricted to being between 0
# and 1.)

# Let's just do an example of
# Sokal-Michener distance:

Plots <- paste("Plot", 1:4, sep = "_")
Species_1 <- c(1, 1, 1, 0)
Species_2 <- c(0, 1, 0, 1)
Species_3 <- c(0, 1, 1, 1)
matrix_2 <- cbind(Species_1, Species_2, Species_3)
rownames(matrix_2) <- Plots
matrix_2

# To calculate Sokal-Michener
# distance between plots, we look to
# see if each species is present in
# both plots or absent in both plots.

# The Sokal-Michener distance between
# plots 1 and 2 is 0.33 because there
# area a total of 3 species, and only
# one species is either present or
# absent in both plots. The other two
# species are absent in one plot and
# present in the other. Again, there
# are advantages and disadvantages to
# calling plots similar when they both
# have the same species missing. If
# you have presence-absence data and
# you think this would be a problem
# for your data, I would suggest using
# Sorensen's distance.

sum(matrix_2[1, ] == matrix_2[2, ]) / ncol(matrix_2)

# Here's the corresponding distance
# matrix:

lower_triangle <- NULL
k <- 1
for (i in 1:(nrow(matrix_2) - 1)) {
  for (j in (i + 1):nrow(matrix_2)) {
    lower_triangle[k] <- sum(matrix_2[i, ] == matrix_2[j, ]) / ncol(matrix_2)
    k <- k + 1
  }
}
distance_matrix <- matrix(1, nrow = nrow(matrix_2), ncol = nrow(matrix_2))
distance_matrix[lower.tri(distance_matrix)] <- lower_triangle
distance_matrix[upper.tri(distance_matrix)] <- t(distance_matrix)[upper.tri(t(distance_matrix))]
rownames(distance_matrix) <- colnames(distance_matrix) <- rownames(matrix_2)
distance_matrix

# Why did I go through this rigamarole
# if you don't have presence-absence
# data?

# If you have categorical variables
# that you need to include in your
# distance calculation, a good approach
# is to create dummy variables, or
# columns of 1s and 0s, to denote which
# category your observation falls into:

Plot <- paste("Plot", 1:10, sep = "_")
soil_type <- c("loamy_sand", "sandy_loam", "silt", "clay", "clayey_loam", "silt", "loamy_sand", "loamy_sand", "sandy_loam", "clay")
df2 <- data.frame(Plot, soil_type)
df2

soil_type_ <- factor(df2$soil_type)
dummies <- model.matrix(~ soil_type_)
df3 <- cbind(df2$Plot, dummies[, 2:ncol(dummies)])
colnames(df3)[1] <- "Plot"
df3

# Then, you can use this new data frame
# to calculate distance using an
# appropriate presence-absence
# distance measure. To get an overall
# distance measure that combines
# the distance you calculated from
# continuous or discrete data with
# the one you calculated from
# categorical data, you can simply
# average them, or calculate some
# weighted average of them if you wish
# to weigh some variable more heavily
# than others.

# Note: use Sorensen's distance for this
# approach for categorical data because
# rows that have 0s in common (i.e.,
# rows that are both NOT a given soil
# type) should not be categorized as
# similar!

