
# Set working directory to same folder as this code

# Load plotting package
library(ggplot2)

# Load data files
library(foreign)
gums <- read.dta("gums.dta")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Hierarchal clustering ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate Euclidian distances between rows, considering factors 1 to 6
d <- dist(gums[, c("V1", "V2", "V3", "V4", "V5", "V6")])

# Apply Ward's linkage clustering
h <- hclust(d, method = "ward.D2")

# view dendogram
plot(h, xlab = "Respondent")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### K-means clustering ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First, standardize the input variables (z-scores)
z <- scale(gums[, c("V1", "V2", "V3", "V4", "V5", "V6")], center = TRUE, scale = TRUE)

# Since the k-means algorithm starts with a random set of centers, setting the seed helps ensure the results are reproducible
set.seed(1)

# Apply K-means clustering with the selected numbers of centers
k <- kmeans(z, centers = 3)

# Cluster sizes
k$size

# Cluster means
# For ease of interpretation, convert standardized values back to original units
sapply(c("V1", "V2", "V3", "V4", "V5", "V6"), function(n) k$centers[, n]*sd(gums[,n]) + mean(gums[,n]))


#------------------------------
# Diagnostics Plot #
#------------------------------

# Create a data frame with cluster assignment
plotdata <- cbind(gums[, c("V1", "V2", "V3", "V4", "V5", "V6")], k$cluster)

# Reshape from wide to long format (required for use of ggplot2)
plotdata <- reshape(plotdata, varying = c("V1", "V2", "V3", "V4", "V5", "V6"), v.names = "score", timevar = "benefit", times = c("Shiny teeth", "Prevents cavities", "Freshens breath", "Strengthens gums", "Attractive teeth", "Prevents tooth decay"), direction = "long")

colnames(plotdata)[1] <- "cluster"

# Build plot
p <- ggplot(data = plotdata)
p <- p + geom_density(aes(x = score, colour = as.factor(cluster), fill = as.factor(cluster)), size = 1, alpha = 0.3)
p <- p + facet_wrap(~ benefit, ncol = 3)
p <- p + labs(title = "Cluster histogram diagnostics")
p <- p + xlim(c(0,8))
p <- p + theme_bw()
p

#------------------------------
# Alternative Solution #
#------------------------------

# Apply K-means clustering with the alternative numbers of centers
k2 <- kmeans(z, centers = 2)

# Cluster sizes
k2$size

# Cluster means
sapply(c("V1", "V2", "V3", "V4", "V5", "V6"), function(n) k2$centers[, n]*sd(gums[,n]) + mean(gums[,n]))

# Create data frame for plotting data
plotdata2 <- cbind(gums[, c("V1", "V2", "V3", "V4", "V5", "V6")], k2$cluster)

# Reshape from wide to long format (required for use of ggplot2)
plotdata2 <- reshape(plotdata2, varying = c("V1", "V2", "V3", "V4", "V5", "V6"), v.names = "score", timevar = "benefit", times = c("Shiny teeth", "Prevents cavities", "Freshens breath", "Strengthens gums", "Attractive teeth", "Prevents tooth decay"), direction = "long")

colnames(plotdata2)[1] <- "cluster"

# Build plot
p2 <- ggplot(data = plotdata2)
p2 <- p2 + geom_density(aes(x = score, colour = as.factor(cluster), fill = as.factor(cluster)), size = 1, alpha = 0.3)
p2 <- p2 + facet_wrap(~ benefit, ncol = 3)
p2 <- p2 + labs(title = "Cluster histogram diagnostics")
p2 <- p2 + xlim(c(0,8))
p2 <- p2 + theme_bw()
p2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Describing the segments ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create a binary variable for gender
gums$gender2 <- 1 # initiate with all males
gums$gender2[which(gums$gender == "female")] <- 2 # correct values for females

# Create data frame for plotting data with cluster assignment
plotdata3 <- cbind(gums[, c("gender2", "age3")], k$cluster)

# Reshape from wide to long format
plotdata3 <- reshape(plotdata3, varying = c("gender2", "age3"), v.names = "category", timevar = "characteristic", times = c("Gender", "Age3"), direction = "long")

colnames(plotdata3)[1] <- "cluster"

# Build plot
p3 <- ggplot(data = plotdata3)
p3 <- p3 + geom_density(aes(x = category, colour = as.factor(cluster), fill = as.factor(cluster)), size = 1, alpha = 0.3)
p3 <- p3 + facet_wrap(~ characteristic, ncol = 2)
p3 <- p3 + labs(title = "Cluster histogram diagnostics for segment")
p3 <- p3 + xlim(c(0,4))
p3 <- p3 + theme_bw()
p3

#------------------------------
# Adding labels to clusters #
#------------------------------

# Create a new variable with the desired cluster names
# There are multiple ways of doing this, which will all give the same result. Here are 3 examples:

# Start by adding cluster assignments to the dataframe
gums <- cbind(gums, cluster = k$cluster)

# Select an option for assigning labels

# Option 1: loop (not recommended for large dataframes)
gums$segment <- ""
for(i in 1:nrow(gums)){
  
  if (gums$cluster[i] == 1) {
    gums$segment[i] <- "healthy"
  } else if (gums$cluster[i] == 2) {
    gums$segment[i] <- "cosmetic"
  } else {
    gums$segment[i] <- "casual"
  }
  
}

# Option 2: iterate through each cluster
gums$segment <- "healthy" # initiate all values with first cluster
gums$segment[which(k$cluster == 2)] <- "cosmetic"
gums$segment[which(k$cluster == 3)] <- "casual"

# Option 3: use dplyr
library(dplyr)
gums <- gums %>% mutate(segment = case_when(cluster == 1 ~ "healthy", cluster == 2 ~ "cosmetic", TRUE ~ "casual"))


#------------------------------
# Gender X Membership Crosstabs #
#------------------------------
library(gmodels)
CrossTable(x = gums$segment, y = gums$gender, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)

