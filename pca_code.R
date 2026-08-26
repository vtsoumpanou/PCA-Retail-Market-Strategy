# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA for Market Expansion Strategy
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Dependencies              ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages(c("eurostat", "tidyverse", "janitor"))

library(eurostat)
library(tidyverse)
library(janitor)

# Choose a recent year
year <- 2018

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Eurostat Urban Indicators ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Available datasets
search_results <- search_eurostat("city")
head(search_results)

# Population
df_pop <- get_eurostat("urb_cpop1", time_format = "num")

# Unemployment
df_unemp <- get_eurostat("urb_clma", time_format = "num")

# Education
df_educ <- get_eurostat("urb_ceduc", time_format = "num")

# Tourism (proxy for activity / attractiveness)
df_tourism <- get_eurostat("urb_ctour", time_format = "num")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Data Wrangling
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_pop <- df_pop %>% 
  filter(TIME_PERIOD == year & indic_ur == "DE1001V") %>%
  select(indic_ur, cities, values)

df_unemp <- df_unemp %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_educ <- df_educ %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_tourism <- df_tourism %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

# Merge the dataset
df <- rbind(df_pop, df_unemp, df_educ, df_tourism)

# Update the labels
df <- label_eurostat(df)

# Remove indicators that are available for the male-female population
df <- df %>%
  filter(!str_detect(indic_ur, regex("male|female", ignore_case = TRUE)))

# Bring to tidy format
df <- pivot_wider(df,
                  names_from = indic_ur,
                  values_from = values)

# Keep only the variables with low missing values
df <- df[, colMeans(is.na(df)) < 0.30]

# Keep cities with no missing values
df <- df[rowMeans(is.na(df)) == 0, ]

head(df)
#The working dataset includes 27 columns-factors 
#for each of the 263 reported cities, the only non numeric column is the 1st 

#Check for missing values- already taken care of in line 77
sum(is.na(df)) #no missing values

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANALYSIS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 1     ~~~~~~~~~~~~~~~~~~~~~~                   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rename columns to facilitate analysis
var_names <- colnames(df)
colnames(df) <- paste0("Var", 1:ncol(df))
var_names
# Check
head(df)

#Check for outliers, poorly cleaned data
plot(df$Var2, type = "h", ylab = "Population", xlab = "City order",
     lwd = 2, main = "Population per city ")

#Cities 180 (France) and 261 (Romania) seem to have country-level population. 
#After check: remove entries as they are indeed countries, not cities.

df <- df[-c(180,261),]
head(df)

#Logical Data checks
check_1 <- df$Var2 - df$Var6 #economically active population 
                              #cannot be larger than total population
check_1

check_2 <-df$Var2 - df$Var4 - df$Var7 - df$Var8
check_2

plot(x = df$Var18, y = df$Var5, main = "Higher education - Unemployment",
     xlab = "Share of students in higher education",
     ylab = "Unemployment rate")

plot(x = df$Var19, y = df$Var5, main = "Higher education 2014 onwards - Unemployment",
     xlab = "Students in higher education (ISCED level 5-8 from 2014 onwards)",
     ylab = "Unemployment rate")

#outliers are greater cities

plot(x = df$Var5, y = df$Var24, main = "No of libraries - Unemployment",
     xlab = "Unemployment rate",
     ylab = "Number of Libraries") #outlier is Paris

# Examine correlations graphically
#scatter plots are not helpful due to scaling issues--> 
#fixable through standardisation and the number of components

# Covariance matrix of the data
cov_matrix <- round(cov(df[,2:28]),3)   #different units of measurement (%, raw numbers)
cov_matrix                              #lead to scaling issues
# Correlation matrix of the data
corr_matrix <- round(cor(df[,2:28]),3)   #much more coherent and easier to work with
corr_matrix


#Create heatmaps to investigate correlations

install.packages("pheatmap")
library(pheatmap)

#Heatmap without clustering
pheatmap(corr_matrix,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = FALSE,
         show_colnames = FALSE,
         main = "Correlation Heatmap without clustering",
         fontsize = 8,
         color = colorRampPalette(c("blue", "white", "red"))(100) 
)

#Heatmap with clustering
pheatmap(corr_matrix,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         show_rownames = FALSE,
         show_colnames = FALSE,
         main = "Correlation Heatmap with clustering",
         fontsize = 8,
         color = colorRampPalette(c("blue", "white", "red"))(100) 
)

#From the heatmaps it is obvious that there are strong correlations-mostly
#positive between the variables --> some columns could be omitted 
strongly_corr <- which(
  abs(corr_matrix) >= 0.95 & upper.tri(corr_matrix),
  arr.ind = TRUE
)
strongly_corr
data.frame(
  var1 = colnames(corr_matrix)[strongly_corr[,1]],
  var2 = colnames(corr_matrix)[strongly_corr[,2]],
  corr = corr_matrix[strongly_corr]
) #we expect to be able to considerable reduce dimensionality!


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 2     ~~~~~~~~~~~~~~~~~~~~~~                   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Apply PCA on correlation matrix
# PCA on correlation matrix
pca_standard <- prcomp(df[2:28], center = TRUE, scale = TRUE)
pca_standard

summary(pca_standard) #principal component is dominant

pca_standard$sdev # square root of the eigenvalues

eigen_matrix <- round(pca_standard$rotation, 3) # eigenvector matrix/ loadings matrix
eigen_matrix

#Find variable contributions --> business interpretation 
contrib <- round(pca_standard$rotation^2, 3)
contrib
pc1_contrib <- sort(contrib[, 1])  #pc1 mainly driven by Education, Employment, Total population
pc2_contrib <- sort(contrib[, 2])  #pc2 mainly driven by Tourism
sort(contrib[, 3])  #pc3 mainly driven by Education (already captured by pc1) 


#Base R screeplot
screeplot(pca_standard, npcs=27, type="lines"
          , col = "lightblue", lwd = 2, pch = 19 )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 3     ~~~~~~~~~~~~~~~~~~~~~~                   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Extract the eigenvalues from the PCA results
eigenvalues <- round(pca_standard$sdev^2, 3)
eigenvalues

#Deside on number of components using Kaiser's Critirion
kaiser_critirion <- which(eigenvalues>1)  #Kaiser's Critirion leads to 4 components
kaiser_critirion                    #overestimates most probably (pc1, pc2 suffice)

# Percentage of variance explained screeplot
plot(eigenvalues/sum(eigenvalues), type = "b",
     xlab = "Principal Component",
     ylab = "Percentage of Variance Explained",col = "lightblue",lwd = 2,
    pch = 19)

#Based on the screeplot the first two components explain most of the variance (~80%)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#For the rest of the analysis, we will be using the first two principal 
#components as suggested by the standardised analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pc1 <- eigen_matrix[,1] #loadings
#lambda1 = 19.24031
pc2 <- eigen_matrix[,2] #loadings
#lambda2 = 2.180082


#Calculate component scores and total score from pca_standardised

scores <- pca_standard$x[,1:2]
total_score <- rowSums(scores)

# Add score columns to df

df$pc1 <- scores[,1]
df$pc2 <- scores[,2]
df$total_score <- total_score
head(df) # check new column integration

# Rank by total score 
df$rankScore <- rank(-df$total_score, ties.method = "first")

# Rank by PC1 score 
df$pc1_score <- rank(-df$pc1, ties.method = "first")

# Top 10 cities by total score
top10_total <- df[order(-df$total_score), c("Var1", "total_score")][1:10, ] #-df$total_score
                                                                    #ensures decreasing order
top10_total

# Top 10 cities by pc1 score
top10_pc1 <- df[order(-df$total_score), c("Var1", "pc1_score")][1:10, ] 
top10_pc1
# same top 10 as by total score

# Bottom 10 cities by total score
bottom10_total <- df[order(df$total_score), c("Var1", "total_score")][1:10, ] 
bottom10_total

# Bottom 10 cities by pc1 score
bottom10_pc1 <- df[order(df$total_score), c("Var1", "pc1_score")][1:10, ] 
bottom10_pc1
# same bottom 10 as by total score

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots and graphs

# Plot of component scores, label by rank according to first PC
plot(df$pc1,df$pc2,
     xlab = "PC 1",ylab = "PC 2", col="blue", pch = 19)
abline(h = 0 ,v = 0,lty = 2)
text(df$pc1,df$pc2,labels = df$pc1_score,pos = 3,cex = 0.7)

# Plot of component scores, label by rank according to total score
plot(df$pc1,df$pc2,
     xlab = "PC 1",ylab = "PC 2", col="steelblue", pch = 19)
abline(h = 0 ,v = 0,lty = 2)
text(df$pc1,df$pc2,labels = df$pc1_score,pos = 3,cex = 0.7)

#Zoom-in on the scatterplot
plot(df$pc1,df$pc2,
     xlab = "PC 1",ylab = "PC 2", xlim = c(-0.7,0.5), ylim = c(-1,1.5),
     col="steelblue", pch = 19)
abline(h = 0 ,v = 0,lty = 2)
text(df$pc1,df$pc2,labels = df$pc1_score,pos = 3,cex = 0.7)

# plot rank according to first PC vs actual rank
plot(df$pc1,df$pc2,
     xlab = "PC 1",ylab = "PC 2", col="navy", pch = 19)
text(df$pc1,df$pc2,labels = df$rankScore,pos = 3,cex = 0.7)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 4    ~~~~~~~~~~~~~~~~~~~~~~                   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plots with the coefficient of the first two principal components
plot(pc1,pc2,xlim = c(-0.05,0.25),ylim = c(-0.8,0.8), pch = 19, 
     col = "steelblue", 
     xlab = "PC1", ylab = "PC2", main = "Variable Scatterplot")
abline(h = 0, v = 0, lty = 2, col = "gray")
# There is one distinct cluster of points, and 7 points that are mainly affecting
# the 2nd principle component, almost not at all the 1st


#Zoom-in on the main cluster of points
plot(pc1,pc2,xlim = c(0.20,0.24),ylim = c(-0.1,0.1), pch = 19, 
     col = "steelblue", 
     xlab = "PC1", ylab = "PC2", main = "Variable Scatterplot")
abline(h = 0, v = 0, lty = 2, col = "gray")

# Contribution of variables to each component for interpretation

pc1_contrib
pc2_contrib

var_names

# PC1 is mainly affected by Education, population and employment variables 
# that form the main cluster

# PC2 is mainly affected by  Tourism variables (Vars 27, 28, 25, 26) and much less
# by variables related to unemployment, activity rate, education



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tasks 5  & 6   ~~~~~~~~~~~~~~~~~~                  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rank <- df[order(-df$total_score), c("Var1", "total_score", "rankScore" )][ , ]
rank

# Color by rankScore
colors <- colorRampPalette(c("lightblue","navy"))(nrow(df))
plot(df$pc1, df$pc2,
     xlab="PC1", ylab="PC2",
     pch=19, col=colors[df$rankScore],
     main="City Scatterplot Colored by Rank")
text(pc1_scores, pc2_scores, labels=df$Var1, pos=3, cex=0.7)


plot(df$pc1, df$pc2,
     xlab="PC1", ylab="PC2",
     col=colors[df$rankScore], pch=19,
     xlim=c(min(df$pc1), quantile(df$pc1, 0.95)),  # zoom to 95% percentile
     ylim=c(min(df$pc2), quantile(df$pc2, 0.95)),
     main="City Scatterplot Zoomed on Main Cluster")

# No distinct clustering. Also no signifficant difference between Northernn
# and Southern cities. Overall, Northern cities seem to rank slightly higher, 
# yet no pattern can be recognised. 

# Larger cities (capitals and European cities) score higher by pc1 scores, 
# smaller tourist friendly cities score higher by pc2. 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 7     ~~~~~~~~~~~~~~~~~~~~~~                   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Apply PCA on centered data-covariance matrix
pca_centered <- prcomp(df[2:28], center = FALSE, scale = FALSE)
pca_centered
summary(pca_centered)

#Principle components 5-27 make no contibution to the proportion of variance 
#explained, thus will be disregarding from further analysis

pca_centered$sdev # square root of the eigenvalues
pca_centered$rotation # eigenvector/loadings matrix

#pca_centered$center

#Base R screeplot
screeplot(pca_centered, npcs=4, type="lines"
          , col = "navy", lwd = 2, pch = 19 )

# Extract the eigenvalues from the PCA results
eigenvalues2 <- round(pca_centered$sdev^2, 3)
eigenvalues2

lambda_hat <- mean(eigenvalues2)

kaiser_critirion2 <- which(eigenvalues2 > lambda_hat)
kaiser_critirion2   #Kaiser's Critirion leads to 1 component

# Percentage of variance explained screeplot
plot(eigenvalues2/sum(eigenvalues2), type = "b",
     xlab = "Principal Component",
     ylab = "Percentage of Variance Explained",col = "navy",lwd = 2,
     pch = 19)

#Based on the screeplot the first component explain most of the variance (~98%)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NOTES      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Awful plots --> labeling is impossible
#use ggplot2 ?
#evaluate robustness of eigenvalue estimation ?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~