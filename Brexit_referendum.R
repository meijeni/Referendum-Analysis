#######################################################################
#######################################################################
######                                                           ######
######                 STAT0023 - ICA 2 Main Code                ######
######                                                           ######
#######################################################################
#######################################################################

sink("ICA2_Code_Output.txt")

# Importing allowed Libraries
library("mgcv")
library("ggplot2")
library("grDevices")
library("lattice")
library("MASS")


################################################################################

# DATA PROCESSING
cat("\nDATA PROCESSING\n")

cat("######################################################################################################")


# Load the ReferendumResults.csv data into a variable called "Votedata"
Votedata <- read.csv("ReferendumResults.csv")


# HIERACHICAL CLUSTERING - Analysis and Output

library(cluster)
# Compute the average values for each region in the Votedata dataset
avg_region_values <- aggregate(Votedata[, -(1:6)], by=list(Votedata$RegionName), FUN=mean)

# Set the row names of the avg_region_values data frame to the corresponding region names
rownames(avg_region_values) <- avg_region_values[, 1]

# Standardize the average values to have a mean of 0 and standard deviation of 1
standardized_avgs <- scale(avg_region_values[, -1])

# Compute the pairwise distances between the standardized average values using Euclidean distance
pairwise_dists <- dist(standardized_avgs)

# Perform hierarchical clustering using complete linkage and store the resulting tree
hierarchical_tree <- hclust(pairwise_dists, method="complete")

# Create a new factor variable called "RegionGroup" based on the values of "RegionName"
Votedata$RegionGroup <- factor(Votedata$RegionName, 
                               levels = c("East Midlands", "East of England", 
                                          "London", "North East", "North West", 
                                          "South East", "South West", "West Midlands", 
                                          "Yorkshire and The Humber"))

# Rename the levels of the "RegionGroup" factor variable based on the region numbers
levels(Votedata$RegionGroup) <- c("EM", "EE_SW", "LON", "Other", "Other", "EE_SW", 
                                  "EE_SW", "Other", "Other")

# Remove rows where the "Leave" column is -1, and save the updated dataframe to "Votedata"
testing <- Votedata[Votedata$Leave == -1,]
Votedata <- Votedata[Votedata$Leave != -1,]

# Determine the optimal number of clusters
clustered_regions <- cutree(hierarchical_tree, k=4)
cat("\nDisplay the new numbered labelling of RegionName, we'll relabel them with the corresponding Categorical labels we:\n")
print(clustered_regions)

# Create a new variable called "proportion_leave" by dividing the "Leave" column 
# by the "NVotes" column
Votedata$proportion_leave <- Votedata$Leave / Votedata$NVotes

# Display the summary statistics of the proportion of leave votes in each ward
cat("\nDisplay the summary statistics of the proportion of leave votes in each ward:\n")
print(summary(Votedata$proportion_leave))

# View the first few rows of the updated dataframe
cat("\nView the first few rows of the updated dataframe\n")
print(head(Votedata))


################################################################################

# EXPLORATORY DATA ANALYSIS
cat("\nEXPLORATORY DATA ANALYSIS\n")

cat("######################################################################################################")


# Calculate the correlation between the proportion of people who voted Leave and 
# the average adult age
cat("\nCalculate the correlation between the proportion of people who voted Leave and the average adult age\n")

print(round(cor(Votedata$proportion_leave, Votedata$AdultMeanAge),2))

# Calculate the correlation between the proportion of people who voted Leave and 
# the overall mean age
cat("\nCalculate the correlation between the proportion of people who voted Leave and the overall mean age\n")
print(round(cor(Votedata$proportion_leave, Votedata$MeanAge),2))

# Figure 1: Scatter plot of proportion_leave against AdultMeanAge
cat("\nFigure 1: Included Separately\n")
plot1 <- ggplot(Votedata, mapping = aes(x=AdultMeanAge, y = proportion_leave)) +
  stat_smooth(method = "lm", col = "red", se = FALSE) +
  geom_point(color = "darkblue", alpha=0.4) +
  labs(x = "Mean Age of Adult Permanent Residents",
       y = "% of Leave Votes") +
  ggtitle("Relationship between AdultMeanAge and proportion_leave") +
  theme(plot.title = element_text(hjust = 0.5))
print(plot1)

# Calculate the correlation between the percentage of people aged 20-24 and 65-74 
# who voted
cat("\nCalculate the correlation between the percentage of people aged 20-24 and 65-74 who voted\n")
print(round(cor(Votedata$Age_20to24, Votedata$Age_65to74),2))

# Calculate the correlation between the proportion of people who voted Leave and 
# the percentage of people with L4 qualifications or higher
cat("\nCalculate the correlation between the proportion of people who voted Leave and the percentage of people with L4 qualifications or higher\n")
print(round(cor(Votedata$proportion_leave, Votedata$L4Quals_plus),2))

# Figure 2: Scatter plot of proportion_leave against HigherOccup
cat("\nFigure 2: Included Separately\n")
plot2 <- ggplot(Votedata, mapping = aes(x=HigherOccup, y = proportion_leave)) +
  stat_smooth(method = "lm", col = "red", se = FALSE) +
  geom_point(color = "darkblue", alpha=0.4) +
  labs(x = "% of Permanent Residents in 'higher-level' Occupations",
       y = "% of Leave Votes") +
  ggtitle("Relationship between HigherOccup and proportion_leave") +
  theme(plot.title = element_text(hjust = 0.5))
print(plot2)

# Calculate the correlation between the proportion of people who voted Leave and
# the percentage of people in higher occupation categories
cat("\nCalculate the correlation between the proportion of people who voted Leave and the percentage of people in higher occupation categories\n")
print(round(cor(Votedata$proportion_leave, Votedata$HigherOccup),2))


# Calculate the correlation between Education and Occupation group 
cat("\nCalculate the correlation between Education and Occupation group \n")
print(cor(Votedata[c("NoQuals", "L4Quals_plus", "HigherOccup", "RoutineOccupOrLTU")]))

# Calculate the correlation between the proportion of people who voted Leave and 
# the percentage of people in privately rented accommodation, as well as social housing
cat("\nCalculate the correlation between the proportion of people who voted Leave and the percentage of people in privately rented accommodation, as well as social housing\n")
print(cor(Votedata[c("proportion_leave", "PrivateRent", "SocialRent")]))


# Calculate the correlation between the three social grade variables
cat("\nCalculate the correlation between the three social grade variables\n")
print(cor(Votedata[c("C1C2DE", "C2DE", "DE")]))

# Figure 3: Interaction between L4Quals_plus and Deprived
cat("\nFigure 3: Included Separately\n")
plot3 <- ggplot(Votedata, mapping = aes(x=L4Quals_plus, y = proportion_leave, color = Deprived)) +
  stat_smooth(method = "lm", col = "red", se = FALSE) +
  geom_point() +
  scale_color_gradient() +
  labs(x = "% of Permanent Residents with Qualifications over Degree Level",
       y = "% of Leave Votes") +
  ggtitle("Interaction between Deprived and L4Quals_plus") +
  theme(plot.title = element_text(hjust = 0.5))
print(plot3)

# Calculate the correlation between the proportion of people who voted Leave and 
# the population density
cat("\nCalculate the correlation between the proportion of people who voted Leave and the population density\n")
print(round(cor(Votedata$proportion_leave, Votedata$Density),2))


# Figure 4: Box Plots of proportion_leave against RegionGroup and AreaType

# proportion_leave & RegionGroup

cat("\nFigure 4: Included Separately\n")

png("Figure 4.png", width = 1000, height = 500)

par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

boxplot(Votedata$proportion_leave~Votedata$RegionGroup, ylab = "% of Leave Votes", xlab = "Region Group",
        col = "grey", main = "Relationship between RegionGroup and Proportion Leave Votes", cex.axis = 0.8)
points(c(mean(Votedata$proportion_leave[Votedata$RegionGroup=="EM"]),
         mean(Votedata$proportion_leave[Votedata$RegionGroup=="EE_SW"]),
         mean(Votedata$proportion_leave[Votedata$RegionGroup=="LON"]),
         mean(Votedata$proportion_leave[Votedata$RegionGroup=="Other"])),
       pch=16, cex=1.5,col="orange")

# proportion_leave & AreaType

boxplot(Votedata$proportion_leave~Votedata$AreaType, ylab = "% of Leave Votes", xlab = "Area Type",
        col = "grey", main = "Relationship between AreaType and Proportion Leave Votes", cex.axis = 0.8)
points(c(mean(Votedata$proportion_leave[Votedata$AreaType=="E06"]),
         mean(Votedata$proportion_leave[Votedata$AreaType=="E07"]),
         mean(Votedata$proportion_leave[Votedata$AreaType=="E08"]),
         mean(Votedata$proportion_leave[Votedata$AreaType=="E09"])),
       pch=16, cex=1.5,col="orange")

dev.off()



# Figure 5: Interaction between L4Quals_plus and White

cat("\nFigure 5: Included Separately\n")

plot5 <- ggplot(Votedata, mapping = aes(x=L4Quals_plus, y = proportion_leave, color = White)) +
                stat_smooth(method = "lm", col = "red", se = FALSE) +
                geom_point() +
                scale_color_gradient() +
                labs(x = "% of Permanent Residents with Qualifications over Degree Level",
                     y = "% of Leave Votes") +
                ggtitle("Interaction between White and L4Quals_plus") +
                theme(plot.title = element_text(hjust = 0.5))
print(plot5)


################################################################################
cat("######################################################################################################")


# MODELLING
cat("\nMODELLING\n")
cat("######################################################################################################")


# MODEL 1: ALL RELEVANT VARIABLES AND INTERACTION TERMS
model1 <- glm(proportion_leave ~
                RegionGroup
              + AdultMeanAge
              + White
              + Owned
              + OwnedOutright
              + PrivateRent
              + NoQuals
              + L4Quals_plus
              + Unemp
              + UnempRate_EA
              + RoutineOccupOrLTU
              + HigherOccup
              + Density
              + Deprived
              + MultiDepriv
              + C2DE
              + DE
              + White * L4Quals_plus
              + L4Quals_plus*Deprived
              , weight = NVotes, family=binomial(link="logit"), data = Votedata)

# Summary output of Model 1
cat("\nMODEL 1 Output:\n")
print(summary(model1))


# MODEL 1: DIAGNOSTIC PLOTS

cat("\nFigure 6: Included Separately\n")

png(file = "Figure 6.png", width = 800, height = 400)

par(mfrow = c(1,2))
plot(model1, which = c(3,4))

dev.off()


# Calculate Variance of Pearson Residuals

cat("\nCalculate Variance of Pearson Residuals\n")

print(sum(resid(model1,type="pearson")^2 ) / model1$df.residual)

# Calculate number of influential observation based on Cook's distance values

cat("\nCalculate number of influential observation based on Cook's distance values\n")

print(sum(cooks.distance(model1) > (8/(803-2*22))))


cat("######################################################################################################")


# MODEL 2: Account for Over dispersion

model2 <- update(model1, .~., family=quasibinomial(link="logit"))

cat("\nMODEL 2 Output\n")
print(summary(model2))

# Calculate number of influential observation based on Cook's Distance values

cat("\nCalculate number of influential observation based on Cook's Distance values\n")
print(sum(cooks.distance(model2) > (8/(803-2*22))))


cat("######################################################################################################")

# MODEL 3: Evaluating covariates from Model 2

cat("\nMODEL 3\n")


# Remove the 'Owned' covariate from Model 2 and create a new model & Compare the original model and the new model using an ANOVA test
cat("\nRemove the 'Owned' covariate from Model 2 and new model & Compare the original model and the new model using an ANOVA test\n")
model2_remove1 <- update(model2, .~. - Owned)
print(anova(model2, model2_remove1, test = "F"))

cat("######################################################################################################")

# Remove the 'OwnedOutright' covariate from the previous model and create a new model 
cat("\nRemove the 'OwnedOutright' covariate from the previous model and create a new model\n")
model2_remove2 <- update(model2_remove1, .~. - OwnedOutright)
print(anova(model2_remove1, model2_remove2, test = "F"))

cat("######################################################################################################")

# Remove the 'PrivateRent' covariate from the previous model and create a new model
cat("\nRemove the 'PrivateRent' covariate from the previous model and create a new model\n")
model2_remove3 <- update(model2_remove2, .~. - PrivateRent)
print(anova(model2_remove2, model2_remove3, test = "F"))

cat("######################################################################################################")


# Create FINAL MODEL 3 by removing all covariates from Model 2 that were evaluated in the previous steps

model3 <- update(model2, .~. - Owned - OwnedOutright - PrivateRent)

# Print a summary of Model 3
cat("\nModel 3 Output\n")
summary(model3)

# Calculate the ratio of deviance of model2 to its null deviance
cat("\nCalculate the ratio of deviance of model2 to its null deviance\n")
print(1 - model2$deviance / model2$null.deviance)

# Calculate the ratio of deviance of model3 to its null deviance
cat("\nCalculate the ratio of deviance of model3 to its null deviance\n")
print(1 - model3$deviance / model3$null.deviance)

# Calculate number of influential observation based on Cook's Distance values
cat("\nCalculate number of influential observation based on Cook's Distance values\n")
print(sum(cooks.distance(model3) > (8/(803-2*15))))


######################################################################################################
cat("######################################################################################################")

# PREDICTION

# FITTING ON TRAINING DATA

prediction_1 <- predict(model1, data = Votedata, se.fit = TRUE, type = 'response')
prediction_2 <- predict(model2, data = Votedata, se.fit = TRUE, type = 'response')
prediction_3 <- predict(model3, data = Votedata, se.fit = TRUE, type = 'response')



plot_prediction <- function(prediction, model_num) {
  
  plot <- ggplot(data = data.frame(x = prediction$fit, y = Votedata$proportion_leave)) + 
              geom_point(aes(x = x, y = y), size = 3, alpha = 0.4, color = "darkblue") +
              labs(x = "Predicted Proportion of Leave Votes",
                   y = "Observed Proportion of Leave Votes",
              title = paste0("Model ", model_num)) +
              geom_abline(intercept=0, slope=1, color="red") +
              xlim(0, 1) +
              theme(plot.title = element_text(hjust = 0.5))
              return(plot)
}

plot7 <- plot_prediction(prediction_1, 1)
plot8 <- plot_prediction(prediction_2, 2)
plot9 <- plot_prediction(prediction_3, 3)

cat("\nFigure 7: Included Separately\n")
cat("\nFigure 8: Included Separately\n")
cat("\nFigure 9: Included Separately\n")


# PREDICTION ON PROVIDED MISSING DATA

prediction_final <- predict(model3, newdata = testing, se.fit = TRUE, type = 'response')

# Create a data frame with the prediction and standard error
prediction_df <- data.frame(testing$ID, prediction_final$fit, prediction_final$se.fit)

# Write the data frame to a .dat file
write.table(prediction_df, "ICA_GROUP_J_pred", sep = " ", row.names = FALSE, col.names = FALSE)


######################################################################################################


# EXPORT THE PLOTS

ggsave("Figure 1.png", plot=plot1, width=8, height=4)
ggsave("Figure 2.png", plot=plot2, width=8, height=4)
ggsave("Figure 3.png", plot=plot3, width=8, height=4)
# Figure 4 plot use PNG function to save
ggsave("Figure 5.png", plot=plot5, width=8, height=4)
# Figure 6 is Diagnostic Plot of Model 1, use PNG
ggsave("Figure 7.png", plot=plot7, width=8, height=4)
ggsave("Figure 8.png", plot=plot8, width=8, height=4)
ggsave("Figure 9.png", plot=plot9, width=8, height=4)

sink(); par(mfrow=c(1,1))






