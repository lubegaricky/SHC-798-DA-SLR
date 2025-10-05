## Part 2: ANOVA
## Question 4
## ~~~~~~~~ONE-WAY ANOVA~~~~~~~~~~~~~~???

## Import data, here manually
meat <- data.frame(steak.id = c(1, 6, 7, 12, 5, 3, 10, 9, 2, 8, 4, 11),
                   treatment = rep(c("Commercial", "Vacuum", "Mixed", 
                                     "CO2"), each = 3),
                   y = c(7.66, 6.98, 7.80, 5.26, 5.44, 5.80, 
                         7.41, 7.33, 7.04, 3.51, 2.91, 3.66))



# *************************************************************************
# Getting started with the dataset in timber.csv :
timber <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"))
# timber
head(timber)
str(timber)
## Convert species column to a factor
timber$species <- factor(timber$species)
## Check levels
levels(timber$species)

## Visualize data
stripchart(stiffness ~ species, data = timber, pch = 1, vertical = TRUE)

# Part a) 
# Box Plots
boxplot(stiffness ~ species,
        data = timber,
        main = "Bending Stiffness by Timber Species",
        xlab = "Timber Species",
        ylab = "Bending Stiffness (kN·m²)",
        col = c("lightblue", "lightgreen", "lightpink"),
        border = "black")

#Adding means as points
means <- tapply(timber$stiffness, timber$species, mean)
points(1:3, means, pch = 19, col = "red")
# Annotate outliers on the plot
text(x = bp$group, y = bp$out, labels = bp$out, pos = 3, cex = 0.7, col = "blue")


# Outliers

# Variability:
# Standard Deviation (SD): Gum has the highest variability (SD = 641.0 kN·m²), followed by cedar (SD = 572.8 kN·m²), and pine has the lowest (SD = 552.6 kN·m²). This suggests that gum's stiffness values are more spread out compared to pine and cedar.
# Interquartile Range (IQR): Pine has the highest IQR (696.8 kN·m²), indicating a slightly wider spread of the middle 50% of stiffness values compared to gum (673.8 kN·m²) and cedar (659.1 kN·m²). However, the differences in IQR are small, suggesting comparable spread in the central data across species.
# Range (Max - Min): Gum shows the largest range (11124.5 - 8314.9 = 2809.6 kN·m²), followed by cedar (10074.9 - 8220.3 = 1854.6 kN·m²), and pine (8982.9 - 6999.2 = 1983.7 kN·m²). This reinforces that gum has the most extreme values.


# Central Tendency:
# Gum has the highest median stiffness (9425.3 kN·m²), followed by cedar (9387.7 kN·m²), and pine (8139.2 kN·m²). This indicates that gum and cedar generally have higher bending stiffness than pine.

# In short

# Variability: Gum exhibits the highest variability in bending stiffness, as seen in its larger SD and range, suggesting less consistency in its mechanical properties compared to pine and cedar. Cedar and pine have similar variability, but pine’s stiffness values are generally lower.
# Outliers: Pine has one low outlier, indicating a single timber sample with unusually low stiffness, possibly due to defects or testing conditions. Gum has both a high and a low outlier, suggesting it can exhibit extreme stiffness values (both stronger and weaker), which may reflect natural variability or quality differences in the samples. Cedar’s lack of outliers suggests greater consistency in its stiffness properties.
# Practical Implications: If consistency is desired, cedar may be preferable due to its lack of outliers and moderate variability. Gum’s higher median stiffness is appealing for strength, but its outliers and variability suggest a need for quality control. Pine’s lower stiffness and single outlier may indicate it’s less suitable for applications requiring high or consistent stiffness.



# Part b)
# Fit a one-way ANOVA test
timber$species <- relevel(timber$species, ref = "gum")
options(contrasts = c("contr.sum", "contr.poly"))
# options(contrasts = c("contr.treatment", "contr.poly")) # used as default anyway

stiff <- aov(stiffness ~ species, data = timber)
summary(stiff) ## ANOVA table including F-test

## Extract coefficients
coef(stiff)       ## be careful with interpretation
dummy.coef(stiff) ## full coefficients, easier to interpret



# Part c)

# Perform pairwise t-tests with Bonferroni correction
tapply(timber$stiffness, timber$species, sd) # check for group SD

pairwise_results <- pairwise.t.test(timber$stiffness, timber$species, 
                                    p.adjust.method = "bonferroni", 
                                    pool.sd = FALSE, # Welch's t-test (unequal variances)
                                    paired = FALSE,  # Independent samples
                                    conf.level = 0.95)

# Print the results
print("Pairwise t-test results with Bonferroni correction:")
print(pairwise_results)


# Part d)
## Residual Diagnostics
plot(stiff, which = 2)
plot(stiff, which = 1)





# ++++++++++++++++++++++++++++++++++++++++++********************+++++++++++++++++++++
# Getting started with the dataset in curing.csv :
curing <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"))
# timber
head(curing)
str(curing)







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## If we want to change the reference level, we can do this using relevel
meat$treatment <- relevel(meat$treatment, ref = "Commercial")

## Display the updated data frame
meat
str(meat)

## Visualize data
stripchart(y ~ treatment, data = meat, pch = 1, vertical = TRUE)

## Fit one-way ANOVA model
# Choose the contrast that you want to use
# A vector is expected that states the contrast for unordered and ordered factors respectively
options(contrasts = c("contr.sum", "contr.poly"))                                 
#options(contrasts = c("contr.treatment", "contr.poly"))

fit <- aov(y ~ treatment, data = meat)
summary(fit) ## ANOVA table including F-test

## Extract coefficients
coef(fit)       ## be careful with interpretation
dummy.coef(fit) ## full coefficients, easier to interpret

## Confidence intervals for group means \mu_i
predict(fit, 
        newdata = data.frame(treatment = c("Commercial", "CO2", "Mixed", "Vacuum")), 
        interval = "confidence")

## Fit single mean model
fit.single <- aov(y ~ 1, data = meat)  # 1 means global mean (intercept)
summary(fit.single)
## Compare with cell means model
anova(fit.single, fit)

## Inference on individual treatment effects
summary.lm(fit)
confint(fit)



## Parameters including standard errors
summary.lm(fit) ## be careful with interpretation
confint(fit)


## Residual analysis ####
plot(fit, which = 2)
plot(fit, which = 1)



## Using a different contras leads to the same output for predict()
options(contrasts = c("contr.treatment", "contr.poly"))
fit2 <- aov(y ~ treatment, data = meat)
summary(fit2) ## ANOVA table including F-test

## Extract coefficients
coef(fit2)       ## be careful with interpretation
dummy.coef(fit2) ## full coefficients, easier to interpret
predict(fit2, 
        newdata = data.frame(treatment = c("Commercial", "CO2", "Mixed", "Vacuum")), 
        interval = "confidence")
