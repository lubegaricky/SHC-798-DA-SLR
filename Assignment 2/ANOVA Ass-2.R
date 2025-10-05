## Part 2: ANOVA
## Question 4
## ~~~~~~~~ONE-WAY ANOVA~~~~~~~~~~~~~~???

## Import data, here manually
meat <- data.frame(steak.id = c(1, 6, 7, 12, 5, 3, 10, 9, 2, 8, 4, 11),
                   treatment = rep(c("Commercial", "Vacuum", "Mixed", 
                                     "CO2"), each = 3),
                   y = c(7.66, 6.98, 7.80, 5.26, 5.44, 5.80, 
                         7.41, 7.33, 7.04, 3.51, 2.91, 3.66))

# Getting started with the dataset in timber.csv :
timber <- read.csv(file.choose(), header = TRUE, na.strings = c("NA"))
# timber
head(timber)
str(timber)
## Convert species column to a factor
timber$species <- factor(timber$species)
## Check levels
levels(timber$species)


# Part a) 
# Box Plots
boxplot(stiffness ~ species,
        data = timber,
        main = "Bending Stiffness by Timber Species",
        xlab = "Timber Species",
        ylab = "Bending Stiffness (kN·m²)",
        col = c("lightblue", "lightgreen", "lightpink"),
        border = "black")

# Optional: Add means as points
means <- tapply(timber$stiffness, timber$species, mean)
points(1:3, means, pch = 19, col = "red")







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
