#' # Dataset 1: Cow diets
#' 
#' <span style="color:purple"> Answers in purple </span>
#' 
#' <span style="color:purple"> These answers are like those on a grading scheme
#' for an exam. You should be able to score your own work or you could
#' swap with someone else to grade each other's. Hopefully this can give an 
#' idea of how marks are assigned in exams. </span>
#' 
#' ![Cow image](cow.jpeg)
#' * credit: commons.wikimedia
#' 
#' These data are from an experiment on cows. Cows were
#' split into 6 groups, each given a different diet treatment. Here you will look at
#' how these different treatments influenced the dry matter intake (DMI) of the cows.
#' 
#' You can find the data [here](https://www.math.ntnu.no/emner/ST2304/2020v/Week09/cowdata.csv) it is a .csv with a header.
#' 
#' **Important! When you import the data it is important to make sure it is 
#' in the right format. Here you need Treatment to be a factor and Baseline to be numeric.
#' See code below.**
#' 
#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
cowdata <- read.csv("https://www.math.ntnu.no/emner/ST2304/2020v/Week09/cowdata.csv", header=T)

# str() checks the data structure
str(cowdata)

# we can see that the variables are not the right format
# so we fix it
cowdata$Treatment <- as.factor(cowdata$Treatment)
cowdata$Baseline <- as.numeric(cowdata$Baseline)

# now check again
str(cowdata)

#' 
#' The columns in the dataframe are: 
#' 
#' * DMI = the dry matter intake during the experiment (grams)
#' * Baseline = the baseline dry matter intake before the experiment (grams)
#' * Treatment = the diet treatment group (1 to 6)
#' 
#' **You want to find out how diet treatment influenced dry matter intake while
#' controlling for the baseline intake of each cow.**
#' 
#' <span style="color:blue"> 1. What model will you use to answer this? (1 mark) </span>
#' 
#' <span style="color:purple"> 1 point for any of: linear model, regression, ancova.
#' No points for anova or t-test. 
#' There is because there is one categorical and one continuous explanatory here. 
#' A linear model
#' that could be viewed as an ancova is maybe most correct but the maths is the 
#' same as a regression so that is allowed too. </span>
#' 
#' <span style="color:blue"> 2. What type of variables do you have and which 
#' are response
#' or explanatory? (3 marks) </span>
#' 
#' <span style="color:purple"> 1 point for each of: DMI = response, continuous. 
#' Treatment = explanatory, categorical. Baseline = explanatory, continuous. </span>
#' 
#' We have given code to run a model for the data below. 
#' Think about what type of model is being
#' run? It is good practice to consider if you would have chosen the same one.
#' 
#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
# Model 1
model1 <- lm(DMI ~ Treatment+Baseline, data = cowdata)

coef(model1)

confint(model1)

#' 
#' <span style="color:blue"> 3. Interpret the output of the model. 
#' What does it tell you
#' about the effect of diet on DMI? (5 marks) </span> 
#' 
#' <span style="color:purple"> 1 mark for: Intercept is intercept of the model line 
#' (Y value when Baseline = 0) for 
#' Treatment group 1.
#' 1 mark for: effect of Baseline intake is positive 
#' (cows that ate more before eat more
#' during the experiment coefficient estimate of `r round(coef(model1)[7],2)` 
#' 95% CI = `r round(confint(model1)[7,1],2)` to `r round(confint(model1)[7,2],2)` 
#' grams per gram). 
#' 1 mark for: the effect of Baseline intake is the same for all groups. 
#' 1 mark for: all treatments
#' 2-6 seem to have a negative effect relative to Treatment group 1 (mention the
#' actual estimate of effect). 1 mark for:
#' the confidence intervals cross 0 for groups 2,3,4, and 6 so the effects do not
#' have a clearly distinguishable direction of effect. 
#' Only group 5 has a distinguishable 
#' difference from group 1 estimate = `r round(coef(model1)[5],2)`,
#' (95% CI `r round(confint(model1)[5,1],2)` to 
#' `r round(confint(model1)[5,2],2)`grams).
#' **To get each mark you should mention confidence intervals 
#' and the coefficient estimate for specific effects.** </span>
#' 
#' Below is the code to make some graphs to check the model fit. 
#' 
#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
# Graph 1

residuals <- residuals(model1)
fitted <- fitted(model1)
plot(fitted, residuals)

qqnorm(residuals)
qqline(residuals)

#' 
#' <span style="color:blue"> 4. What are the assumptions of the model? (5 marks) </span>
#' 
#' <span style="color:purple"> 1 mark each for: linearity, equal variance 
#' (homoscedasticity - spelling not counted!!), independence of data points,
#' no outliers, normality of residuals. Could also include mean of residuals = 0. </span>
#' 
#' <span style="color:blue"> 5. Are the assumptions met? Reference which plot you use to decide and
#' why you make the choice. (6 marks) </span>
#' 
#' <span style="color:purple"> 3 marks for: Yes - equal variance, 
#' tested using residuals vs fitted plot
#' and there is no structure there. 
#' Also 3 marks for: Yes - normality of residuals, tested
#' using normal qq, fall mostly along the line, some deviation at edges. 
#' For both, could argue that assumptions are not met as there is some 
#' deviation from perfection, but it is not strong. Would need to say
#' exactly where the deviations are e.g. for low values in the normal
#' qq plot and slightly higher variance at fitted values 23 and 24 in the 
#' residuals vs fitted plot. Could suggest a slight curve in the
#' residuals vs fitted as more points above 0 on left than right of graph. </span>
#' 
#' <span style="color:blue"> 6. What other plot might you also want for checking assumptions? (1 mark) </span>
#' 
#' <span style="color:purple"> 1 mark: cook's distance. Scale and leverage could 
#' also be accepted but would be unexpected as an answer. </span>
#' 
#' Here is code for another model on the same data.
#' 
#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
# Model 2
model2 <- lm(DMI ~ Treatment*Baseline, data = cowdata)

coef(model2)

confint(model2)

#' 
#' <span style="color:blue"> 7. How is this model different to the first one? (1 mark) </span>
#' 
#' <span style="color:purple"> 1 mark: this model now has an interaction between
#' treatment and baseline intake. </span>
#' 
#' <span style="color:blue"> 8. Given the new model, does this change your
#' interpretation of the effect of diet on DMI? Why? (4 marks) </span>
#' 
#' <span style="color:purple"> 1 mark for: yes. 
#' 1 mark for: Treatment 5 no longer
#' has a clear effect as the confidence intervals now span 0 (95% CI = 
#' `r round(confint(model2)[5,1],2)` to `r round(confint(model2)[5,2],2)` ). 
#' 1 mark for: the effect of baseline
#' has decreased and confidence intervals also now span 0, estimate of effect =
#'  `r round(coef(model1)[7],2)`
#' (95% CI 
#' `r round(confint(model1)[7,1],2)` to `r round(confint(model1)[7,2],2)` ). 
#' 1 mark for:
#' all other effects have similar interpretation, there does not seem to be 
#' a statistically distinguishable interaction between baseline intake and 
#' treatment. All estimates for the differences in the slope of the effect of
#' baseline intake within each treatment had confidence intervals that spanned 0. 
#' **Important to know
#' here that the interaction terms tells you a difference in the slope 
#' of the effect of baseline on DMI.** </span>
#' 
#' <span style="color:blue"> 9. Which model do you prefer, why? (4 marks) </span>
#' 
#' <span style="color:purple"> 1 mark for: I prefer model 1. 
#' 1 mark for: because it had
#' clearer estimates of some effects. 
#' 1 mark for: model 2 could not identify the
#' direction of any effects (confidence intervals spanning 0). 
#' 1 mark for:
#' as none of the interaction terms had a clear direction, 
#' I could conclude the interaction
#' was not needed. I do not think you would be able to justify choosing model 2. </span>
#' 