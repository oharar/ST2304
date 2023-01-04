#' # Dataset 2: Iris petals - EXAM STYLE ANSWERS
#' 
#' <span style="color:purple"> Answers in purple </span>
#' 
#' <span style="color:purple"> These answers are like those on a grading scheme
#' for an exam. You should be able to score your own work or you could
#' swap with someone else to grade each other's. Hopefully this can give an 
#' idea of how marks are assigned in exams. </span>
#' 
#' ![Iris image](iris.jpeg)
#' * credit: commons.wikimedia
#' 
#' These data are from three species of the plant, iris.
#' They include measures of petal width and length. Here you will look at how the length
#' of petals influences their width.
#' 
#' You can find the data [here](https://www.math.ntnu.no/emner/ST2304/2020v/Week09/irisdata.csv)
#' it is a .csv with a header.
#' 
#' **Important! When you import the data it is important to make sure it is 
#' in the right format. Here you need Species to be a factor and PetalLength to be numeric.
#' See code below.**
#' 
#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
irisdata <- read.csv("https://www.math.ntnu.no/emner/ST2304/2020v/Week09/irisdata.csv", header=T)

# str() checks the data structure
str(irisdata)

# we can see that the variables are ok but best to be sure
irisdata$Species <- as.factor(irisdata$Species)
irisdata$PetalLength <- as.numeric(irisdata$PetalLength)

# now check again
str(irisdata)

#' 
#' The columns in the dataframe are: 
#' 
#' * PetalWidth = width of the petal in cm.
#' * PetalLength = length of the petal in cm.
#' * Species = which species it is.
#' 
#' **You want to find out how petal length and species effect petal width.**
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
#' <span style="color:blue"> 2. What type of variables do you have and which are 
#' response
#' or explanatory? (3 marks) </span>
#' 
#' <span style="color:purple"> 1 point for each of: PetalWidth = response, 
#' continuous. 
#' Species = explanatory, categorical. 
#' PetalLength = explanatory, continuous. </span>
#' 
#' We have given code to run a model for the data below. 
#' Think about what type of model is being
#' run? It is good practice to consider if you would have chosen the same one.
#' 
#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
# Model 1
model1 <- lm(PetalWidth ~ Species+PetalLength, data = irisdata)

coef(model1)

confint(model1)

#' 
#' <span style="color:blue"> 3. Interpret the output of the model. 
#' What does it tell you
#' about the effect of petal length on petal width? (5 marks) </span> 
#' 
#' <span style="color:purple"> 1 mark for: Intercept is intercept of line 
#' (Y value when X (petal length) = 0) for 
#' Species = color.
#' 1 mark for: effect of Petal length is positive 
#' (longer petals are also wider, `r round(coef(model1)[4],2)`cm (95% CI 
#' `r round(confint(model1)[4,1],2)` to `r round(confint(model1)[4,2],2)`) 
#' wider for every 
#' 1 cm length). 
#' 1 mark for: this petal length effect is the same for all species. 
#' 1 mark for: Species Setosa seems to have a lower
#' petal width than Species color (`r round(coef(model1)[2],2)`cm, 95% CI
#' `r round(confint(model1)[2,1],2)` to `r round(confint(model1)[2,2],2)`) 
#' but Species Virginica is higher 
#' (`r round(coef(model1)[3],2)`cm, 95% CI
#' `r round(confint(model1)[3,1],2)` to `r round(confint(model1)[3,2],2)`) 
#' 1 mark for:
#' the confidence intervals do not cross 0 for the species effects, 
#' so the direction seems clear.
#' **To get each mark you should mention confidence intervals and 
#' the coefficient estimate.** </span>
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
#' <span style="color:blue"> 5. Are the assumptions met? 
#' Reference which plot you use to decide and
#' why you make the choice. (6 marks) </span>
#' 
#' <span style="color:purple"> 3 marks for: Not quite - equal variance, 
#' tested using residuals vs fitted plot
#' and there is different variance for different species. 
#' Also 3 marks for: Not quite - normality of residuals, tested
#' using normal qq, quite a lot of deviation at edges. </span>
#' 
#' <span style="color:blue"> 6. What other plot might you also want for checking 
#' assumptions? (1 mark) </span>
#' 
#' <span style="color:purple"> 1 mark: cook's distance. Scale and leverage could 
#' also be accepted but would be unexpected as an answer. </span>
#' 
#' Here is code for another model on the same data.
#' 
#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
# Model 2
model2 <- lm(PetalWidth ~ Species*PetalLength, data = irisdata)

coef(model2)

confint(model2)

#' 
#' <span style="color:blue"> 7. How is this model different to the first one? (1 mark) </span>
#' 
#' <span style="color:purple"> 1 mark: the model now has an interaction between
#' petal length and species. </span>
#' 
#' <span style="color:blue"> 8. Given the new model, does this change your
#' interpretation of the effect of petal length on petal width? Why? (4 marks) </span>
#' 
#' <span style="color:purple"> 1 mark for: yes. 
#' 1 mark for: Species Setosa no longer
#' has a clear effect as the confidence intervals now span 0. 
#' 1 mark for: the effect of petal length
#' and Species Virginica still seem very similar and in same direction. 
#' 1 mark for:
#' Species Virginica also seems to have an interaction, 
#' the effect of petal length
#' on petal width is weaker for this species. 
#' But still positive. **Important to know
#' here that the interaction term tells you a difference in the slope 
#' of the effect of petal length on petal width and include confidence intervals 
#' and
#' coefficient estimates in the answer.** </span>
#' 
#' <span style="color:blue"> 9. Which model do you prefer, why? (3 marks) </span>
#' 
#' <span style="color:purple"> 1 mark for: I prefer model 2. 
#' 1 mark for: because it had
#' clearer estimates of some effects. 
#' 1 mark for: model 2 showed that there
#' is an interaction for Species Virginica so it should be included in the model. </span>
#' 