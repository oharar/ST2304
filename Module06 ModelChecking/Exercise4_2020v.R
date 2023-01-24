#' # Exercise 4: Numbers of police
#' 
#' ### Instructions:
#' 
#' This document contains information, questions, R code, and plots.
#' 
#' **Hints and reminders are bold**
#' 
#' <span style="color:blue"> Questions appear in blue. </span>
#' 
#' 
#' **Needs to be completed and handed in by 25th February 23:59**
#' 
#' ### Rationale:
#' 
#' To complete this exercise you will need to use:
#' 
#' * The theory you have been taught in the
#' lectures
#' * The R code you have been given in the intro
#' * The R help pages and google
#' * Some biological knowledge
#' * A bit of creativity and problem solving
#' 
#' **This week the exercise looks at how model selection can be important for
#' fitting the data, and drawing the right conclusions.** 
#' 
#' -----
#' 
#' ### R hints and general help
#' 
#' ## Resources:
#' 
#' * P60-65 in 'The New Statistics with R'
#' * Google (or any other search engine) - especially "diagnostic plots in R"
#' * [Week 6 lectures](https://www.math.ntnu.no/emner/ST2304/2020v/Week06/)
#' 
#' ## R this week:
#' 
#' New functions:
#' 
#' * `qqnorm()` this is a function that produces a normal QQ plot (quantile-quantile). It
#' plots the quantiles from your data (here residuals) against those of a theoretical perfect normal distribution.
#' * `qqline()` this function adds a line to the plot from `qqnorm()` of 1 to 1 match between
#' your data (residuals) and the theoretical normal distribution. 
#' * `plot(YOURMODEL, which=4)` this produces a cook's distance plot. `plot(YOURMODEL)` by 
#' default produces 4 diagnostic plots. Using the `which=4` argument just gives us the 4th one.
#' * indexing: to take only parts of an object in R, we use `[ ]`. If that object has two dimensions e.g.
#' rows and columns, we can take part of it like this: OBJECT[row number, column number] 
#' * `MASS::boxcox()` this code uses the `boxcox()` function within the MASS package. It can
#' be used to plot the log-likelihood for parameters based on different power transformations. 
#' * extracting R-squared: `summary(model)$r.squared`
#' 
#' Things to remember:
#' 
#' * `lm()` takes the arguments x, y, and data e.g. `lm(y ~ x, data = YOURDATA)` x and y here correspond to an 
#' explanatory variable (x) and a response (y)
#' * `plot()` and `abline()` **hint: you can use `abline(a=intercept, b=slope)` OR `abline(YOURMODEL)`**
#' * `install.packages()` **if you can't remember go [here](https://www.math.ntnu.no/emner/ST2304/2020v/RIntro/R_tutorial.html)** Part F.
#' * `predict()` **hint: remember to use `lm()` with a data argument**
#' 
#' -----
#' 
#' ## The challenge: How many police officers do you need?
#' 
#' You are part of the police headquarters team in Chicago. 
#' It is predicted to be a very cold weekend, with an average temperature of
#' -10&deg;C. Obviously most of the police officers would like to make the most
#' of the cold weather by going skiing and ice skating. But you also need to 
#' keep the public safe. Previous research has shown that approximately 2 police
#' officers (they work in pairs) are needed for every 20 daily crimes. The Chief of Police
#' has asked your team to provide a recommendation for how many officers
#' they need for this cold weekend.
#' 
#' Luckily (as always) you already have data on temperature and crime numbers
#' that you can use.
#' 
#' <span style="color:blue"> **It is your job to find out how many
#' police officers you would recommend to be on duty on Saturday.** </span>
#' 
#' ![Politi](Politi.png)
#' 
#' -----
#' 
#' ## Part A: First recommendation
#' 
#' The data from last week's exercise can be found at
#'  https://www.math.ntnu.no/emner/ST2304/2019v/Week5/NoFreezeTempCrimeChicago.csv
#' 
#' The first step is to import the data and assign it to an object. You can use the whole
#' web link above to import the data. It is a csv file with column names (header) included.
#' 
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
NoFreeze <- read.csv("https://www.math.ntnu.no/emner/ST2304/2019v/Week5/NoFreezeTempCrimeChicago.csv", 
                     header=T)

YOURDATA <- NoFreeze

#' The next step is **to plot the data**, its good to remind yourselves what it looked like.
#' 
#' <details><summary>R hint</summary>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
# Plot the data, include x and y axis labels
plot(YOURDATA$TempC, YOURDATA$Crime, pch=16, 
     xlab="Temp (degree C)",
     ylab="Daily crime number", las=1)

# Run the regression model 
model <- lm(Crime~TempC, data=YOURDATA)

# Plot the regression line in dark blue
abline(model, col="darkblue")

#' </details>
#' 
#' <span style="color:blue"> A1. Using the linear regression model
#' from last week, **predict** the number of daily crimes for an average temperature of -10&deg;C.
#' Report the answer with prediction interval. </span>
#' 
#' **Hint1: you will need to use the predict function, 
#' create newdata, and predict with a prediction interval**
#' 
#' **Hint2: for the lm() you will need to use format lm(y ~ x, data = YourData)**
#' 
#' <details><summary>R hint</summary>
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
# run the model
model1 <- lm(Crime~TempC, data=YOURDATA)

# create newdata
newdata <- data.frame(TempC=-10)

# create the predictions
predictions <- predict(model1, newdata, interval="prediction")

#' </details>
#' 
#' <span style="color:blue"> A2. Based on this result
#' how many police officers would you recommend to be on duty?
#' **Explain** reasons behind your answer. Include prediction intervals. </span>
#' 
#' 
#' -----
#' 
#' ## Part B: Model checking
#' 
#' You are speaking to a colleague at lunch, discussing this 
#' project (because you are very excited about it). Your
#' colleague tells you they have some extra data. **They 
#' have mean daily crime numbers from temperatures under 0&deg;C.
#' This is just what you needed!**
#' 
#' You can find the new complete dataset here:
#' 
#' https://www.math.ntnu.no/emner/ST2304/2019v/Week6/TempCrimeChicago.csv  
#' 
#' It is important to import the data and **plot it**.
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
WithFreeze <- read.csv("https://www.math.ntnu.no/emner/ST2304/2019v/Week6/TempCrimeChicago.csv",
                       header=T)

YOURDATA <- WithFreeze

plot(WithFreeze$TempC, WithFreeze$Crime, pch=16, xlab="Temp (degree C)",
     ylab="Daily crime number", las=1)

#' 
#' <span style="color:blue"> B1. **Fit** a linear regression to the complete
#' dataset including days <0&deg;C </span> <span style="color:blue"> and **plot** the regression line. </span>
#' 
#' <details><summary>R hint</summary>
#' 
#' You should be able to do this using the code in Part A but change the 
#' dataset and the model.
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# Create a new model
model2 <- lm(Crime ~ TempC, data = YOURDATA)

# Repeat the plot above
plot(WithFreeze$TempC, WithFreeze$Crime, pch=16, xlab="Temp (degree C)",
     ylab="Daily crime number", las=1)

# Add the model line
abline(model2, col=2)

#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval=FALSE
# Create a new model
YOURMODEL <- lm(Crime ~ TempC, data = YOURDATA)

#' </details>
#' 
#' <span style="color:blue"> B2. Look at your plot of the data
#' and regression line. **What do you think** of the fit?
#' Are there any problems with using a straight line on this data? Do this just by looking.</span>
#' 
#' 
#' You have had a go at checking this model just by looking at the data
#' and the regression line. But there are some more thorough ways you can
#' explicitly check whether the model meets the assumptions of a linear regression.
#' 
#' The four graphs that statisticians typically use are called:
#' **Residuals vs fitted, Normal Q-Q, Residuals vs leverage, and cook's distance**
#' 
#' <span style="color:blue"> B3. Create a residuals vs fitted plot for
#' the linear model on all the data. **Interpret** the plot in terms of
#' model fit. </span>
#' 
#' Think about which assumption this plot assesses, what would you expect it to look
#' like if the assumption is met? and how does your plot look different? 
#' 
#' <details><summary>R hint</summary>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
# This is some code to help make a residuals vs fitted plot

# Step 1 take out the residuals from the model
# the model is the result of using lm()

# You can also round them
# create a vector of rounded residuals
CrimeResiduals <- round(residuals(model2),2)

# Step 2, take out the fitted values from the model
# create a vector of rounded fit
CrimeFitted <- round(fitted(model2),2)

# plot the fitted and residuals
# Fitted on X, residuals on Y
plot(CrimeFitted, CrimeResiduals)

# add a horizontal line at 0
# line is grey and dashed (lty=2)
abline(h=0, lty=2, col="grey")

#' </details>
#' 
#' <span style="color:blue"> B4. Create a Normal Q-Q plot for
#' the linear model on all the data. **Interpret** the plot in terms of
#' model fit. </span>
#' 
#' Again: Think about which assumption this plot assesses, what you expect it to look
#' like if the assumption is met, and how your data differs from what you expect.
#' 
#' 
#' <details><summary>R hint</summary>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
# use the residuals you calculated above 

# now create the Normal Q-Q plot 
qqnorm(CrimeResiduals)
# add the ideal line
qqline(CrimeResiduals)

#' </details> 
#' **You should be able to do this using R section at beginning of this document and the
#' code for Question B3**. 
#' 
#' <span style="color:blue"> B5. Plot and **Interpret** the Cook's
#' distance plot for your model. What does it tell you that the Residuals vs fitted and
#' Normal Q-Q plots did not? </span>
#' 
#' <details><summary>R hint</summary> 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
# Code to make cook's distance plot
# this is done by using the plot function (which would
# create 4 plots of the model) and choosing just the
# last one (which=4). A bit of a cheat to a nice plot.

plot(model2, which=4)

#' </details> **But try using the R help at the beginning of this document first!**
#'     
#' <span style="color:blue"> B6. Based on your assessments of the model
#' checking plots, **how** could you improve the model fit for
#' these data? (suggest at least 2 things you could do - try not to cheat) </span>
#' 
#' 
#' -----
#' 
#' ## Part C: 
#' 
#' <details><summary>Only open once you have answered B6.</summary>
#' 
#' The colleague that gave you the extra data has come back to see how you are 
#' getting on. They suggest that the main assumption not being met is linearity.
#' A straight line does not seem to capture the data because it is curved. There 
#' are also some outliers. The outliers are values of 900 and 1200 crimes per day for two cold days 
#' close to -20&deg;C
#' 
#' Your team decides to remove the outliers. You have reason to believe 
#' they might be typos.
#' 
#' <span style="color:blue"> C1. **What** are some positives and negatives of
#' removing outliers? e.g. What things should you consider when removing them? </span>
#' 
#' 
#' <span style="color:blue"> C2. **Remove** the outliers from your data and plot again. </span>
#' 
#' The outliers are at rows 4 and 7. You can combine these numbers using the function
#' `c(,)`. If you are unsure what to do, look at the R help section at the
#' beginning of this document or Google "how to remove rows in R" and see what you get.
#' 
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# Remove the outliers (my data was called WithFreeze)
# This is done by using indexing brackets [,]
# These work by searching inside a data set
# by row first then column e.g. [row,column]
# Here we take ALL columns, which is why it is blank.

# But we want to remove the rows of the outlier
# to do this we put - e.g. data[-row,]

#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE

NoOutliers <- WithFreeze[-c(4,7),]

plot(NoOutliers$TempC, NoOutliers$Crime, pch=16, xlab="Temp (degree C)",
     ylab="Daily crime number", las=1)

#' 
#' The data is still curved. So, you will want to use a transformation
#' of the response variable or a polynomial (square or cube etc). But which one?
#' 
#' You can use Box-Cox to indicate what
#' kind of transformation might help with improve the linear regression.
#' The plot shows the likelihood for different powers of transformation.
#' E.g. 2 is a squared transformation, 3 is cubic etc.
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval=FALSE
# You might need to install the package MASS
# install.packages("MASS")

# Run the boxcox function
# lambda = the power transformation
MASS::boxcox(YOURMODEL, lambda = seq(1,4, length=30))

#' Box-Cox suggests that a quadratic (x<sup>2</sup>)
#' transformation. You could either transform the response variable
#' OR add a quadratic term as an explanatory variable. You choose
#' to try the second and add the quadratic term. 
#' 
#' <span style="color:blue"> C3. **Add** the quadratic term to your model and run again. 
#' (show code for this answer) </span>
#' 
#' When you add a quadratic variable, remember to keep the original variable in
#' the model too!
#' 
#' <details><summary>R hint</summary>
#' 
#' To add a quadratic effect you need to use the following format:
#' 
#' I(variable\^2), where variable = your variable name e.g. TempC.
#' 
#' You then put this quadratic term after your original variable separated by a +.
#' e.g. `lm(Y~ X + I(X^2), data = YOURDATA)`
#' 
#' <details><summary>I'm still confused</summary>
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
# Create a linear model
# to add a quradtic (or any power) term you must right the
# explanatory variable as I(variable^2) AND keep in the original
# explanatory variable. See example below.
# We need both the linear and quadratic components.

model3 <- lm(Crime ~ TempC + I(TempC^2), data = NoOutliers)

#' </details>
#' </details>
#' 
#' <span style="color:blue"> C4. **Plot** a new residuals vs fitted plot for the new model from C3. </span>
#' 
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# Also plot the Residuals vs fitted
# create a vector of rounded residuals
CrimeResiduals2 <- round(residuals(model3),2)

# create a vector of rounded fit
CrimeFitted2 <- round(fitted(model3),2)

# plot the fitted and residuals
plot(CrimeFitted2, CrimeResiduals2)
# add a horizontal line at 0
# line is grey and dashed (lty=2)
abline(h=0, lty=2, col="grey")

#' 
#' <span style="color:blue"> C5. Look at the new Residuals vs Fitted plot.
#' **What** do you think of this new model? Has it improved the fit? </span>
#' 
#' 
#' </details>
#' 
#' 
#' <span style="color:blue"> C6. Now **predict** the number of crimes for a day of -10&deg;C 
#' from the new model from C3. 
#' Does this change your recommendation for the number of police needed? If so, **how**? </span>
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# create newdata (or use the same one you made at the beginning)
newdata <- data.frame(TempC=-10)

# create the predictions
predictions2 <- predict(model3, newdata, interval="prediction")

#' 
#' -----
#' 
#' ## Part D: Reflection
#' 
#' <span style="color:blue"> D1. Which model was better at explaining variation in crime? 
#' Work out the R squared for the models from 
#' Questions A1 and C3. **How much** of the variance
#' in daily crime numbers does each model explain? **Compare them.** </span>
#' 
#' The code to do this is at the beginning of this document. 
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# First calculate the R squared.
summary(model3)$r.squared

# The R squared is 0.86 so temperature explains 86% of the variance
# in daily crime rate. This is pretty high! So this model seems to 
# be doing pretty well at capturing what is going on with just one
# explanatory variable (temperature and its quadratic).

# If you compare to model2, the R squared there was 0.65.
# So adding in the quadratic term and removing 2 outliers lead to
# an extra 20% of the variance being explained. 

#' 
#' <span style="color:blue"> D2. Think about the biological context of the results.
#' **Why** could there be a quadratic relationship between daily crime numbers and
#' temperature? **How** could you try to find out what the reasons are? E.g.
#' new studies or data you would need. </span>
#' 