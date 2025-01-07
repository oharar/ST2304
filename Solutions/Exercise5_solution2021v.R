#' # Exercise 5: Alien escape
#' # SOLUTION
#' 
#' ### Instructions:
#' 
#' This document contains information, questions, R code, and plots.
#' 
#' **Hints and reminders are bold**
#' 
#' <span style="color:blue"> Questions appear in blue. </span>
#' 
#' <span style="color:purple"> Answers are in purple. </span>
#' 
#' **Needs to be completed and handed in by 5th March 23:59**
#' 
#' -----
#' 
#'  ### Rationale:
#' 
#' This week you will be looking at multiple regression, working out when
#' to use it, practicing how to use it, and interpreting results.
#' 
#' -----
#' 
#' ### R hints and general help
#' 
#' ## Resources:
#' 
#' * Google (or any other search engine) e.g. "multiple regression in R"
#' * [Week 7 lectures](https://www.math.ntnu.no/emner/ST2304/2021v/Week07/)
#' 
#' ## R this week:
#' 
#' New functions:
#' 
#' * `pairs()` a function to plot all scatterplots of each variable in your data against the others in paired plots.
#' It takes the argument `x`, which is your data object. e.g. `pairs(YOURDATA)`
#' * `plotting_function()` a function we have created for you to plot your predictions on a map. 
#'  It takes the arugments:
#' `InputData` and `Predictions`. 
#' 
#' Things to remember:
#' 
#' * `lm()` takes the arguments x, y, and data e.g. `lm(y ~ x, data = YOURDATA)` x and y here correspond to an 
#' explanatory variable (x) and a response (y)
#' * `plot()` and `abline()` 
#' * `predict()`
#' * `confint()` and `coef()`
#' * `par(mfrow=(NumberRows, NumberColumns))` to make more than one plotting window.
#' * `summary(YourModel)$r.squared`
#' * code to make diagnostic plots for model checking e.g.  residuals vs fitted, normal Q-Q, and cook's distance.
#' 
#' ### A bit more on the `predict()` function
#' 
#' You have been using the `predict()` function for a few weeks now, 
#' so it seemed like a good time to have a quick
#' recap of how the function works.
#' 
#' The main arguments we use for predict are: `object` = YourModelObject, 
#' `newdata` = a dataframe of data you want
#' to predict for, and `interval` = either prediction or confidence.
#' 
#' A few comments on the arguments:
#' 
#' * `object`. The model objects you have used to predict with so far have been 
#' created using the `lm()` function. You know there are two ways
#' to write an model using `lm()`, `lm(y ~ x, data = YOURDATA)` and 
#' `lm(YOURDATA$y ~ YOURDATA$x)`. Some of you might have noticed that
#' if you use predict without a `newdata` argument, it does not matter how 
#' you wrote your `lm()`. **But** if you do
#' need to use a `newdata` argument, then predict only works correctly if you 
#' wrote your `lm()` like this: `lm(y ~ x, data = YOURDATA)`.
#' This is because the column names in `newdata` need to perfectly 
#' match those in the model formula (`y~x`) for predict to work.
#' When you write your formula with a `$` then this is how R sees the 
#' variable name e.g. it is YOURDATA$x not x. By splitting out
#' the column names from the data, it makes it clear to R what the variables 
#' are called and then it can match those to what
#' you give it in `newdata`.
#' * `newdata`. This week you will start using `newdata` with more than one 
#' column. When you do multiple regression, you have
#' multiple explanatory (x) variables. Therefore, when you predict, you need to 
#' include values for all of your x's, not just one.
#' Many of you have already been creating `newdata` using
#'  `<- data.frame(x=ValueYouWantToPredictFor)`, now you will need to include
#' two columns in these dataframes. The `data.frame()` function tells R to 
#' create a dataframe, the bits inside the brackets tell it
#' what you want inside. The column name goes on the left, = in the middle, 
#' and the values for the column on the right. An
#' example of a two column `newdata` 
#' `newdata <- data.frame(x = 1:5, x2 = 6:10)`. 
#' **Remember the column names in newdata should 
#' match your x variable names.**
#' 
#' -----
#' 
#' ## The challenge: Will we be taken over by aliens?
#' 
#' It is a time in the near future, space travel has become more common.
#' Ships leave from earth every few months on exploration missions of the
#' galaxy. Ship "Explorer 5" has been searching to see if any life on other planets 
#' could be useful on earth.
#' During the mission the crew
#' found a planet with life and collected some organisms. The alien organism 101
#' is a multicoloured, modular, plant-like organism (the plant-animal divide is
#' less clear for aliens). The organism seems to grow well in air similar to earth's
#' atmosphere and could be useful as it grows flower-like structures made of gold.
#' 
#' ![Alien picture](Alien101.png)
#' 
#' On their way back to earth the scientific crew of "Explorer 5"
#' have been conducting experiments on alien 101
#' to try and determine the conditions under which it grows best. 
#' The scientists grew alien 101 in containers under different temperatures (ºC)
#' and rainfall (mm) conditions. They
#' recorded the weight of biomass (g) of alien 101 per square metre after
#' one week.
#' 
#' Unfortunately, on the journey back, the ship hit a satellite and has 
#' crash landed on Australia. The crew were able to survive, 
#' but the experimental lab has been badly damaged. Alien 101 has been
#' released into the wild on earth. 
#' 
#' You are a local team of scientists assisting the space travel company
#' with predicting how the organism might spread. The company want
#' to know where to focus their containment resources to stop the organism
#' from taking over. Even if it could be useful, they don't know how it
#' could damage earth's wildlife.
#' 
#' <span style="color:blue"> **It is your job to predict where
#' the alien organism will spread to and recommend how to deploy resources** </span>
#' 
#' -----
#' 
#' 
#' The data can be found at https://www.math.ntnu.no/emner/ST2304/2021v/Week07/Alien101LabData.csv
#' 
#' The first step is to import the data and assign it to an object. You can use the whole
#' web link above to import the data. It is a csv file with column names (header) included.
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
Alien <- read.csv("https://www.math.ntnu.no/emner/ST2304/2021v/Week07/Alien101LabData.csv", 
                     header=TRUE)

#' <span style="color:blue"> A1. Plot the data using the `pairs()` function. 
#' What can you tell from the plots?</span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
pairs(Alien)

#' <span style="color:purple"> The plot shows the paired relationships between
#' the three variables: temperature, rainfall and biomass. It looks like they
#' are have relationships with each other. Temperature and rainfall have a negative
#' relationship (it looks like it anyway), rainfall and biomass seems negative too,
#' but temperature and biomass look positive. </span>
#' 
#' <span style="color:blue"> A2. Have a look at the raw data from Explorer 5, do you think 
#' rainfall and temperature were tested separately in their experiments? Why/why not? </span>
#' 
#' <details><summary>Hint</summary>
#' Here you need to look at the raw data i.e. 
#' the data frame you have. `head()` is one function
#' you can use to do this. or click on the data object in the Environment panel
#' on the top left. 
#' 
#' You need to think about how the way data are collected relates to the way it 
#' is presented.
#' For instance, if you tested temperature and rainfall separately, 
#' how would you write this
#' into a table? 
#' How many columns would you have? What would each row represent?
#' 
#' If you aren't sure, pretend you are designing the experiment from the beginning
#' and make a draft of the table you would use. 
#' 
#' </details>
#' 
#' </br>
#' 
#' <span style="color:purple"> If you look at the raw data i.e. the numbers in the actual
#' data frame, you will see that both temperature and rainfall are in 
#' each row, with only one biomass measure. So, it seems both rain and temperature
#' were changed at the same time in the experiments. If they had not been
#' you would expect one to be held fixed while the other changed or to be 0. </span>
#' 
#' <span style="color:blue"> A3. **Run** a simple linear regression for each of 
#' i) temperature and biomass,
#' ii) rainfall and biomass. Give the coefficient estimates (of each parameter), 
#' the confidence intervals, and
#' say how much variance each model explains. </span>
#' 
#' **Remember to put the explanatory variable and the response in the right 
#' places, you might
#' need to think about which is which.**
#' 
#' <details><summary>I've forgotten how to do this!</summary>
#' 
#' use `lm()` for the model, `coef()` to see the estimates, `confint()` to 
#' get confidence intervals, 
#' and `summary()$r.squared` to see how much variance is explained.
#' 
#' If none of this is familiar - look over the exercises from previous weeks!
#' 
#' </details>
#' 
#' </br>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE

ModelTemp <- lm(Biomass ~ Temperature, data = Alien)
ModelRain <- lm(Biomass ~ Rainfall, data = Alien)

coef(ModelTemp)
coef(ModelRain)

confint(ModelTemp)
confint(ModelRain)

summary(ModelTemp)$r.squared
summary(ModelRain)$r.squared

#' 
#' <span style="color:purple"> The coefficient estimates and confidence intervals
#' are listed above. The R squared tells us how much variance is explained, which
#' is 63% for the temperature model and 29% for the rainfall model. </span>
#'  
#' <span style="color:blue"> A4. **Interpret** the results of the separate 
#' linear regressions.
#' What do the results suggest about the ideal conditions for alien 101? </span>
#' 
#' <span style="color:purple"> Separately the models suggest a positive relationship between
#' biomass and temperature (confidence intervals do not span 0) and a negative relationship
#' between rainfall and biomass (confidence intervals also do not span 0). As the confidence intervals
#' of each relationship do not span 0, then no relationship is outside of the 
#' plausible range for the true population parameter. **We cannot say anything about
#' how probable it is that the true parameter = any number.** Only that when we including 
#' uncertainty, we still get 
#' relationships in the estimated direction. The effect of temperature is between
#' `r round(confint(ModelTemp)[2,1],2)` grams per degree and `r round(confint(ModelTemp)[2,2],2)` 
#' grams per degree. The effect of rainfall
#' is between `r round(confint(ModelRain)[2,1],3)` grams per mm and 
#' `r round(confint(ModelRain)[2,2],3)` grams per mm. 
#' Altogether this suggests that the ideal conditions
#' for alien 101 are hot and dry (these conditions would increase biomass). </span>
#' 
#' You now have some results about how temperature and rainfall (individually) influence the
#' growth of alien 101. But you want to generate predictions of how it could spread around
#' Australia. You can do this by creating predictions of the amount of biomass for the
#' actual average annual temperature and rainfall of Australia. The company have provided
#' you with very simplified data of the average annual temperature and total annual rainfall
#' for different coordinates in Australia. Found at:
#' https://www.math.ntnu.no/emner/ST2304/2021v/Week07/AustraliaEnvironmentalData.csv
#' 
#' This is plotted below.
#' 
#' Using this you can try to guess where the alien might spread to based on what you
#' have found out about the influence of temperature and rainfall on alien 101's growth.
#' **Assume that
#' amount of biomass is an indicator of how suitable the conditions are for alien 101.** 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = FALSE

library(tidyverse)
library(ggplot2)
library(patchwork)

Australia <- read.csv("https://www.math.ntnu.no/emner/ST2304/2021v/Week07/AustraliaEnvironmentalData.csv", 
                      header=TRUE)

Rain <- ggplot(Australia)+
  geom_raster(aes(x=Longitude, y=Latitude, fill=Rain))+
  theme_void()+
  scale_fill_viridis_c(begin=1, end=0, name ="Rainfall (mm)")+
  theme(legend.position = "top")

Temp <- ggplot(Australia)+
  geom_raster(aes(x=Longitude, y=Latitude, fill=Temp))+
  theme_void()+
  scale_fill_viridis_c(option="B", name="Temperature (ºC)")+
  theme(legend.position = "top")

Rain + Temp + plot_layout(nrow=1)

#'
#' While guessing is ok. It would be better to predict actual numbers based on
#' our linear models. To do this, you will need to:
#' 
#'  * Import the Australia environmental data.
#'  * Use the `predict()` function to generate the predictions.
#'  
#' <span style="color:blue"> A5. **Predict** biomass of alien 101 across Australia. </span>
#' 
#' To make the predictions you will use the data on environmental conditions in Australia. 
#' These will become your new explanatory variable (x) values that you want to predict 
#' a response (y) from. 
#' 
#' 
#' You need to create an object to be your `newdata` argument for the predict function.
#' `data.frame()` is a good function to use for this. 
#' You will need two because you have two different models, one with temperature
#' and one with rainfall. 
#' 
#' Remember that the column names in `newdata` must be the same as those used
#' to make your model. 
#' 
#' **If it doesn't work - check your `lm()` is written as described in the start of this document.**
#' 
#' 
#' **Make sure to read all comments.** You may need to change the name
#' of the datafile to match what you called the Australia data.
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# First thing for the prediction is to create the newdata object
# you will need as an argument for the predict() function.

# Here you need two, one for temperature and one for rainfall.
# Newdata needs to be a datafame so the function data.frame() is very
# useful here. My data is called Australia and my models are ModelTemp
# and ModelRain.
# Column names in newdata MUST be identical to those in the
# data used to make the model (the lab data). 

# newdata for temperature (it makes a dataframe with a column called 
# Temperature from the Temp column in the Australia data)
newdataTemp <- data.frame(Temperature = Australia$Temp)

# newdata for rainfall
newdataRain <- data.frame(Rainfall = Australia$Rain)

# now predict - this should be familiar now
PredictionsTemp <- predict(ModelTemp, newdata = newdataTemp, 
                           interval="prediction")
head(PredictionsTemp)

# now predict - this should be familiar now
PredictionsRain <- predict(ModelRain, newdata = newdataRain, 
                           interval="prediction")
head(PredictionsRain)

#' 
#' </br>
#'
#' Now you have predictions for the biomass in g/m<sup>2</sup> for the whole of Australia.
#' If you look at the results, the numbers are very large. This is because biomass is in grams.
#' From looking just at the numbers it is difficult to interpret these predictions. 
#' 
#' Another member of your team (Emily) has created a function in R 
#' to plot the predicted results back onto the map of Australia. It is quite a complex
#' function and uses the package `ggplot2`. You can google this if you want to learn more
#' but it can make some very pretty plots!
#' 
#' To use the function, first install the following packages using `install.packages()`:
#' 
#' * "ggplot2"
#' * "viridis"
#' 
#' Then load them using the `library()` function. e.g. `library(ggplot2)`
#' 
#' The plotting function is available here:
#'  https://www.math.ntnu.no/emner/ST2304/2021v/Week07/plotting_function.R. 
#'  You can bring the function into your R by using the `source()` function.
#' 
#' ### What does source really do?
#' 
#' The `source()` function is something we can use to run all of the code in an R script. We use this
#' in this course to provide you with functions that we write ourselves. It is a bit like
#' bringing in data, but instead of data, it is R code. Have a go at sourcing the script for this 
#' week using `source()` and using the url above as the argument. 
#' You should see that in the right hand Environment panel you now have a function
#' called `plotting_function()`, it is ready to use. 
#' 
#' `plotting_function()` was written by Emily, it takes two input arguments:
#' `Input_data` which is the data for
#' Australia and `Predictions` which is your prediction object.
#' 
#' <span style="color:blue"> A6. Plot your predictions using `plotting_function()`. </span>
#' 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# source the script
source("https://www.math.ntnu.no/emner/ST2304/2021v/Week07/plotting_function.R")

plotting_function(InputData = Australia, Predictions = PredictionsRain[,1])

plotting_function(InputData = Australia, Predictions = PredictionsTemp[,1])

#' 
#' <span style="color:blue"> A7. Based on these results where would you
#' recommend the company focusses its containment resources? 
#' Include some mention of uncertainty in your answer. </span>
#' 
#' <span style="color:purple"> The temperature predictions suggest 
#' high biomass only in the very
#' north of Australia. This would suggest only a small number of resources
#' needed in the this area.
#' The rainfall predictions suggest high biomass in the very middle of Australia.
#' To combine you could have resources in the middle and only in the very
#' north. The rest of the island has low predicted biomass so does not need 
#' high resources. </span>
#' 
#' -----
#' 
#' ## Part B: Multiple regression
#' 
#' Your team have made some predictions based on using temperature and rainfall
#' separately. But you know what you can put both variables into a combined
#' multiple regression model. So you now decide to do this.
#' 
#' <span style="color:blue"> B1. **Write** a line of R code (and run it) to run 
#' a multiple linear regression including temperature **and** rainfall. </span>
#' 
#' If you are not sure how to do this, have a look at question C3 from Exercise 4. 
#' In Exercise 4 you added a quadratic term to your model, this was actually 
#' creating a type of 
#' multiple regression. You add any other explanatory variables in the same way, 
#' but without
#' `I` or `^2`. 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
ModelBoth <- lm(Biomass ~ Temperature+Rainfall, data = Alien)

coef(ModelBoth)

confint(ModelBoth)

summary(ModelBoth)$r.squared

#' <span style="color:blue"> B2. **Write** the multiple linear regression as
#' a mathematical equation? E.g. Y=.... **Hint: think about how this is different to a simple regression** </span>
#' 
#' <span style="color:purple"> You can write it as matrices or as a sum of all 
#' betas multiplied by all Xs.
#' Writing as a matrix is neater and easier. It will also be useful for other
#' linear models. e.g. $Y_i$ = $\alpha$ + $\beta_1$*$x_1i$ + $\beta_2$*$x_2i$ or 
#' $Y$ = $X$$\beta$ + $\epsilon$ (matrix) </span>
#' 
#' <span style="color:blue"> B3. **Compare** the estimates from the multiple regression
#' to those from the individual regressions. **Include coefficients,
#' confidence intervals, and R squared** </span>
#' 
#' Remember you can also plot the results. To do this you should plot each
#' variable against biomass separately i.e. biomass against temperature and
#' biomass against rainfall but you will need to adjust the line you are 
#' plotting with the mean value of the other variable. 
#' 
#' To do this, you can still use `abline()`
#'
#' <span style="color:purple"> The estimate values have changed a bit. They are
#' still in the same direction as before. The mean estimate for temperature
#' stayed the same at `r round(as.numeric(coef(ModelBoth)[2]),2)` but the 
#' uncertainty decreased from `r round(as.numeric(confint(ModelTemp)[2,]),2)` 
#' to `r round(as.numeric(confint(ModelBoth)[2,]),2)`. 
#' The effect of rainfall decreased `r round(as.numeric(coef(ModelRain)[2]),3)` 
#' to `r round(as.numeric(coef(ModelBoth)[3]),3)` and the uncertainty still does
#' not span zero. (Confidence intervals = `r round(as.numeric(confint(ModelBoth)[3,]),4)`. 
#' The R-squared
#' has not increased much from the individual models 
#' (`r round(summary(ModelBoth)$r.squared,2)` for joint and `r round(summary(ModelTemp)$r.squared,2)` 
#' for temperature model), 
#' it is certainly not the sum of the two.
#' The two variables do not explain completely different variation. </span>
#' 
#' 

#+ warning = FALSE, error = FALSE, eval = TRUE, echo = TRUE
# EXAMPLE:

# make two plot window
par(mfrow=c(1,2))

# make the plot of temperature and biomass
plot(Alien$Temperature, Alien$Biomass,
     pch = 19, col = "red",
     ylab = "Biomass", xlab = "Temp")

# save the coefficient values as an object
# contains 3 values, intercept, slope for temperature,
# slope for rainfall
coefs <- coef(ModelBoth)

# use the coefficients to plot the line for temperature
abline(a=coefs[1]+(mean(Alien$Rainfall)*coefs[3]), b=coefs[2], col="black")

# make the plot of Rainfall and biomass
plot(Alien$Rainfall, Alien$Biomass,
          pch = 19, col = "blue",
          ylab = "Biomass", xlab = "Rain")

# save the coefficient values as an object
# contains 3 values, intercept, slope for temperature,
# slope for rainfall
coefs <- coef(ModelBoth)

# use the coefficients to plot the line for temperature
abline(a=coefs[1]+(mean(Alien$Temperature)*coefs[2]), b=coefs[3], col="black")

#'
#' <span style="color:blue"> B4. **How** does R find the estimates of the
#' parameters for these linear regressions? **Hint: think back to earlier weeks - its the same** </span>
#'
#'<span style="color:purple"> Maximum likelihood, we are still using it here. </span>
#' 
#' <span style="color:blue"> B5. **Perform** model checking of the multiple linear
#' regression. **Include 3 plots of model checking and decide if you think the fit is ok** </span>
#' 
#' **Hint: code available in Exercise 4**
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# Take rounded fitted and residual values from the multiple regression
# model
BothFitted <- round(fitted(ModelBoth),2)
BothResiduals <- round(residuals(ModelBoth),2)

# Plot the fitted versus residuals 
plot(BothFitted, BothResiduals)
# add a horizontal line at 0
# line is grey and dashed (lty=2)
abline(h=0, lty=2, col="grey")

# Plot a normal Q-Q plot
qqnorm(BothResiduals)
qqline(BothResiduals)

# Plot Cook's distance
plot(ModelBoth, which=4)

#'<span style="color:purple"> I am happy with the fitted versus residuals. 
#'It seems to
#' meet the linearity assumption, there is a slight funnel shape. But not very
#' strong. The Normal QQ shows that it
#' also meets the normality assumption well - much better than we would usually 
#' expect for biological data.
#' The Cook's distance identifies 3 outliers, but these are not
#' far from the rest of the data points and do not seem to have
#' a big influence so I would not remove them. I would not
#' find a strong reason to do so. They do not seem to be typos. </span>
#' 
#' Now you have checked your model and hopefully are either happy with it or have
#' fixed any assumptions that were violated. So now you can generate new predictions
#' of biomass in Australia based on the multiple regression model. **This is
#' still not the full picture, remember to look at the uncertainty too. You 
#' can do this by entering the upper or lower prediction as the `Predictions` 
#' argument in `plotting_function()`.** To do this you need to use indexing like
#' last week. The lower confidence interval is column 2 in the output
#' from the `predict()` function 
#' e.g. 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval=FALSE
Predictions <- predict(YOURMODEL, newdata=newdata, interval="prediction")
Predictions[,2] # this bit gives the second column
#' 
#' <span style="color:blue"> B6. **Predict** where the organism will spread to,
#' based on the multiple regression. Plot it using `plotting_function()`. </span>
#' 
#' **Hint: You can do this by editting the prediction code above so it predicts
#' from the multiple regression. Now you also only need one 
#' `newdata` object with two columns.**
#' 
#' 
#' Try editting the code from A5 and A6
#' 
#' Australia = dataset of environmental conditions in Australia. 
#' 
#' This shows code to plot the mean predictions. To check how uncertainty changes
#' the patter you can change which column of the object `PredictionsBoth` that
#' you plot.
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
PredictionsBoth <- predict(ModelBoth, newdata = data.frame(Temperature = Australia$Temp,
                                                           Rainfall = Australia$Rain), 
                           interval="prediction")

#+ warning = FALSE, error = FALSE, include = TRUE, echo = FALSE
plotting_function(InputData = Australia, 
                             Predictions = PredictionsBoth[,1])

#' </details>
#'
#' <span style="color:blue"> B7. **What** would you recommend in terms of deployment of
#' resources based on your predicted spread? Has this changed from before?</span>
#'
#' <span style="color:purple"> Hopefully it has now changed from focussing purely 
#' on the very north and
#' middle to now focussing less on the middle and more on only the north of Australia.
#' The multiple regression gives a slightly different picture than
#' having two individual regression analyses. The pattern is similar
#' but the multiple regression predicts a smaller area of highest biomass,
#' but covering more of the north.
#' It seems rain is not so important when we also include temperature,
#' this is because we are uncertain about the direction of rainfall effect. 
#' Without the multiple regression you might have incorrectly predicted how many
#' resources to use and where. You might have wasted a lot of money, or done
#' a bad job by trying to cover too big an area but missing others. </span>
#' 
#' -----
#' 
#' ## Part C: Reflection
#' 
#' <span style="color:blue"> C1. Can you draw any biological conclusions about this
#' alien species? Can you say anything about the relative influence of 
#' temperature and rainfall? Are the variables in this analysis enough, what else could
#' have an influence? </span>
#' 
#' <span style="color:purple"> Alien 101's growth is influenced by both temperature and rainfall.
#' The effects are in opposite directions. The alien grows best in warm
#' but dry conditions. We would expect it to spread in desert like conditions.
#' But the effect of rainfall is not certain in direction.
#' Could draw any links to biology we know about deserts. 
#' We cannot say much from this analysis about the relative influences
#' of temperature and rainfall, because they have been measured on very
#' different scales. Therefore the beta (slope) values are not directly
#' comparable. If we were to standardise our explanatory variables to remove
#' the units (divide by standard deviation and center on the mean) 
#' then we could do a comparison. </span>
#' 
#' <span style="color:purple"> We could include any other variables that are biologically sensible e.g.
#' herbivory, nutrients, human pressure, pollution. Basically what we
#' have is not enough for a complete picture but it is a good
#' start. We are also missing any explicit consideration of space or connectivity.
#' We do not assume that any areas are similar beyond rain or temperature.
#' Nor do we look at where the space station crashed and any barriers to 
#' disperal. These could all be added. This is not a full list of all possible additions.
#' Other ideas could be right too, this is just an example. </span>
#' 
#' <span style="color:blue"> C2. **Why** do we use multiple regressions rather
#' than running individual simple linear regressions? </span>
#'
#' <span style="color:purple"> Rarely does a single variable influence something. 
#' This could be the case for carefully designed experiments. But often many variables influence our observations and
#' we should take account of these. As we saw here, it can change
#' what we would predict and the conclusions we can draw.
#' But we don't want to include everything. In this case, because we know that
#' both temperature and rainfall were changed at the same time in the 
#' experiment, we should include them both in our model. They did not
#' occur in isolation.
#' Models should still be a simplification of reality. More on this in model selection. </span>
#' 
#' -----
#' 
#' ## Part D: Feedback
#' 
#' <span style="color:blue"> D1. How do you think this exercise went? What do
#' you think your group did well, what are you less sure about? 
#' (2 examples of each) </span>
#' 
#' <span style="color:blue"> D2. What do you think you improved from last 
#' week? </span>
#' 
#' <span style="color:blue"> D3. Are there any concepts you are very unsure of? </span>
#' 
#' <span style="color:blue"> D4. What would you like feedback on this 
#' week? </span>
  
