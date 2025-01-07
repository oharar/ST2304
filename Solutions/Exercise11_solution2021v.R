#' # Excerise 11: Investigating fraud
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
#' </br>
#' </br>
#' </br>
#' 
#' **In this exercise, the aim is to practice all of the modelling tools you
#' learnt in this course. It is indicated how these questions might relate to
#' an exam and how long an answer is required in each case. You can use the 
#' solution to grade your own work and then direct your revision.**
#' 
#' You can decide how much help you want. For each question there is a general hint
#' and an R hint (if applicable). 
#' 
#' In the exam you will not get hints. So, now is a time to practice without!
#' 
#' </br>
#' </br>
#' </br>
#' 
#' ### R this week:
#'  
#' Things to remember:
#' 
#' * `glm(Y ~ X1+X2, family=YOURFAMILY(link=YOURLINK), data=Data)`
#' * `anova(NullModel, AltModel, test = "LRT")` for confirmatory model selection
#' * `AIC(Model1), BIC(Model1)` for exploratory model selection
#' * `plot(HastingsModel, which=4)` # Cook's distance
#' * `plot(fitted,residuals)`  # residuals vs fitted
#' * `qqnorm(residuals)` # normal QQ plot
#' 
#' New this week:
#' 
#' * `dispersion <- deviance(Model)/df.residual(Model)` to check for overdispersion
#' * `summary(Model, dispersion = dispersion)` to correct for overdispersion
#' * `MASS:glm.nd(Y~X)` to run a negative binomial GLM
#' 
#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## The challenge: Investigating fraud in the bird world.
#' 
#' Back in 1962 some British birders (people who spend a lot of time looking at rare birds) 
#' suspected that a lot of observations from around [Hastings](https://en.wikipedia.org/wiki/Hastings) 
#' from betwen 1890 and 1930 were frauds.
#' John Nelder (co-inventor of GLMs) took a look at the data, 
#' and compared Hastings with two nearby areas.
#' 
#' This 'scandal' even has it's own wikipedia page. The link is hidden, 
#' try to solve the mystery yourselves before looking
#' at the answer.
#' 
#' <details><summary>Wikipedia</summary>
#' <p>
#' https://en.wikipedia.org/wiki/Hastings_Rarities
#' </details>
#' 
#' <span style="color:blue"> Your job is to find out whether there were more 
#' rare birds seen in Hastings
#' than in surrounding areas before 1925 compared to after 1925? </span>
#' 
#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## The data
#' 
#' You have data on:
#' 
#' * Year (1895 to 1954)
#' * Era (pre-1925 and after-1925)
#' * Area (Hastings, Sussex, Kent) These are three regions in the UK.
#' * Count: number of records (number of reports of a rare species: could be the same species at different times)
#' 
#' We are only looking at the rarest species, nationally categorised as "Very Rare". 
#' We have numbers observed
#' in nearby areas and the focus area (Hastings) from two time periods. 
#' **The 'pre' time period is when we suspect 
#' fraud might have occurred, the 'post' time period is after.**
#' 
#' Data can be found at:
#' https://www.math.ntnu.no/emner/ST2304/2019v/Week13/HastingsData.csv it is a .csv
#' file with column headers.
#' 
#' **Remember to add stringsAsFactors = TRUE**
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
HastingsData <- read.csv("https://www.math.ntnu.no/emner/ST2304/2019v/Week13/HastingsData.csv", 
                         header=TRUE, stringsAsFactors = TRUE)
head(HastingsData)

#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## Part A: Choosing an appropriate model
#' 
#' In this section, the aim is to practice explaining and justifying different modelling concepts.
#' 
#' Before you begin any analyses, you should to think about what steps you will need to conduct 
#' in order to reach a conclusion. This relates to the modelling process.
#' 
#' <span style="color:blue"> A1. What steps would you want to take to model the 
#' Hastings data? (at least 4 steps) (4 marks) </span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Think back to steps you've taken in previous analyses 
#' or look at Section G of last week's model for some prompts. 
#' 
#' </details>
#' 
#' <span style="color:purple"> **This question is asking for the general steps, 
#' that you would take for any data,
#' including this one.** </span>
#' 
#' One mark for each step up to 4 max
#' 
#' * <span style="color:purple">You begin with looking at our data and choosing an appropriate model.
#' This depends on our question and data. </span>
#' * <span style="color:purple">Then you fit the model and conduct model selection to find 'best' one, this could be 
#' confirmatory or exploratory, depending on your question.
#' Fitting the model involves maximum likelihood estimation of parameters.</span>
#' * <span style="color:purple">Then you check that the model meets assumptions and fits the data, how we assess this will depend on the
#' model and on the data. </span>
#' * <span style="color:purple">Then you can interpret the output and reach a conclusion.
#' Final step needs uncertainty, which is quantified as part of maximum likelihood 
#' estimation. </span>
#' 
#' <span style="color:purple"> Some of these steps are mixed together there are 
#' 6 steps in total here.</span>
#'  
#' <span style="color:blue"> A2. What are the response and
#' explanatory variable(s) and what data type are they, be specific? (4 marks) </span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Remember we have been introduced to several different data types. The 
#' main distinction is categorical/continuous. But continuous can be 
#' discrete or fully continuous. 
#' 
#' </details>
#' 
#' <span style="color:purple"> Response is number of records 
#' (discrete but continuous AND 
#' count data. They are not in categories)
#' Explanatory are Era and Area (both categorical) </span>
#' 
#' **1 mark for response and explanatory (2 total), 1 for the data type (of
#' each variable - 2 total)**
#' 
#' <span style="color:blue"> A3. What model would you use for this data and why? Include
#' which distribution you would choose for the random part and if you need a link function 
#' (say which if you do). (3 marks) </span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Think about all of the models we have covered in the course: distributions,
#' linear models, T-tests, generalised linear models. Which works best here? 
#' 
#' Then think about what distribution it is using for error.  
#' 
#' </details>
#'
#' <span style="color:purple"> Poisson GLM because the response is count data. 
#' We also know there is a
#' low mean value, so the counts cannot approximate to normal, 
#' choosing Poisson is important here.
#' The Poisson distribution reflects how this count data were generated.
#' The log link as it is the canonical link for Poisson and reflects counting effort. </span>
#' 
#' **1 mark for correct GLM type, 1 mark for reason for Poisson, 1 for correct link**
#' 
#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## Part B: Running the model
#' 
#' <span style="color:blue"> B1. How would you fit the model from A3 in R, 
#' write one line of code. (3 marks) </span>
#' 
#' <details><summary>General hint: if you are not sure about the model</summary>
#' 
#' If you were not sure you got A3 correct: the key thing is that the
#' response variable is count data. This will not be normally distributed in terms of 
#' error, so cannot be modelled with a linear model. We will need a GLM. A 
#' Poisson GLM with a log link is the appropriate chose for count data. 
#' 
#' </details>
#'  
#' <details><summary>R hint</summary>
#' 
#' The code you need to edit slightly is in the R section of this document.
#' 
#' </details>
#' 
#' **1 mark for correct function (glm), 1 for correct formula, 1 for correct 
#' family and link**
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
HastingsModel <- glm(Count ~ Area*Era, family="poisson"(link=log), data=HastingsData)

#' <span style="color:blue"> B2. Run the model in R (this would not be in the exam but it is 
#' helpful here). </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
coef(HastingsModel)
confint(HastingsModel)

#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## Part C: Model selection
#' 
#' <span style="color:blue"> C1. How can you test the hypothesis: 'there is an interaction 
#' between Era and Area' i.e.
#' Is the effect of area different before and after 1925? Include the name of the method
#' you would use and say why you picked that method. (3 marks) </span> 
#' 
#' <details><summary>General hint</summary>
#' 
#' The hypothesis mentioned above is asking if there is an interaction. This
#' is the same as asking which variables to include in a model. We have a
#' specific idea of which variables we think are important, we are not 
#' testing lots of different ones. We need to find if including the interaction
#' is needed and select the model that balances explanation and complexity.
#' 
#' </details>
#'  
#' <details><summary>R hint</summary>
#' 
#' The code you need to edit slightly is in the R section of this document.
#' You won't need all the code listed, so pick the right part!
#' It should take 2 lines.
#' 
#' </details>
#' 
#' <span style="color:purple"> Conduct model selection. 
#' In this case it is confirmatory because we have
#' a specific hypothesis of whether there
#' is an interaction between era and area. 
#' As the model is a GLM we do an analysis of deviance. </span>
#' 
#' **1 mark = confirmatory model selection, 1 mark = why, 1 mark = analysis of deviance**
#' 
#' <span style="color:blue"> C2. Run the method you chose for C1. 
#' What is your conclusion regarding the hypothesis? (again, in an exam you 
#' wouldn't run yourself in R) </span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Make sure to include support for your conclusion. Which part 
#' of the output did you use to make the choice?
#' 
#' </details>
#' 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
NullModel <- glm(Count ~ Area+Era, family="poisson"(link=log), data=HastingsData)
anova(NullModel, HastingsModel, test = "LRT")

#' <span style="color:purple"> The test was significant (p < 0.05) so we reject 
#' the null hypothesis and choose a model with an interaction</span>
#' 
#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## Part D: Model checking
#' 
#' <span style="color:blue"> D1. What are the assumptions of this model? (I count 6) (so 6 marks)</span>
#' 
#' <details><summary>General hint</summary>
#' 
#' These were listed in [the GLM module](https://www.math.ntnu.no/emner/ST2304/2021v/Week12/GLM_module11_2021.html) but can also be found on google. 
#' There are several different ways to write them. But I am looking for approx. 6. 
#' 
#' </details>
#'
#' <span style="color:purple"> GLM assumptions should focus around: 
#' Lack of outliers, Correct distribution used, Correct link function is used, 
#' Correct variance function is used, Dispersion parameter is constant, 
#' Independence of y. 
#' You could also say more on the variance assumptions here as we know we have a 
#' Poisson GLM i.e. that variance = mean in Poisson GLM, or no over- or under-dispersion. 
#' This last bit would get an extra point. </span>
#' 
#' **1 mark for each assumption up to 6**
#' 
#' <span style="color:blue"> D2. How can we test these assumptions? 
#' (I expect 4 checks) (again 4 marks) </span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Just list the different methods here with the assumptions they test. 
#' You run them in the next part.
#' 
#' They are not all plots.
#' 
#' </details>
#' 
#' <span style="color:purple">In plots such as residuals vs fitted, 
#' cook's d, and normal qq. Also need to check overdispersion with the deviance
#' ratio</span>
#' 
#' **1 mark for each plot/test**
#' 
#' <span style="color:blue"> D3. Run the checks in R for your preferred
#' GLM model. This should be decided by the outcome of C2 (again, wouldn't need to in an 
#' exam) </span>
#' 
#' <details><summary>R hint</summary>
#' 
#' All the necessary code is at the top of this document.
#' 
#' </details>
#' 
#' <span style="color:purple"> Answers in the R code </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# We can test lack of outliers with Cook's distance plot
plot(HastingsModel, which=4)

# We can test correct distribution and link from our knowledge of data
# We can also look at whether the deviance residuals have equal variance and seem normal
# use residuals vs fitted
# this also checks linearity
residuals <- residuals(HastingsModel)
fitted <- fitted(HastingsModel)
plot(fitted,residuals)

# and normal QQ
qqnorm(residuals)
qqline(residuals)

# We can test variance with overdispersion and deviance ratio
deviance(HastingsModel)/df.residual(HastingsModel)

#' 
#' <span style="color:blue"> D4. How good is the fit of the model based on your
#' checks? (5 marks) </span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Go through each check and determine if the assumption is met.
#' 
#' </details>
#' 
#' <span style="color:purple"> The model fit seems ok
#' but over dispersed. Normality (normal QQ), linearity (residual-fitted), 
#' outliers (cook's D), and variance equality (residual-fitted) all look ok.
#' But the model is overdispersed (deviance ratio).</span>
#' 
#' **1 mark for an overall answer (ok, bad, good), then 1 each for an assumption
#' checked and method.**
#' 
#' <span style="color:blue"> D5. Would you want to improve it? If so, how? (2 marks)</span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Think about which assumption wasn't met. How do you fix it? 
#' 
#' </details>
#' 
#' <span style="color:purple"> We should look at ways to deal with the overdispersion
#'  - like correct likelihood or negative 
#' binomial glm. </span>
#' 
#' **1 mark for yes/no, 1 mark for improvement suggestion**
#' 
#' <span style="color:blue"> D6. Try an improvement. (would not be in exam) </span>
#' 
#' <details><summary>R hint</summary>
#' 
#' The code for possible corrections is also included at the start of this 
#' document. 
#' 
#' </details>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
HastingsModelNB <- MASS::glm.nb(Count ~ Area*Era, data = HastingsData)
summary(HastingsModelNB)

# correct likelihood
dispersion <- deviance(HastingsModel)/df.residual(HastingsModel)
summary(HastingsModel, dispersion = dispersion)

#' 
#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## Part E: Conclusions
#' 
#' <span style="color:blue"> E1. Interpret the output (coefficient values) 
#' for your final model (the one you decided on in C2 and used in D). (14 marks) </span>
#' 
#' <details><summary>General hint</summary>
#' 
#' Focus on the size of effects and whether they have statistical support, i.e.
#' when we include uncertainty are we still clear about the direction? and
#' remember to include the correct uncertainty numbers in your answer. 
#' 
#' Deciding what this means for bird fraud is the next part.
#' 
#' </details>
#' 
#' <span style="color:purple"> **In all interpreting from GLMs - remember the link function!**
#' So, what you want to do to answer this questions is to look at the coefficient
#' estimates AND the confidence intervals to interpret. You will need to predict
#' the mean number of birds seen for each area back on the original scale, using 
#' the inverse of the log link - `exp()` (it 
#' will be easier to compare). You can also see if we are confident about the 
#' direction of each difference by looking at the confidence intervals. </span>
#' 
#' 
#' <span style="color:purple"> We can see that the mean of the 
#' afetr 1925 Hastings = exp(0.62) = 1.86, 
#' this is the intercept of the model (our contrast group).
#' To find the means of the other areas for the same time period we need to predict them using
#' the linear equation and the inverse of the link $Y_i = e^{(\alpha + \beta X_i)}$. 
#' We get: Kent = exp(0.62+-0.73) = 0.9
#' Sussex = exp(0.62+-1.04) = 0.66. So,
#' there do seem to be differences between the areas after 1925. None of the confidence
#' intervals cross 0 (-1.43 to -0.03 for Kent and -1.82 to -0.26 for Sussex) 
#' therefore even with uncertainty, the estimated direction of these differences
#' is the same. 
#' **Remember**
#' there was overdispersion, so we need to multiply the standard errors by the 
#' dispersion to get better confidence estimates (correcting likelihood) or use
#' a negative binomial model. The results here are for a corrected 
#' likelihood. The standard errors from the negative binomial model. </span>
#' 
#' <span style="color:purple"> 
#' If we look at the effect of Era for Hastings, 
#' we can see that pre 1925 had a higher number of rare birds.
#' This effect is 1.44 (SE 0.22) on the log scale, 
#' giving a mean of exp(0.62+1.43) = 7.77 for pre 1925.
#' This is MUCH higher than post 1925.
#' If we then look at the interaction, 
#' we can see that both Kent and Sussex have negative values
#' pre-1925, meaning they are lower than would be expected from an additive model.
#' We can work out the means of Kent in 1925 = exp(0.62+-0.73+1.43-2.36) = 0.35
#' and Sussex in 1925 = exp(0.62+-1.04+1.43-1.74) = 0.48
#' From this we can see that pre-1925, both Kent and Sussex 
#' had lower mean counts than post-1925 (standard errors were 0.57 and 0.55, 
#' both of which are smaller than half the size of the mean estimate of the 
#' differences, therefore we can tell that the confidence intervals would not
#' cross 0).
#' Hastings on the other hand had much higher counts. 
#' Hastings behaves differently pre and post 1925 
#' compared to Sussex and Kent - causing an interaction
#' between Era and Area.
#' Can either give results from negative binomial or corrected Poisson, 
#' interpretation is the same. </span>

#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
coef(HastingsModel)

# Results - corrected the likelihood
summary(HastingsModel, dispersion = dispersion)

# Results - negative binomial glm
summary(HastingsModelNB)

#' **2 mark for each result discussed (I count 6 main effects, each location
#' before 1925 then each location after. But also interaction and whether it
#' made a difference) MUST inc confidence interval and
#' correct understanding of which scale you interpret on to get the marks.**
#' 
#' <span style="color:purple"> Code to create a plot of the data </span>
#' 
#' <span style="color:blue"> E2. Do you think there was fraud 
#' going on in Hastings pre-1925? Explain your answer (5 marks) </span>
#' 
#' We have plotted the data to help with this too. You can use this as well as the 
#' other information to support your conclusion. 
#' 
#' <details><summary>General hint</summary>
#' 
#' Now use the interpretation you had before to draw a conclusion.
#' 
#' </details>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = FALSE
# Might want to plot
# colour code by Area
HastingsData$Colour <- c("red2", "blue", "orange")[as.numeric(HastingsData$Area)]

plot(HastingsData$Year, 
     HastingsData$Count, type="n", xlab="Year", ylab="Frequency")
# put a rectangle below 1925
rect(1850, -5, 1925, 59, col="grey90", border=NA)
# plot counts colour coded by area
points(HastingsData$Year, 
       HastingsData$Count, col = HastingsData$Colour, pch=18, cex=1.5)

# add legend to show different colours
legend(x=1935, y=20, levels(HastingsData$Area), 
       col=c("red2", "blue", "orange"), 
       pch=18, cex=1)

#'
#' <span style="color:purple"> It is very suspicious that Hastings follows 
#' an opposite pattern to the other
#' areas. Particularly because it had an almost 7 times higher number of rare birds
#' pre 1925 than after.
#' There could be biological reasons for this, like reduction of rare species over time.
#' But fraud seems more likely given the patterns in the other areas. </span>
#' 
#' **1 mark for a yes/no answer. 4 marks for giving reasons for how you made that
#' decision**
#' 
#' </br>
#' </br>
#' </br>
#' -----
#' </br>
#' </br>
#' </br>
#' 
#' ## Part F: Reflection
#' 
#' <span style="color:blue"> F1. Think about how this went. Were you still using most
#' of the hints? What were you still unsure of? </span>
#' 
#' <span style="color:blue"> F2. Are there some areas you want to 
#' prioritise for revision? </span>
#' 
#' <span style="color:blue"> F3. Is there anything you are very proud of from
#' your group? </span>
#' 
#' <span style="color:blue"> F4. Anything specific for your final feedback? </span>
