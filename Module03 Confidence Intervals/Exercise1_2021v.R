#' # Exercise 1: Dragon management
#' 
#' ### Instructions:
#' 
#' This document contains information, questions, R code, and plots.
#' 
#' *Hints and reminders are bold*
#' 
#' <span style="color:blue"> Questions appear in blue. </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# R code looks like this.
# Comments are in grey and explain the code

2+2 # add two and two

# outputs from R appear like the above ^^^^

#' You should work through the exercise in groups and write answers in a 
#' separate document to be submitted on Blackboard by 
#' **Friday 5th Feb midnight**. 
#' 
#' Answer all questions. The assignment can be written in any format 
#' (e.g. `.docx`, `.pdf`, `.R`) but
#' should easily be able to include pictures, R code, and text. 
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
#' The exercise is designed to use statistical theory in an applied context
#' to solve biology related problems similar to those you will face in real 
#' life.**This week the exercise looks at how we can use maximum likelihood 
#' estimation to
#' make biological and political conclusions.** 
#' These exercises will get you to use
#' all steps in statistical modelling (choosing models, estimating parameters, 
#' estimating uncertainty, and interpretation of results.)
#' 
#' We will give some help with R and some hints, these will reduce each week.
#' We also expect you to use some problem solving yourselves and to ask the
#' assistants for help. 
#' 
#' We would expect this to take you at least 2 hours. 
#' 
#' -----
#' 
#' ### R hints and help
#' 
#' #### Use the help pages and google
#' 
#' In the exam you will be given the R help pages for any function that is used. 
#' Here you can practice using them. Type `?` followed by the function name to 
#' bring up
#' the help pages for that function e.g. 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
?optimise

#' 
#' For the function `optimise()` that we use this week. 
#' 
#' ### A new R function we will use `optimise()`
#' 
#' This function provides an easy way for us to optimise over a parameter 
#' (find the 'best' value).
#' What we consider the 'best' value of a parameter depends on what criteria 
#' we want to optimise.
#' For most of the work we are doing we want to find the parameter that 
#' maximises the likelihood.
#' We can use optimise to calculate the likelihood for all possible values of a 
#' parameter (within bounds we set)
#' and find the maximum of these.
#' 
#' Arguments we have to provide to `optimise()` (the arguments optimise takes):
#' 
#' * `f`, the function to be optimised, for us that is the likelihood.
#' * `lower`,  the lower bound of parameter values to try.
#' * `upper`,  the upper bound of parameter values to try.
#' * `maximum`, TRUE or FALSE, whether to maximise or minimise the function.
#' We want to maximise.
#' * Other arguments. We also need to include any other arguments the 
#' function in f needs, as well as the one we want to find the optimum of. 
#' **More detail on these later.** 
#' 
#' ### Other functions to remember from previous weeks
#' 
#' * `read.csv()`
#' * `hist()`
#' * `dbinom()`
#' * `plot()`
#' * `points()`
#' * `seq()`
#' * some other basic functions whose names say what they do: `sum()`, `mean()`,
#'  `length()`
#' 
#' -----
#' 
#' ## The challenge: management of dragons in Trondheim
#' 
#' **You are a team of scientists studying the reintroduction of dragons into 
#' Trondheim.**
#' 
#' Dragons were reintroduced last year as part of rewilding efforts. 
#' The exact species of dragon introduced is a seed eater and so no threat to 
#' people or livestock.
#' They also help maintain lower vegetation across the river, which helps 
#' kayakers and other river users.
#' Since reintroduction you have been collecting data on these dragons as part 
#' of your research. 
#' Now the government wants you to present your findings to help them choose a 
#' management strategy:
#' 
#' * 1) Business as usual (do nothing)
#' * 2) Manage the population negatively (shooting)
#' * 3) Manage the population positively (extra feeding or habitat creation)
#' 
#' From previous work you know that an average population abundance of 35 
#' individuals is needed 
#' to maintain the population and keep vegetation under control.
#' An average population abundance of more than 50 individuals will start to put 
#' pressure on resources
#' and increase risks of fire. 
#' 
#' **It is your job to analyse your data and advise the government how to manage 
#' the population.**
#' 
#' -----
#' 
#' ### Part A: analysis of the sex ratio of dragon eggs
#' 
#' ![Dragon picture](Dragons.png)
#' 
#' One of your team has been collecting data on the sex of dragon eggs. 
#' Conveniently, male dragons are purple and females are red (even as eggs).
#' 
#' They have collected data from 100 eggs. 
#' Their data can be found at (https://www.math.ntnu.no/emner/ST2304/2021v/Week03/DragonEggData.csv).
#' This file has a header and is a `.csv`. 
#' You can either download and then import it using read.csv() or import directly using the whole web address above. 
#' 
#' **DO NOT open the file in excel and re-save after downloading.**
#' 
#' The data has two columns `Egg_number` and `sex`. 
#' In the sex column 0 = male and 1 = female. 
#' 
#' *Remember: to access a particular column of a data frame you use 
#' $ e.g. sex_ratio_data$Egg_number*
#' 
#' **First thing we should always do is look at our data.**
#' 
#' <span style="color:blue"> A1. Plot a histogram of the data and look at the 
#' raw data in R. </span>
#' 
#' <details><summary>Hint</summary>
#' 
#' Use the `hist()` function to make the plot.
#' 
#' `xlab` and `ylab` and `main` 
#' arguments change the axis labels and plot title.
#' 
#' </details>
#' 
#' This is not a pretty graph but lets you see what sort of data you have. 
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# import the data and name it
SexRatioData <- read.csv('https://www.math.ntnu.no/emner/ST2304/2021v/Week03/DragonEggData.csv', header=T) 

# plot the histogram - I have changed the title (main) and 
# the x axis label (xlab) manually
hist(SexRatioData$sex, xlab="Sex (0 = male, 1 = female)", 
     main = "Histogram of sex ratio data", col="black")

#' From this data you can calculate the observed proportion of female eggs:
#' 
#' <span style="color:blue"> A2. What is the proportion of female eggs in the 
#' observed data? (include one line on how you calculated it) </span>
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# There are several ways to do this. The exact way does not matter as long as it 
# is correct.

# create an object called ProportionFemale 

# Option 1:
# Sum the numbers in sex column (basically counts all the 1s) 
# then divide by the total number of trials (100)
# round() rounds the numbers, here to 2 decimal places.
ProportionFemale <- round(sum(SexRatioData$sex)/length(SexRatioData$sex),2) 

# Option 2: 
# We sum up the number of 1s and divide by the total number. 
# mean() is a sneaky way of doing this.
ProportionFemale <- mean(SexRatioData$sex)

# display answer
ProportionFemale # 0.44

#' Now you have the proportion of female eggs that were observed. But this does 
#' not tell us much. 
#' You need to complete some statistical analyses to make generalisable 
#' statements about the results.
#' 
#' **But which distribution can we use as a model?**
#' 
#' You want to choose a model to represent the data. Just like last week, this 
#' is binary data (male or female) and each egg
#' is an independent trial. It should be familiar that we can use the **binomial 
#' distribution** as our model.
#' As we have chosen the binomial distribution, you know that there is one 
#' unknown parameter to estimate $p$ = the 
#' probability of success (being female). 
#' 
#' **We have a model, how do we estimate the parameter?**     
#' 
#' <span style="color:blue"> A3i. Begin by **calculating** the likelihood for 
#' different probabilities of being female (here being female = getting a 
#' success). </span>
#' 
#' * You will need to create an object of your number of successes
#' * Work out your number of trials (how many times did you try to get a success?)
#' * Use the `dbinom()` function to find the density for different parameter values. 
#' In this case the density is the same as the likelihood (use ?dbinom if you can't remember what arguments it takes) 
#' * Try and change the x and y axis labels to something clearer than the defaults (use help and google to find out how)
#' 
#' <details><summary>Hint</summary>
#' 
#' You used `dbinom()` in the model this week just before Figure 1. 
#' 
#' You then plotted the likelihood in Figure 2. 
#' You can find the code for both there.
#' 
#' </details>
#' 
#' <span style="color:blue"> A3ii. Then **plot** the likelihood curve. </span>
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval = FALSE
# save the number of successes as an object
NSuccess <- sum(SexRatioData$sex) # 44
# save the number of trials as an object (total number of eggs)
NTrials <- length(SexRatioData$sex) # 100

# Help! what probabilites (probability of being female) do I use?
# You can use any but chosing a large number of possibilities covering 
# from almost 0 to almost 1 is best.
# seq() is a good function to use. 
# It creates a vector of sequence of numbers from the start and end you choose.
Probabilities <- seq(from=0.001, to=0.999, by=0.001) # 999 numbers

# generate likelihoods
likelihoods <- dbinom(x=NSuccess, size=NTrials, prob=Probabilities)

# plot the curve as a line (type="l")
# manually set x and y labels (xlab and ylab)
plot(x=Probabilities, y=likelihoods, type="l", xlab="Probability of female", 
     ylab="Likelihood") 

#' <span style="color:blue"> A4. At what point along the curve is the maximum 
#' likelihood estimate? and **How** can we find it, mathematically? 
#' (**hint: you can describe the mathematical process in words or use the 
#' equation**) </span>
#' 
#' 
#' Below is the code to find the maximum likelihood estimate of the probability 
#' of being female in R and plot it on the graph. You could also use the
#' equations from last week and find the maximum likelihood estimate 
#' analytically. 
#' 
#' But we present an example using `optimise()`, this is easier for more complex 
#' likelihoods.
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, eval = FALSE
# f = the dbinom() function 

# You now add an upper and lower bound for the probability (0 and 1)
# R will only calculate the likelihood for parameter values between 0 and 1

# You still need to include the arguments of the number of successes
# and number of trials. dbinom() needs these to run

# You have set maximum to TRUE

MLE.binom <- optimise(f=dbinom, lower=0, upper=1, x=NSuccess, 
                      size=NTrials, maximum=TRUE)
# $maximum takes only the maximum value from the output

# Look at the result:
MLE.binom 
# $maximum = the parameter value that gave the highest likelihood
# $objective = the likelihood vlaue for that parameter

#+ warning = FALSE, error = FALSE, include = TRUE, eval = FALSE
# Now to plot the results

# Repeat the plot from A3ii
plot(x=Probabilities, y=likelihoods, type="l", xlab="Probability of female", 
     ylab="Likelihood") 

# add a point at the maximum likelihood

points(MLE.binom$maximum, # the parameter value
       MLE.binom$objective, # the maximum likelihood value
       col="red", pch=16) # pch = 16 and col = "red", makes the point red and filled in

#'
#' You should now have a plot of the likelihoods and the probability (our
#' parameter) with the maximum likelihood.
#' Although you have managed to calculate this, it is not the whole picture.
#' 
#' <span style="color:blue"> A5. **Why** is the maximum likelihood estimate 
#' alone not enough to draw statistical conclusions? 
#' and **What** other information should we include? </span>
#'
#' **Below is the code to calculate the confidence intervals for the maximum 
#' likelihood estimate and plot them, using a normal approximation.** 
#' 
#' <span style="color:blue"> A6. **Write** a definition for the standard error. 
#' You can use equations but it must also include words. </span>
#' 
#+ warning = FALSE, error = FALSE, include = FALSE
# The standard error is the standard deviation of the 
# sampling distribution of a statistic/parameter.
# If we were to estimate a statistic/parameter (e.g. probability) 
# many times using different samples from the population,
# the expected standard deviation of the different estimated 
# probabilities would be the standard error. 
# (p22 of New statistics with R, can also google)
# Formula (sqrt(variance/n)) is also acceptable.
# But should include some reference to the sampling distribution.

#+ warning = FALSE, error = FALSE, include = TRUE, eval = FALSE
r <- NSuccess  # number of successes
N <- NTrials # total number of trials
p <- NSuccess/NTrials # proportion of success (rounded)
phat <- r/N # get maximum likelihood estimate
stderr <- sqrt(phat*(1-phat)/N) # creating the standard error

# create normal approximation of likelihood
likelihoods.approx <- dnorm(Probabilities, phat, stderr) 
# scale this by the maximum likelihood (becomes relative likelihood)
# i.e. relative to the maximum calculated
likelihoods.approx <- likelihoods.approx/max(likelihoods.approx)

# plot the previous likelihood (binomial)
plot(x=Probabilities, y=likelihoods, type="l", ylab="Likelihood", xlab="Probability")
# add a line for the new normal approximation likelihood
# need to multiply the relative likelhoods by the maximum from the binomial
lines(Probabilities, likelihoods.approx*max(likelihoods), col="red")

# calculate the lower and upper confidence intervals using the standard error
CI_lower <- MLE.binom$maximum - 1.96*stderr
CI_upper <- MLE.binom$maximum + 1.96*stderr
# display the rounded result
round(c(CI_lower, CI_upper), 2)

# mark the CIs on the plot - can do with lines, polygons or points
# I have chosen lines because it is simple code
abline(v=0.44) # MLE line
abline(v=c(CI_lower, CI_upper), col="red") # CI lines
# add text labels x and y tell R the coordinates to put text at
text(x=0.2, y=0.04, labels="Lower CI", col="red") 
text(x=0.7, y=0.04, labels="Upper CI", col="red")

#' 
#' Now, that you have estimated parameters and uncertainty, **it is time to 
#' interpret the results.** 
#' 
#' <span style="color:blue"> A7. Do the dragons have a skewed sex ratio as eggs 
#' (is it different to 50-50)?
#' Use the confidence intervals in your answer. **Explain your answer** </span>
#' 
#+ warning = FALSE, error = FALSE, include = FALSE
# The CI for probability of being female is 0.34 to 0.54, 
# which includes 0.5. 
# So there seem to be as many females as would be expected 
# from a 50:50 ratio, even though the MLE is less than 0.5.
# You do not need the MLE, which is 0.44, to reach the conclusion.

#' 
#' -----
#' 
#' ### Part B: adult dragon numbers
#' 
#' Another member of your team has been collecting data on the number of adult 
#' dragons in Trondheim. 
#' They have set up a feeding station along the river. 
#' They have counted the number of dragons that visited the feeding station in a 
#' single day. 
#' The number of dragons they counted was **29**.
#' 
#' This is just a single count, if you repeated the count on more days each one 
#' would differ.
#' Just like the amount of land and sea we sampled differed each time we moved 
#' the globe.
#' 
#' **But which distribution can we use as a model?**
#' 
#' The data you have are counts, someone went out and counted dragons. Count 
#' data, can only be whole numbers (integers) that are always positive 
#' (non-negative). You cannot have half a dragon or negative dragons. 
#' The Poisson distribution also has these properties, therefore it is the 
#' appropriate choice.
#' So, here we assume that the count data has come from a 
#' **Poisson distribution**. 
#' 
#' In the same way as for binomial data, we want to find what parameter values 
#' (but this time for the Poisson distribution), which produce the highest
#' likelihood given our observed data, those that are most likely to give rise 
#' to our data.
#' 
#' Here you only have information on when you did see a dragon, 
#' not how many you did not see one (there is no record of failures). 
#' 
#' You need to save your number of observations as an object in R.
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
ObservedAbundance <- 29

#' ### The Poisson distribution
#' 
#' The Poisson distribution is characterised by a single parameter. 
#' Lambda $latex \lambda $ which is the mean (and also the variance) of the 
#' distribution.
#' As with the binomial distribution (and $p$, the probability), 
#' we can also find the maximum likelihood estimate for the Poisson, 
#' but here we are finding the likelihood of different $latex \lambda $ (means).
#' 
#' **Below you can see the code to find the likelihoods of different 
#' $latex \lambda $ and then plot them.**
#' 
#' **Details are as follows:**
#' 
#' Remember: you can only have whole dragons.
#' 
#' * x is the observed count, in the same way observed successes was for the 
#' binomial. 
#' * Use the `dpois()` function to find the likelihoods 
#' (`dpois(x=ObservedData, lambda=TestLambdas)` or use ?dpois) 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, eval = FALSE
# Begin by creating a sequence of lambdas to find likelihoods for
lambdas <- seq(1,70,1) # 70 lambdas (means)

# Calculate the likelihoods
likelihoods.Pois <- dpois(x=ObservedAbundance, lambdas)

# plot the outcome
plot(lambdas, likelihoods.Pois, type="l", ylab="Likelihood", xlab="Lambda (mean)")

#' To find the maximum likelihood we again use `optimise()`.
#' 

#+ warning = FALSE, error = FALSE, include = TRUE, eval = FALSE
# replot the outcome
plot(lambdas, likelihoods.Pois, type="l", ylab="Likelihood", xlab="Lambda (mean)")

# This time we use optimise on the dpois function used above
# Set lower and upper to the bounds of your lambdas above
MLE.Pois <- optimise(dpois, lower=0, upper=70, x = ObservedAbundance, 
                     maximum = TRUE)
MLE.Pois

# plot at point at the maximum likelihood 
points(MLE.Pois$maximum, # parameter value that gives the highest likelihood
       MLE.Pois$objective, # the actual likelihood value for that parameter
       col="blue", pch=16) 


#' You now have a maximum likelihood estimate for the mean abundance 
#' ($latex \lambda $) of adult dragons in Trondheim.
#' As with the sex ratio though, this is not all of the information that we need 
#' for statistical inference. 
#' We would also like a measure of our confidence (uncertainty) in our estimate.
#' 
#' **The following code calculates the confidence intervals for the maximum 
#' likelihood estimate of $latex \lambda $**
#' 
#' Remember: the variance is the square of the standard deviation. `dnorm()` 
#' takes the standard deviation = sqrt(lambda).
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = FALSE
# calculate approximate likelihoods from the normal distribution
# mean is MLE from Poisson and standard deviation is square root of MLE Poisson
likelihoods.approx.Pois <- dnorm(lambdas, MLE.Pois$maximum,
                                 sqrt(MLE.Pois$maximum)) 
likelihoods.approx.Pois <- likelihoods.approx.Pois/max(likelihoods.approx.Pois)

plot(lambdas, likelihoods.Pois, type="l", ylab="Likelihood", xlab="Probability")
lines(lambdas, likelihoods.approx.Pois*max(likelihoods.Pois), col="blue")

stderr_Pois <- sqrt(MLE.Pois$maximum) # standard deviation as normal approximation
CI_lower <- MLE.Pois$maximum - 1.96*stderr_Pois
CI_upper <- MLE.Pois$maximum + 1.96*stderr_Pois
ApproxCI <- round(c(CI_lower, CI_upper),2)
ApproxCI

# Exact CIs by knowing the maths
## You do not need to know this!!! It's just here so we can plot them.
ExactCI <- qchisq(p=c(0.025, 0.975), df = 2*MLE.Pois$maximum)/2

# Mark on the plot - can do with lines, polygons or points
# This plots a polygon for the exact confidence intervals
InExactCI <- lambdas > ExactCI[1] & lambdas < ExactCI[2]

# This code shades the plot under the curve and between the exact confidence intervals
polygon(x=c(lambdas[InExactCI], max(lambdas[InExactCI]), min(lambdas[InExactCI])),
        y=c(likelihoods.Pois[InExactCI], 0,0), border=NA, col="grey70")

# YOU ONLY NEED TO KNOW HOW TO PLOT POLYGONS IF YOU ARE INTERESTED

# Then mark on the plot approximate intervals
# can do with lines, polygons or points
# This one is a line
abline(v=29) # MLE line
abline(v=c(CI_lower, CI_upper), col="blue") # CI lines
text(x=10, y=0.04, labels="Lower\nCI", col="blue")
text(x=50, y=0.04, labels="Upper\nCI", col="blue")

# Adds a legend (key) to the plot in the top right corner
# Labelled Exact CI and Approx CI, with a grey and blue filled square and no line around the edge (bty = 'n')
legend("topright", c("Exact CI", "Approx CI"), 
       fill=c("grey70", "blue"), bty='n')


#' 
#' The approximate 95% confidence intervals for the Poisson distribution are 18 
#' to 40 (to nearest whole dragon) with a maximum likelihood estimate of 29 
#' individuals.
#' 
#' <span style="color:blue"> B1. **Interpret** the confidence intervals here. 
#' What do they represent? </span> 
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE
# The confidence intervals are 95% confidence intervals
# so they represent the boundaries within which we would likely
# expect the true population value of the parameter (here the mean of the Poisson)
# to be found 95% of the time. If we were to repeat our sampling
# many times and each time create a confidence interval,
# on average the true value would be within the confidence intervals
# 95% of the time. 

# This means that based on our results, it is unlikely that the
# true population abundance is greater than 40 but it could be less than 18. 
# We are not in danger of too many dragons. 
# But the population could be in danger of collapse.


#' 
#' 
#' <span style="color:blue"> B2. Are you happy with the normal approximation and 
#' the confidence intervals here? **Why/Why not?** </span> 
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE
# I am not so happy with these approximate ones because 
# they do not cover 95% of the Poisson distribution.

# Particularly the lower CI is too low in the approximation. 
# The approximate intervals cover > 95% of the Poisson. 

#' 
#' 
#' -----
#' 
#' ### Part C: Make a recommendation
#' 
#' **Your team have analysed two sets of data on the dragon population in 
#' Trondheim.** 
#' 
#' Now you have to create a management recommendation for the local government.
#' 
#' <span style="color:blue"> C1. Produce a recommendation for the government. 
#' It should include: 
#' * **Which** management option you would recommend based on your analyses. </span>
#' * **Why** you suggest this particular option (using analyses to support your reasons). </span>
#' * **Suggest** some biological reasons for what you have found (i.e. if the population seems lower/higher than you might expect, why could this be? </span>
#' Does the data you looked at tell you anything about reasons?) </span>
#' * **Ideas** for future studies you could conduct to understand more about this population, such as </span>
#' some data you could collect or a question you might want to ask (e.g. do dragons prefer some areas to others?) </span>
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE
# We expecting that you should want to increase the population slightly. The MLE
# is a lower than the number of dragons needed to sustain the population and 
# while the confidence interval does contain the number needed for a stable 
# population, the majority of the interval is much below this. 
# So quite a strong case to try and increase numbers to stop the population 
# disappearing. 

# Whatever management strategy you choose should include discussion of the 
# upper and lower bounds of the confidence intervals when justifying the answer.

# You have information on sex ratio. Which is ok, 50:50 was within the plausible
# range based on our data. There
# should be at least some reference to this. But you have no data that could 
# let us look at the causes of either pattern (sex ratio or abundance).
# Speculating on mechanisms is ok as long as it is biologically plausible 
# but you must be aware that you cannot say much about anything we have no data on. 

# Anything biologically sensible is accepted here for how to improve. 
# For example: higher sample
# size, repeated counts along the river, environmental data, food availability.
# Any of these things would allow you to get better estimates of abundance and 
# start to explore some mechanisms and relationships that could influence 
# abundance or sex ratio. 

#' -----
#' 
#' ## Part D: Reflection and feedback
#' 
#' <span style="color:blue"> D1. How do you think this exercise went? What do
#' you think your group did well, what was a challenge? 
#' (2 examples of each) </span>
#' 
#' <span style="color:blue"> D2. What would you like feedback on this 
#' week? </span>
#' 
#' This question (D2) might be hard to answer, but it will be really helpful if 
#' you do. We want the feedback we give to be most useful for you! 
#' You may want feedback
#' on interpretation, or concepts, or R code, on specific questions, 
#' or on everything.
#' It is up to your group
#' what will be most helpful! You will still get a solution to this exercise so 
#' you can see
#' a model answer. 
#' Therefore, choose feedback that will help beyond the solution. 
#' 
#' **Feedback should
#' be to help you improve, so let us know what will be most helpful.**
#' 

