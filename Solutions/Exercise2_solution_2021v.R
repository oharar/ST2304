#' # Exercise 2: Can you outrun a zombie?
#' 
#' <span style="color:purple"> SOLUTION. </span>
#' 
#' ### Instructions:
#' 
#' This document contains information, questions, R code, and plots.
#' 
#' *Hints and reminders are bold*
#' 
#' <span style="color:blue"> Questions appear in blue. </span>
#' <span style="color:purple"> Answers appear in purple or as R code. </span>
#' 
#' **Exercise to be handed in by end of 12th February**
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
#' **This week the exercise looks at how we can use maximum likelihood 
#' estimation for distributions
#' that use two parameters to
#' make a life saving (or not) decision.** These exercises will get you to use
#' all steps in statistical modelling (choosing models, estimating parameters, 
#' estimating 
#' uncertainty, and interpretation of results.)
#' 
#' -----
#' ### R hints and general help
#' 
#' ## Question vocabulary: 
#' 
#' * **Describe** is asking you to summarise some outputs or findings e.g. 
#' "X is larger than Y" or "X has a straight line relationship with Y with some 
#' outliers" 
#' or "The confidence interval is 2.4 to 3.1"
#' * **Interpret** is asking you to take what you can describe and put it
#' into context (especially back into the units it was calculated from) 
#' e.g. "X is larger than Y by 5 days, this means that..." or
#' "X has a straight line relationship with Y of 1.5 degrees to every day change 
#' in X. 
#' This shows that it will be 10.5 degrees warmer at the end of the week" or 
#' "The confidence interval is 2.4 to 3.1. The CI does not span zero so we are confident 
#' our
#' estimate is statistcally different to zero."
#' * **Your opinion** if we ask what you think then we need an opinion as an 
#' answer. 
#' Give reasons to your answer, there may not be a clear wrong or right. But you
#' should use data available to back up your opinion. 
#' 
#' ## Resources:
#' 
#' * Chapter 3 in 'The New Statistics with R'
#' * Google (or any other search engine)
#' * Recap maximum likelihood estimation and parameter distributions
#' * Recap t-tests
#' 
#' ## R this week:
#' 
#' New functions:
#' 
#' * `lm()` takes the arguments x, and y e.g. `lm(y ~ x)` x and y here 
#' correspond to an 
#' explanatory variable (x) and a response (y)
#' * `confint()` gives confidence intervals for an object created using the 
#' `lm()` function. Uses t-distribution.
#' * `summary()` produces a summary of an object created using the `lm()` 
#' function.
#' * `exp()` computes the exponential function, takes e to the power of the 
#' number in (). Opposite of taking natural log.
#' * `sapply()` repeats a function across a vector of values e.g. 
#' `sapply(seq(1,10,1), sqrt)` takes the square root of numbers 1 to 10 in order
#' and produces an output of 10 numbers. 
#' 
#' -----
#' 
#' ## The challenge: Should you try to outrun a zombie?
#' 
#' It is day 40 after the event. An infection has been spreading through the 
#' world.
#' Its symptoms produce a zombie-like state of reduced cognitive function 
#' coupled with increased aggression. 50% of the population have already been 
#' infected. It is not known how many are immune.
#' 
#' **You are a group of statisticians who have managed to remain safe inside 
#' your office. You now need to decide whether to stay here or try and run to a 
#' different location.** But, can you outrun the zombies? From TV and film you 
#' have pieced together mixed information on zombie abilities. 
#' Some sources suggest zombies are slow, only walking or shuffling.
#' Others show that zombies can move incredibly fast, even running. 
#' The only way we can
#' find out how these zombies behave is with **data**.
#' 
#' Luckily! One of your team has a dataset that might help. 
#' 
#' One infection outbreak began at the 2018 athletics tournament in Paris. 
#' The 100m final had been run two days before
#' when an outbreak occurred during the 100m relay. For some reason the newly 
#' infected 
#' athletes still decided to run the relay event (we don't know why). 
#' So you have data
#' on the 100m times for the same runners both before and after infection. 
#' 
#' ![Zombie picture](Zombies.png)
#' 
#' The data can be found here: 
#' https://www.math.ntnu.no/emner/ST2304/2021v/Week04/Ztimes100.csv
#' 
#' **It is your job to analyse the data and decide whether to run or stay.**
#' 
#' -----
#' 
#' ### Part A: Should you stay or go? using the t-distribution
#' 
#' The first step is to import the data and assign it to an object. 
#' You can use the whole
#' web link above to import the data. 
#' It is a csv file with column names (header) included.
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = TRUE
Ztimes100 <- read.csv("https://www.math.ntnu.no/emner/ST2304/2021v/Week04/Ztimes100.csv", 
                      header = TRUE)

#' Once you have imported the data it is important to look at it. 
#' 
#' We have made two graphs for you (you do not need to know all the code used 
#' to do it).
#' In **Figure a** the grey lines show which times are from the same athlete. 
#' The means are marked in purple.
#' You can also look at the raw data in R to get familiar with the column names 
#' and layout.
#'
#' **Figure b** is a histogram of the difference in time before and
#' after being infected. The x-axis is the difference in seconds and
#' the y-axis is the frequency that difference was observed in the data. 
#' 
#' <span style="color:purple"> ANSWER BELOW </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# To make the plot it is easier if we re-organise our data
# we want to have a column indicating if the time was before or after
# infection and a column with all the times.

# We do this by making a data frame.
# We will code before and after as 0 and 1 to make plotting easier.
# 0 = before, 1 = after. We use rep to repeat 0 and 1 ten times each.
# We join together the before and after times into new column Times.
# Athlete is created by repeating 1 to 10 twice.
Ztimes100_2 <- data.frame(Group = rep(c(0,1), each = 10),
                          Times = c(Ztimes100$times_before, 
                                    Ztimes100$times_after),
                          Athlete = rep(1:10, 2))

# Then plot all of the data
par(mfrow=c(1,2))
# xaxt = 'n' tells R not to draw an x axis, we will do this ourselves
plot(Ztimes100_2$Group, Ztimes100_2$Times, pch = 16, xaxt = 'n',
     xlim = c(-0.5,1.5), ylim = c(5,15), xlab = "Group", ylab = "Time (seconds)",
     main="a.")
# draw the x axis (side = 1) and give appropriate labels
axis(side = 1, at = c(0,1), labels = c("Before", "After"))

# Then mark on the means in purple
points(c(0,1), c(mean(Ztimes100$times_before),
                 mean(Ztimes100$times_after)), col="purple", pch = 4, lwd=3)
# to do this we loop through the 10 values of before and after
# we make the line grey
for(i in 1:10){
  lines(c(0,1), c(Ztimes100$times_before[i], Ztimes100$times_after[i]),
        col = "grey50")}

hist(Ztimes100$times_after-Ztimes100$times_before,
     col="grey50", xlab="Difference in times \n(seconds)", las=1, main="b.",
     xlim=c(0,2))

#' 
#' The first step when deciding how to analyse the data is to choose
#' a model to represent how the data were generated. 
#' 
#' **Remember: When deciding how to model any data, you need to consider the 
#' characteristics
#' of the data and match these to an equation or distribution.**
#' 
#' In this case you have data that are differences in times, 
#' time is continuous (it can take any value, even negative here). You can
#' also see in Figure b above that the data are almost bell-shaped. Our
#' data seem to be **normally distributed - they follow a normal distribution 
#' at the population level.**
#' 
#' The normal distribution is characterised by two parameters:
#' 
#' * The mean ($\mu$)
#' * The variance ($\sigma^2$)
#' 
#' In this case you are only interested in the mean. You want to estimate
#' the value of the mean ($\hat{\mu}$) using maximum likelihood estimation.
#' **Remember that the distribution of those estimates of the mean ($\hat{\mu}$)
#' will be a t-distribution**. More on this later in the exercise.
#' 
#' In this example the distribution of the data and the parameter are not
#' the same!
#'  
#' The t-distribution has a single parameter;
#' **the variance**, which is defined by the degrees of freedom (calculated as 
#' sample size -1). 
#' The mean of the t-distribution is equal to 0.
#' 
#' In this example today you want to use your model to answer
#' a specific question **Are zombies faster than
#' humans?** 
#' 
#' In order to do this you will be doing the equivalent of a paired t-test. 
#' You should have covered this
#' in ST0103 during 'hypothesis testing (in practice)'. But here we will look
#' at this from a modelling perspective and put it in the framework of the
#' rest of this course. 
#' 
#' To pair the data you will need to create an object which contains the 
#' difference in the times before and
#' after infection. To do this you need to subtract one column from the other.
#' 
#' <span style="color:blue"> 
#' A1. **Create** the object of differences in time. 
#' Why does the data need to be paired like this? </span>
#' 
#' <span style="color:purple"> ANSWER:
#' It needs to be paired because there is some dependence in the data.
#' The same athletes are present in each dataset.
#' We need to compare each athlete post infection to their own time
#' pre-infection. </span>
#' 
#' <details><summary>R hint</summary>
#' 
#' To look at your column names either type the name of the object into R
#' e.g. `Ztimes100`, use the `str()` function to see the structure of the
#' data e.g. `str(Ztimes100)`, or use the specific function `colnames(OBJECT)`.
#' Remember to replace OBJECT with your data name. 
#' 
#' To access a specific column you use `$` in the format:
#' 
#' `DATANAME$COLUMNNAME` e.g. `x$column1`
#' 
#' </details>
#' 
#' </br>
#' 
#' <span style="color:blue"> A2. What are you trying to find out by 
#' modelling these differences? 
#' (What do you want to know from the data and how can you find
#' that out? i.e. what criteria would you use to decide on the answer 
#' to the question?) </span>  
#' 
#' <span style="color:purple"> ANSWER:
#' In a paired t-test we can ask if the mean difference 
#' (the mean of the paired differences)
#' between two groups is different to zero. </span>
#' 
#' You can model the data in R using the `lm()` function.
#' 
#' The `lm()` function in R is equivalent to using maximum likelihood estimation 
#' but R does all of the estimating for you. It then gives you a summary that 
#' contains the
#' maximum likelihood estimate for each parameter (the mean) and a measure
#' of uncertainty (the standard error or confidence intervals - using the 
#' t-distribution).
#' 
#' You will need two arguments to give to `lm()`, these are `x` and `y`.
#' 
#' `y` is the object of differences you made in A1. 
#' This will make the data paired. These times will be argument `y`, which
#' is the response variable. 
#' 
#' **Which column you substracted from the other will influence how you 
#' interpret the answer.**
#' 
#' **Remember: to assign the differences to an object.**
#' 
#' <span style="color:purple"> ANSWER BELOW </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# Create a vector of paired differences
# the - symbol automatically subtracts each element of a vector from the
# same location in another vector (creates paired differences)
differences <- Ztimes100$times_before - Ztimes100$times_after

#' 
#' Argument `x` here is 1. This is because you are looking only at the mean 
#' difference. 
#' There is no 
#' explanatory variable. By taking the difference in times rather than the
#' raw times themselves, you only model the differences.
#' 
#' <span style="color:blue"> A3. Run the model using the `lm()` function. </span> 
#' 
#' Look at the results using `summary(lm_output)` **Remember to replace 
#' 'lm_output' with
#' the name of your own `lm()` model!**
#' 
#' The output of the `lm()` using `summary()` displays:
#' 
#' * model you have run
#' * the quantiles of the residuals (distance between data and estimate)
#' * values of the coefficient here:
#' 
#' - estimate = estimate of the parameter we are interested in (the mean of the sample)
#' 
#' - Std. Error = the standard error of the estimated parameter (taken from t-distribution)
#'
#' - t value = the calculated t-statistic
#' 
#' - Pr(>|t|) = probability value
#' 
#' - Significance codes (ignore these)
#' 
#' - Residual error and degrees of freedom - more on these next week
#' 
#' You can also find the confidence intervals for your parameter estimate
#' based on the t-distribution
#' using the `confint()` function as in the code below. 
#' 
#' <span style="color:purple"> ANSWER BELOW </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# run a paired t-test on the differences between before and after 100m times.
t_test1 <- lm(differences ~ 1)

# summarise
summary(t_test1)

#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = TRUE
# We can show the rounded 95% confidence intervals like this
round(confint(t_test1),2)

#' 
#' <span style="color:blue"> A4. What are the confidence intervals for your 
#' estimate of the mean difference (what values are the upper and lower bound)?
#' **Describe** what they mean statistically. </span>  
#' 
#' <span style="color:purple"> ANSWER:
#' Lower bound is -1.33 and upper is -0.95 seconds. They do not span zero. 
#' There is a statistically significant mean difference in the two groups.' 
#' Check the direction based on which column you subtracted from the other. 
#' Humans should be faster than zombies. ' 
#' I took after times away from before. Negative differences indicate Zombies (after)
#' being slower than humans (before) as they will have had a larger time e.g. 
#' 9 seconds before minus the 10 seconds after = -1 seconds. </span>
#' 
#' 
#' <span style="color:blue"> A5. Your team must decide whether to stay or go. 
#' **Based on the data
#' you have analysed, do you decide to try and outrun the zombies?** 
#' Include reasons and the results in your answer (MLE and confidence intervals). 
#' **hint: think about what the estimated mean represents in terms of 100m times** </span>
#' 
#' <span style="color:purple"> ANSWER: 
#' Based on the data you have, you should choose to run.
#' The confidence intervals do not cross zero so there is 
#' a statistically significant difference in times and the direction
#' is that humans are faster. They are faster by an average of 1.14 seconds
#' with 95% confidence the mean is between 0.95 and 1.33 seconds. 
#' But you may choose to stay, because 1 second or even just over
#' 1 second, is not a big difference!!! And it could even be less. </span>
#' 
#' <span style="color:purple"> Make sure the direction is correct! Humans are faster. </span>
#' 
#'<span style="color:purple"> Realising that statistical significance of an effect is not always the most
#' important thing if the effect is small is good here. </span>
#' 
#' -----
#' 
#' ### Part B: Was there a better way to model uncertainty? 
#' 
#' <span style="color:blue"> B1. Do you think you could you have used a normal 
#' distribution here
#' to represent the parameter distribution instead of a t-distribution? 
#' Say **why/why not**. Think
#' about what the difference between a t-distribution and normal 
#' distribution is. </span>  
#' 
#' <span style="color:purple"> ANSWER: 
#' It would not be the most appropriate distribution here because
#' of our low sample size of 10 points. Once you get above 30
#' data points the T distribution becomes almost identical to the normal.
#' But lower it has a more squashed shape and takes account of 
#' the higher uncertainty in small sample sizes. </span>
#' 
#' One way to test whether using a normal distribution would be different to a 
#' t-distribution, 
#' is to try it!
#' 
#' <span style="color:blue"> B2. How many unknown parameters does
#' the normal distribution have? </span>  
#' 
#' <span style="color:purple"> ANSWER:
#' Two the mean and the standard deviation/variance </span>
#' 
#' To get a likelihood curve for the estimate of the mean of the differences 
#' ($\hat{\mu}$) 
#' in 100m times using a normal distribution, you can use
#' the `dnorm()` function (similar to what you did last week with the Binomial 
#' and Poisson distributions). This allows you to calculate the likelihood,
#' which for a continuous distribution is the **density**.
#' 
#' Here you create a distribution for the estimated parameter.
#' 
#' **Remember: as you have many data points, you will need to sum the log 
#' likelihood for all data points.** 
#' 
#' **You have code to do this in [week 4 module](https://www.math.ntnu.no/emner/ST2304/2021v/Week04/Lecture4Module.html)**
#' 
#' <details><summary>R hints</summary>
#' 
#' You used `dnorm()` in the exercise part of this week's lecture. 
#' 
#' There is also a function there to sum log likelihoods, you can copy and paste
#' to use that here. It is `CalcNormlogLh()` it takes the arguments:
#' (mu, sigma, and data), mu = mean, sigma = standard deviation and data = observed
#' data. 
#' 
#' The function is in this script https://www.math.ntnu.no/emner/ST2304/2021v/Week04/NormalDistFunctions.R
#' 
#' </details>
#' 
#' </br>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE

# Source the function:

source('https://www.math.ntnu.no/emner/ST2304/2021v/Week04/NormalDistFunctions.R')

# This is taken from the functions you loaded for the lecture module.

# Below is the code for the function in case you are interested.

# this line sets up the function
CalcNormlogLh <- function(data, mu, sigma) { 
  # this line sums the log likelihoods
  likelihood <- sum(dnorm(data, mean=mu, sd=sigma, log=TRUE))
  # this line returns the likehood for the whole dataset
  return(likelihood)
}

# next set up a vector of different mean values to 
# get the likelihood for
means <- seq(-3,3,0.01)

# Now use the 'CalcNormlogLh' function.
# Use it inside a function called sapply.
# You don't need to know much about this function,
# but it runs another function for all values of a vector (here 'means')
likelihoods.norm <- sapply(means, CalcNormlogLh, 
                           data = differences, sigma = sd(differences))

# We can look at a few of the likelihoods (exp back transforms the log)
head(exp(likelihoods.norm))

# WOW! Those are really small numbers. 
# So when we plot we subtract the maximum likelihood to scale the 
# numbers and make them a bit more relatable. (scaled 0 to 1) 
# plot the outcome
plot(means, exp(likelihoods.norm-max(likelihoods.norm)), 
     type="l", xlab="Mean", ylab="Density", main = "Normal likelihood")

#' The estimate of the
#' mean of the differences stays exactly the same as what you
#' got before, because this part has not changed. It is still the mean of the 
#' sample. What does change is how we quantify uncertainty.
#' **So, how does the estimate of uncertainty change?** To find out you need to
#' calculate the confidence intervals. 
#' 
#' <span style="color:blue"> B3. **Calculate** the confidence intervals for
#' the estimate of $\hat{\mu}$ using the normal distribution (code below). </span> 
#' 
#' <span style="color:purple"> ANSWER BELOW </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE, eval = TRUE
# Calculate the confidence intervals using a normal distribution 

# First save an object that is the standard error
standard_error_norm <- sd(differences)/sqrt(length(differences))

# Then + or - 1.96 times the standard error from the 
# maximum likelihood estimate
CI_upper <- mean(differences) + 1.96*standard_error_norm
CI_lower <- mean(differences) - 1.96*standard_error_norm

# display the confidence intervals to 2 decimal places
round(c(CI_lower, CI_upper),2)

#' 
#' <span style="color:blue"> B4. **Compare** these confidence intervals those 
#' calculated
#' from the t-distribution? </span>  
#' 
#' You should have those written down from 
#' question A4. 
#' 
#' <span style="color:purple"> ANSWER: The Normal confidence intervals are 
#' narrower than those calculated
#' from the t-distribution. They are under-estimating the uncertainty. </span>

#' You can also plot the difference confidence intervals.
#' Here we plot them both on the Normal distribution density plot.
#' 
#' We have given you the code to do this - it isn't something you need to 
#' memorise but do read the comments if you want to understand what it does. 
#'
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# First create a plot with the Normal distribution density.
# We use xlim and ylim to set the limits of our axes to make
# the plot look nicer and be easier to read. 
plot(means, exp(likelihoods.norm-max(likelihoods.norm)), 
     type="l", ylab="Density", xlab="Mean", xlim = c(-3,3), ylim=c(0,1.1))
# We then add a point for the maximum likelihood estimate of the mean.
points(mean(differences), 1, col = "purple", pch=16)

# Then add the t-distribution confidence intervals in purple
abline(v = c(confint(t_test1)[1], confint(t_test1)[2]), col = "purple")
# and the Normal distribution confidence intervals in blue
abline(v = c(CI_lower, CI_upper), col = "blue")

# The final step is a legend to explain the colour scheme
# bty = 'n' just means there will be no box around the legend
# the first argument gives the location of the legend
# then the labels
# lty = line type to display
# col = colours
legend("topright", c("Normal CI", "t CI"), bty = "n",
       lty= c(1,1), col = c("blue", "purple"))

#' 
#' <span style="color:blue"> B5. Now you have tried the Normal distribution,
#' do you think the Normal distribution is a good model for the uncertainty 
#' in your parameter estimate? 
#' **Why/Why not** In what situation might it become better? </span>  
#' 
#' <span style="color:purple"> ANSWER: It is not a great model here because the 
#' confidence intervals
#' are too tight, but only by a small amount. They under-estimate the 
#' uncertainty because we 
#' have only a few data points. The T distribution captures the 
#' uncertainty better. You could say it is either good or bad, just justify the
#' choice e.g. the answer is nearly the same or the uncertainty is under estimated. </span> 
#' 
#' <span style="color:purple"> The Normal distribution will be more appropriate 
#' with more data. 
#' You can always test this out by creating more data and running
#' all the code again. </span>
#' 
#' <span style="color:blue"> B6. Consider the Normal distribution
#' confidence intervals. If you use these only, would it change your decision to
#' run or stay (question A5)? **Explain** your answer </span>
#' 
#' <span style="color:purple"> ANSWER: Using the Normal distribution confidence 
#' intervals should not
#' change your decision. Both sets of CIs do not cross 0 so we are
#' reasonably confident in both cases that zero difference in 100m times before 
#' and after infection is not a plausible result 
#' given our data.
#' The difference is very minimal. You might be slightly more likely to
#' run using normal distribution confidence intervals. </span>
#' 
#' <span style="color:purple"> This difference is that humans are faster than 
#' zombies. </span>
#'
#' -----
#' 
#' ### Part C: Reflection
#' 
#' When modelling, it is always important to consider how good our model
#' and our data are. 
#' 
#' <span style="color:blue"> C1. What other data might you want to improve your 
#' decision making?
#' Could there be any problems with the current study design? </span>  
#' 
#' <span style="color:purple"> ANSWER:
#' Any answer with a good reason provided is acceptable. Some examples: </span>
#' 
#' <span style="color:purple"> - Wind data, this could influence the change in times (problem).</span>
#' <span style="color:purple"> - More data so we can be more confident of results (data).</span>
#' <span style="color:purple"> - Data from another time, maybe there is an initial increase then 
#' it drops off (data).</span>
#' <span style="color:purple"> - Could be other reasons for the increase e.g. warmer (problem). </span>
#' 
#' -----
#' 
#' ## Part D: Feedback
#' 
#' <span style="color:blue"> D1. How do you think this exercise went? What do
#' you think your group did well, what was a challenge? 
#' (2 examples of each) </span>
#' 
#' <span style="color:blue"> D2. What do you think you improved from last 
#' week? </span>
#' 
#' <span style="color:blue"> D3. What would you like feedback on this 
#' week? </span>
#' 