#' # Exercise 7: Maximising plant productivity 
#' 
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
#' **Needs to be completed and handed in by 19th March 23:59**
#' 
#' -----
#' 
#' ## Resources:
#' 
#' * More details on the data p84 The New Statistics with R
#' * Chapters 6 and 7 in The New Statistics with R (both examples in these chapters)
#' 
#' ### R this week:
#' 
#' Things to remember:
#' 
#' * `lm()` with interactions. Use `\*`.
#' * `relevel()` a function to change the reference level of a categorical variable/factor. This
#' controls which group in the categorical variable will appear as the (Intercept).
#' 
#' -----
#' 
#' ## The challenge: How can we maximise plant productivity?
#' 
#' You are a team of agricultural scientists working for a large farming company.
#' The company has employed you to find out how they can increase the productivity
#' of their arable (plants) crops. The company don't want to pay for new experiments
#' but they have given you some older data that you can analyse. The older data
#' is from two different experiments from 2009 and 1990.
#' 
#' <span style="color:blue"> Your job is to find out how different management
#' practices influence plant growth and recommend a plan for the company. </span>
#' 
#' -----
#' 
#' ## Part A: Dataset one - no interaction
#' 
#' ![Meadow](Meadow.jpg)
#' * credit Wikipedia
#' 
#' The first dataset the company has given you can be found at 
#' https://www.math.ntnu.no/emner/ST2304/2019v/Week9/FertilizerData.csv
#' 
#' The data were published in 2009 by Hautier, Niklaus, and Hector 
#' [Paper link](http://science.sciencemag.org/content/324/5927/636.long).
#' The study was designed to look at the influence of fertiliser and light on 
#' grassland plants.
#' 32 different plots were exposed to fertiliser addition, a light addition to 
#' the understory, neither (control),
#' or both treatments. The treatments were conducted for two years and above 
#' ground biomass
#' collected twice a year to mimic cutting regimes in European meadows.
#' 
#' The dataset includes the variables; **Biomass.m2** the above ground biomass 
#' of grassland plants
#' in grams per metre squared, **Fert** a column indicating if the plots had 
#' fertiliser treatment,
#' **Light** a column indicating if the plots had light addition.
#' 
#' As always, the first step is to import the data and assign it to an object 
#' then **plot it**. You can use the whole
#' web link above to import the data. It is a csv file with column names 
#' (header) included.
#' 
#' **`pairs()` should work here.**
#' 
#' You might also want to relevel the factors (Fert and Light) to make
#' sure that the contrast level will be the control treatment 
#' (light and fertiliser = FALSE).
#' 
#' To do this you use the function `relevel()`. The arguments it takes are the column
#' name of the factor/categorical variable that you want to relevel and `ref=` the
#' level that you want to be the reference e.g. 
#' `FertData$Light <- relevel(FertData$Light, ref="L-")`.
#' 
#' **You will also need to turn BOTH Light and Fert into factors using 
#' `as.factor()`**
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
FertData <- read.csv("https://www.math.ntnu.no/emner/ST2304/2019v/Week9/FertilizerData.csv", header=T)

FertData$Light <- as.factor(FertData$Light)
FertData$Fert <- as.factor(FertData$Fert)

pairs(FertData)
FertData$Fert <- relevel(FertData$Fert, ref="F-")
FertData$Light <- relevel(FertData$Light, ref="L-")

#' Take a look at the plot and think about the data
#' 
#' <span style="color:blue"> A1. **What** is the response variable and **what** are the
#' explanatory variables here? **What** kind of data are each of these? </span>
#' 
#' <span style="color:purple"> Explanatory = Fert and light (both categorical),
#' Response = biomass (continuous). </span>
#' 
#' In biology, it is always important to think about what we want to find out
#' before creating a model or doing
#' analyses.  
#' 
#' <span style="color:blue"> A2. **Write** a biological question 
#' (e.g. does temperature influence
#' lay date in birds?) that you
#' can answer using the data you have here. </span>
#'
#'<span style="color:purple"> We have data on fertiliser treatment, a light treatment, and
#' biomass. So we can ask "Does the addition of fertiliser and/or light 
#' increase above ground biomass?"
#' Anything similar to this is ok. </span>
#' 
#' Now that you have a biological question, your team wants to create a model of the data.
#' You can do this using `lm()`. It is important to remember that this time there are two
#' explanatory variables, so both need to be included in the model. For now we will include
#' them without an interaction. 
#' 
#' <span style="color:blue"> A3. **Run** the `lm()` to look at the impact of
#' fertiliser and light on above ground biomass. **What** are the coefficient estimates you get? 
#' **What** do they represent? </span>
#' 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# run the model
FertModel <- lm(Biomass.m2 ~ Fert + Light, data = FertData)

# get the coefficient estimates
coef(FertModel)

# and the confidence intervals
confint(FertModel)

#' 
#'<span style="color:purple"> Because we have categorical data, 
#'the coefficient estimates will be
#' (Intercept) = the mean of control group
#' each other value is a $\beta$ value representing the difference between 
#' the control and the fertiliser treatment,
#' then the difference between the control and the light treatment. </span>
#' 
#' <span style="color:blue"> A4. **Interpret** the results in qu A3. **What**
#' conclusions would you draw about the effectiveness of Fertiliser
#' and Light from these results? </span>
#' 
#' <span style="color:purple"> The results show an estimated effect of 
#' fertiliser of +141 grams 
#' per square metre and light has an estimated effect of
#' +77 grams per square metre. Fertiliser has a large and bigger
#' effect. The effect of fertiliser is positive even when uncertainty
#' is considered (93.91 to 188.88). 
#' Therefore, we can say that zero is not in the plausible range for this
#' parameter. 
#' Light, has a smaller effect (about half the size) of
#' fertiliser (based on mean estimate). But the confidence intervals also
#' do not cross zero for this effect (30.34 to 125.31), even with uncertainty included, we
#' can still estimate the direction of this effect.
#' Both fertiliser and light have a positive effect on above
#' ground biomass. fertiliser has a stronger estimated mean effect but
#' there is some overlap in uncertainty so we would not
#' conclude the effects are different. </span>
#' 
#' <span style="color:purple"> If the contrast is F+ and L+,
#' not the control, then all differences will be reversed (-141 and -77).
#' Should realise though that this means it will be harder to interpret, 
#' contrasting with control makes more sense. </span>
#' 
#' Given the results from your `lm()` above, it was not possible to see
#' the combined effect of the two treatments (Fertiliser and Light). 
#' However, there was a treatment where they were both applied together
#' so we should consider their combined effect. 
#' 
#' <span style="color:blue"> A5. **How** can you use your model
#' output to estimate the mean of the group that has fertiliser and light? </span>
#' 
#' <details><summary>Hint.</summary>
#' 
#' In the output you have an estimate for the effect of fertiliser (difference
#' in mean from control to fertiliser group). You also have an estimate
#' of the effect of light (difference in mean from control to light group).
#' In this model there is no interaction, so you assume that the fertiliser + light
#' group has the same effects as the fertiliser only and light only, but, both of them.
#' 
#' </details>
#'
#' <span style="color:purple"> You need to add the effect of light AND
#' the effect of fertiliser to the intercept value. In this case
#' it should result in 551 grams per square metre. </span>
#'   
#' <span style="color:blue"> A6. Calculate the actual mean of the fertiliser + light 
#' group. Is the estimate you got in A5 correct? If not, why not? </span>
#' 
#' <details><summary>How do I get the mean?</summary>
#' 
#' You can use the same code as in the module this week. Hint: it uses
#' `[]` and `==`.
#' 
#'  <details><summary>No, I don't remember</summary>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# take mean of the group F+ and L+
mean(FertData$Biomass.m2[FertData$Fert == "F+" & FertData$Light == "L+"])

#' </details>
#' 
#' </details>
#' 
#' <span style="color:purple"> The actual mean of this group is 575 grams of biomass 
#' per square metre. </span>
#' 
#' <span style="color:blue"> A7. Can you give a biological reason why this
#' might be the case? i.e. why does the actual mean differ from our model estimate?</span>
#'
#' <span style="color:purple"> The reason is because there is an interaction between fertiliser 
#' and Light. Our model works out the joint effect by
#' simply adding the individual effects. However,
#' it could be (and is) the case that both treatments
#' together alter the effect of each, creating a different effect
#' to that assumed by adding: an interaction. </span>
#'
#' Your team has looked at the results above and decided that the first model
#' they fitted is not capturing the effects of Fertiliser and Light well enough.
#' There seems to be something else going on when the two treatments are combined.
#' The two effects are not simply added together. Therefore, your team decides to 
#' fit a model including an interaction term. 
#'
#' -----
#' 
#' ## Part B: Dataset one - interaction
#' 
#' <span style="color:blue"> B1. **Run** a `lm()` with an interaction and **interpret** the output. 
#' How has this changed from the first `lm()` you ran? </span>
#' 
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
FertModel2 <- lm(Biomass.m2 ~ Fert*Light, data=FertData)

coef(FertModel2)

confint(FertModel2)

#' <span style="color:purple"> The mean effect of fertiliser has decreased to +93 grams/$m^2$ and Light to +30 grams/$m^2$.
#' While fertiliser still has a positive effect, even 
#' including uncertainty (28.24 to 159.15), the confidence intervals for light
#' now cross zero (-35.33 to 95.58). This means when we include the interaction
#' we are no longer confident of the direct of the effect
#' of Light alone. A negative, no effect or positive effect
#' of Light on its own are all plausible. 
#' The interaction effect is positive at +95 ggrams/$m^2$
#'  and this appears the case even with uncertainty 
#' considered (2.84 to 187.98). But the uncertainty is very wide! 
#' This means the effect of light and fertiliser
#' together is greater than you would expect based on their
#' individual effects. </span>
#' 
#' <span style="color:purple"> Again if the reference of the factor is F+ and L+
#' not the control then all differences will be reversed. </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# save the coefficients as an object 
coefficients <- coef(FertModel2)
# the first number in the coefficients is the mean of the control
# then the effect of fertiliser, the effect of Light
# finally the interaction

# The additive assumption for fertiliser + Light group =
coefficients[1]+coefficients[2]+coefficients[3]

# Including the interaction = 
coefficients[1]+coefficients[2]+coefficients[3]+coefficients[4]

# which gives the actual mean of that group

#' 
#' <span style="color:blue"> B2. **Describe** the interaction effect
#' in your own words. You might want to draw out the effects to help. </span>
#'
#' <span style="color:purple"> The interaction effect is to increase the effect of 
#' fertiliser under the influence of Light.
#' It also increases the effect of Light under the influence
#' of fertiliser. Both fertiliser and Light have a larger
#' effect when combined, than alone. </span>
#' 
#' <span style="color:blue"> B3. **What** would you recommend to the 
#' company as a strategy to maximise their production based on the results
#' you have so far? </span>
#'
#' <span style="color:purple"> If only one treatment can be applied should use fertiliser
#' but ideally use Light and fertiliser combined. This statement/recommendation
#' should be supported by the results above. Stating estimates and their confidence
#' intervals to show what level of result the company could expect. e.g. with 
#' light alone they could expect either a decrease in biomass by -35 grams/$m^2$
#' or an increase of 95 grams/$m^2$. Fertiliser alone would be expected to be
#' an increase of 28 to 159 grams/$m^2$. But combining both could cause
#' an extra increase of 3 to 188 grams/$m^2$. All estimates are from the 
#' interaction model as this one seems to better reflect the true relationship
#' between the three variables. 
#'  </span>
#' 
#' -----
#' 
#' ## Part C: Dataset two
#' 
#' ![Soya bean](Soybean.jpg)
#' * credit = Wikipedia
#' 
#' The second dataset that your team has access to is from Heggestad and Lesser (1990).
#' This was published in a study in the Journal of Environmental Quality 
#' [Paper link](https://dl.sciencesocieties.org/publications/jeq/abstracts/19/3/JEQ0190030488).
#' 
#' The data are from an experiment looking at the effects of low-level atmospheric
#' pollution and drought on agricultural yields. Specifically this study looked at
#' yields in soya bean plants and included treatments of water (either well watered or 
#' low water - i.e. water stressed) and a gradient of an atmospheric pollutant (sulphur dioxide).
#' 
#' The variables in the data are **Yield** log of the yield of the crop, 
#' **Water** the water treatment either Well-watered or
#' Stressed and **SO2** sulphur dioxide level (**remember it is O not zero!**)
#' 
#' The data can be found at 
#' https://www.math.ntnu.no/emner/ST2304/2019v/Week9/PollutionData.csv
#' 
#' As always, the first step is to import the data and assign it to an object then **plot it**. You can use the whole
#' web link above to import the data. It is a csv file with column names (header) included.
#' 
#' **Again, you will need to convert some columns to factors. This time it is 
#' only the `Water` column.**
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
PollutionData <- read.csv("https://www.math.ntnu.no/emner/ST2304/2019v/Week9/PollutionData.csv", 
                          header=TRUE)
PollutionData$Water <- as.factor(PollutionData$Water)

pairs(PollutionData)

#' <span style="color:blue"> C1. **What** is the response variable and **what** are the
#' explanatory variables here? **What** kind of data are each of these - give 
#' a reason why? </span>
#' 
#' **Hint: think carefully about whether values between the ones you have can exist.**
#' 
#' You can also check how they are stored in R, anything stored as `num` should be 
#' continuous numeric. 
#' 
#'<span style="color:purple"> Explanatory = Water (categorical, has two groups) 
#' and SO2 (continuous, has only 3 values but could take any value above 0),
#' Response = Yield (continuous, can take any value above 0) </span>
#' 
#' <span style="color:blue"> C2. **What** is the biological question you want
#' to address with these data? </span>
#'
#'<span style="color:purple"> We want to see if either Water stress or SO2 have an 
#' influence on yield achieved in soya beans.
#' For water we want to see the effect on mean yield.
#' For SO2 we want to see the relationship between SO2 and yield. </span>
#' 
#' You realise that you can, again, address this question using an `lm()`
#' You decide to keep things simple and not include an interaction. 
#' 
#' <span style="color:blue"> C3. **Run** an `lm()` for the effect of 
#' water stress and sulphur dioxide on yield. Look at the output (do not interpret yet)
#' can you work out what the coefficients mean in terms of a regression line? </span>
#' 
#' **Hint: Think carefully about what kind of data each variable is. This
#' will decide what its coefficient estimate means.**
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
PollutionModel <- lm(Yield ~ Water+SO2, data = PollutionData)

coef(PollutionModel)
confint(PollutionModel)

#' <span style="color:purple"> The answer is that the (Intercept) is the Intercept of a line
#' i.e. where X (SO2 and water) = 0, The Well-watered is the difference
#' in intercept for that group from the Stressed group,
#' SO2 value is the slope of the relationship between
#' SO2 and log of yield. </span>

#' It could be easier to see the effects on a graph. 
#' Below is some code to plot of the effect
#' of SO2 on Yield with a line for each treatment level (Well-watered and Stressed).
#' 
#' This is the same as some code you used in the module for week 9.
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = FALSE

# make the plot of the data
plot(Yield ~ SO2, data = PollutionData, pch=16, 
     las=1, ylab="log Yield")
# las = 1 makes the y axis numbers display horizontal

# add lines for each group (watered/stressed)
abline(a=coef(PollutionModel)[1], b=coef(PollutionModel)[3], col="grey")
abline(a=coef(PollutionModel)[1]+coef(PollutionModel)[2], 
       b=coef(PollutionModel)[3], col="blue")

# add a legend
legend("topright", c("Well-watered", "Stressed"), col=c("blue","grey"),
       lty=1, cex = 0.5)

# if you aren't sure what the code does, try running just one part at a time
# e.g. coef(PollutionModel)[1] to see what it does

#' 
#' <span style="color:blue"> C4. **Interpret** the output of your `lm()` using
#' the coefficients and the plot. </span>
#'
#' <span style="color:purple"> There is a negative relationship between SO2 concentration and
#' plant yield. log Yield decreases by 4 for every 1 increase in SO2.
#' This relationship is negative when we include uncertainty (does not cross 0: 
#' -7.8 to -0.39).
#' The effect of watering can be seen through the intercept.
#' Watering the plants leads to an increase in yield.
#' In the model we ran there is no interaction, so the effect of SO2 is the same
#' in Well-watered and Stressed plots. Plants that are watered more have 0.17 higher 
#' log yield on average. Uncertainty is also all positive, does not cross zero (0.005 to 0.35). 
#' So, even when we include uncertainty, we can still estimate the direction of
#' the effects and the plausible values are all negative for SO2 and positive for water. </span> 
#' 
#' **While there are only 3 values for SO2 it is NOT categorical data.**
#' 
#' **Hint: look at the numbers given in coefficient values and try to match
#' them to the graph. This will help you work out what the (Intercept) and
#' Well-watered values mean.**
#' 
#' -----
#' 
#' ## Part D: Recommendation
#' 
#' <span style="color:blue"> D1. Based on all of your results from both
#' datasets, what would
#' you recommend as a strategy for the company to improve productivity? Include
#' discussion of what you cannot say from the current data and analyses. </span>
#' 
#'<span style="color:purple"> Should include discussion of adding fertiliser and light, 
#' avoiding SO2 and
#' making sure plants are watered.
#' What you cannot say is whether there is any interaction between water and SO2 and
#' fertiliser and light. Both experiments have also been conducted on different
#' plants, so the results cannot be generalised.
#' Both a recommendation and draw backs should be included in a discussion. </span> 
#' 
#' -----
#' 
#' ## Part E: Feedback
#' 
#' <span style="color:blue"> E1. How do you think this exercise went? What do
#' you think your group did well, what are you less sure about? 
#' (2 examples of each) </span>
#' 
#' <span style="color:blue"> E2. What do you think you improved from last 
#' week? </span>
#' 
#' <span style="color:blue"> E3. Are there any concepts you are very unsure of? </span>
#' 
#' <span style="color:blue"> E4. What would you like feedback on this 
#' week? </span>
