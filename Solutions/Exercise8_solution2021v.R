#' # Excerise 8: Dinosaur park 
#'
#' # SOLUTION
#' 
#' ### Instructions:
#' 
#' # Make sure to read these - there have been some updates
#' 
#' This document contains information, questions, R code, and plots.
#' 
#' **Hints and reminders are bold**
#' 
#' <span style="color:blue"> Questions appear in blue. </span>
#' 
#' <span style="color:purple"> Answers in purple. </span>
#' 
#' #### Extra exercise tips:
#' 
#' 1. While questions are in blue, there might be some extra tasks in the text 
#' of the exercise. These extra tasks are not mandatory but might help you
#' achieve the blue task or explain why the next blue task is being done.
#' 
#' 2. Group work - while you should submit just one exercise per group, it is a
#' good idea that either all members run all of the code on their own computers
#' and then collectively write the answers. Or, all members help to write the
#' code on one computer. Remember that a lot of the answers require words or
#' an answer not just code!
#' 
#' 3. Definitions of question words. We received feedback that it is not always
#' clear what you need to do for each question, there are definitions of the
#' question words in the Glossary on Blackboard (but we know it is hard to find!).
#' [Link to glossary](https://ntnu.blackboard.com/webapps/Bb-wiki-BB5def77a38a2f7/wikiView?course_id=_16763_1&wiki_id=_91149_1&page_guid=c38de43765074c5d94623b8f2ad11970)
#' 
#' </br>
#' 
#' **Needs to be completed and handed in by 26th March 23:59**
#' 
#' -----
#' 
#' ## Rationale:
#' 
#' **This week we want you to make some decisions on when to do each analysis. 
#' Therefore,
#' we have included code help and hints in separate HTMLs with code 
#' not here in the exercise.**
#' 
#' As we are getting further through the course, these exercises will start to 
#' ask you to make more decisions for yourself. The aim of the course is that 
#' you will be able to analyse your own data, so it is good to have some practice
#' doing this without us telling you every step. 
#' 
#' ## Resources:
#' 
#' * [Confirmatory code](https://www.math.ntnu.no/emner/ST2304/2021v/Week10/ConfirmatoryScript.html)
#' * [Exploratory code](https://www.math.ntnu.no/emner/ST2304/2021v/Week10/ExploratoryScript.html)
#' * [This week's module](https://www.math.ntnu.no/emner/ST2304/2021v/Week10)
#' 
#' ### R this week:
#' 
#' All R parts explained in extra code htmls.
#' 
#' * `lm()` as always
#' * `bestglm()`
#' * `AIC()`
#' * `BIC()`
#' * `anova()`
#' 
#' Might need to install the `bestglm` package.
#' 
#' -----
#' 
#' ## The challenge: How can we create the best dinosaur exhibit at a new zoo?
#' 
#' You are the board of directors of a new zoo opening in Norway. You want your
#' zoo to be both exciting and educational, to teach visitors about all different
#' kinds of plants and animals from throughout time. You have been very excited by
#' new advances in cloning technology that you saw in Jurassic Park (you are not very 
#' up to date). You have set
#' up a team of biologists to try and clone some dinosaurs to complete your "Ancient
#' History" exhibit. They have been trialling different cloning techniques to try
#' and work out the best protocol. 
#' 
#' You have also set up another team to investigate public opinion of dinosaurs.
#' It is very expensive to clone and to keep dinosaurs so you want to make sure that
#' you are investing in the right ones.
#' 
#' Both teams have sent their results back you. Now you must analyse the data and
#' decide on how to set up your dinosaur exhibit. 
#' 
#' <span style="color:blue"> Your job is to find out how to use resources
#' most efficiently to create an exciting dinosaur exhibit. </span>
#' 
#' ![Dino](Dino.jpg)
#' 
#' -----
#' 
#' ## Part A: Concepts
#' 
#' This week, we will begin with some very general conceptual points. As you are
#' part of a board of directors for the zoo, you will need to answer to your  
#' investors. Therefore, you need to begin your recommendation for which
#' dinosaurs to include with a brief introduction into the statistics you will
#' use to make this decision and why. 
#' 
#' **Remember to think about your audience**. The shareholders are not scientists,
#' or statisticians. So, make sure your explanation is suitable for non-scientists too. 
#' 
#' <span style="color:blue"> A1. **Why** do we have model selection in statistics? </span>
#' 
#' <span style="color:purple"> We use model selection in statistics to try and 
#' find the most appropriate explanatory variables (variables that influence
#' the response e.g. variables that impact cloning success) to include in our model.
#' Once we have chosen the model that best explains how our data were generated 
#' e.g. a Poisson GLM or a linear regression with normal error,
#' we then need to decide which explanatory variables to include to 
#' explain our response. Often in the real world, there are many
#' different explanatory variables we could include, either with or 
#' without interactions (whether they impact each other, such as light could 
#' change the effect of fertiliser on a plant). 
#' There is a balance between including everything
#' and explaining a lot of variance and keeping a model simple. 
#' The more variables you add, the more parameters (things) you estimate from the data
#' which could lead to a model becoming over-fitted and no more simple than 
#' reality itself
#' (where we have equal to or more variables than data). Therefore, we need
#' to conduct model selection. All variables we add will 
#' increase the amount of variation that we explain
#' but not always because they actually influence the response 
#' (even random data will increase R squared - see lecture module).
#' So, we need ways to tell whether a variable is worth adding to a model or not. 
#' We do this in two ways, either by confirming a hypothesis
#' or exploring which variables best explain the data. This way our team can
#' find the models that best balance complexity and explanation for each
#' part of dinosaur cloning. (You could probably make this even less technical!)</span>
#' 
#' <span style="color:blue"> A2. **What** are two of the different aims of model selection? </span>
#' 
#' <span style="color:purple"> The two different aims are 1) hypothesis testing/confirmatory 
#' model selection i.e. does this extra variable or interaction effect
#' have a statistically significant effect on our response (our Y variable - cloning success index)? 
#' 2) which variables, of many, best explain variance in our response (Y)/
#' exploratory model selection i.e. of these 3 variables we measure, 
#' which explain more variation than would be expected by chance and are worth including
#' despite the extra parameters we need to estimate? </span>
#' 
#' <span style="color:blue"> A3. **How** do you perform model selection for each of these? i.e.
#' name the technique don't give all the details </span>
#' 
#' <span style="color:purple"> 1) Conduct confirmatory model selection using anova (for linear models) 
#' or analysis of deviance (generalised linear models - learn this in a week or so).
#' 2) Conduct exploratory model selection using the AIC or BIC. </span>
#' 
#' -----
#' 
#' ## Part B: Which variables influence cloning success?
#' 
#' These data have been collected by your team of scientists in the cloning facility.
#' They have been trying to clone several different species of dinosaur using 
#' fossils of different
#' ages and different lab procedures. 
#' They have recorded the 'success' of the cloning
#'  as an index created from the
#' number of viable embryos created, longevity of embryos, 
#' and the cost of the cloning method. 
#' The index has positive and
#' negative values, positive indicating greater success from the investment. 
#' This is called **SuccessIndex** in the
#' data and is the response variable.
#' 
#' The explanatory variables they collected are: **Age** this is age of 
#' the fossil being cloned in million years, **Size** this is the average 
#' adult body weight
#' of the dinosaur species being cloned in metric tons, **Herbivore** 
#' this is an indicator of whether the species
#' is a herbivore (TRUE) or a carnivore (FALSE).
#' 
#' **Think about what kind of data (continuous or categorical) each of these are. 
#' It will help you with interpreting.**
#' 
#' It is thought that some of these variables might explain the variation 
#' in cloning success index. But it is 
#' not yet known which.
#' 
#' The dataset for this questions can be found at 
#' https://www.math.ntnu.no/emner/ST2304/2021v/Week10/CloneData_2021v.csv
#' 
#' As always, the first step is to import the data and assign it to an object. 
#' You can use the whole
#' web link above to import the data. 
#' It is a csv file with column names (header) included.
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval=FALSE
CloneData <- read.csv("https://www.math.ntnu.no/emner/ST2304/2021v/Week10/CloneData_2021v.csv", header=T)

pairs(CloneData)

#' 
#' <span style="color:blue"> B1. Look at the question at the start of this 
#' section (in the title of Part B). 
#' **Is** this
#' question confirmatory or exploratory? **Why**? </span>
#' 
#' <span style="color:purple"> Exploratory because it is asking which variables, 
#' of many, and
#' not a specific hypothesis about any of them. </span>
#' 
#' Based on your answer to question B1, open the appropriate help HTML for this section.
#' These are listed in the Resources section.
#' 
#' You do not have to use the HTML document, the code is included in the module
#' for week 10. See if you can try and list the steps you need for B2 before 
#' looking at the HTML. 
#' 
#' <span style="color:blue"> B2. **Conduct** model selection for answering
#' "which variables influence cloning success?" and **Write** a report of
#' the steps you took to do this, why, and state what the result was.</span> 
#' 
#' **To answer this question include a 
#' bullet point list of the steps you take to do this. You can include a line or two
#' of R code with each bullet point but you should not need a lot (so, you also
#' need to use words to explain why you are doing each step not only the R code).** </span>
#' 
#' Image that you are writing a report to be read by the investors in the zoo 
#' when you answer B2. They will want to know the results of each step but also
#' have some idea why you are running certain lines of code. 
#'
#' <span style="color:purple"> For this question you should conduct exploratory 
#' model selection to find the variables that 
#' best explain variation in SuccessIndex. 
#' Should use the AIC or BIC, either is fine but should be justified. 
#' BIC has a higher penalty for
#' extra parameters, so you should choose why this would be beneficial or not. 
#' Do you 
#' want to aim for a simple model? 
#' Or to explain most variance without extra penalty? </span>
#' 
#' <span style="color:purple"> Detailed steps in R code, justification as 
#' comments: It is important to look at the full output of the selection i.e.
#' each model and the AIC or BIC, because models with less than 2 difference
#' are not considered to be substantially different in their support. </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE
# Steps of the exploratory model selection

# There are two ways that you can do exploratory model section.

# METHOD 1: is manual. This is more useful when there are only a few 
# combinations of explanatory variables. I would not recommend here but you can
# you would not lose marks for doing it. 

# First: create a null model with no variables
# This is what you want to compare the other models to. 
null <- lm(SuccessIndex ~ 1, data = CloneData)

# Then one for each explanatory variable alone
# This is so you can compare the effect of adding each variable to the model
model1 <- lm(SuccessIndex ~ Age, data = CloneData)
model2 <- lm(SuccessIndex ~ Size, data = CloneData)
model3 <- lm(SuccessIndex ~ Herbivore, data = CloneData)

# Then you would run for all combinations of explanatory variables and 
# interactions (at least 13 models in this case)

# Then you would calculate the AIC or BIC. Both let you compare the models.
# They have a penalty for adding extra parameters so help to reduce model 
# complexity. The lowest AIC or BIC indicates the preferred model with the best
# (of the subset tested) balance of complexity and explanation. 
AIC(null, model1, model2, model3) # find AIC
BIC(null, model1, model2, model3) # find BIC


# Another way to do this, would be to use the bestglm package. I would 
# recommend here because this is easier than writing out all possible models. 

library(bestglm)

# create NewData to use in bestglm make sure the response is last in the dataframe.
NewData <- CloneData[ ,c(2:4, 1)] 

# Then you can use bestglm() using AIC or BIC
OutputAIC <- bestglm(NewData, IC="AIC")
OutputBIC <-bestglm(NewData, IC="BIC")

# You should then look at the results and find the preferred model. 

coef(OutputAIC$BestModel)
confint(OutputAIC$BestModel)

coef(OutputBIC$BestModel)
confint(OutputBIC$BestModel)

OutputAIC$Subsets
OutputBIC$Subsets

# Should get a model with Age and Size if you use either the BIC or AIC. 

# You might notice not ALL combinations are included. It shows the null and the
# lowest AIC or BIC for one variable, then two, then three etc. 

# This disagreement between the two model selection techniques suggests that
# there is not a clear model that best balances complexity and explanation/
# prediction. 

#' 
#' <span style="color:blue"> B3. **Interpret** the results from the model selection.
#' **Include reference to model selection and the final model you end up with. I.e.
#' you should also mention what the effect any variables have** </span>
#' 
#' <span style="color:purple"> Model selection showed that 
#' the model with age and size, was the best supported. 
#' This was shown by it having the lowest BIC and AIC value.
#' Age has a positive relationship with cloning success,
#' success increases `r coef(OutputBIC$BestModel)[2]` 
#' (confidence interval of `r confint(OutputBIC$BestModel)[2,]`) 
#' for every 1 million year increase in age.
#' Size has a negative relationship, success decreases 
#' `r coef(OutputBIC$BestModel)[3]` (confidence interval of 
#' `r confint(OutputBIC$BestModel)[3,]`) for
#' every 1 ton increase in size. Neither sets of confidence intervals cross 0. 
#' So, we have reasonable support for the direction of the effects. Even
#' when we consider uncertainty, the direction is still clear. </span>
#' 
#' <span style="color:purple"> Extra bit for AIC: AIC has a lower penalty 
#' than BIC for extra parameters. This led to the result being a bit less clear
#' using the AIC. For the AIC the difference between a model with Herbivore
#' included and one without was <2. This means that these models are not 
#' considered to have substantially different support based on the AIC.   
#' So, this model without Herbivore is no 'better' than one with Herbivore, 
#' but no worse either. Ideally, 
#' you want to represent the results from both models, but that is a bit 
#' beyond this course. </span>
#' 
#' -----
#' 
#' ## Part C: Does the size of a dinosaur affect their popularity?
#' 
#' These data were collected from a large survey of the general public. 
#' The participants were
#' asked to rate, on a continuous scale (0-100), 
#' how much they liked different dinosaur species.
#' The species all differed in size. 
#' The board members (your team) think that visitors to your
#' zoo will be more excited to see bigger dinosaurs because bigger dinosaurs 
#' are more popular.
#' 
#' The dataset has columns: **PopularityScore** the popularity score of the d
#' inosaur species, 
#' **Weight** weight of the species in metric tons (a measure of size).
#' 
#' The data for this question can be found at 
#' https://www.math.ntnu.no/emner/ST2304/2021v/Week10/DinoData_2021v.csv
#' 
#' As always, the first step is to import the data and assign it to an object. 
#' You can use the whole
#' web link above to import the data. It is a csv file with column names (header) included.
#'
#' 
#+ warning = FALSE, error = FALSE, include = FALSE, echo = FALSE, eval=FALSE
DinoData <- read.csv("https://www.math.ntnu.no/emner/ST2304/2021v/Week10/DinoData_2021v.csv", header=T)

pairs(DinoData)

#' <span style="color:blue"> C1. Look at the question at the start of this 
#' section (in the title
#' for Part C). **Is** this
#' question confirmatory or exploratory? **Why**? </span>
#' 
#' <span style="color:purple"> Confirmatory, 
#' it is a specific hypothesis about a relationship
#' between size and popularity. </span>
#' 
#' <span style="color:blue"> C2. **Conduct** model selection for answering
#' "does the size (weight) of a dinosaur affect its popularity?" 
#' and **Write** a report of
#' the steps you took to do this, why, and what the result was.</span>
#' 
#' **To answer this question include a 
#' bullet point list of the steps you take to do this. You can include a line or two
#' of R code with each bullet point but you should not need a lot. (Again, R
#' code alone will not answer this question, also say why you do each step).**
#' 
#' Image that you are writing a report to be read by the investors in the zoo 
#' when you answer C2. They will want to know the results of each step but also
#' have some idea why you are running certain lines of code. 
#' 
#' <span style="color:purple"> Correct steps will be in the R code below. 
#' Should conduct confirmatory model selection to test whether there is a relationship
#' between popularity and weight. </span>
#' 
#+ warning = FALSE, error = FALSE, include = TRUE, echo = TRUE

# The first step is to decide what your null and alternative hypotheses are.
# Here the null is nothing influences popularity
# the alternative is the weight of a dinosaur influences their popularity.

# Begin model selection by creating H0 model 
ModelH0 <- lm(PopularityScore ~ 1, data = DinoData)

# Then create the H1 model 
ModelH1 <- lm(PopularityScore ~ Weight, data = DinoData)

# Then you compare them using the ANOVA function. This allows you to compare
# the likelihood of each model through a hypothesis test. This is done by 
# comparing the likelihood you estimated for the alternative hypothesis with the
# distribution of the likelihood if the null hypothesis was true and working
# out the probability of getting the likelihood you observed or higher if the
# null was true. 
HypothesisTest <- anova(ModelH0, ModelH1) 
HypothesisTest

# - make sure the models are in correct order
# The DF in the anova output should be 1 NOT -1

# Then you interpret the results (answer below) and also the results of the 
# preferred model

# In this case, the selection showed support for the alternative hypothesis
# so we REJECT the null (H0) and can interpret ModelH1

coef(ModelH1)
confint(ModelH1)

#' 
#' <span style="color:blue"> C3. **Interpret** the results from the model selection.
#' **Include reference to model selection and the final model you end up with. I.e.
#' you should also mention what the effect of any variables are** </span>
#' 
#' <span style="color:purple"> The F-value from the anova is 
#' `r HypothesisTest$F[2]` with probability of 
#' getting this value or higher if H0 is true of `r HypothesisTest$"Pr(>F)"[2]`. 
#' This lower than the usual cut off of 0.05 so we can reject H0.
#' We can conclude that Weight of species does have a relationship
#' with popularity.
#' Bigger dinosaurs are more popular by `r coef(ModelH1)[2]` points per metric ton.
#' Confidence intervals do not span 0 (`r confint(ModelH1)[2,]`), 
#' so we are unlikely to see this effect if H0 is true. 
#' R squared for this model is very low (`r round(summary(ModelH1)$r.squared, 2)`). 
#' Although statistically 
#' significant, weight does not explain much variation in PopularityScore. </span>
#' 
#' -----
#' 
#' ## Part D: Recommendation
#' 
#' <span style="color:blue"> D1. Based on all of your results, what would
#' you recommend as a way to create an efficient and exciting exhibit? </span>
#' 
#' <span style="color:purple"> Recommend bigger dinosaurs (based on the results
#' of popularity) and older fossils (because they have higher cloning success). 
#' But bigger dinosaurs are also harder to clone - so there needs to be a 
#' balance to be most cost effective. 
#' Got conflicting results so could be hard to decide.
#' There is no single correct answer but all answers should discuss the 
#' conflicting
#' results. That big dinosaurs are more population but harder
#' to clone. Some mention of the low R squared for popularity model should 
#' also be included.  
#' This indicates that there is more influencing popularity than just size. 
#' Also all dinosaurs are
#' very popular, so maybe it is ok to have smaller ones. 
#' (These are just examples of points and justification). Whether you take 
#' Herbivores or Carnivores is not so important based on these results, but
#' could be very important if you don't want your dinosaurs to eat each other. </span>
#' 
#' -----
#' 
#' ## Part E: Reflection and feedback
#' 
#' <span style="color:blue"> E1. If you were to do this analysis in real life,
#' is there anything you would change about the experimental design or the
#' analysis? Was anything missing? Any extra steps you would take or extra information
#' you might want? </span>
#' 
#' <span style="color:purple"> Any sensible answer is ok here. Emily's example:
#' I would want to add a few things to the experimental design. For instance, 
#' you could look at the family of the dinosaur in cloning success or the lab
#' conditions or type of fossil e.g. what sediment it was preserved in. 
#' For popularity, I would maybe want to look at age of the dinosaur too so that
#' it matched better with the cloning success data and also whether herbivores
#' are more popular than carnivores. You might even want to go qualitative and
#' have a focus group to find out in more depth what people like about dinosaurs.
#' In terms of extra analyses, I would want to add model checking for each
#' final model. </span>
#' 
