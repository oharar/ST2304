# ST2304

This is the material for ST2304 Statistisk modellering for biologer/bioteknologer (or ST2304 Statistical modelling for biologists/bioteknologists), which is run at NTNU. It includes a module for each week, and exercises starting at week 3. I have not included the solutions to the exercises, for obvious reasons.

Each module is written as an R markdown file. It includes explanations of the topic, in-class exercises (with hints and solutions) and links to videos and other material. In theory rendering the R markdown files will provide everytning, with the exception of embedded videos (see below).

Please consider the licence for this repo to be [CC BY-NC-SA](https://creativecommons.org/licenses/by-nc-sa/4.0/)


## Videos

The videos are hosted on NTNU's Panopto site. If you need to render the R Markdown files to create a new html file, the videos don't embed properly. So instead you need to embed them by hand. There are two ways to do this, depending on what we've done:

The easiest is if we have already added the html code in the RMarkdown document. If we have done this it will be commented out, so try this:

- search the html file for "<!---". 
- check that this comments out a video: it should start with an iframe tag ("<iframe src=...."), and should be next to a link to the video
- remove the commenting out: "<!---" and "--->"
- save the file (and check it's OK)

If you do not find any videos this way, we probably added them with Plan B. So you need to do this:

- search the html file for "ADD EMBED". 
- work out which video goes with this. The link to the video should be just above the ADD EMBED.
- Go to the video, and getthe code: Settings (the cog at the top of the screen) > Share > Embed > Copy Embed Code
- Paste the code into the html file, in place of the "ADD EMBED X"

If that doesn't work, you could try to extract them from [previous years' pages](https://wiki.math.ntnu.no/st2304/2022v/start).

Note that you will have to do this evert time you re-make the html document.

## Acknowledgements

The material was initially created and revised by @oharar and @emilygsimmonds.

## Course Content



### Module 1: Introduction

* [Intro Lecture slides](Module01%20Introduction/Lecture1.pdf)
* [R Lecture](Module01 Introduction/Introduction_to_R.pdf)
* [R Module](Module01 Introduction/R-tutorial.html)
* [Additional help: dealing with errors and warnings](Module01 Introduction/Errors-and-warnings.html)

[Taskcard link](https://ntnu.taskcards.app/#/board/7855bc8c-411d-4561-926c-474906820227?token=140a75f0-9c9c-4bc7-a1d4-3eac9203b1e2) (this will be explained)

### Module 2: Estimating a Parameter

  * [Module 2](Module02 One Parameter/Module02MLEs.html)
  * [An introduction to directories](Module02 One Parameter/Directories.html), for those confused by them.
  * [R functions for the module](Module02 One Parameter/Module02Functions.R). You probably don't need to look at this file - we will use it in the module.

### Module 3: Confidence Intervals

*Exercises start this week!*

  * [Module 3](Module03 Confidence Intervals/Module03ConfidenceIntervals.html)
  * [Exercise 1](Module03 Confidence Intervals/Exercise1.html)
  * [R functions for the module](Module03 Confidence Intervals/InferenceFunctions.R).
  * [Notes on the difference between probability distribution and likelihood](https://www.math.ntnu.no/emner/ST2304/2025v/Module03/St2304-240123.pdf)

### Module 4: Normal Distribution

  * [Module 4](Module04 Normal Distribution/Module04Normal.html)
  * [Exercise 2](Module04 Normal Distribution/Exercise2.html)
  * [R functions for the module](Module04 Normal Distribution/NormalDistFunctions.R).

### Module 5: Regression

  * [Module 5](Module05 Regression/Module05Regression.html)
  * [Exercise 3](Module05 Regression/Exercise03.html)


### Module 6: Model Checking
  * [Module](Module06 ModelChecking/Module06ModelChecking.html)
  * [Exercise 4](Module06 ModelChecking/Exercise04.html)


### Module 7: Multiple Regression
  * [Module](Module07 Multiple Regression/Module07MultipleRegression.html)
  * [Exercise 5](Module07 Multiple Regression/Exercise05.html)

### Module 8: Categorical Variables (aka ANOVA)

  * [Module](Module08 Categorical/Module08Categorical.html)
  * [Exercise 6](Module08 Categorical/Exercise06.html)

### Module 9: Interactions between Categorical Variables
  * [Module](Module09 MoreCategorical/Module09Interactions.html)
  * [Exercise 7](Module09 MoreCategorical/Exercise07.html)

### Module 10: Model Selection
This week's module is split into 3 parts, because it was getting a bit long.
  * [Module, Part A](Module10 ModelSelection/Module10PartA.html)
  * [Module, Part B](Module10 ModelSelection/Module10PartB.html)
  * [Module, Part C](Module10 ModelSelection/Module10PartC.html)
  * [Exercise 8](Module10 ModelSelection/Exercise08.html)

In addition we have a couple of scripts that might help you when running the problems in R:
  * [Confirmatory Script](Module10 ModelSelection/ConfirmatoryScript.html)
  * [Exploratory Script](Module10 ModelSelection/ExploratoryScript.html)

### Module 10a: Full Analyses (the precise location of this might vary depending on things like Easter)

This week will be a bit different. We want you to look at the whole process of data analysis, i.e. use all the parts you have been learning over the last few weeks to answer some biological questions.

We only expect you to answer one of these, but we will be happy if you try both. There are two ways to answer them: some exam-style questions (for those of you looking ahead), and a more free-form "try to do the analysis on your own". For those of you taking the continuation exam, note that this will probably be an oral exam, so we will ask you to do an analysis like this, and discuss in in the exam.

  * [Introduction](Module10a - Example Analyses/Module10Introduction.html)
  * [Cow Questions](Module10a - Example Analyses/Module10aCowQuestions.html)
  * [Iris Questions](Module10a - Example Analyses/Module10aIrisQuestions.html)


### Module 11: GLMs (week beginning March 23rd) ===
  * [Module](Module11 Generalised linear models/Module11GLMs.html)
  * [Exercise 9](Module11 Generalised linear models/Exercise09.html)

### Module 12: Binomial Generalised Linear Models

  * [Binomial Module](Module12 Binomial GLMs/Module12Binomial.html)
  * [Exercise 10](Module12 Binomial GLMs/Exercises10.html)

### Module 13: Poisson Generalised Linear Models

  * [Poisson Module](Module13 Poisson GLMs/Module13Poisson.html)
  * [Exercise 11](Module13 Poisson GLMs/Exercise11.html)



