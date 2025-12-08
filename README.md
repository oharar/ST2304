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

[Intro Lecture slides](Module01%20Introduction/Lecture1.pdf)

### Module 1: Introduction

* ???
* [R Lecture](Module01 Introduction/Introduction_to_R.pdf)
* [R Module](Module01 Introduction/R-tutorial.html)
* [Additional help: dealing with errors and warnings](Module01 Introduction/Errors-and-warnings.html)

[Taskcard link](https://ntnu.taskcards.app/#/board/7855bc8c-411d-4561-926c-474906820227?token=140a75f0-9c9c-4bc7-a1d4-3eac9203b1e2) (this will be explained)

=== Module 2: Estimating a Parameter (week beginning January 13th) ===

  * [Module 2]([Module02/Module02MLEs.html)
  * [An introduction to directories](Module02/Directories.html), for those confused by them.
  * [R functions for the module](Module02/Module02Functions.R). You probably don't need to look at this file - we will use it in the module.

=== Module 3: Confidence Intervals (week beginning January 20th) === 

\\ Exercises start this week!\\  
  * [Module 3](Module03/Module03ConfidenceIntervals.html)
  * [Exercise 1](Module03/Exercise1.html).  (deadline: end of Sunday 2nd February)\\   
  * [R functions for the module](Module03/InferenceFunctions.R).
 [ * ]Notes on the difference between probability distribution and likelihood](https://www.math.ntnu.no/emner/ST2304/2025v/Module03/St2304-240123.pdf].

=== Module 4: Normal Distribution (week beginning January 27th) ===

  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module04/Module04Normal.html|Module 4]]
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module04/Exercise2.html|Exercise 2]].   (deadline: end of Sunday 9th February)
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module03/NormalDistFunctions.R|R functions for the module]].

=== Module 5: Regression (week beginning February 3rd)===
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module05/Module05Regression.html|Module 5]]   
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module05/Exercise03.html|Exercise 3]].   (deadline: end of Sunday 16th February)



=== Module 6: Regression (week beginning February 10th) ===  
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module06/Module06ModelChecking.html|Module (includes all learning material for this week)]]  
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module06/Exercise04.html|Exercise 4]] (hand in by end of February 23th)


=== Module 7: Multiple Regression (week beginning February 17th) ===  
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module07/Module07MultipleRegression.html|Module (includes all learning material for this week)]]  
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module07/Exercise05.html|Exercise 5]] (hand in by end of March 2nd)

=== Module 8: Categorical Variables (aka ANOVA) (week beginning February 24th) ===
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module08/Module08Categorical.html|Module (includes all learning material for this week)]]
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module08/Exercise06.html|Exercise 6]] (hand in by end of March 9th)

=== Module 9: Interactions between Categorical Variables (week beginning March 3rd) ===
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module09/Module09Interactions.html|Module (includes all learning material for this week)]]
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module09/Exercise07.html|Exercise 7]] (hand in by end of March 16th)

=== Module 10: Model Selection (week beginning March 10th) === 
This week's module is split into 3 parts, because it was getting a bit long.
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10/Module10PartA.html|Module, Part A]]
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10/Module10PartB.html|Module, Part B]]
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10/Module10PartC.html|Module, Part C]]
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10/Exercise08.html|Exercise 8]] (hand in by end of March 23rd)

In addition we have a couple of scripts that might help you when running the problems in R:
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10/ConfirmatoryScript.html|Confirmatory Script]]
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10/ExploratoryScript.html|Exploratory Script]]

=== Module 10a: Full Analyses (week beginning March 17th) ===

This week will be a bit different. We want you to look at the whole process of data analysis, i.e. use all the parts you have been learning over the last few weeks to answer some biological questions.

We only expect you to answer one of these, but we will be happy if you try both. There are two ways to answer them: some exam-style questions (for those of you looking ahead), and a more free-form "try to do the analysis on your own". For those of you taking the continuation exam, note that this will probably be an oral exam, so we will ask you to do an analysis like this, and discuss in in the exam.

 The deadline for handing in is the end of March 30th.  

  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10a/Module10Introduction.html|Introduction]]  
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10a/Module10aCowQuestions.html|Cow Questions]]   
  * [[https://www.math.ntnu.no/emner/ST2304/2024v/Module10a/Module10aIrisQuestions.html|Iris Questions]]


=== Module 11: GLMs (week beginning March 23rd) ===
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module11/Module11GLMs.html|Module]]
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module11/Exercise09.html|Exercise 9]] (hand in by end of April 6th)

=== Module 12: Binomial Generalised Linear Models (week beginning 30th March) ===
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module12/Module12Binomial.html|Binomial Module]]
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module12/Exercise10.html|Exercise 10]] (hand in by end of April 13th. This is before Easter week starts, so grading may be a bit delayed)

=== Module 13: Poisson Generalised Linear Models (week beginning 7th April) ===
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module13/Module13Poisson.html|Poisson Module]]
  * [[https://www.math.ntnu.no/emner/ST2304/2025v/Module13/Exercise11.html| Exercise 11]] (hand in by end of April 28th: note a couple of extra days, because of Easter)



