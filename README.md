# LatinR: Creating data plots for effective decision-making using statistical inference with R

Instructor: Professor Di Cook, Department of Econometrics and Business Statistics, Monash University

Website: [https://dicook.github.io/LatinR](https://dicook.github.io/LatinR)

## Structure of tutorial

- Review of making effective plots using ggplot2's grammar of graphics:
    - Organising your data to enable mapping variables to graphical elements, 
    - Common plot descriptions as scripts,
    - Do's and don'ts following cognitive perception principles.
- Making decisions and inferential statements based on data plots
    - What is your plot testing? Determining the hypothesis based on the type of plot.
    - Creating null samples to build lineups for comparison and testing.
    - Conducting a lineup test using your friends to determine whether what you see is real or spurious, and to determine the best design for your plot.

Background: Participants should have a good working knowledge of R, and tidy verse, and some experience with ggplot2. Familiarity with the material in R4DS (https://r4ds.hadley.nz) is helpful.

## Course Schedule

| time | topic |
|------|-------|
|1:40-1:55|	Why, philosophy and benefits|
|1:55-2:15|	Organising data to map variables to plots|
|2:15-2:45|	Making a variety of plots|
|2:45-3:10|	Do but don’t, and cognitive principles|
|3:10-3:40|	COFFEE BREAK|
|3:40-4:00|	What is your plot testing?|
|4:00-4:15|	Creating null samples|
|4:15-4:45|	Conducting a lineup test|
|4:45-5:00|	Testing for best plot design|

[Session 1 Slides](https://dicook.github.io/LatinR/slides1.html)

[Session 2 Slides](https://dicook.github.io/LatinR/slides2.html)

[Zip file of materials](https://dicook.github.io/LatinR/vis-tutorial.zip)

## Getting started

1. You should have a reasonably up to date version of R and R Studio, eg RStudio RStudio 2023.06.2 +561 and R version 4.3.1 (2023-06-16). 
```
install.packages(c("ggplot2", "tidyr", "dplyr", "readr", "nullabor", "colorspace", "palmerpenguins", "broom"))
```

2. Download the [Zip file of materials](https://dicook.github.io/LatinR/vis-tutorial.zip) to your laptop, and unzip it. 

3. Open your RStudio be clicking on `tutorial.Rproj`. 

There might be a few more packages ot install to run various parts.

GitHub repo with all materials is 
[https://github.com/dicook/LatinR](https://github.com/dicook/LatinR).
