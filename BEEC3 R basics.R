# BEEC course stats tutorial 
# Adéla Pafková, University of Edinburgh, s1978469@ed.ac.uk

# Session 2: Plotting data, regression analysis, T-test ----
rm(list=ls())  # clear the work environment
library(ggplot2)  # load necessarz libraries
limpetdata <- read.csv("Limpetdata.csv")  # loading data
ggplot(limpetdata, aes(x = shoreheight, y = width)) + geom_point()  # plot the data

# Get basic statistics
summary(limpetdata)
mean(limpetdata$width)
sd(limpetdata$width)
by(limpetdata$width, limpetdata$shoreheight, mean)  # Filters data by site (width), gives the mean shore height for each site

# Fit linear model
my.model <- lm(width ~ shoreheight, data = limpetdata)
summary(my.model)

headupdatalong <- read.csv("Headupdatalong.csv")
# The data is the rate of raising the head (a behaviour associated with vigilance) in lone adult roe deer.
# You are going to try to decide whether there is evidence that there is a sex difference in this behaviour.

headupdatalong$rate <- headupdatalong$headup # create new column "rate" identical to "headup"
by(headupdatalong$rate, headupdatalong$sex, mean)  # get mean rate for each sex

# T test
hist(headupdatalong$rate)  # check if data is normally distributed
t.test(headupdatalong$rate ~ headupdatalong$sex, data = headupdatalong)

# Plot data in a way relevant to the question

## Scatterplot
(vigilance_scatterplot <- ggplot(headupdatalong, aes (x = sex, y = rate, colour = sex)) +
  geom_point() +
  xlab ("Sex") +
  ylab ("Rate of raising the head") +
  theme_bw())

## Boxplot
(vigilance_boxplot <- ggplot(headupdatalong, aes(sex, rate)) +
    geom_boxplot (aes(fill = sex)) +
    theme_bw())

# Session 3: Data manipulation ----

limpetdata <- cbind(limpetdata, (limpetdata$height/limpetdata$width))  # create a new column with values for ratio heigh/width
limpetdata$ratio <- limpetdata$height/limpetdata$width  # give it a reasonable name

# Examples of other other mathematical operations we can perform (non exhaustive)
limpetdata <- cbind(limpetdata, log10(limpetdata$height))  # create new column with log transformed data
limpetdata <- cbind(limpetdata, sqrt(limpetdata$height))  # create new column with sqrt of data

# Session 4: Correlation analysis ----
cor.test(limpetdata$width, limpetdata$height)  # Pearson correlation analysis

# Session 5: T-test ----

headupdata <- read.csv("Headupdata.csv")  # upload data
headupdata  # check the data

t.test(headupdata$male, headupdata$female, paired = FALSE)
t.test(headupdata$male, headupdata$female, paired = FALSE, var.equal = TRUE)  # if variences are equal

# Session 6: ANOVA ----

#install.packages("dplyr")
library(dplyr)  # needed for rename() function

latitudebirds <- read.csv("Latitudebirdsdata.csv")  # load data
latitudebirds  # check data

latitudebirds <- rename(latitudebirds, latitude = Latitude, length = tarsal_length)  # rename columns

(birdsboxplot <- ggplot(latitudebirds, aes(latitude, weight)) +   # plot a boxplot, colour by latitude
    geom_boxplot(aes(fill = latitude)) +
    theme_bw())

# ANOVA

bird.model <- lm(weight ~ latitude, data = latitudebirds)  # fit a linear model
anova(bird.model)  # ask for the anova of the model

# Could you alter the script to analyse the tarsal length measurements rather than
# the weight measurements to check there is also no difference in size across the different categories of sites?
# Using what you learned in earlier practicals could you combine these two variables
# to produce a condition index of the birds and analyse whether this differs between sites?

(birdsboxplotlength <- ggplot(latitudebirds, aes(latitude, length)) +   # plot a boxplot using tarsal length as the y variable
    geom_boxplot(aes(fill = latitude)) +
    theme_bw())

bird.model.length <- lm(length ~ latitude, data = latitudebirds)  # fit a linear model
anova(bird.model.length)  # ask for the anova of the model

# Condition index weight/length

latitudebirds$index <- (latitudebirds$weight/latitudebirds$length)  # create a new column with weight/length ratio numbers

(birdsboxplotindex <- ggplot(latitudebirds, aes(latitude, index)) +   # plot a boxplot, colour by latitude
    geom_boxplot(aes(fill = latitude)) +
    theme_bw())

bird.model.index <- lm(index ~ latitude, data = latitudebirds)  # fit a linear model
anova(bird.model.index)  # ask for the anova of the model

# Condition index length/weight

latitudebirds$index2 <- (latitudebirds$length/latitudebirds$weight)  # create a new column with weight/length ratio numbers

(birdsboxplotindex2 <- ggplot(latitudebirds, aes(latitude, index2)) +   # plot a boxplot, colour by latitude
    geom_boxplot(aes(fill = latitude)) +
    theme_bw())

bird.model.index2 <- lm(index2 ~ latitude, data = latitudebirds)  # fit a linear model
anova(bird.model.index2)  # ask for the anova of the model
