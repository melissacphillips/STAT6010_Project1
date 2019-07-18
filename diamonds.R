# Activity: STAT 6021 Project 1 - Diamonds
# Team Members: Brigitte Hogan, Sherry, ...
# Source of Data: BlueNile.com
# Deadline: Friday, July 19th @0900


# Each project should feature:
# - Clear central analytic goals and/or questions to answer 
#   (the more interesting/ challenging/ practical, the better)
# - a "substantial" computational component for the analysis
# - analytic methods developed in this course.

# The deliverables are
# 1. Report (.doc, .docx, or .pdf)
#    - less than 10 pages, and include:
#    - an execsum describing the high-level goals/questions of the project
#    - the nature/characteristics of the data used in the analysis
#    - the results of the analysis, including any recommendations
#    - make use of graphics wherever appropriate.
#    - use correct grammar, clear explanations, and professional presentation
#    - audience: both clients (no experience analyzing data) & data scientists/
#      statisticians (given the report for a second opinion)
#
# 2. Reproducible R-code (.r file)
#    - commented/annotated R code
#
# 3. Presentation document (.pdf, .ppt, etc.)
#    - each team will give an 8-minute presentation
#    - not everyone needs to speak, but all team members should make equal
#      contributions to the project as a whole.
#    - audience: understandable by anyone familiar with the course material, but
#      who has not read your project report yet


#__________________###########################################################
# IMPORT LIBRARIES ####
library(here) # for setting working directory
library(tidyverse) # for ggplot2 and piping
library(GGally) # for interaction plots
library(treemap) # for plotting

# to get p-value of model without "peeking" at t-tests
getPmodel <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#__________________############################################################
# READ IN DATA ####
diam <- read.csv("clean_diamond_data.csv")
head(diam)

#__________________#############################################################
# CLEAN DATA ####
str(diam)

# Re-leveling clarity
# actual order: (worst) SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF < FL (best)
diam$clarity <- factor(diam$clarity,
                       levels = c("SI2","SI1","VS2","VS1",
                                  "VVS2","VVS1","IF","FL"))
contrasts(diam$clarity) # how clarity will be coded in lm

# Re-leveling color
# actual order: J (worst) to D (best)
diam$color <- factor(diam$color, levels=c("J","I","H","G","F","E","D"))
contrasts(diam$color) # how color will be coded in lm

# re-leveling cut
# actual order: (worst) Good < Very Good < Ideal < Astor Ideal (good)
#levels = c("Good", "Very Good", "Ideal", "Astor Ideal"))
# our focus is on "Astor Ideal vs "Ideal"
# we move "Ideal" into 1st position to use as a reference point for comparison
diam$cut <- factor(diam$cut, 
                   levels = c("Ideal", "Good", "Very Good", "Astor Ideal"))
contrasts(diam$cut) # how cut will be coded in lm

#__________________#############################################################
# DESCRIPTIVE STATS ####
summary(diam)

# y (dependent)
range(diam$price) # large range from 229 to 2,000,000+
mean(diam$price) # mean of $5540
quantile(diam$price) %>% plot()

# carat - continuous
range(diam$carat) # large range from 0.23 to 20.45
mean(diam$carat) # 0.76
quantile((diam$carat)) %>% plot()

# clarity - discrete
table(diam$clarity) # fewer high-clarity diamonds

# cut - discrete
table(diam$cut) # most diamonds are ideal, followed by Very Good & Good

# color - discrete
table(diam$color) # few poor color diamonds, but otherwise even distribution

## descriptive plots ####

diam$freq <- 1 # creates count variable for plotting
# RColorBrewer::display.brewer.all() # color palettes
mapColor <- hcl.colors(8, palette="Blues")

# Based on number of values
treemap(diam,
        index=c("cut","color","clarity"),
        vSize="freq", 
        type="categorical", vColor="color",
        # graphic options
        palette = mapColor,
        fontsize.labels = c(26, 20, 8), bg.labels=0, 
        fontface.labels = c("bold", "bold", "plain"),
        fontcolor.labels = c("red3", "gray40", "gray10"),
        border.col = c("black", "black", "white"),
        border.lwds = c(7,3,1))

# Based on Price (trickier)
treemap(diam, 
        index=c("cut","color"),      # how to divide up data
        vSize="freq",                           # size of rectangles
        type="manual",                          # manual color palette
        # graphic options
        vColor = "price", fun.aggregate="sum", # color is average price
        palette = "Greens",
        fontsize.labels = c(26, 20), bg.labels=0, 
        fontface.labels = c("bold", "bold"),
        fontcolor.labels = c("red3", "gray40"),
        border.col = c("black", "black"),
        border.lwds = c(7,3))

diam %>% group_by(cut) %>% summarize(n=mean(price))
# Astor Ideal has lowest (!) average price

#__________________#############################################################
# CHECKING ASSUMPTIONS ####
diam %>% ggplot(aes(carat, price)) + geom_point() # scatterplot
# relationship between carat & price does not look linear

# we know from descriptive stats that they both have a wide range and there is very little data in the upper quantile
# to adjust the scale, we try log-transformation of y value
diam$logPrice <- log10(diam$price) # log base 10 transform
diam %>% ggplot(aes(carat, logPrice)) + geom_point() # scatterplot
# scatterplot shows data more evenly distributed in y direction but not x
# try similar transformation for carat
diam$logCarat <- log10(diam$carat)

diam %>% ggplot(aes(logCarat, logPrice)) + geom_point() # scatterplot
# relationship now appears linear and points are more evenly spread

# 1. Price (continuous)
hist(diam$price)
hist(diam$logPrice) 
# the log transformation has improved skew in data, but still some R skew
hist(sqrt(diam$price + 0.5)) # no help

# 1. Carat (continuous) ----
hist(diam$carat)
hist(diam$logCarat)
diam %>% ggplot(aes(carat)) + geom_histogram(bins = 30)
diam %>% ggplot(aes(logCarat)) + geom_histogram(bins = 30)

# 2. Clarity (discrete, 8 levels) ----
diam %>% ggplot(aes(clarity)) + geom_bar()

# 3. Color (discrete, 7 levels) ----
diam %>% ggplot(aes(color)) + geom_bar()

# 4. Cut (discrete, 4 levels) ----
diam %>% ggplot(aes(cut)) + geom_bar()


## Interactions
# All pair plots ----
ggpairs(diam)
diam %>% ggplot(aes(logCarat, logPrice)) + geom_point()
diam %>% ggplot(aes(clarity, logPrice)) +  geom_boxplot()
diam %>% ggplot(aes(color, logPrice)) +  geom_boxplot()
diam %>% ggplot(aes(cut, logPrice)) +  geom_boxplot()



#__________________#############################################################
# MODELING ####

# untransformed data, for reference
mod0 <- lm(price ~ ., data = diam)
extern_s_resids0 <- studres(mod0)
qqnorm(extern_s_resids0)
qqline(extern_s_resids0)

# Going FWD with model 1

# linear model 1
mod1 <- lm(logPrice ~ logCarat + clarity + color + cut, data = diam)

extern_s_resids1 <- studres(mod1)
qqnorm(extern_s_resids1)
qqline(extern_s_resids1) # still an issue at tails, but much improved

plot(fitted.values(mod1), extern_s_resids1) # looks random

vif(mod1)

summary(mod1)$r.squared # 0.9813
summary(mod1)$fstatistic # 651,746 huge!
getPmodel(mod1) # pretty much 0

aov(mod1)

#__________________#############################################################
# Question: Is Astor Ideal important? ----

summary(mod1)


# make model without astor ideal and with astor ideal and compare??



#__________________#############################################################
# x vs y Plots ----


# some different options
diam %>%
  ggplot(aes(x=logCarat, y=logPrice, alpha=0.1, color=color, size=cut)) +
  geom_point()

diam %>%
  ggplot(aes(x=logCarat, y=logPrice, alpha=rev(color), color=clarity)) +
  scale_color_brewer(type = 'div') + 
  geom_point()
  
diam %>%
  ggplot(aes(x=carat, y=logPrice, alpha=0.1, shape=cut, 
             color=clarity, size=color)) +
  scale_color_brewer(type = 'div') + 
  geom_point()

  
