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
library(here)
library(tidyverse)
library(GGally)
library(MASS)
library(car)
#__________________############################################################
# READ IN DATA ####
diam <- read.csv("clean_diamond_data.csv")
str(diam)

#__________________#############################################################
# DESCRIPTIVE STATS ####
summary(diam)

## Divide Into Groups ====
#diam_big <- diam %>% filter(carat >= 15)
#diam_med <- diam %>% filter(5 <= carat & carat < 15)
#diam_sml <- diam %>% filter(carat < 5)
#hist(diam_sml$carat)
#hist(diam_med$carat)
#hist(diam_big$carat)

## Plots ====

# All pair plots ----
#ggpairs(diam)

# Dependent variable ----
#diam %>% ggplot(aes(price)) + geom_histogram(bins=30)
#hist(diam$price)
#range(diam$price)
# data is highly right-skewed, but no zeros

# 1. Transformation ----
# attempt log transformation
#diam$logPrice <- log10(diam$price) # log base 10 transform
#hist(diam$logPrice) # # looks more normal, still some R skew
#hist(sqrt(diam$price + 0.5)) # no help


# Independent Variables ----

# 1. Carat (continuous) ----
#table(diam$carat)
#hist(diam$carat)
#diam %>% ggplot(aes(carat)) + geom_histogram(bins = 30)
#diam$logCarat <- log10(diam$carat)
#diam %>% ggplot(aes(logCarat)) + geom_histogram(bins = 30)

# 2. Clarity (discrete, 8 levels) ----
# SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF < FL (best)
diam$clarity <- factor(diam$clarity,
                       levels = c("SI2","SI1","VS2","VS1",
                                  "VVS2","VVS1","IF","FL"))
table(diam$clarity)
#diam %>% ggplot(aes(clarity)) + geom_bar()

# 3. Color (discrete, 7 levels) ----
# D best to J worst
diam$color <- factor(diam$color, levels=c("J","I","H","G","F","E","D"))
table(diam$color)
#diam %>% ggplot(aes(color)) + geom_bar()


# 4. Cut (discrete, 4 levels) ----
# quality of the cut (Good < Very Good < Ideal < Astor Ideal)
# reorder levels
diam$cut <- factor(diam$cut, 
                   levels = c("Good", "Very Good", "Ideal", "Astor Ideal"))
#table(diam$cut)
#diam %>% ggplot(aes(cut)) + geom_bar()


# Independent vs. Dependent ----
# 1. continuous ----
#diam %>% ggplot(aes(carat, price)) + geom_point()
#diam %>% ggplot(aes(logCarat, logPrice)) + geom_point()

# 2. discrete ----
#diam %>% ggplot(aes(clarity, logPrice)) +  geom_boxplot()
#diam %>% ggplot(aes(color, logPrice)) +  geom_boxplot()
#diam %>% ggplot(aes(cut, logPrice)) +  geom_boxplot()

#####____________###############################################
#HYPOTHESIS TESTING

#1.  testing for a linear relationship between price and carat.

plot(diam$carat, diam$price)

price_v_carat <- lm(price ~ carat, data = diam)

summary(price_v_carat)

#apply transformation of y to y' = log(y)
diam$transformed_price <- log10(diam$"price")

#apply transformation of x to x' = log(x)
diam$transformed_carat <- log10(diam$"carat")


plot(diam$transformed_carat, diam$transformed_price)

#fitting another linear model to the new data
tprice_v_tcarat <- lm(transformed_price ~ transformed_carat, data = diam)

summary(tprice_v_tcarat)

#externally studentized residuals
ext_s_resids <- studres(tprice_v_tcarat)

#qq-plot
qq_plot1 <- qqnorm(ext_s_resids)

#fitting the complete linear model


full_mod <- lm(transformed_price ~ transformed_carat + clarity + color + cut, data = diam)
summary(full_mod)

#find externally studentized residuals
ext_s_resids2 <- studres(full_mod)

#qq-plot
qq_plot2 <- qqnorm(ext_s_resids2)
qq_line2 <- qqline(ext_s_resids2)

#plot looks quite linear suggesting normal distribution of residuals, though there is some indication of fat tails

##another check for normality of residuals - Histogram

x_grid <- seq(min(ext_s_resids2)-.5, max(ext_s_resids2)+.5, by = .01)
diam <- nrow(my_data) - 7 - 1 # 7 predictors and one intercept
hist(ext_s_resids2, probability = T, ylim = c(0,.5))
#t_densities <- dt(x_grid, diam)
#points(x_grid, t_densities, type = "l", col = "red")


##checking Homoskedastitcity

plot(fitted.values(full_mod), ext_s_resids2)

## This plot shoes that the residuals are unpredictable in relation to the fitted values, so the variance is random.


#### added variable plots

#av_plots <- avPlots(full_mod)


vif <- vif(full_mod)

#Hypothesis testing to test H0: B2 = B3 = B4 = B5 = 0

anova(tprice_v_tcarat, lm(transformed_price ~ transformed_carat + cut + color+ clarity, data= diam))





#__________________#############################################################
# MODELING ####

# linear model 1
mod1 <- lm(logPrice ~ carat + clarity + color + cut, data = diam)
summary(mod1)
anova(mod1)
plot(mod1)
contrasts(diam$clarity)
contrasts(diam$color)
contrasts(diam$cut)

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



# linear model 2
mod2 <- lm(price ~ carat + clarity + color + cut, data = diam)
summary(mod2)
anova(mod2)
