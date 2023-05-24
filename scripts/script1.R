library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr23/main/data/weight.csv")

M_1 <- lm(weight ~ height, data = weight_df)
coef(M_1) # view estimated/inferred values of linear coefficients
sigma(M_1) # view estimated value of stdev of outcome variable

M_2 <- lm(weight ~ height + age, data = weight_df)
coef(M_2)

# binary logistic regression ----------------------------------------------

theta <- c(0.1, 0.25, 0.5, 0.75, 0.9)
odds <- theta / (1 - theta)

log(odds) # log odds or logits


affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr23/main/data/affairs.csv")

affairs_df <- mutate(affairs_df, had_affair = affairs > 0)

# probability of having an affair and how it varies by number of years married

M_3 <- glm(had_affair ~ yearsmarried, 
           family = binomial(link = 'logit'),
           data = affairs_df)

# DON'T DO THIS
# glm(had_affair ~ yearsmarried, data = affairs_df)
# that would be identical to 
# lm(had_affair ~ yearsmarried, data = affairs_df)

coef(M_3)
summary(M_3)

# What is the predicted log odds if e.g. yearsmarried = 10?
estimates <- coef(M_3)

estimates[1] + estimates[2] * 10

# this corresponds to the following probability
plogis(estimates[1] + estimates[2] * 10)

# What is the predicted log odds if e.g. yearsmarried = 20?
estimates[1] + estimates[2] * 20

# this corresponds to the following probability
plogis(estimates[1] + estimates[2] * 20)

yearsmarried_range <- seq(1, 50)

# what is the predicted log odds from that yearsmarried_range?
estimates[1] + estimates[2] * yearsmarried_range

# this corresponds to the probabilities ...
plogis(estimates[1] + estimates[2] * yearsmarried_range)

# Interlude ---------------------------------------------------------------

summary(M_3)$coefficients

# formatting tools include 
# * kable (knitr)
# * pander 
# * xtable 
kable(summary(M_3)$coefficients)

# Predictions with predict etc --------------------------------------------


affairs_df_new <- tibble(yearsmarried = c(5, 10, 15, 20, 25))

# predicted log odds
predict(M_3, newdata = affairs_df_new)

# predicted probabilities
predict(M_3, newdata = affairs_df_new, type = 'response')

library(modelr) # load the add_predictions function

add_predictions(affairs_df_new, M_3, var = 'logit') # predicted log odds

# predicted probabilities
predictions <- add_predictions(affairs_df_new, M_3, var = 'prob', type = 'response') 

# plot it with ggplot
ggplot(predictions, aes(yearsmarried, prob)) + geom_line() + geom_point()


# Another logistic regression

M_4 <- glm(had_affair ~ yearsmarried + age + gender, 
           family = binomial(link = 'logit'),
           data = affairs_df)

summary(M_4)$coefficients

round(summary(M_4)$coefficients, 3)


affairs_df_new_2 <- tibble(yearsmarried = c(5, 10, 15, 20, 25),
                           age = 25,
                           gender = 'female')

add_predictions(affairs_df_new_2, M_4, type = 'response')


# Odds ratios -------------------------------------------------------------

exp(coef(M_3)[2]) # odds ratio for the yearsmarried predictor in M_3
exp(coef(M_4)[2]) # odds ratio for the yearsmarried predictor in M_4



# Confidence intervals ----------------------------------------------------

confint.default(M_4) # 95%
confint.default(M_4, level = 0.99) # 99%
confint.default(M_4, level = 0.99, parm = 'age') # 99% for age only
confint.default(M_4, level = 0.95, parm = 'yearsmarried') # 95% for yearsmarried only

# 95% confidence interval on odds ratio
exp(confint.default(M_4, level = 0.95, parm = 'yearsmarried') )

# Deviance and log likelihood ---------------------------------------------

summary(M_3)
deviance(M_3)
logLik(M_3) * -2

summary(M_4)
deviance(M_4)
logLik(M_4) * -2

# Difference in deviances of the two models
deviance(M_3) - deviance(M_4)

anova(M_3, M_4, test = 'Chisq')

M_5 <- glm(had_affair ~ yearsmarried + age, 
           family = binomial(link = 'logit'),
           data = affairs_df)

deviance(M_5)
deviance(M_3)

anova(M_3, M_5, test = 'Chisq')
