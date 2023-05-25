library(tidyverse)

doctor_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr23/main/data/DoctorAUS.csv")

doctor_df <- mutate(doctor_df, age = age * 100)

M_8 <- glm(doctorco ~ age, 
           data = doctor_df, 
           family = poisson(link = 'log'))

summary(M_8)

# what is predicted rate aka average number of visits
# if age = 20?
estimates <- coef(M_8)
# log of the rate
estimates[1] + estimates[2] * 20

# the rate
exp(estimates[1] + estimates[2] * 20)

# what is predicted rate aka average number of visits
# if age = 40?
exp(estimates[1] + estimates[2] * 40)

# what is predicted rate aka average number of visits
# if age = 80?
exp(estimates[1] + estimates[2] * 80)


# counterpart of odds ratio 
exp(estimates[2])


M_9 <- glm(doctorco ~ age + sex + income + insurance, 
           data = doctor_df, 
           family = poisson(link = 'log'))

deviance(M_9)
deviance(M_8)

deviance(M_8) - deviance(M_9)

anova(M_8, M_9, test = 'Chisq')



M_10 <- glm(doctorco ~ age + income + insurance, 
            data = doctor_df, 
            family = poisson(link = 'log'))

anova(M_10, M_9, test = 'Chisq')




M_11 <- glm(doctorco ~ age + sex + income, 
            data = doctor_df, 
            family = poisson(link = 'log'))

anova(M_11, M_9, test = 'Chisq')

# Negative binomial distributions -----------------------------------------


biochem_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr23/main/data/biochemist.csv")

M_12 <- glm(publications ~ prestige, 
            data = biochem_df,
            family = poisson(link = 'log')
)

M_13 <- glm(publications ~ prestige, 
            data = biochem_df,
            family = quasipoisson(link = 'log')
)

summary(M_12)
summary(M_13)

# negative binomial regression
M_14 <- glm.nb(publications ~ prestige, data = biochem_df)

summary(M_14)

# to get deviance of M_15, use log likelihood
logLik(M_14) * -2

biochem_df_new <- tibble(prestige = seq(0, 5))

add_predictions(biochem_df_new, M_14)

estimates <- coef(M_14)
estimates[1] + estimates[2] * seq(0, 5)

# like in Poisson
add_predictions(biochem_df_new, M_14, type = 'response')

exp(estimates[2]) # 

M_15 <- glm.nb(publications ~ prestige + married, 
               data = biochem_df)

deviance(M_15) # don't do this

-2 * (logLik(M_14) - logLik(M_15))

anova(M_14, M_15)


# Zero inflated count models ----------------------------------------------

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr23/main/data/smoking.csv")

table(smoking_df$cigs)


M_16 <- zeroinfl(cigs ~ educ, data = smoking_df)
summary(M_16)

smoking_df_new <- tibble(educ = seq(6, 18))

# predicted probability that latent variable = 1
# i.e. data is drawn from zero distribution
add_predictions(smoking_df_new, M_16, type = 'zero')

# predicted average assuming that data is drawn from count model
add_predictions(smoking_df_new, M_16, type = 'count')

add_predictions(smoking_df_new, M_16, type = 'response')
