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
