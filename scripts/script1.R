library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr23/main/data/weight.csv")

M_1 <- lm(weight ~ height, data = weight_df)
coef(M_1) # view estimated/inferred values of linear coefficients
sigma(M_1) # view estimated value of stdev of outcome variable

M_2 <- lm(weight ~ height + age, data = weight_df)
coef(M_2)
