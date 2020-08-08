covid_data <- read.csv("C:/Users/namit/Desktop/COVID_R/datasets_494724_994000_COVID19_line_list_data.csv")

install.packages("Hmisc")
library(Hmisc)

describe(covid_data)

covid_data$death <- as.integer(covid_data$death != 0)

#AGE
# claim: people who die are older
dead = subset(covid_data, death == 1)
alive = subset(covid_data, death == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect
men = subset(covid_data, gender == "male")
women = subset(covid_data, gender == "female")
mean(men$death, na.rm = TRUE) #8.5%!
mean(women$death, na.rm = TRUE) #3.7%
# is this statistically significant?
t.test(men$death, women$death, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant




