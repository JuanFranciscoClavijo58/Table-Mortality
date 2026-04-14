##########################
#         EXERCISE       #
##########################

###############################################################################
# The following table shows mortality data and graduated mortality rates of   #
# male and female members of a pension scheme                                 #
###############################################################################

# The file containing the data is called “data”;
# don't forget to change the data path. 

Data <- read.csv("C:/Users/User/OneDrive - Universidad Nacional de Colombia/Escritorio/Maestria/Examen IFOA/Examination/Git/Table Mortality/Data.csv", sep=";")
# First, we examine the structure of our data 
Data
# and adjust our last column
library(dplyr)
library(tidyverse)
library(ggplot2)
tbl <- as.data.frame(Data)
tbl <- tbl %>%
  mutate(Graduated.rates = parse_number(Graduated.rates, locale = locale(decimal_mark = ",")))
class(tbl$Graduated.rates)

###############################################################################
# the table was graduated using the following parametric formula              #
#                    log(mu_x) = alpha + beta I_f + delta x                   #
# when I_f is an indicator variable taking the value 1 for female members and #
# 0 for male members                                                          #
###############################################################################

###############################################################################
# i) Calculate the value of alpha, beta and delta, by consideringe the        # 
# graduated rates at age 70 and 71                                            #
###############################################################################
# By way of hypothesis, we assume that for men it is:
#                log(mu_x) = alpha +  delta x
# As a hypothesis, we assume that for women it is:
#                log(mu_x) = alpha + beta + delta x

m70 <- round(log(tbl$Graduated.rates[tbl$Gender== "Male" & tbl$Age..x.years.== 70]),2)
m70
m71 <- round(log(tbl$Graduated.rates[tbl$Gender== "Male" & tbl$Age..x.years.== 71]),2)
m71
delta <- m71 - m70
delta
alpha <- m70 - delta * 70
alpha
#Since it is a slope, we can express it as y_2 - y_1 / x_2 - x_1
f70 <- round(log(tbl$Graduated.rates[tbl$Gender == "Female" & tbl$Age..x.years. == 71]/tbl$Graduated.rates[tbl$Gender == "Female" & tbl$Age..x.years. == 70]),2)
beta <- log(tbl$Graduated.rates[tbl$Gender == "Female" & tbl$Age..x.years. == 70]) - alpha - f70*70
beta
#############################
# ii)Comment on your results#
#############################

#Since delta = 0.15, mortality increases with age
#Since beta = 0.95, women have a lower mortality rate than men

####################################################################################
# iii) Perform a Chi-square test at a 5% significance level to assess the overall  #
#appropriateness of these graduated rates for both male and female members, stating#
# your null and alternative hypotheses.                                            #
####################################################################################

#We want to know whether graduated rates reflect the observed deaths 
# HO : The graduated rates are appropriate
# H1 : Graduated rates are not appropriate

#observations 11 parameters 3 
df= 11-3
crit_val <- qchisq(p=0.95,df)
crit_val
#Expected deaths

tbl <- tbl %>% 
        mutate(`Expected Deaths`= .[[3]]*.[[5]])
tbl <- tbl %>% 
        mutate(`Z_x`= (.[[4]]-.[[6]])/sqrt(.[[6]]))

tbl <- tbl %>% 
        mutate(`(Z_x)^2` = (Z_x)^2)
sum(tbl$`(Z_x)^2`)
tbl <- tbl %>% 
  mutate(`O-E` = (.[[4]]-.[[6]]))

tbl <- tbl %>% 
  mutate(`(O-E)^2` = (.[[9]])^2)

tbl <- tbl %>% 
  mutate(`((O-E)^2)/E` = (.[[10]])/.[[6]])
sum(tbl$`((O-E)^2)/E`)
#Since 2.011119 < 15.50731, there is no reason to reject H0

ggplot(data.frame(x = c(0, 30)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = df), size = 1) +
  # Rejection rate (5%)
  stat_function(fun = dchisq, args = list(df = df), 
                xlim = c(crit_val, 30), geom = "area", fill = "Red", alpha = 0.5) +
  
  geom_vline(xintercept = crit_val, linetype = "solid", size = 1) +
  annotate("text", x = crit_val, y = -0.005, label = "15.51") +
  
  geom_vline(xintercept = sum(tbl$`((O-E)^2)/E`), color = "darkblue", linetype = "dashed", size = 1) +
  annotate("text", x = sum(tbl$`((O-E)^2)/E`), y = -0.005, label = paste("P=", round(sum(tbl$`((O-E)^2)/E`),3)), color = "darkblue", size = 4) +
  
  labs(title = "Chi-square test (df=8)", x = "Value Chi^2", y = "Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )






