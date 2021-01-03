# Clear memory and change the scientific notation
rm(list=ls())
options(scipen=999)

# Load packages
library(tidyverse)
library(moments)
library(ggthemes)
library(scales)
library(estimatr)

# Call the data from github
home <- "https://raw.githubusercontent.com/semihozankaya/DA-2_Final_Assignment/master/Data/Clean/dataset.csv"
df <- read_csv(home, guess_max = 2075)


##### Data and Preparation
# Quick check of all the variables
df  %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(fill = "navy blue")

glimpse(df)
summary(df)

# Some of the variables have large tails but no extreme values or mismeasurement.

# Some make up
names(df)[3] <- "state_name" 
names(df)[9] <- "CPI" 
names(df)[6] <- "Income_per_capita" 

# Adding some simple transformations
df <- df %>% mutate(vcrime_per_capita = violent_crime * 10000 / population, 
                    executions_per_vcrime = Executions * 10000 / violent_crime)

df %>% group_by(year) %>% summarize(sum(Executions, na.rm = TRUE)) %>% arrange(desc(`sum(Executions, na.rm = TRUE)`))
# The highest number of executions happened at late 90s. So, for my cross sectional analysis, 
# I will chose 2000 with lagged effects from 1999. 

df00 <- df %>% filter(year == 2000)

# Also add lags and differences for some variables
df99 <- df %>% filter(year == 1999) %>% select(2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
df_merged <- merge(df00, df99, by= "state_abbr", all.x = TRUE)
df_merged <- df_merged %>% select(-year)
df_merged <- df_merged %>% mutate(violent_crime_diff = violent_crime.x - violent_crime.y, 
                                  Unemployment_diff = Unemployment.x - Unemployment.y, 
                                  Income_diff = Income_per_capita.x - Income_per_capita.y)

names(df_merged)[3:22] <- c("population00", "violent_crime00", "Income_per_capita00",
                            "Unemployment00", "Poverty00", "CPI00", "Executions00", 
                            "Urbanization00", "vcrime_per_capita00",
                            "executions_per_vcrime00", "population99", "violent_crime99", 
                            "Income_per_capita99", "Unemployment99", "Poverty99", "CPI99", 
                            "Executions99", "Urbanization99", "vcrime_per_capita99",
                            "executions_per_vcrime99")


# Checking for missing variables
sum(is.na(df_merged))
sapply(df_merged, function(x) sum(is.na(x)))

# It seems that execution data has a missing value whereas urbanization for 1999 is completely missing.
# I must have forgot that we have urbanization data only for 2000.

df_merged %>% filter(is.na(Executions00) == TRUE)
# The execution data is missing for DC, since it is not technically a state. 

df_merged <- df_merged %>% filter(state_abbr != "DC")
df_merged <- df_merged %>% select(-Urbanization99)

sum(is.na(df_merged))

# Now check some summary statistics and prepare a table for it
var_to_summarize <- c("Income_per_capita00", "Unemployment00", "Poverty00", "Urbanization00", 
                      "violent_crime00", "Executions00", "vcrime_per_capita00",
                      "executions_per_vcrime00")
stats_to_summarize <- c("Mean", "Median", "Std", "IQR", "Min", "Max", "numObs" )

df_summary <- select(df_merged, all_of(var_to_summarize))

summary_table <- tibble(`Income Per Capita` = rep(0, 7), `Unemployment` = rep(0, 7), Poverty = rep(0,7),
                        `Population Living in Urban Areas` = rep(0, 7), `Violent Crime` = rep(0, 7), 
                        Executions = rep(0, 7), `Violent Crime Per 10,000 People` = rep(0, 7),
                        `Executions Per 10,000 V. Crimes` = rep(0, 7))


for(i in 1:length(names(summary_table))){
  summary_table[,i] <- df_summary %>%
    summarise(mean  = mean(df_summary[[i]], na.rm = TRUE),
            median   = median(df_summary[[i]], na.rm = TRUE),
            std      = sd(df_summary[[i]], na.rm = TRUE),
            iq_range = IQR(df_summary[[i]], na.rm = TRUE), 
            min      = min(df_summary[[i]], na.rm = TRUE),
            max      = max(df_summary[[i]], na.rm = TRUE),
            numObs   = sum( !is.na( df_summary[[i]] ) ) ) %>% t()
}
summary_table_var <- tibble(Statistics = stats_to_summarize)

for(i in 1:8){
  summary_table[[i]] <- format(round(summary_table[[i]], 2), nsmall=2, big.mark=",")
  }

summary_table <- cbind(summary_table_var, summary_table)

# Checking histograms for 2000

df_merged %>% select(4, 5, 6, 7, 9, 10, 11, 12) %>%
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram( fill = "navy blue", bins = 25) +
  facet_wrap(~key, scales = "free") +
  scale_x_continuous(labels = comma)

# It seems that Violent Crime shows a skewed distribution. Violent Crime per capita might be a better representation
# of our data seens it is more intuitive and also its distribution seems better.

# We can also check for a log transformation of Poverty data. Maybe we can benefit from such a simple
# transformation.

###### Variables
# Checking basic scatter-plots 

#### Violent Crime and Unemployment
# 1) Violent Crime Per 10,000 People - Unemployment in 2000
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Unemployment00)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Unemployment `00",y = "Violent Crime Per 10,000 People `00") 

# 2) Violent Crime Per 10,000 People - Unemployment in 1999
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Unemployment99)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Unemployment `99",y = "Violent Crime Per 10,000 People `00") 

# 3) Violent Crime Per 10,000 People - Unemployment YoY Change
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Unemployment_diff)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Unemployment Difference YoY",y = "Violent Crime Per 10,000 People `00") 

## The pattern of association between unemployment and violent crime per capita seems to offer some insight. 
# It seems that higher unemployment coincides with higher violent crime rate. However, the YoY increase in unemployment
# doesn't seem to have an effect. Perhaps unemployment have a persistent effect on violent crime rate throughout time,
# or perhaps it doesn't have an effect at all? Or perhaps the change should be higher than we can observe.

# P.S, I didn't see a reason to make log transformations since the histograms shows a good fit.
# There are also no big variations within the variables. Heteroskedasticity might be an issue though, maybe we 
# can check it later.

# Maybe we can control for a quadratic Unemployment though.

#### Violent Crime and Per Capita Income

# 1) Violent Crime Per 10,000 People - Income per capita
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Income_per_capita00)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Income `00",y = "Violent Crime Per 10,000 People `00") 

# 2) Violent Crime Per 10,000 People - Income per capita in 1999
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Income_per_capita99)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Income `99",y = "Violent Crime Per 10,000 People `00") 

# 3) Violent Crime Per 10,000 People - income YoY Change
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Income_diff)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Income Difference YoY",y = "Violent Crime Per 10,000 People `00") 

## The results are not very promising. High crime rates seems to occur mostly in relatively low income states,
# but it is hard to make inferences on expected values. 

# Maybe we can add a quadratic variable to control for possible nonlinear patterns

# However, there doesn't seem to be a need for log transformations. Both values are per capita values and
# seems reasonably distributed.

#### Violent Crime and Executions

# 1) Violent Crime - Executions 
ggplot( df_merged , aes(y = violent_crime00, x = Executions00)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "Executions `00",y = "Violent Crime `00") 

# 2) Violent Crime Per 10,000 People - Executions
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Executions00)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "Executions `00",y = "Violent Crime Per 10,000 People `00") 

# 3) Violent Crime Per 10,000 People - Executions in 1999
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Executions99)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "Executions `99",y = "Violent Crime Per 10,000 People `00") 

# 4) Violent Crime Per 10,000 People - Executions per 10,000 violent crime
ggplot( df_merged , aes(y = vcrime_per_capita00, x = executions_per_vcrime00 )) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "Executions per 10,000 V. Crime `00",y = "Violent Crime Per 10,000 People `00") 

# 5) Violent Crime Per 10,000 People - Executions per 10,000 violent crime in 99
ggplot( df_merged , aes(y = vcrime_per_capita00, x = executions_per_vcrime99 )) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(x = "Executions per 10,000 V. Crime `99",y = "Violent Crime Per 10,000 People `00") 

## The variation in Executions are very low. Most of the observations are 0. Even though it is allowed in
# our OLS framework, it is not very ideal for our purposes. Perhaps we should consider using a dummy instead.

# There is also an endogeneity problem here. The dependent and independent variables are effecting each other.
# This can be seen from the basic scaterplot as well. Most of the countries that have relatively low violent
# crime rates, don't have capital punishment. The exception seems to be Virgina and perhaps concidentally, 
# it has the highest execution rate per violent crimes in 99 and second highest in 2000. 
# Perhaps after a certain treshold (killing many convicts) the capital punishment becomes a deterrent.

#### Violent Crime and Poverty

# 1) Violent Crime Per 10,000 People - People living in Poverty
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Poverty00)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Poverty `00",y = "Violent Crime Per 10,000 People `00") 

# 2) Violent Crime Per 10,000 People - Poverty in 1999
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Poverty99)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Poverty `99",y = "Violent Crime Per 10,000 People `00") 

# 1) Violent Crime Per 10,000 People - log of People living in Poverty
ggplot( df_merged , aes(y = vcrime_per_capita00, x = log(Poverty00))) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = " log of Poverty `00",y = "Violent Crime Per 10,000 People `00") 


# Not surprisingly, impoverished communities seems to suffer from violent crimes more. Perhaps there is a 
# subtle endogeneity here as well.

# Yearly patterns seems similar. Poverty rates doesn't seem to change dramatically.

# We can definitely benefit from a log transformation here. The variation of x has increased and the new
# pattern has a much more linear characteristics.

#### Violent Crime and Urbanization

# 1) Violent Crime Per 10,000 People - People living in Urban Areas
ggplot( df_merged , aes(y = vcrime_per_capita00, x = Urbanization00)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Urbanization `00",y = "Violent Crime Per 10,000 People `00") 


# Violent crimes tend to happen more in urban communities. This is expected.

###### Models  
#   Testing some models:
#     reg1: vcrime_per_capita00 = alpha + beta * Unemployment99
#     reg2: vcrime_per_capita00 = alpha + beta_1 * Unemployment99 + beta_2 * Unemployment99_sq
#     reg3: vcrime_per_capita00 = alpha + beta_1 * Unemployment99 + beta_2 * Unemployment99_sq + 
# beta_3 * Income_per_capita00
#     reg3: ln_deaths_pc = alpha + beta_1 * ln_cases_pc + beta_2 * ln_cases_pc^2 + beta_3 * ln_cases_pc^3
#     reg4: ln_deaths_pc = alpha + beta_1 * ln_cases_pc*(ln_cases_pc < 50) + beta_2 * ln_cases_pc*(ln_cases_pc >= 50)
#     reg5: ln_deaths_pc = alpha + beta * ln_cases_pc, weights: population

###
# First model:
reg1 <- lm_robust( vcrime_per_capita00 ~ Unemployment00 , data = df_merged , se_type = "HC2" )
summary( reg1 )

# It seems that the above model is significant at almost 1% significance level. 
# It shows a slope coefficient of 6.24.

###
# Second model:
# We add the square of unemployment to our second model.
df_merged <- df_merged %>% mutate(Unemployment00_sq = Unemployment00^2) 

reg2 <- lm_robust( vcrime_per_capita00 ~ Unemployment00 + Unemployment00_sq, data = df_merged, se_type = "HC2")
summary( reg2 )

# The model's statistical significance vastly improved by adding the squared unemployment variable.
# Individual significance of the variables have suffered though, probably due to multicollinearity. 
# Since the standad errors of the coefficients have increased. 
# The slope coefficients are approximately 47 and -5. According to the coefficients, 
# it seems that unemployments effect on violent crime follows a concave pattern but we cannot be 
# certain about the effects. The coefficient of squared unemployment itself is not very important
# for us and even though its inclusion have increased the unemployment coefficient's standard error
# It also increased the overall signifcance of the model. The adjsuted R-squared is significantly
# higher. Perhaps we should keep it.

###
# Third model:
# We add Income per capita to our model.

reg3 <- lm_robust( vcrime_per_capita00 ~ Unemployment00 + Unemployment00_sq + Income_per_capita00, 
                   data = df_merged, se_type = "HC2")
summary( reg3 )

# It seems that income is indiviudally not statistically significant.
# and adding it doesn't contribute very much to the model's overall significance. 
# The adjusted R-squared also barely changes.

###
# Fourth model:
# We now add the log poverty to our model
df_merged <- df_merged %>% mutate(Poverty00_ln = log(Poverty00)) 


reg4 <- lm_robust( vcrime_per_capita00 ~ Unemployment00  + 
                     Poverty00_ln,
                   data = df_merged, se_type = "HC2")
summary( reg4 )

# It seems that adding the poverty rates significantly increased our model's signifcance. 

###
# Fifth model:

reg5 <- lm_robust( vcrime_per_capita00 ~ Unemployment00 + Poverty00_ln + Urbanization00,
                   data = df_merged, se_type = "HC2")
summary( reg5 )

# Urbanization itself is not statistically significant even in 10% level. It also causes an increase
# in standard errors of other estimators. However, adding it contributes to the goodness of fit of the 
# model. 

###
# Sixth model:

reg6 <- lm_robust( vcrime_per_capita00 ~ Unemployment00 + Poverty00_ln + Urbanization00 +
                     executions_per_vcrime00,
                   data = df_merged, se_type = "HC2")
summary( reg6 )

# Executions itself is not a signifcant variable. However, the harshness of laws, i.e. execution rate per
# violent crime seems to increase the fit of our model. 


###
# Seventh model:
# We now add Executions as a qualitative variable to control for the use of capital punishment. 

df_merged <- df_merged %>% mutate(Execution_dummy00 = ifelse(Executions00 >0, 1, 0))

reg7 <- lm_robust( vcrime_per_capita00 ~ Unemployment00 + Poverty00_ln + Urbanization00 + 
                     executions_per_vcrime00 + Execution_dummy00,
                   data = df_merged, se_type = "HC2")
summary( reg7 )

# Surprisingly, adding the execution dummy have caused an increase in Poverty estimators standard error.
# It is a sign of multicollinearity. It seems that states that use capital punishment also suffer
# from high poverty rates. (And probably high violent crime rates as well and once again, the 
# endogeneity problem shows itself.)

###
# Eight model:
# We now add an interactive dummy to control for the multicollineairty between
# execution dummy and Poverty rates.
df_merged <- df_merged %>% mutate(Execution_dummy00 = ifelse(Executions00 >0, 1, 0))

reg8 <- lm_robust( vcrime_per_capita00 ~ Unemployment00 + Poverty00_ln + Urbanization00 + 
                     executions_per_vcrime00 + Execution_dummy00 + Poverty00_ln*Execution_dummy00,
                   data = df_merged, se_type = "HC2")
summary( reg8 )


