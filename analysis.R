library(here)
library(tidyverse)
library(dlookr)
library(flextable)


#importing the data 

data <- read.csv(here("data", "data.csv"))

#structure of the dataset

glimpse(data)

#> Our dataset has 7 variables and 725 observations.

#DATA PREPROCESSING 


#Changing all the categorical variables into factor formats 

#Note: All the categorical variables were coded as character
data <- data %>% mutate_if(is.character, as.factor)

#CHECKING FOR MISSING VALUES 
sum(is.na(data))
#There are no missing vaues 

#CHECKING FOR DUPLICATES 

sum(duplicated(data))


#removing the first variale(insignificant)

data$X = NULL


#setting the theme 
theme_set(theme_bw()+
            theme(plot.title = element_text(face = "bold", hjust = 0.5)))



#checking for outliers in the numeric vatriabls 

#1. AGe 
data %>% ggplot(aes(Age))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Boxplot of Age ")+
  coord_flip()
#2.Height
data %>% ggplot(aes(Height))+
  geom_boxplot(fill = "lightblue")+
  labs(title = "Boxplot of Height in inches ")+
  coord_flip()

#3. Lung capacity
data %>% ggplot(aes(LungCap))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Boxplot of Lung capacity ",
       x = "Lung capacity in Liters")+
  coord_flip()

#ALl the three numeric variables in our datasets dont have outliers 



#EXPLORATORY DATA ANALYSIS 

#UNIVARIATE ANALYSIS 

#descriptive statistics 
diagnose_numeric(data) %>% flextable()


#1. Cesarian Variable 
data %>% ggplot(aes(Caesarean, fill = Caesarean))+
  geom_bar(show.legend = FALSE)+
  labs(title = "Born Via Caesarian Section")


#2. Gender 
data %>% ggplot(aes(Gender, fill = Gender))+
  geom_bar(show.legend = FALSE)+
  labs(title = "Distribution By Gender")

#3. Smoking status 
data %>% ggplot(aes(Smoke, fill = Smoke))+
  geom_bar(show.legend = FALSE)+
  labs(title = "Smoking Status")

#4. AGe 
data %>% ggplot(aes(Age))+
  geom_histogram(fill = "skyblue", color = "black", bins = 15)+
  labs(title = "Age Distribution")

#5. Height 
data %>% ggplot(aes(Height))+
  geom_histogram(fill = "lightblue", color = "black", bins = 20)+
  labs(title = "Height Distribution",
       x = "Height (inches)")
#Lung Capacity 
data %>% ggplot(aes(LungCap))+
  geom_histogram(fill = "skyblue", color = "black", bins = 15)+
  labs(title = "Lung Capacity(Litres)")


#Bivariate Analysis

#1. How does Lung Capacity vary with Age?

#calculating the correlation

cor = round(cor(data$LungCap, data$Age),2)

##Plotting a scatter plot
data %>% ggplot(aes(x= Age, y = LungCap))+
  geom_jitter()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_text(x= 5, y = 11, label = paste0("cor =", cor), color = "red")+
  labs(title = "Lung Capacity vs Age",
       x= "Age (yrs)",
       y = "Lung Capacity(Litres)")

?geom_text()
#>Lung capacity and age show a strong positive correlation of a correlation 
#>coeffiecient of (0.82)
#>


#2. How does Lung Capacity Vary With Height?

cor1 = round(cor(data$LungCap, data$Height),2)
data %>% ggplot(aes(x= Height, y = LungCap))+
  geom_jitter()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_text(x= 55, y = 11, label = paste0("cor =", cor1), color = "red")+
  labs(title = "Lung Capacity vs Height",
       x= "Height(inches)",
       y = "Lung Capacity(Litres)")
#> Lung Capacity and Height show a strong positive correlation(0.91), thus
#> Lung capacity increases with increase in Age
#> 

#3. Do male have a Higher Lung Capacity than females

#plotting barplots with error bars 

#preparing the data to get the standard errors

new_data <- data %>% group_by(Gender) %>% 
  summarise(mean_gender = mean(LungCap),
            sd_lung = sd(LungCap), # calculating the standard deviation
            n= n(), # counting (sample size)
            se_lung =sd_lung/sqrt(n)
            )

#> In the above code snippet I calculated the mean of lung capacity in 
#> females and males and their respective standard errors to help 
#> see the uncertainities of the mean

new_data %>% ggplot(aes(x = Gender, y = mean_gender, fill = Gender))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_errorbar(aes(ymin = mean_gender-se_lung,
                    ymax = mean_gender+se_lung), width = 0.2)+
  labs(title = "Mean Lung Capacity By Gender",
       y= "Mean Lung Capacity (ltrs)")

#A t-test 

#testing for homogenity of variance 
library(car)

leveneTest(LungCap~Gender, data = data)

#> Levene's Test shows that homogeneity of variance is not 
#> violated (p = 0.4114)
 t.test(LungCap~Gender, data= data, var.equal =TRUE)

#4. Is there a difference in Lung Capacity between smokers and non-smokers

#plotting bar chart
 
 #preparing the data 
 new_dat <- data %>% group_by(Smoke) %>% 
   summarise(mean_smoke = mean(LungCap),
             sd_lung = sd(LungCap), # calculating the standard deviation
             n= n(), # counting (sample size)
             se_lung =sd_lung/sqrt(n)
   )
 
#plotting
 new_dat %>% ggplot(aes(x = Smoke, y = mean_smoke, fill =Smoke))+
   geom_bar(stat = "identity", show.legend = FALSE, alpha = 0.7)+
   geom_errorbar(aes(ymin = mean_smoke-se_lung,
                     ymax = mean_smoke+se_lung), width = 0.2)+
   labs(title = "Mean Lung Capacity By Smoking Status",
        y= "Mean Lung Capacity (ltrs)")
 
 
#iNDEPENDENT T-TEST
 #Homogeneity of variance testing
 library(car)
leveneTest(LungCap~Smoke, data = data) 
#> Homogeneity of variance is violated p<0.05 thus we will use an Welch's 
#> T-test
#> 


t.test(LungCap~Smoke, data = data, var.equal = FALSE)


##Does the children Born with Caesarian Section Have a lower lung capacity
##than those without

#PLOTTING A BAR CHART
#preparing the data 
new_da <- data %>% group_by(Caesarean) %>% 
  summarise(mean_cs = mean(LungCap),
            sd_lung = sd(LungCap), # calculating the standard deviation
            n= n(), # counting (sample size)
            se_lung =sd_lung/sqrt(n)
  )

#plotting 
new_da %>% ggplot(aes(x = Caesarean, y = mean_cs, fill =Caesarean))+
  geom_bar(stat = "identity", show.legend = FALSE, alpha = 0.8)+
  geom_errorbar(aes(ymin = mean_cs-se_lung,
                    ymax = mean_cs+se_lung), width = 0.2)+
  labs(title = "Mean Lung Capacity By Caesarean Section",
       y= "Mean Lung Capacity (ltrs)")

#INDEPENDENT T-TEST

#checking for homogeneity of variance 
leveneTest(LungCap~Caesarean, data = data)

#> Homogeneity of variance is not violated p = 0.726

t.test(LungCap~Caesarean, data = data, var.equal = TRUE)



##MULTIPLE LINEAR REGRESSION 

#fitting the model
model <- lm(LungCap ~ Caesarean+Gender+Smoke+Height+Age, data = data)

#Checking all the model assumptions
library(performance)

check_model(model,  verbose = TRUE)


##
#checking multicollinearity
check_collinearity(model)
plot(check_collinearity(model))



##PLOTTING THE MODEL RESULTS 

library(ggeffects)
library(sjPlot)
ggeffect(model) %>%  
  plot() %>%
  sjPlot::plot_grid()


##the predictor importance 
library(gtsummary)

model_table <- 
  tbl_regression(model,
                 pvalue_fun = ~style_pvalue(.x,digits = 3)) %>% 
  bold_p() %>% 
  modify_header(label = "**Predictors**") %>% 
  modify_caption("**Lung Capacity Prediction**") %>% sort_p() %>% bold_labels()

summary(model)
