#Set up the working library
install.packages("tidyverse")
install.packages("psych")
install.packages("ggplot2")
install.packages("skimr")
install.packages("visreg")
install.packages("stargazer")
install.packages("sjPlot")
install.packages("car")
install.packages("gtsummary")

library(tidyverse)
library(psych) 
library(ggplot2) 
library(skimr)
library(visreg)
library(stargazer)
library(sjPlot)
library(car)
library(gtsummary)


#upload the dataset
ESS11data <- read_csv("ESS11e04_0.csv", show_col_types = FALSE)

#Filter UK subdataset
ESS11_GB <- ESS11data %>% 
  filter(cntry == "GB")

#Select and rename variables
ESS11_GB <- ESS11_GB %>% 
  select(age = agea, 
         gender = gndr,
         born_GB = brncntr, 
         education = eduyrs, 
         income = hinctnta,
         health,
         happy,
         trstprl,
         trstplt,
         trstprt,
         stfhlth,
         lrscale)

#Manually code missing values as NA
ESS11_GB <- ESS11_GB %>% 
  mutate(age = if_else(age %in% c(99, 999), NA, age),
         gender = na_if(gender, 9), 
         born_GB = if_else(born_GB %in% c(7, 8, 9), NA, born_GB), 
         education = if_else(education >= 77, NA, education),
         income = if_else(income >= 77, NA, income), 
         health = if_else(health %in% c(7, 8, 9), NA, health), 
         happy = if_else(happy >= 77, NA, happy), 
         trstprl = if_else(trstprl >= 77, NA, trstprl),
         trstplt = if_else(trstplt >= 77, NA, trstplt),
         trstprt = if_else(trstprt >= 77, NA, trstprt),
         stfhlth = if_else(stfhlth > 11, NA, stfhlth),
         lrscale = if_else(lrscale >= 77, NA, lrscale))

#Mutate variables
ESS11_GB <- ESS11_GB %>% 
  mutate(age = as.numeric(age), 
         gender = as_factor(gender), 
         born_GB = as_factor(born_GB), 
         education = as.numeric(education),
         income = as.numeric(income),
         health = as.numeric(health), 
         happy = as.numeric(happy), 
         trstprl = as.numeric(trstprl), 
         trstplt = as.numeric(trstplt),
         trstprt = as.numeric(trstprt),
         stfhlth = as.numeric(stfhlth),
         lrscale = as.numeric(lrscale)) 

#create index
ESS11_GB <- ESS11_GB %>% 
  mutate(trst_index = ((trstprl + trstplt + trstprt)/3)) 

#test to verify the index
index_institution <- data.frame(ESS11_GB$trstprl, 
                         ESS11_GB$trstplt, 
                         ESS11_GB$trstprt)
alphatest <- alpha(index_institution)
summary(alphatest)
#results raw_alpha 0.91, it is reliable


#overview all data informations
skim(ESS11_GB) 


#number of missing variables
sum(is.na(ESS11_GB))

#graphs for focal variables
# Plot focal X
tiff("Institutional Trust Index.tiff", units="in", width=8, height=5, res=600) 
ggplot(ESS11_GB, aes(trst_index)) +
  geom_histogram(binwidth = 1, color="black", fill="chartreuse3") +
  geom_vline(xintercept=mean(ESS11_GB$stfhlth,na.rm=TRUE), col="red", lwd=1,
             linetype="dashed") +
  theme_classic() +
  ggtitle("Focal Independent Variable") +
  xlab("Institutional Trust Index") + # # riguarda cosa scriverci AAA ELIMINA NOTA
  ylab("Frequency") # # riguarda cosa scriverci AAA ELIMINA NOTA
dev.off() 

# Summary stats for focal X
summary(ESS11_GB$trst_index)

# Plotting focal Y
tiff("Pubblic opinion about health.tiff", units="in", width=8, height=5, res=600)
ggplot(ESS11_GB, aes(stfhlth)) +
  geom_histogram(binwidth = 1, color="black", fill="cyan3") +
  geom_vline(xintercept=mean(ESS11_GB$stfhlth,na.rm=TRUE), col="red", lwd=1,
             linetype="dashed") +
  theme_classic() +
  ggtitle("Focal Dependent Variable") +
  xlab("Public opinion about health") +
  ylab("frequency") # riguarda cosa scriverci AAA ELIMINA NOTA
dev.off()

# Summary stats for focal Y
summary(ESS11_GB$stfhlth) 

#Plot bivariate relationship
tiff("Bivariate analysis.tiff", units="in", width=8, height=5, res=600)
ggplot(ESS11_GB, aes(x=trst_index,
                    y= stfhlth)) +
  geom_jitter(alpha=0.3, color="red") + 
  geom_smooth(method = "lm", se=TRUE)+
  theme_classic() +
  ggtitle("Bivariate Model") +
  xlab("Institutional Trust Index") + 
  ylab("Pubblic opinion about health")
dev.off()

#regressions models
#bivariate analysis
model1 <- lm(stfhlth ~ trst_index, 
             data = ESS11_GB)
summary(model1) #info about the model
par(mfrow = c(2, 2)) #with this I can see all the 4 graphs together
plot(model1)
par(mfrow = c(1, 1)) # at the end I reset 

#table bivariate relationship
table1 <- tbl_regression(model1, label = trst_index ~ "Institutional trust",
                         intercept = TRUE) %>%
  add_glance_table(include = c(adj.r.squared)) %>%
  modify_header(label = "**Variable**", p.value = "**P**") %>%
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) %>%
  modify_caption("**Bivariate Model: political trust and opinion about healthcare** (N =
{N})") %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")
print(table1)
gt::gtsave(table1, file = "Table1_Bivariate.png")

#Add to the model the control variables
model2 <- lm(stfhlth ~ trst_index + 
               age + 
               gender + 
               born_GB + 
               education + 
               income + 
               health + 
               happy,
             data = ESS11_GB)
summary(model2) #info about the model

#I didn't create dummy variables because in my analysis it was useless and it makes me lose some important information about control varaibles

#table control variables
table2 <- tbl_regression(model2,intercept = TRUE,
                         label = c(trst_index ~ "Institutional trust", education = "Years of Education", gender = "Gender", 
                                   age = "Age", born_GB = "Not born in UK", income= "Income", 
                                   health = "Health condition", happy = "Happiness"),
                         show_single_row = c("gender", "born_GB")) %>% 
  add_glance_table(include = c(adj.r.squared)) %>% 
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)%>% 
  modify_header(label = "**Variable**", p.value = "**P**") %>%
  modify_caption("**Exclusionary Strategy: Model with control variables** (N = {N})")%>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")
print(table2)
gt::gtsave(table2, file = "Table2_Exclusionary.png")


#model with moderator 
model3 <- lm(stfhlth ~ trst_index * lrscale + 
               age + 
               gender + 
               born_GB + 
               education + 
               income + 
               health + 
               happy,
             data = ESS11_GB)
summary(model3) #info about the model
# political opinion is not a moderator because the p-value is too high

table3 <- tbl_regression(model3,intercept = TRUE,
                         label = c(trst_index ~ "Institutional trust", education = "Years of Education", gender = "Gender", 
                                   age = "Age", born_GB = "Not born in UK", income= "Income", 
                                   health = "Health condition", happy = "Happiness", lrscale = "Left-Right Scale"),
                         show_single_row = c("gender", "born_GB")) %>% 
  add_glance_table(include = c(adj.r.squared)) %>% 
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)%>% 
  modify_header(label = "**Variable**", p.value = "**P**") %>%
  modify_caption("**Model with control variables and moderator** (N = {N})")%>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")
print(table3)
gt::gtsave(table3, file = "Table3_Control.png")


#search the outliers
#Display the cases with the highest numbers of Cook's D
CooksD <- cooks.distance(model2) #
sort(CooksD, decreasing = TRUE) %>% head() #Top 6 of the outlier
#there weren't any outlier that was too problematic

#Linearity
plot(model2, which = 1)

#Homogeneity of variance
plot(model2, which = 3)

#Normality of the residuals
plot(model2, which = 2)

par(mfrow = c(2, 2)) #with this I can see all the 4 graphs together
plot(model2)
par(mfrow = c(1, 1))










