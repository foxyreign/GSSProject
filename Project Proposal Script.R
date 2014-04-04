# Research Question
## Does religion preference have significant relationship 
## with how they accept sexual relations of the same sex?

# Citation
## General Social Survey (GSS)
## A sociological survey used to collect data on demographic characteristics 
## and attitudes of residents of the United States.

## Codebook can be found here:
## https://d396qusza40orc.cloudfront.net/statistics%2Fproject%2Fgss1.html
load(url("http://bit.ly/dasi_gss_data"))
source('http://bit.ly/dasi_inference')

# Data Collection
## Provided in the course.

# Cases (observational/experimental units)
## There are a total of 57,061 cases and 114 variables in this dataset.
## this is a cumulative data file for surveys conducted between 1972 - 2012 
## and that not all respondents answered all questions in all years.

# Variables
## Homosexual - sexual relationship of the same sex
## numerical, categorical
### 1 - Always Wrong Almst
### 2 - Almst Always Wrg
### 3 - Sometimes Wrong
### 4 - Not Wrong At All
### 5 - Other
## Religion - religious preference
## numerical, categorical
### 1 - Protestant
### 2 - Catholic
### 4 - None

# Type of Study
## Observational - ??????????????????????????

# Scope of inference - generalizability
## ??????????????????????????

# Scope of inference - causality
## ???????????????????????????

# Other Notes
## Originally, there are 57,061 cases that fall under the criteria
## of selected religion, Protestant, Catholic and None. 32,789 of these
## cases are valid while 24,272 contains NA or missing values; 57.46%
## and 42.54% respectively. 
summary(gss$homosex, exclude.missing=T)

# Install packages, if necessary
install.packages("Hmisc", dep = T)
install.packages("vcd", dep = T)
install.packages("ggplot2", dep = T)
install.packages("gmodels", dep = T)

# Load Libraries
library(Hmisc)
library(vcd)
library(ggplot2)
library(gmodels)

# Subset only selected religions - Protestant, Catholic and None 
project <- subset(gss, relig == "Protestant" | 
                    relig == "Catholic" | 
                    relig == "None")

# And omit NA cases in homosexual variable
project <- subset(project, !is.na(project$homosex))

# Factor only the selected religions and homosexual
religion <- factor(project$relig)
homosexual <- factor(project$homosex)

# Exploratory Data Analysis
# Summary Statistics - Religion and Homosexual
describe(religion)
describe(homosexual)

summary(religion)
head(gss)

dataset <- data.frame(religion, homosexual)
head(dataset)
tail(dataset)

# Moscaic Plot
mosaic(religion ~ homosexual, data = dataset, shade = T, gp = shading_max)

?mosaic

mosaicplot(table(dataset$religion, dataset$homosexual), las = 1, 
           main = "Mosaic Plot of Religion and Homosexual")

table(dataset$homosexual, dataset$religion)
summary(dataset)

CrossTable(x = dataset$homosexual, y = dataset$religion, 
           prop.r = F, prop.c = F, prop.t = F, expected = T, fisher = T, digits = 2, 
           prop.chisq = T, resid = T,
           dnn = c("Homosexual", "Religion"))

########################################## for markdown
# Subset only selected religions - Protestant, Catholic and None 
project <- subset(gss, relig == "Protestant" | 
                    relig == "Catholic" | 
                    relig == "None")

# And omit NA cases in homosexual variable
project <- subset(project, !is.na(project$homosex))

# Factor only the selected religions and homosexual
religion <- factor(project$relig)
homosexual <- factor(project$homosex)

# Produce a dataset from the subsets
gssdataset <- data.frame(religion, homosexual)

CrossTable(x = gssdataset$homosexual, y = gssdataset$religion, 
           prop.r = F, prop.c = F, prop.t = F, expected = T, digits = 2, 
           dnn = c("Homosexual", "Religion"))

H0 - Protestant - Always Wrong = Catholic - Always Wrong = None - Always Wrong
H0 - Protestant - Almost Always Wrong = Catholic - Almost Always Wrong = None - Almost Always Wrong
H0 - Protestant - Sometimes Wrong = Catholic - Sometimes Wrong = None - Sometimes Wrong
H0 - Protestant - Not Wrong At All = Catholic - Not Wrong At All = None - Not Wrong At All
H0 - Protestant - Other = Catholic - Other = None - Other

Ha - At least one of the null hypotheses is false

The p-value is the probability that a chi-square statistic having 8 degrees of freedom is more extreme than 2831.089. Since

inference(y = gssdataset$homosexual, x = gssdataset$religion, est = "proportion", 
          type = "ht", method = "theoretical", las = 1
          success = "Always Wrong")

inference
