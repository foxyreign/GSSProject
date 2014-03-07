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
install.packages("ggplot2")

# Load Libraries
library(Hmisc)
library(vcd)
library(ggplot)

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
head(gss)

# Moscaic Plot
mosaic(table(religion, homosexual), shade = T)


# Complete
