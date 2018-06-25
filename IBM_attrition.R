#Homework 4
#Tanay Pardeshi - SUID: 871023872

########################################################################
#Installing relevant packages------>
########################################################################
setwd("/Users/tanay/Desktop/IST 719 Lab/Lab 5/")
getwd()
options(gsubfn.engine = "R")
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(ggplot2)
library(plotly)
########################################################################
#Data ETL------>
########################################################################
#Data set source:- https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset/data
#Dataset Score formula:- (NumberOfColumns * 4) * (NumberOfRows/100) >= 100
#Number of Columns = 35
#Number of Rows = 1471
#Dataset Score:- (35 * 4) * (1471/100) = 2059.4
dataframe <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
dataframe
View(dataframe)

########################################################################
#Cleaning and munging the dataset--------->
########################################################################
#Data is clean and without any null values or inconsistencies.
#Metadata information------>

#Attrition parameters--->
#'Attrition' -> Employee have resigned.

#Employee related information--->
#'Age' -> Age of employee
#'EducationField' -> Education type
#'Gender' -> Gender
#'NumCompaniesWorked' -> Number of companies worked
#'MaritalStatus' -> Marital Status
#'DistanceFromHome' -> Distance from home to workplace

#Role of the employee in the company--->
#'JobRole' -> Job Role
#'JobLevel' -> Job level (1 to 5)
#'Department' -> Department
#'YearsAtCompany' -> Number of years in the company
#'TotalWorkingYears' -> Total experience
#'YearsInCurrentRole' -> Years in the current role
#'YearsSinceLastPromotion' -> Years sunce last promotion
#'YearsWithCurrManager' -> Years with the current manager

#Satisfaction information--->
#'JobSatisfaction' -> Job Satisfaction (1 to 4)
#'RelationshipSatisfaction' -> Relationship with team members (1 to 4)
#'WorkLifeBalance' -> Worklife balance (1 to 4)
#'EnvironmentSatisfaction' -> Working environment satisfaction (1 to 4)

#Salary and money related info--->
#'MonthlyIncome' -> Salary
#'MonthlyRate' -> Monthly rate
#'DailyRate' -> Amount of money you are paid per day
#'HourlyRate' -> Amount of money you are paid hourly

#Company related info--->
#'PercentSalaryHike' -> % change in salary from 2016 vs 2015.
#'StockOptionLevel' -> How many company stocks you own. (0 to 3)
#'TrainingTimesLastYear' -> Training received

#Rating and involvement related
#'PerformanceRating' -> Performance rating (1 to 4)
#'JobInvolvement' -> Job involvement (1 to 4)
#'OverTime' -> Y/N
#'BusinessTravel' ->  Rare / Frequent

#Redundant fields (Not required)
#'EmployeeNumber' -> EmpID
#'EmployeeCount' -> All values are one
#'Over18' -> All values are 'Y'
#'StandardHours' -> Standard working hours (All are equal to 40)

########################################################################
#Plots--------->
########################################################################

#Single Dimensional plots--->
#1.Barplot----->
#Plotting a barplot for Job role vs Total number of employees
brplt <- table(dataframe$JobRole)
brplt

#Plotting the chart
x <- barplot(brplt, xaxt="n", space = 0.2, col = c("#87CEFA","dark red","#00CEAA","dark blue","yellow","#87CEEB","orange","#9ACD32","#FF6347"), main = "Job Role vs Employee Count at IBM", xlab = "Job type", ylab = "Employee Count", ylim = c(0,400))
labs <- paste(names(table(dataframe$JobRole)), "role")

#Adding values to bins in the bar chart
text(cex=0.6, x=x-0.25, y=-40, labels = labs, xpd=TRUE, srt=45)
text(0.7,brplt[1],brplt[1],pos = 3, col = "Dark Red", cex = 0.8)
text(2,brplt[2],brplt[2],pos = 3, col = "Dark Red", cex = 0.8)
text(3,brplt[3],brplt[3],pos = 3, col = "Dark Red", cex = 0.8)
text(4.3,brplt[4],brplt[4],pos = 3, col = "Dark Red", cex = 0.8)
text(5.5,brplt[5],brplt[5],pos = 3, col = "Dark Red", cex = 0.8)
text(6.7,brplt[6],brplt[6],pos = 3, col = "Dark Red", cex = 0.8)
text(7.8,brplt[7],brplt[7],pos = 3, col = "Dark Red", cex = 0.8)
text(9,brplt[8],brplt[8],pos = 3, col = "Dark Red", cex = 0.8)
text(10.3,brplt[9],brplt[9],pos = 3, col = "Dark Red", cex = 0.8)

#Inference :- 1. Sales Executive, Research Scientist and Laboratory Technicians form the chunk of the employees. 
#             2. HR, Sales Representatives and Research Directors are the monority in the group



#2.Pie Chart--->
#Plotting pie chart for gender distribution in the company
slices <- table(dataframe$Gender)
lbls <- c("Male", "Female")
colr <- c("Dark red", "Yellow")
pie(slices,labels=lbls, main="Pie Chart of Gender distribution at IBM", col = c("Dark Red","Yellow"))
#Inference :- 1. Majority of the employees are female.
#             2. Male to Female ratio is 2:3


#3.Line Chart-->
#Plotting a line chart for Education Field vs Employee Count
linechart <- table(dataframe$EducationField)
plot(linechart,type = "b", col = "Dark Blue",main= "Education Field vs Employee Count at IBM", xlab = "Education Field", ylab = "Employee Count", ylim = c(0,700), pch = 19, lwd = 3.5, cex = 2)

#Inference :- 1. Majority of the employees have a life sciences and a Medical background.
#             2. HR and technical degree employees are a minority in the group.


#Multi-Dimensional Plots--->
#1. Boxplot--->
#Showing boxplots for JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance, EnvironmentSatisfaction--->
datanew <- data.frame(dataframe$JobSatisfaction,dataframe$RelationshipSatisfaction,dataframe$WorkLifeBalance,dataframe$EnvironmentSatisfaction)
library(reshape2)
meltData <- melt(datanew)
View(meltData)
boxplot(data=data.frame(meltData), value~variable, main = "Attrition factors vs Measures", xlab = "Possible Attrition Factors", ylab = "Measure on a scale of 1 - 4", col = (c("Yellow","Dark Red","Violet","Purple")))





