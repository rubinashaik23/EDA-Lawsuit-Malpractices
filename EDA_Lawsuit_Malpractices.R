library(dplyr)
library(data.table)
library(plyr)
library(readxl)
library(e1071)
library(formattable)
library(tidyr)
lawsuits = read_xlsx("lawsuits.xlsx")
dt_lawsuits = data.table(lawsuits)
#Data Prep
attach(lawsuits)
df_attorney = data.frame(lawsuits$`Private Attorney`)
colnames(df_attorney)= "Attorney"
# Changing 0/1 to Yes/No
df_attorney = df_attorney %>% 
  mutate(Attorney = as.character(Attorney)) %>% 
  mutate(Attorney = replace(Attorney, Attorney ==0, 'No'))
df_attorney = df_attorney %>% 
  mutate(Attorney = as.character(Attorney)) %>% 
  mutate(Attorney = replace(Attorney, Attorney ==1, 'Yes'))
# Create Severity vs Private Attorney table
# Frequency Distribution with class interval defined by Severity
df_lawyer1 = data.frame(lawsuits$Severity,df_attorney)
colnames(df_lawyer1) = c("Severity","Private Attorney")
formattable(df_lawyer1,align =c("c","c"))
lawyertab1 = table(df_lawyer1)
#Export Table to Archive and Reupload to format table
write.csv(lawyertab1,"C:\\Users\\rose_\\Downloads\\BANA6610\\Datasources\\lawyertab1.csv",col.names = TRUE)
lawyertab1 = read.csv('lawyertab1.csv')
colnames(lawyertab1) = c("Severity","No","Yes")
formattable(lawyertab1,align = c(rep("c", NCOL(lawyertab1))),
            list(`Severity` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))))
# Percentage Distribution
lawyertab2 = table(df_lawyer1)
lawyertab2 = prop.table(lawyertab2)*100
#Export Table to Archive and Reupload to format table
write.csv(lawyertab2,"C:\\Users\\rose_\\Downloads\\BANA6610\\Datasources\\lawyertab2.csv",col.names = TRUE)
lawyertab2 = read.csv('lawyertab2.csv')
colnames(lawyertab2) = c("Severity","No","Yes")
formattable(lawyertab2,align = c(rep("c", NCOL(lawyertab2))),
            list(`Severity` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                 area(col = 2:3) ~ function(x) percent(x / 100, digits = 0)))
# Percentage Distribution by Column
lawyertab3 = table(df_lawyer1)
lawyertab3 = prop.table(lawyertab3,2)*100
write.csv(lawyertab3,"C:\\Users\\rose_\\Downloads\\BANA6610\\Datasources\\lawyertab3.csv",col.names = TRUE)
lawyertab3 = read.csv('lawyertab3.csv')
colnames(lawyertab3) = c("Severity","No","Yes")
formattable(lawyertab3,align = c(rep("c", NCOL(lawyertab3))),
            list(`Severity` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                 area(col = 2:3) ~ function(x) percent(x / 100, digits = 0)))

# Cumulative Sum Distribution
lawyertab4 = table(df_lawyer1)
df_cumsum1 = data.frame(lawyertab4)
df_cumsum1 = cumsum(df_cumsum1[10:18,3])
df_cumsum1s = data.frame(lawyertab4)
df_cumsum1s = df_cumsum1s[1:9,1]
df_cumsum2 = data.frame(lawyertab4)
df_cumsum2 = cumsum(df_cumsum2[1:9,3])
df_cumsumtotal = data.frame(df_cumsum1s,df_cumsum2,df_cumsum1)
colnames(df_cumsumtotal)=c("Severity","Cumulative Sum No","Cumulative Sum Yes")
formattable(df_cumsumtotal, align = c("c","c","c"))
#table of severity vs specialty
#need to combine OGBYN fields
ogbyncombo = lawsuits
ogbyncombo = ogbyncombo %>% 
  mutate(Specialty = as.character(Specialty)) %>% 
  mutate(Specialty = replace(Specialty, Specialty =='ObGyn', 'OGBYN'))
specialtytab = table(lawsuits$Severity, ogbyncombo$Specialty)
write.csv(specialtytab,"C:\\Users\\rose_\\Downloads\\BANA6610\\Datasources\\specialtytab.csv",col.names = TRUE)
specialtytab= read.csv('specialtytab.csv')
specialtytab$OBGYN = specialtytab$OBGYN+specialtytab$OGBYN
specialtytab = select(specialtytab, -c(12))
colnames(specialtytab) = c("Severity","Anesthesiology","Cardiology","Dermatology","Emergency Medicine","Family Practice","General Surgery","Internal Medicine","Neurology/ Neurosurgery","OBGYN","Occupational Medicine", "Opthamology","Orthopedic Surgery","Pathology","Pediatrics","Physical Medicine", "Plastic Surgeon", "Radiology", "Resident","Thoracic Surgery", "Urological Surgery")
formattable(specialtytab, align = c(rep("c",NCOL(specialtytab))), 
            list(`Severity` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                 area(col = 2:21) ~ color_tile("#DeF7E9", "#71CA97")))
#Class Frequency distribution by 10 years
ages = lawsuits$Age
range(ages)
breaks = seq(0,90,10)
AgeFrequencyDistribution = cbind(table(cut(lawsuits$Age, breaks, right=FALSE)))
write.csv(AgeFrequencyDistribution,"C:\\Users\\rose_\\Downloads\\BANA6610\\Datasources\\AgeFrequencyDistribution.csv",col.names = TRUE)
AgeFrequencyDistribution = read.csv('AgeFrequencyDistribution.csv')
colnames(AgeFrequencyDistribution) = c("Age Range","Age Frequency Distribution")
formattable(AgeFrequencyDistribution)
#create vector of mean payments made to Females of various severities
fempayout = c((mean(lawsuits$Payment[(lawsuits$Severity == 1) & (lawsuits$Gender == "Female")])), 
              mean(lawsuits$Payment[(lawsuits$Severity == 2) & (lawsuits$Gender == "Female")]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 3) & (lawsuits$Gender == "Female")]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 4) & (lawsuits$Gender == "Female")]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 5) & (lawsuits$Gender == "Female")]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 6) & (lawsuits$Gender == "Female")]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 7) & (lawsuits$Gender == "Female")]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 8) & (lawsuits$Gender == "Female")]) ,
              mean(lawsuits$Payment[(lawsuits$Severity == 9) & (lawsuits$Gender == "Female")]))
#create vector of mean payments made to Males of various severities
malepayout = c((mean(lawsuits$Payment[(lawsuits$Severity == 1) & (lawsuits$Gender == "Male")])), 
               mean(lawsuits$Payment[(lawsuits$Severity == 2) & (lawsuits$Gender == "Male")]), 
               mean(lawsuits$Payment[(lawsuits$Severity == 3) & (lawsuits$Gender == "Male")]), 
               mean(lawsuits$Payment[(lawsuits$Severity == 4) & (lawsuits$Gender == "Male")]), 
               mean(lawsuits$Payment[(lawsuits$Severity == 5) & (lawsuits$Gender == "Male")]), 
               mean(lawsuits$Payment[(lawsuits$Severity == 6) & (lawsuits$Gender == "Male")]), 
               mean(lawsuits$Payment[(lawsuits$Severity == 7) & (lawsuits$Gender == "Male")]), 
               mean(lawsuits$Payment[(lawsuits$Severity == 8) & (lawsuits$Gender == "Male")]), 
               mean(lawsuits$Payment[(lawsuits$Severity == 9) & (lawsuits$Gender == "Male")]))
#create matrix of row female payout and male payout
paymat = rbind(fempayout,malepayout)
df_pay = paymat
rownames(df_pay)=c("Female Payout","Male Payout")
colnames(df_pay)=c("Severity:1","Severity:2","Severity:3","Severity:4","Severity:5","Severity:6","Severity:7","Severity:8","Severity:9")
# Change NaN values to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
df_pay[is.nan(df_pay)] = 0
write.csv(df_pay,"C:\\Users\\rose_\\Downloads\\BANA6610\\Datasources\\df_pay.csv",col.names = TRUE)
df_pay= read.csv('df_pay.csv')
colnames(df_pay)=c("Gender","Severity:1","Severity:2","Severity:3","Severity:4","Severity:5","Severity:6","Severity:7","Severity:8","Severity:9")
formattable(df_pay,align =c(rep("c", NCOL(df_pay))), 
            list(`Gender` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))
#side by side barplot comparing the different mean payouts vs severity for male vs female
barplot(paymat, beside = TRUE, col = c("aquamarine3","coral"), names.arg = 1:9, main = "Severity vs. Payout by Male/Female", xlab = "Severity", ylab = "Payout")
legend("topleft", c("Female", "Male"), pch = 15, co = c("aquamarine3","coral"), bty = "n")
#means for male and female
malemean = mean(lawsuits$Payment[lawsuits$Gender == "Male"])
femalemean = mean(lawsuits$Payment[lawsuits$Gender == "Female"])
#add overall means to graph
femalemeantxt = sprintf("Average Payout for Females: %g", femalemean)
text(7, 4000, femalemeantxt)
malemeantxt = sprintf("Average Payout for Males: %g", malemean)
text(7, 3000, malemeantxt)

#adding legend to label data

#create function that takes input of type of insurance and returns a vector of the mean payment for each level of severity for that type of insurance
insurancepay = function(typeinsurance){
  payvect = c(c(mean(lawsuits$Payment[(lawsuits$Severity == 1) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 2) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 3) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 4) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 5) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 6) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 7) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 8) & (lawsuits$Insurance == typeinsurance)])),
              c(mean(lawsuits$Payment[(lawsuits$Severity == 9) & (lawsuits$Insurance == typeinsurance)])))
  return(payvect)
}
plot(c(1:9), insurancepay("Private"), type = "b", main = "Payout vs Severity by Type of Insurance", xlab = "Severity", ylab = "Payout")
#plots private insurance means of various seveirites calculated using the functin insurancepay and joins them with a line
points(c(1:9), insurancepay("No Insurance"), type = "b", col = "red")
#add points/lines to previous plot in new color for other insurance types 
points(c(1:9), insurancepay("Medicare/Medicaid"), type = "b", col = "blue")
points(c(1:9), insurancepay("Workers Compensation"), type = "b", col = "green")
legend("topleft", c("Private", "No Insurance", "Medicare/Medicaid", "Workers Compensation"), pch = 15, co = c("black","red", "blue", "green"), bty = "n")
#legend for previous plot
unobserv = count(lawsuits$Insurance == "Unknown" | lawsuits$Insurance == "unknown")
#getting number of unknown insurance types
txtunobvserv = sprintf("Number of cases not captured by graph: %d", unobserv[2,2])
text(3, 1500, txtunobvserv)
#adding number and label to non captured data points

#function to create a vector for the payouts of all severity levels for different "private Attorney" inputs
attorneypay = function(attorney){
  payvectlaw = c(c(mean(lawsuits$Payment[(lawsuits$Severity == 1) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 2) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 3) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 4) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 5) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 6) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 7) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 8) & (lawsuits$'Private Attorney' == attorney)])),
                 c(mean(lawsuits$Payment[(lawsuits$Severity == 9) & (lawsuits$'Private Attorney' == attorney)])))
  return(payvectlaw)
}
#vectors for attorney/no attorney using previous function
representpay = attorneypay(1)
norepresentpay = attorneypay(0)
#mean of means for each pay level (attorney vs not attorney) to show total difference between average payouts for private attorney vs not private attorney
meanprivate = (mean(attorneypay(1)) + .00)
meannoprivate = mean(attorneypay(0), na.rm = TRUE)
#matrix of two attorney vectos
attorneymat = rbind(representpay, norepresentpay)
df_attorneypay = attorneymat
rownames(df_attorneypay)=c("With Representation Payout","Without Representation Payout")
colnames(df_attorneypay)=c("Severity:1","Severity:2","Severity:3","Severity:4","Severity:5","Severity:6","Severity:7","Severity:8","Severity:9")
# Change NaN values to 0
df_attorneypay[is.nan(df_attorneypay)] = 0
write.csv(df_attorneypay,"C:\\Users\\rose_\\Downloads\\BANA6610\\Datasources\\df_attorneypay.csv",col.names = TRUE)
df_attorneypay= read.csv('df_attorneypay.csv')
colnames(df_attorneypay)=c("Representation","Severity:1","Severity:2","Severity:3","Severity:4","Severity:5","Severity:6","Severity:7","Severity:8","Severity:9")
formattable(df_attorneypay,align = c("l", rep("r", NCOL(df_attorneypay) - 1)), 
            list(`Representation` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))
#barplot of serverity vs payout by attoutney vs no private attorney
barplot(attorneymat, beside = TRUE, col = c("blue","red"), names.arg = 1:9, main = "Severity vs. Payout by Private Attorney Representation", xlab = "Severity", ylab = "Payout")
legend("topleft", c("Represented by Private Attorney", "No Private Attorney"), pch = 15, co = c("blue","red"), bty = "n")
representtxt = sprintf("Average Payout with Private Attorney: %g", meanprivate)
text(8, 2500, representtxt)
norepresenttxt = sprintf("Average Payout without a Private Attorney: %g", meannoprivate)
text(8, 2000, norepresenttxt)




#piechat for number of malpractices suits by specialty
pie(table(lawsuits$Specialty), main = "Number of Malpractice Payouts by Specialty (Total 118)", col = rainbow(length(unique(lawsuits$Specialty))))


stdage = sd(lawsuits$Age)
meanage= mean(lawsuits$Age)
payouttoage = lawsuits$Payment/lawsuits$Age

#boxplot for payments, wanted one with outliers to talk about
boxplot(lawsuits$Payment,main = "Distrubiton of Payments", xlab = "Payment", horizontal = TRUE)
summary(lawsuits$Payment)
#gives element number of outliers
Outlierspay = boxplot(lawsuits$Payment, plot = FALSE)$out
length(Outlierspay)
#lenght of list of outliers

#Frequency Polygon for Severity
plot(table(lawsuits$Severity), main = "Frequency Polygon for Severity", xlab = "Severity", ylab = "Frequency", type = "b")

#create regression line formula
lmpayout = lm(Payment~Age, data = lawsuits)
#summary of regression info
summary(lmpayout)
#scatter plot of age vs payment
plot(lawsuits$Age, lawsuits$Payment, main = "Age vs. Payment", xlab = "Age", ylab = "Payment")
#plot regression line
abline(lmpayout)
#put regression line equation on scatterplot
text(30,5000, "Regression Line Equation: y = -3.958 x + 843.268")
#covariance between age and payment
cov(lawsuits$Age, lawsuits$Payment)
#correlation coeeficent between age and payment
cor(lawsuits$Age, lawsuits$Payment)


hist(lawsuits$Age, main = "Histogram of Age distribution", xlab = "Age", ylab = "Frequency")

meanhistage = sprintf("Mean: %g", mean(lawsuits$Age))
text(15, 25, meanhistage)
sdhistage = sprintf("Standard Deviation: %g", sd(lawsuits$Age))
text(15, 22.5, sdhistage)

median(lawsuits$Age)
#allows to compare mean and median for normalcy
kurtosis(lawsuits$Age)
#check for normalcy
skewness(lawsuits$Age)
qqnorm(lawsuits$Age, main = "Normal Probability Plot for Distribution of Age")
#check normal probability plot for aproximate straightline



#means for different marital status
divorcedmean = mean(lawsuits$Payment[(lawsuits$`Marital Status` == 0)])
singlemean = mean(lawsuits$Payment[(lawsuits$`Marital Status` == 1)])
marriedmean = mean(lawsuits$Payment[(lawsuits$`Marital Status` == 2)])
widowedmean = mean(lawsuits$Payment[(lawsuits$`Marital Status` == 3)])
#plots means in bar plot for visualization
barplot(c(divorcedmean,singlemean,marriedmean,widowedmean), names.arg = c("Divorced", "Single", "Married", "Widowed"), col = rainbow(4), main = "Malpractice Payout by Marital Status", ylab = "Payout")

munobserv = sum(lawsuits$`Marital Status` == 4)
#getting number of unknown martial status
mtxtunobvserv = sprintf("Number of cases not captured by graph: %g", munobserv)
text(3, 1500, mtxtunobvserv)
#adding number and label to non captured data points

#mean payout per severity level
payoutmeans = c((mean(lawsuits$Payment[(lawsuits$Severity == 1)])), 
              mean(lawsuits$Payment[(lawsuits$Severity == 2)]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 3)]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 4)]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 5)]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 6)]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 7)]), 
              mean(lawsuits$Payment[(lawsuits$Severity == 8)]),
              mean(lawsuits$Payment[(lawsuits$Severity == 9)]))
#plot mean payout per severity level for graphical comparison
barplot(payoutmeans, names.arg = seq(1,9,1), xlab = "Severity", ylab = "Payout", col = rainbow(9), main = "Average Payout by Severity of Damage")

