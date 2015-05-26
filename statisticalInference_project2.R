library(ggplot2)
library(graphics)
library(graphics)

data(ToothGrowth)
nrow(ToothGrowth)

hist(ToothGrowth$len)
barplot(table(ToothGrowth$supp)) #equal number of subjects for both supplements
barplot(table(ToothGrowth$dose)) #equal number of subjects for each level of dosage

#from the help file of ToothGrowth
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

#This shows that the length of teeth is different when treated with OJ & VC supplements with same dose levels
#Let us look at a box plot to understand the spread better.
p= ggplot(ToothGrowth, aes(factor(dose),len))
p + geom_boxplot(aes(fill = factor(supp))) + geom_jitter()

#This shows that length increases with dose levels in general for both the supplements
# For low and medium dosage levels (0.5 & 1), on an average, OJ results in higher length than VC
# we observe that at high dosage levels(2), the medians of len for both supplements are almost equal
# we would like to estimate the mean for two groups when dosage is high and test the hypothesis if true difference in means is equal to zero 

# We will first subset the data to get only high-dose specific rows
# Next split the data first by supplement and look at the distribution of len for each supplement separately

ToothGrowth_high_dose= ToothGrowth[ToothGrowth$dose==2.0, ]
VC_TG= ToothGrowth_high_dose[ToothGrowth_high_dose$supp=='VC' , ]
OJ_TG= ToothGrowth_high_dose[ToothGrowth_high_dose$supp=='OJ', ]

hist(VC_TG$len)
hist(OJ_TG$len)

mean(VC_TG$len)
mean(OJ_TG$len)

# First, we will assume that these tests are paired, i.e., they are done on the same group at different times, and estimate the difference in the mean of len
difference= OJ_TG$len - VC_TG$len
mn= mean(difference)
s= sd(difference)
n= nrow(VC_TG)
# mn + c(-1,1) * qt(0.975,n-1) * s/sqrt(n)
# the difference in the mean of len between two groups at 95% confidence level
t.test(difference)$conf
# or alternatively
# t.test(OJ_TG$len, VC_TG$len, paired=TRUE)

#Now, we will assume that the groups are independent
var(OJ_TG$len-VC_TG$len)
# we will checkout both assumptions: both variances being unequal and variances being equal
#t.test(OJ_TG$len, VC_TG$len, paired=FALSE, var.equal=FALSE)$conf
#t.test(OJ_TG$len, VC_TG$len, paired=FALSE, var.equal=TRUE)$conf

t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data= ToothGrowth_high_dose)$conf
t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data= ToothGrowth_high_dose)$statistic 


#t-statistic - how many estimated standard errors are

#if we disregard pairing, the interval contains 0

# if the len variable is an iid draw from a population of interest, 
# then, the 95% confidence intervals for the average of 'len' variable would be
x= ToothGrowth$len
(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x)))

mean(ToothGrowth$len)



