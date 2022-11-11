###VIDEO 1###
###This sets the working directory - I suggest you create a folder where you store all the materials for each week of the course.####
###In my case - I've stored on my One Drive - Under a folder called 2028 under a folder called Week 1.###
### Please note if you are using a mac - you will need to use a different way of setting the working directory ###
setwd("c:/Users/lnc20nlh/OneDrive - Bangor University/STATS_Ideas/2028/Week1//")
getwd()
dir() 
#C:\Users\lnc20nlh\OneDrive - Bangor University\STATS_Ideas\2028\Week1
### Read the data in - please use csv files as this is simpler ###
data <- read.csv("SDECdata_tidy.csv")
###View the data you've imported###
View(data)

str(data)
###What are the dimensions of this data?###
dim(data)
###Answer comes back 1258 9, which means 1258 rows, 9 columns, or 1258 obs, 9 variables ###
###What are the column names?
colnames(data)
#[1] "Identifier"           "Age"                  "gender"               "admittance.method"    "number.of.visits"     "Length.of.Stay..LOS."
#[7] "speciality"           "DISCHARGE.WARD"       "discharged" 
###Please note if there are gaps - R adds these as a .(dot), R does not like spaces##
##R is REALLY sensitive about spelling and caps letters etc### 
#For example
view(data)
#Doesn't work - it brings an error message - must be a capital V #
View(data)



####Examining the 1st column ####
data$Identifier
data$Identifier
length(data$Identifier)
#how many unique Indentifiers are there?
unique(data$Identifier)
#Looks like it finishes on 966
length(unique(data$Identifier))
#962 - ?! Why the difference?! Must be 4 missing???
### what is missing?
### manually examined = why no 144? 677? 688? 933? What is going on with these?
### These unique identifiers weren't used by the original coder. They genuinely don't exist. But this shows the power ###
#Sample n=962

###Key stats on column 2 = Age ###
data$Age

?mean

mean(data$Age)
#57.0779
mean(data$Age)
median(data$Age)
range(data$Age)
#standard deviation#
sd(data$Age, na.rm = FALSE)

### summary is a really helpful function ###
summary(data$Age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#17.00   42.00   59.00   57.08   73.00   97.00 
### I can write this to a .txt file in the directory I made before.
sink('Age.txt')
summary(data$Age)
sink()
### I can make a table of Ages ###
table(data$Age)
tableAge <- table(data$Age)
write.csv(tableAge, "tableAge.csv")

####GENDER####
class(data$gender)
#[1] "character"
table(data$gender)
#F   M 
#681 577
tableGender <- table(data$gender)
tableGender
#   F   M 
# 681 577
write.csv(tableGender, "tableGender.csv")


####admittance.method####
table(data$admittance.method)
tableadmittancemethod<- table(data$admittance.method)
write.csv(tableadmittancemethod, "tableadmittancemethod.csv")

####specialty####
table(data$speciality)

####DISCHARGE.WARD####
table(data$DISCHARGE.WARD)
tableDISCHARGEWARD<- table(data$DISCHARGE.WARD)
write.csv(tableDISCHARGEWARD, "tableDISCHARGEWARD.csv")

data$discharged ==`home'


table(data$discharged)
tabledischarged <- table(data$discharged)
write.csv(tabledischarged, "tabledischarged.csv")
####discharged####

############ VIDEO 2 ######################################

#### make Age Categorical ####
data$AgeCategorical <-"90+"
data$AgeCategorical[data$Age<=90]<-"81-90"
data$AgeCategorical[data$Age<=80]<-"71-80"
data$AgeCategorical[data$Age<=70]<-"61-70"
data$AgeCategorical[data$Age<=60]<-"51-60"
data$AgeCategorical[data$Age<=50]<-"41-50"
data$AgeCategorical[data$Age<=40]<-"31-40"
data$AgeCategorical[data$Age<=30]<-"21-30"
data$AgeCategorical[data$Age<=20]<-"11-20"
data$AgeCategorical[data$Age<=10]<-"0-10"
tableAgeCategorical <- table(data$AgeCategorical)
tableAgeCategorical
write.csv(tableAgeCategorical, "tableAgeCategorical.csv")

#### make LOS Categorical ####
data$LOSCategorical <- data$Length.of.Stay..LOS.
data$LOSCategorical[data$Length.of.Stay..LOS.>=10]<-"10+"
data$LOSCategorical[data$Length.of.Stay..LOS.>=30]<-"30+"
data$LOSCategorical[data$Length.of.Stay..LOS.>=50]<-"50+"

tableLOSCategorical<- table(data$LOSCategorical)
tableLOSCategorical

class(data$LOSCategorical)

data$LOSCategorical <- as.factor(data$LOSCategorical)

###https://statisticsglobe.com/ordered-bars-in-ggplot2-barchart-in-r
### Change ordering manually
data$LOSCategorical <- factor(data$LOSCategorical,                                    
                  levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+", "30+", "50+"))

tableLOSCategorical<- table(data$LOSCategorical)
tableLOSCategorical
write.csv(tableLOSCategorical, "tableLOSCategorical.csv")

############# VIDEO 3 = How many repeat visits? ###########
#####Questions####
#(1) How many repeat visits? 
table(data$Identifier)

howmanyvisits <- table(data$Identifier)
howmanyvisits2 <- as.data.frame(howmanyvisits)
#What are the column headings in this newly formed table#
names(howmanyvisits2) 
#"Var1" "Freq"
#rename column headings from Var1, Freq to Identifier and Number of visits#
names(howmanyvisits2)  <- c("Identifier", "Number of visits")
#merge howmanyvisits2 with 
data2 <- merge(data, howmanyvisits2, all=TRUE)
### overide the original data table ###
data <- data2

tablehowmanyvisits  <- table(table(data$Identifier))
tablehowmanyvisits <- as.data.frame(tablehowmanyvisits)
tablehowmanyvisits

###  1   2   3   4   5
### 796 142  21   2   1
write.csv(tablehowmanyvisits, "tablehowmanyvisits.csv")

###########################VIDEO 4############################################################

#### 2-way tables ###
tableLoSspec<- table(data$Length.of.Stay..LOS., data$speciality)
tableLoSspec
write.csv(tableLoDspec, "tableLoSspec.csv")

# same data different way around #
tablespecLoD <- table(data$speciality, data$Length.of.Stay..LOS.)
tablespecLoD
write.csv(tablespecLoD, "tablespecLoD.csv")

specialityLOSCategorical <- table(data$speciality, data$LOSCategorical)
specialityLOSCategorical

write.csv(specialityLOSCategorical, "specialityLOSCategorical.csv")

###########################VIDEO 5############################################################

#####Making a new column - to seperate out "normal SDEC" and "surgical SDEC".####
####normal or surgical####
##make a new column #
data$medicalorsug <- 0
##code that column #
data$medicalorsug <- ifelse(data$speciality == "resp", "medicalSDEC",
                     ifelse(data$speciality == "acute med", "medicalSDEC", 
                      ifelse(data$speciality == "gen surg", "surgicalSDEC",
                      ifelse(data$speciality == "vascular", "surgicalSDEC",
                      ifelse(data$speciality == "urology", "surgicalSDEC", 
                      ifelse(data$speciality == "gen medicine", "medicalSDEC", 
                      ifelse(data$speciality == "gastroenterology", "medicalSDEC", 
                      ifelse(data$speciality == "endocrinology", "medicalSDEC", 
                      ifelse(data$speciality == "maxillo facial surgery", "surgicalSDEC", 
                      ifelse(data$speciality == "geriatric medicine", "medicalSDEC", 
                      ifelse(data$speciality == "orthopaedics", "surgicalSDEC", 
                      ifelse(data$speciality == "ENT", "surgicalSDEC", 
                      ifelse(data$speciality == "resp diagnostics", "medicalSDEC", 
                      ifelse(data$speciality == "gynaecology", "surgicalSDEC", 
                      ifelse(data$speciality == "gastro endoscopy", "surgicalSDEC", 
                      ifelse(data$speciality == "breast surgery", "surgicalSDEC",
                      ifelse(data$speciality == "nephrology", "medicalSDEC", NA
                                                   )))))))))))))))))

#### Filter by this new column - make new data tables ####
#https://www.statmethods.net/management/subset.html
data_medicalSDEC <- subset(data, medicalorsug == "medicalSDEC")
data_surgicalSDEC <- subset(data, medicalorsug == "surgicalSDEC")

######
library(ggplot2)
graphmedsurg <- ggplot(data, aes(x=medicalorsug))+
                geom_bar()
graphmedsurg

graphmedsurg2 <- ggplot(data, aes(x=medicalorsug, fill=LOSCategorical))+
  geom_bar()
graphmedsurg2

graphmedsurg2B<- ggplot(data, aes(x=medicalorsug, fill=LOSCategorical))+
  geom_bar(position = position_dodge(preserve = 'single'))
graphmedsurg2B

graphmedsurg3 <- ggplot(data, aes(x=LOSCategorical, fill=medicalorsug))+
  geom_bar()
graphmedsurg3

graphmedsurg4 <- ggplot(data, aes(x=LOSCategorical, fill=medicalorsug))+
  geom_bar(position = position_dodge(preserve = 'single'))
graphmedsurg4

graphmedsurg5<- ggplot(data, aes(x=medicalorsug, y = value, fill=LOSCategorical))+
  geom_bar(stat = "identity")
graphmedsurg5






summary(data_medicalSDEC$Length.of.Stay..LOS.)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   0.000   0.000   2.086   1.000  72.000 
summary(data_surgicalSDEC$Length.of.Stay..LOS.)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.425   0.000  41.000 

sink('SDEC_LOS.txt')
paste('medical SEDC LOS')
summary(data_medicalSDEC$Length.of.Stay..LOS.)
paste('surgical SEDC LOS')
summary(data_surgicalSDEC$Length.of.Stay..LOS.)
sink()

#### Can you do this for Age? #####

################################VIDEO 6##############################################################
###Comparison between 2 things = non-paired - question - is it parametric or non-parametric. Is it normally distributed? ##
##if normal - parametric - then T-test.
##if non-normal - non-para - then mann-whitney U.

### to test for normality - use sharpiro-wilko test #

#paste('shapiro.test LOS')
shapiro.test(data$Length.of.Stay..LOS.)
#	Shapiro-Wilk normality test
#
#data:  data$Length.of.Stay..LOS.
#W = 0.36075, p-value < 2.2e-16
#
#From the output, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. 
#In other words, we can NOT assume the normality.
##Our data is non-normal. non-para
### We need to use Mann Whitney U.
#https://www.statmethods.net/stats/nonparametric.html 

#t.test(data$Length.of.Stay..LOS.~data$medicalorsug)
#paste('wilcox.test LOS')
wilcox.test(data$Length.of.Stay..LOS.~data$medicalorsug)
wilcox.test(data$Length.of.Stay..LOS.~data$medicalorsug)
#Wilcoxon rank sum test with continuity correction

#data:  data$Length.of.Stay..LOS. by data$medicalorsug
# W = 204477, p-value = 0.000421
#alternative hypothesis: true location shift is not equal to 0

#There is a difference and yes significant #

######Can you do this for age? ###########
