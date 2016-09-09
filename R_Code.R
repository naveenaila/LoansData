rm(list=ls(all=T))
setwd("C:/Users/navee/Desktop/DataChallange")
#packageurl <- "http://cran.r-project.org/src/contrib/Archive/knitr/knitr_1.12.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
#sqldf is used for querying on dataset using SQL
#install.packages("sqldf")
#ggplot2 is used for displaying graphical relationships among variables
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("ggrepel")
#install.packages("RJSONIO")

library(sqldf)
library(ggplot2)
library(plyr)
library(scales)
library(ggrepel)
library(RJSONIO)

## TAsk1: merging application data and institution data and returning an object of the merged data

hmda_init <- function() 
{
  loans <- read.csv("2012_to_2014_loans_data.csv",na.strings = c("","NA"))
  institution <- read.csv("2012_to_2014_institutions_data.csv",na.strings = c("","NA"))
  final_merged_data<- sqldf("select ln.*, inst.Respondent_Name_TS Respondent_Name from loans ln left join institution inst 
                            on ln.Respondent_ID=inst.Respondent_ID and ln.Agency_Code=inst.Agency_Code
                            and ln.As_of_Year=inst.As_of_Year")
  
  return(final_merged_data)
  
}    

final_merged_data = hmda_init()

# By calling hmda_init() function, it invokes the expanded dataset i.e. merged data of application and institution data set 

## Export the expanded data set to disk for the states filtered by product segment i.e.conventional_conforming_flag 

hmda_to_json <- function(final_merged_data,states=FALSE,conf_flag=FALSE)
{
  if(states != FALSE)
  {
    final_merged_data = final_merged_data[final_merged_data$State %in% states,]
    
  }
  
  
  if(conf_flag != FALSE)
  {
    final_merged_data = final_merged_data[final_merged_data$Conventional_Conforming_Flag %in% conf_flag,]
    
  }
  
  return(final_merged_data)
}

temp = final_merged_data[1:100000,]

return_val = hmda_to_json(temp,states = "WV",conf_flag = "Y")
JSONFile = toJSON(unname(split(return_val,1:nrow(return_val))))
write(JSONFile,file="JSONDATA.json")

View(JSONFile)

# TASK2: Data Quality check 
# Checking missing values in the dataset.
if(nrow(final_merged_data) == sum(is.na(final_merged_data))) 
{
  print("No Missing data")
}else
{
  print("Missing data exists")
}

# Summary of the data
summary(final_merged_data)

# Checking for data types , class of the variables 
str(final_merged_data)
unique(final_merged_data$As_of_Year)

###### Data preprocessing

# Remove Trailing zero from the variables
# Because of the presence of trailing variables, some of the numeric variables are showing as Factor variables
# Function to remove trailing zeroes from the variable 
Remove_zeroes <- function(var,dataset)
{
  var_values =  as.numeric(str_trim(substr(as.character(dataset[,var]), 
                                           regexpr("[^0]",dataset[,var]),
                                           nchar(as.character(dataset[,var]))),side = "both"))
  return(var_values)
  
}

# Converting variables to data types
require(stringr)
final_merged_data$Applicant_Income_000 = Remove_zeroes("Applicant_Income_000",final_merged_data)
final_merged_data$FFIEC_Median_Family_Income = Remove_zeroes("FFIEC_Median_Family_Income",final_merged_data)
final_merged_data$Number_of_Owner_Occupied_Units = Remove_zeroes("Number_of_Owner_Occupied_Units",final_merged_data)
#final_merged_data$Respondent_ID = Remove_zeroes("Respondent_ID",final_merged_data)
#terminating respondent_id doesn't make any changes. Moreover, as per the data it should be a 10-digit number

# Missing values by each column
Missing_val = data.frame(apply(final_merged_data,2,function(x){sum(is.na(x))}))
Missing_val$Column = row.names(Missing_val)
names(Missing_val)[1] = "Missing_Count"
Missing_val$Missing_Count = ((Missing_val$Missing_Count) * 100)/nrow(final_merged_data)
View(Missing_val)
require(ggplot2)
ggplot(data = Missing_val, aes(x= Column, y = Missing_Count)) + geom_bar(stat = "identity",fill = "blue")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ ggtitle("Percentage of Missing Values in the expanded dataset")
# Above is the percenatge of Missing values chart for every variable


##Descriptive statistics##
######## Univariate analysis####
summary(final_merged_data)

# Loan_Amount_000
hist(final_merged_data$Loan_Amount_000)
# Hitstogram shows the data distribution is right skewed. 
summary(final_merged_data$Loan_Amount_000)
boxplot(final_merged_data$Loan_Amount_000, main="boxplot showing range of loan amount given by the institution")
#Above boxplot is not clear where the average Loan amount lies, so by taking subset of data its easy to analyze
par(mfrow=c(1,2))
boxplot(final_merged_data[final_merged_data$Loan_Amount_000 < 700 ,]$Loan_Amount_000, main="Loan Amount < 700 in $000's")
boxplot(final_merged_data[final_merged_data$Loan_Amount_000 > 700 ,]$Loan_Amount_000, main="Loan Amount > 700 in $000's")
#From above boxplot,since the median of Loan Amount lies around 200 and the 75th quartile around 350 and there are outliers above 700. 
#So studying data points > 700 gives some insights by studying this susbset of data.

# Applicant_Income_000
par(mfrow=c(1,1))
summary(final_merged_data$Applicant_Income_000)
boxplot(final_merged_data$Applicant_Income_000, main="boxplot showing range of applicant income ")
# Comparisison of Applicant_Income_000 < 500 and > 500 in $000's
par(mfrow=c(1,2))
boxplot(final_merged_data[final_merged_data$Applicant_Income_000 < 500 ,]$Applicant_Income_000, main="Applicant Income < 500 in $000's")
boxplot(final_merged_data[final_merged_data$Applicant_Income_000 > 500 ,]$Applicant_Income_000, main="Applicant Income > 500 in $000's")
#From above boxplot, since the median of Applicant Income lies around 100 and the 75th quartile around 150 and there are outliers above 270. 
#So studying data points > 270 gives some insights by studying this susbset of data.

#Conforming_Limit_000
par(mfrow=c(1,1))
summary(final_merged_data$Conforming_Limit_000)
boxplot(final_merged_data$Conforming_Limit_000,main="boxplot showing range of confirming limit ")
# Above boxplot shows, the data distribution of Conforming_Limit_000 is little left skewed and median lies around 530 

#Finding Correlation between variables
# Correlation between Income and Loan Amount by each state
cor(final_merged_data[final_merged_data$State == "VA",]$Applicant_Income_000,
    final_merged_data[final_merged_data$State == "VA",]$Loan_Amount_000,use = "pairwise.complete.obs") 

Corr_data = ddply(final_merged_data,.(State),function(x){cor(x$Applicant_Income_000,x$Loan_Amount_000,use = "pairwise.complete.obs" )})

# Bar chart of Correlation between Applicant Income and Loan Amount
ggplot(data = Corr_data, aes(x= State, y = Corr_data$V1)) + geom_bar(stat="identity", aes(fill=factor(State)),position = "dodge") + 
  labs(x="State",y = " Correlation between Applicant Income and Loan Amount") + scale_y_continuous(labels = comma)+
  ggtitle("Analysis of correlation for every state")
# From the above chart, VA has second least correlation between Applicant Income and loan amount.


## EXPLORATORY DATA ANALYSIS #######

######## Bivariate analysis ####

# Relation between Applicant Income and Loan Amount
ggplot(final_merged_data,aes(x=Applicant_Income_000,y = Loan_Amount_000)) + geom_point()+ ggtitle("Loan Amount vs Applicant Income scatter plot ($000's)")
#From the above plot, majority of Loan amount is given to the applicant with income is just greater than 0, i.e. around 100($000's) 
#ggplot(final_merged_data,aes(x=Applicant_Income_000,y = Loan_Amount_000)) + 
#geom_point(aes(size = Number_of_Owner_Occupied_Units))+ theme_bw()


# Applicant Income by each state
qplot(State,Applicant_Income_000, data = final_merged_data, color = State, geom = "boxplot", ylim = c(1,1500),
      ylab = 'Applicant Income in $000s') + ggtitle("Boxplot showing Applicant Income VS State")
# From the above boxplot, Average Applicant's income was greater for State "DC" from rest of the states which is around 125k$. 
#States WV, DE have lower average income levels than MD, VA which have greater average applicant income and are almost same and there are many outliers(High Income groups)


# Loan_Amount by each state
qplot(State,final_merged_data$Loan_Amount_000, data = final_merged_data, color = State, geom = "boxplot", ylim = c(1,1500),
      ylab = 'Loan_Amount_000 in $000s')
# Above Boxplot shows the Loan amount given for every state. 
#Similar to Applicant Income, "DC" state has highest average loan amount distributed.  

##Conventional_Conforming_Flag VS Loan amount (Then Conforming and conventional deep dive) 

ggplot(data = final_merged_data, aes(x= reorder(Conventional_Conforming_Flag,As_of_Year), y = Loan_Amount_000)) + 
  geom_bar(stat="identity", aes(fill=factor(As_of_Year)),position = "dodge") +
  ggtitle("Conventional_Conforming_Flag VS Loan Amount")
# Above plot shows, majority of the loan amount invloves either non-conventional or non-confirming 

# Conventional Status VS Loan Amount

ggplot(data = final_merged_data, aes(x= reorder(Conventional_Status,As_of_Year), y = Loan_Amount_000)) + 
  geom_bar(stat="identity", aes(fill=factor(As_of_Year)),position = "dodge")+
  ggtitle("Conventional Status VS Loan Amount")
  # Above plot shows, The loan amount given was much higher with conventional i.e. not involved in any government program.

# Conforming status vs Loan Amount 
ggplot(data = final_merged_data, aes(x= reorder(Conforming_Status,As_of_Year), y = Loan_Amount_000)) + 
  geom_bar(stat="identity", aes(fill=factor(As_of_Year)),position = "dodge")+
  ggtitle("Conforming Status VS Loan Amount")

#Total Number of Loans vs State by each year
TotalLoans <- ddply(final_merged_data,c("State","As_of_Year"),function(x) c(num_loans = sum(nrow(x)),sum_loan_amt = sum(x$Loan_Amount_000)))
ggplot(data = TotalLoans, aes(x= reorder(State,As_of_Year), y = num_loans)) + geom_bar(stat="identity", aes(fill=factor(As_of_Year)),position = "dodge") + 
  labs(x="State",y = "Total number of loans in each year") + scale_y_continuous(labels = comma)+
  ggtitle("Total number of Loans given for all the states")

#Loan amounts per year for each state
ggplot(data=aggregate(final_merged_data$Loan_Amount_000, by=list(Year=final_merged_data$As_of_Year,State=final_merged_data$State),FUN =sum),
       aes(x=factor(Year),y=x,group=State))+
  geom_point(aes(colour=State,size=3))+
  geom_line(aes(colour=State,size=2))+
  geom_text_repel(aes(label=x))+
  ggtitle("Loan amounts per year for each state")+
  labs(x="Year",y="Total loan amount")+
  guides(size=FALSE)
# Above graph shows the total loan amount given each state for every year. 
# From above hraph we can observe that VA has given highest loan amount among all other states 
#on the other side DE was the lowest. There is a trend that the loan amount provided by each state got decreasing evry year 


#Mean Loan amount vs State by each year
avg_amountbystate <- ddply(final_merged_data,c("State","As_of_Year"),summarize, avg_LoanAmt = mean(Loan_Amount_000))
ggplot(data = avg_amountbystate, aes(x= reorder(State,As_of_Year), y = avg_LoanAmt)) + geom_bar(stat="identity", aes(fill=factor(As_of_Year)),position = "dodge") + 
  labs(x="State",y = "Average Loan Amount in each year") + scale_y_continuous(labels = comma)+
  ggtitle("Average Loan Amount given for all the states")

# Number of first and subordiate lien loans for each year 
Lien_Statusfn <- ddply(final_merged_data,c("State","Lien_Status_Description","As_of_Year"), function (x) c(NumofLoans = nrow(x)))
ggplot(data=Lien_Statusfn, aes(x=reorder(As_of_Year,Lien_Status_Description), y = NumofLoans)) + 
  geom_bar(stat = "identity", aes(fill=Lien_Status_Description), position = "dodge") +
  scale_y_continuous(labels = comma) + labs(x="Year", y="Number of Loans")
# From the above we can observe the number of least risk loans are decreasing. i.e. Increase of risk factor
# Subordinate loans i.e. High risk loans remained almost constant throughout these years.

# NUmber of Coventioanl& non-conventional loans for every State

Totalconv <- ddply(final_merged_data,c("State","Conventional_Status"),function(x) c(num_loans = sum(nrow(x))))
ggplot(data = Totalconv, aes(x= reorder(State,Conventional_Status), y = Totalconv$num_loans)) + geom_bar(stat="identity", aes(fill=factor(Conventional_Status)),position = "dodge") + 
  labs(x="State",y = "Total number of loans for each state") + scale_y_continuous(labels = comma)+
  ggtitle("Total number of Loans given for all the states")
# From the above plot, the number of Conventional and non-conventional loans are high for VA state.
#Overall, the total number of coventional loans are given higher than non-conventiional loans in every state.
# MD & VA states gave Major contribution for both type of loans

## TASK 4 : Cluster Analysis

d1 <- final_merged_data[c(2,7,19)]
Clust <- na.omit(d1)
cstate <- na.omit(d1)
View(cstate)
Clust$State <- NULL

#View(Clust)
#sum(is.na(Clust))
Clust <- scale(Clust)
View(Clust)
#View(clusterData)
# Determine number of clusters
wss <- (nrow(Clust)-1)*sum(apply(Clust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Clust,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# from above we can conclude that we can cluster the data into 4 clusters
# K-Means Cluster Analysis
fit <- kmeans(Clust, 4) # 4 cluster solution
# get cluster means 

aggregate(Clust,by=list(fit$cluster),FUN=mean)
# append cluster assignment
Clust <- data.frame(Clust, fit$cluster)
View(Clust)
Clust <- data.frame(Clust, cstate$State)
View(Clust)
#
table(fit$cluster, cstate$State)

