
#important things. Dataset with correlated: Finaln3. dataset without:Finaln5
#Reading in new merged data set
Final=read.csv("Final.csv")

#finding correlation between direct indicators of digital inclusion (Mobile,landline,computer with/without internet etc)
cor(Final[81:92])
#Did not find anything significant

#using only numeric data
Finaln=Final[sapply(Final,is.numeric)]
Final_Factors=Final[sapply(Final,is.factor)] #storing the removed factor variables

#Searching for constant variables and removing them
names(Finaln[, sapply(Finaln, function(v) var(v, na.rm=TRUE)==0)]) #displays features with constant values
Finaln2= Finaln[,sapply(Finaln, function(v) var(v, na.rm=TRUE)!=0)] #removes features with constant values. Total 9 removed

#There was 1 NA value. Substituting it to 0
Finaln2[is.na(Finaln2)]=0


#Removing identical features but keeping one copy in the dataset
features_pair <- combn(names(Finaln2), 2, simplify = F) #creates all possible combinations of the features, 2 at a time, and generates a list
toRemove <- c()
for(pair in features_pair) {
f1 <- pair[1]
f2 <- pair[2]
if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
if (all(Finaln2[[f1]]==Finaln2[[f2]])) {
cat(f1, "and", f2, "are identical \n")
toRemove <- c(toRemove,f2) 
   }
  }
 }
`%ni%` <- Negate(`%in%`)
Finaln3<- subset(Finaln2,select = names(Finaln2) %ni% toRemove)  #8 Variables removed

write.csv(Finaln3,"Final_without_identicalorconstant_variables.csv",row.names=FALSE)


library(party)
library(caret)
library(randomForest)

Finaln3$PIN.Code=NULL
Finaln3=Finaln3[!Finaln3$Total...Households==0,]

#Converting some of the variables to factors. these were orignally factors but were read in as numeric by R
Finaln3[,8:36]=sapply(Finaln3[,8:36],factor)

###CASE1:TARGET- With.Internet_Computer with highly correlated variables###

#Creating 1st model using With.Internet_Computer as target variable, as it is indicator of digital inclusion
set.seed(100)
model <- randomForest(With.Internet_Computer~. , data= Finaln3, importance=TRUE)# fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model) 

#Adding the variable names to the importance data frame
Finaln4=Finaln3
Finaln4$With.Internet_Computer=NULL 
importance$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importance[order(-importance$Overall),]

#keeping the top 25 most important variables for TARGET internet with computer
Final_Variables_For_netcomp=sortedimp$Variable[1:25]
str(Final_Variables_For_netcomp)


####CASE2:TARGET- With.Internet_Computer without highly correlated variables###
#Removing Highly correlated features but keeping one copy in the dataset
features_pair <- combn(names(Finaln3), 2, simplify = F) #creates all possible combinations of the features, 2 at a time, and generates a list
toRemove <- c()
for(pair in features_pair) {
f1 <- pair[1]
f2 <- pair[2]
if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
if (cor(Finaln3[[f1]],Finaln3[[f2]])>=.9||cor(Finaln3[[f1]],Finaln3[[f2]])<=-.9) {
cat(f1, "and", f2, "are highly correlated \n")
toRemove <- c(toRemove,f2) 
   }
  }
 }
`%ni%` <- Negate(`%in%`)
Finaln5<- subset(Finaln3,select = names(Finaln3) %ni% toRemove)  # 38 Variables removed. 82 variables left
Finaln5$Village.Code=Finaln3$Village.Code
Finaln5$Sub.District.Code =NULL

#Creating model on this
#Creating 2nd model using With.Internet_Computer as target variable, as it is indicator of digital inclusion
set.seed(100)
model2 <- randomForest(With.Internet_Computer~. , data= Finaln5, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model2) 

#Adding the variable names to the importance data frame
Finaln4=Finaln5
Finaln4$With.Internet_Computer=NULL 
importance$Variable=names(Finaln4)
Finaln4$With.Internet_Computer=Finaln5$With.Internet_Computer

#Sorting the variables based on decreasing order of importance
sortedimp=importance[order(-importance$Overall),]

#keeping the top 25 most important variables for TARGET internet with computer
Final_Variables_For_NetComp_Cor=sortedimp$Variable[1:25]
str(Final_Variables_For_NetComp_Cor)



####CASE 3: TARGET VARIABLE: Mobile.only and with correlated


#Creating 3rd model using Mobile.only as target variable, as it is indicator of digital inclusion
set.seed(100)
model3 <- randomForest(Mobile.only~. , data= Finaln3, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model3) 

#Adding the variable names to the importance data frame
Finaln4=Finaln3
Finaln4$Mobile.only=NULL 
importance$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importance[order(-importance$Overall),]

#keeping the top 25 most important variables for TARGET internet with computer
Final_Variables_For_mobileonly=sortedimp$Variable[1:25]
Final_Variables_For_mobileonly



####CASE4:TARGET VARIABLE: Mobile.only and without correlated

#Creating 4th model using Mobile.only as target variable, as it is indicator of digital inclusion
set.seed(100)
model4 <- randomForest(Mobile.only~. , data= Finaln5, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model4) 

#Adding the variable names to the importance data frame
Finaln4=Finaln5
Finaln4$Mobile.only=NULL 
importance$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importance[order(-importance$Overall),]

#keeping the top 25 most important variables for TARGET 
Final_Variables_For_mobileonly_cor=sortedimp$Variable[1:25]
Final_Variables_For_mobileonly_cor



####CASE5:TARGET VARIABLE: Landline.only and with correlated

#Creating 5th model using Landline.only as target variable, as it is indicator of digital inclusion
set.seed(100)
model5<- randomForest(Landline.only~. , data= Finaln3, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model5) 

#Adding the variable names to the importance data frame
Finaln4=Finaln3
Finaln4$Landline.only=NULL 
importance$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importance[order(-importance$Overall),]

#keeping the top 25 most important variables for TARGET 
Final_Variables_For_landlineonly=sortedimp$Variable[1:25]
Final_Variables_For_landlineonly



####CASE6:TARGET VARIABLE: Landline.only and without correlated

#Creating 6th model using Landline.only as target variable, as it is indicator of digital inclusion
set.seed(100)
model6<- randomForest(Landline.only~. , data= Finaln5, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model6) 

#Adding the variable names to the importance data frame
Finaln4=Finaln5
Finaln4$Landline.only=NULL 
importance$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importance[order(-importance$Overall),]

#keeping the top 25 most important variables for TARGET 
Final_Variables_For_landlineonly_cor=sortedimp$Variable[1:25]
Final_Variables_For_landlineonly_cor





####CASE7:TARGET VARIABLE: Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..and with correlated

#Creating 7th model using Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..as target variable, as it is indicator of digital inclusion
set.seed(100)
model7<- randomForest(as.factor(Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..)~. , data= Finaln3, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model7) 
importancee=importance[1]

#Adding the variable names to the importance data frame
Finaln4=Finaln3
Finaln4$Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..=NULL 
importancee$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importancee[order(-importancee[,1]),]

#keeping the top 25 most important variables for TARGET 
Final_Variables_For_netcafe=sortedimp$Variable[1:25]
Final_Variables_For_netcafe


####CASE8:TARGET VARIABLE: Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..and without correlated

#Creating 8th model using Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..as target variable, as it is indicator of digital inclusion
set.seed(100)
model8<- randomForest(as.factor(Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..)~. , data= Finaln5, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model8) 
importancee=importance[1]

#Adding the variable names to the importance data frame
Finaln4=Finaln5
Finaln4$Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..=NULL
importancee$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importancee[order(-importancee[,1]),]


#keeping the top 25 most important variables for TARGET 
Final_Variables_For_netcafe_cor=sortedimp$Variable[1:25]
Final_Variables_For_netcafe_cor


 



####CASE9:TARGET VARIABLE: Mobile.Phone.Coverage..Status.A.1..NA.2.. and with correlated

#Creating 9th model using Mobile.Phone.Coverage..Status.A.1..NA.2.. as target variable, as it is indicator of digital inclusion
set.seed(100)
model9<- randomForest(as.factor(Mobile.Phone.Coverage..Status.A.1..NA.2..)~. , data= Finaln3, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model9) 
importancee=importance[1]

#Adding the variable names to the importance data frame
Finaln4=Finaln3
Finaln4$Mobile.Phone.Coverage..Status.A.1..NA.2..=NULL 
importancee$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importancee[order(-importancee[,1]),]


#keeping the top 25 most important variables for TARGET 
Final_Variables_mobile_coverage=sortedimp$Variable[1:25]
Final_Variables_mobile_coverage


####CASE10:TARGET VARIABLE: Mobile.Phone.Coverage..Status.A.1..NA.2.. and without correlated

#Creating 10th model using Mobile.Phone.Coverage..Status.A.1..NA.2.. as target variable, as it is indicator of digital inclusion
set.seed(100)
model10<- randomForest(as.factor(Mobile.Phone.Coverage..Status.A.1..NA.2..)~. , data= Finaln5, importance=TRUE) # fit the random forest

#Storing the overall importance measure for each variable
importance=varImp(model10) 
importancee=importance[1]

#Adding the variable names to the importance data frame
Finaln4=Finaln5
Finaln4$Mobile.Phone.Coverage..Status.A.1..NA.2..=NULL 
importancee$Variable=names(Finaln4)

#Sorting the variables based on decreasing order of importance
sortedimp=importancee[order(-importancee[,1]),]


#keeping the top 25 most important variables for TARGET 
Final_Variables_mobile_coverage_cor=sortedimp$Variable[1:25]
Final_Variables_mobile_coverage_cor




#Top25 variable list for different cases mentioned above as follows 
#Final_Variables_For_netcomp
#Final_Variables_For_NetComp_Cor
#Final_Variables_For_mobileonly
#Final_Variables_For_mobileonly_cor
#Final_Variables_For_landlineonly
#Final_Variables_For_landlineonly_cor
#Final_Variables_For_netcafe
#Final_Variables_For_netcafe_cor
#Final_Variables_mobile_coverage
#Final_Variables_mobile_coverage_cor


#Putting them into a dataset for side by side comparision
Computers_net_with_cor=Final_Variables_For_netcomp
Computers_net_no_cor=Final_Variables_For_NetComp_Cor
Mobileonly_with_cor=Final_Variables_For_mobileonly
Mobileonly_no_cor=Final_Variables_For_mobileonly_cor
Landlineonly_with_cor=Final_Variables_For_landlineonly
Landlineonly_no_cor=Final_Variables_For_landlineonly_cor
netcafe_with_cor=Final_Variables_For_netcafe
netcafe_no_cor=Final_Variables_For_netcafe_cor
mobile_coverage_with_cor=Final_Variables_mobile_coverage
mobile_coverage_no_cor=Final_Variables_mobile_coverage_cor

Top25Data= data.frame(Computers_net_with_cor,Computers_net_no_cor,Mobileonly_with_cor,Mobileonly_no_cor,Landlineonly_with_cor,Landlineonly_no_cor,netcafe_with_cor,netcafe_no_cor,mobile_coverage_with_cor,mobile_coverage_no_cor)
