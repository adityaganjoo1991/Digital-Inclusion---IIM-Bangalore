#Aditya Ganjoo, Pratyay Bhattacharjee, Shantala Daddi 



#DCH Cleaning and Feature Engineering

DCH=read.csv("DCH.csv")
str(DCH)
summary(DCH)

#filter column names containing "Status" 
DCH_status = DCH[,grepl("Status",names(DCH))]
str(DCH_status) #142 variables. 
#We want to remove all the status variables for schools and colleges as we want to sum up the numbers for the different subcategories 
which(colnames(DCH_status)=="Private.Others..Status.A.1..NA.2..") #All variables upto this point are related to school/college statuses. We want to remove these, so finding the column index
DCH_status2=DCH_status[,1:43] 
colnames_status=names(DCH_status2)
DCH2= DCH[,!names(DCH) %in% colnames_status]

#Remove all columns containing following in their column names
terms=c("Population","If.not.","Name","CD.Block","Gram.Panchayat","Reference.Year","Practitioner","Within.the.State","Outside.the.State","Land","Commodities","Doctors","Staff","Prctitioner","Patient","per.day","Tap.Water","Well","Pump","Spring","Canal","Pond","Drain","Sanitary","Toilet")
DCH_unwanted= DCH2[,grepl(paste(terms,collapse= "|"),names(DCH2))]
colnames_unwanted= names(DCH_unwanted)
DCH3= DCH2[,!names(DCH2) %in% colnames_unwanted]
str(DCH3)
#124 variables left


#Replace all NAs in the dataframe with 0s
DCH3[is.na(DCH3)]=0


#Combine all school subtypes. This will be considered numeric and as an indicator of number of different schools in the village
DCH3$Schools= DCH3[,10]
for (i in 11:19)
{
DCH3$Schools= DCH3$Schools+DCH3[,i]
}

DCH3=DCH3[,-c(10:19)]



#Combine all College/institution subtypes
DCH3$Colleges=DCH3[,10] #first in the list
for (i in 11:27)
{
DCH3$Colleges= DCH3$Colleges+DCH3[,i]
}
DCH3=DCH3[,-c(10:27)]


#Combine all medical center subtypes
DCH3$Medical=DCH3[,10] #first in the list
for (i in 11:23)
{
DCH3$Medical= DCH3$Medical+DCH3[,i]
}
DCH3=DCH3[,-c(10:23)]

#removing some useless columns
DCH3=DCH3[,-c(10:12)]
DCH3=DCH3[,-c(71:78)]



#Combine Transportation.First, We'll have to replace 2's with 0 in the variables as these variables indicate statuses, with 1 meaning the service is present, and 2 meaning it is not
DCH4=DCH3
#making sure all variables are numeric so that they can be summed up
for (i in 24:43)
{
DCH4[,i]=as.numeric(DCH4[,i])
}
DCH4[,24:43][DCH4[,24:43]==2]=0  #converted all '2' values to 0 (2 stood for NA)
DCH4$Transport=DCH4[,24] #first in the list
for (i in 25:43)
{
DCH4$Transport= DCH4$Transport+DCH4[,i]
}
DCH4=DCH4[,-c(24:43)]



#Same process for nutritional centers

#making sure all variables are numeric so that they can be summed up
for (i in 31:36)
{
DCH4[,i]=as.numeric(DCH4[,i])
}
DCH4[,31:36][DCH4[,31:36]==2]=0  #converted all '2' values to 0 (2 stood for NA)
DCH4$Nutritional_Centres=DCH4[,31] #first in the list
for (i in 32:36)
{
DCH4$Nutritional_Centres= DCH4$Nutritional_Centres+DCH4[,i]
}
DCH4=DCH4[,-c(31:36)]

#50 Variables remaining

write.csv(DCH4,file="DCH_Reduced.csv",row.names=FALSE)



########################################################################3
#PCA_Village/Ward cleaning and feature engineering

#Removed Main headers manually and only kept subheaders because there were three line headers. Also removed some variables manually

Adilabad_PCA=read.csv("Adilabad.csv")
#105 variables

#Replacing NAs with 0
Adilabad_PCA[is.na(Adilabad_PCA)]=0

#Removing some variables 
Adilabad_PCA[,11]=NULL


#Combine all non exclusive rooms so that we can have room vs no room
Adilabad_PCA$exclusive.rooms=Adilabad_PCA[,15] #first in the list
for (i in 16:20)
{
Adilabad_PCA$exclusive.rooms= Adilabad_PCA$exclusive.rooms+Adilabad_PCA[,i]
}
Adilabad_PCA=Adilabad_PCA[,-c(15:20)]


#Combining households which are good and livable into one variable called livable. So end result will be livable vs dilapidated
Adilabad_PCA$livable_households=Adilabad_PCA[,11]+ Adilabad_PCA[,12] #first in the list
Adilabad_PCA=Adilabad_PCA[,-c(11,12)]
Adilabad_PCA$Dilapidated_households= Adilabad_PCA$Dilapidated
Adilabad_PCA$Dilapidated=NULL

#Combining household size fields into two major fields: Household size<=4, Household size>4
Adilabad_PCA$Household_Size_till4=Adilabad_PCA[,12] #first in the list
for (i in 13:15)
{
Adilabad_PCA$Household_Size_till4= Adilabad_PCA$Household_Size_till4+Adilabad_PCA[,i]
}
Adilabad_PCA=Adilabad_PCA[,-c(12:15)]


Adilabad_PCA$Household_Size_morethan4=Adilabad_PCA[,12] #first in the list
for (i in 13:14)
{
Adilabad_PCA$Household_Size_morethan4= Adilabad_PCA$Household_Size_morethan4+Adilabad_PCA[,i]
}
Adilabad_PCA=Adilabad_PCA[,-c(12:14)]


#Removing water sources
Adilabad_PCA2=Adilabad_PCA[,-c(12:21)]

#combining water within premises/near premises for comparision against water away
Adilabad_PCA2$Water_premises= Adilabad_PCA2[,12]+Adilabad_PCA2[,13]
Adilabad_PCA2$Water_Away= Adilabad_PCA2[,14]
Adilabad_PCA2=Adilabad_PCA2[,-c(12:14)]
str(Adilabad_PCA2)

#Combining all lighting sources into one variable called lighting, so that we can compare just lighting vs no lighting
12:16
Adilabad_PCA2$Lighting=Adilabad_PCA2[,12] #first in the list
for (i in 13:16)
{
Adilabad_PCA2$Lighting= Adilabad_PCA2$Lighting+Adilabad_PCA2[,i]
}
Adilabad_PCA2=Adilabad_PCA2[,-c(12:16)]


#subcategories under latrine within premises
Adilabad_PCA2=Adilabad_PCA2[,-c(14:21)]

#Combine public latrine, not having latrine, and open into No latrine
Adilabad_PCA2$No_Latrine= Adilabad_PCA2[,14]+Adilabad_PCA2[,15]+Adilabad_PCA2[,16]
Adilabad_PCA2=Adilabad_PCA2[,-c(14:16)]


#Combining bathroom and enclosure without roof to compare against no bathroom
Adilabad_PCA2$Bathroom= Adilabad_PCA2[,14]+Adilabad_PCA2[,15]
Adilabad_PCA2=Adilabad_PCA2[,-c(14:15)]
Adilabad_PCA2$No.Bathroom=Adilabad_PCA2$No

#combining close and open drainage for comparision with no drainage
Adilabad_PCA2$Drainage= Adilabad_PCA2[,15]+Adilabad_PCA2[,16]
Adilabad_PCA2=Adilabad_PCA2[,-c(15:16)]

#Readded electricity, households in the end as Q&A session with project faculty


#write file into csv
write.csv(Adilabad_PCA2,file="Adilabad_Reduced_PCA.csv",row.names=FALSE)


###################################################################
#Population Census Data. Removed All subcategories of Marginal and Main worker manually. Also removed population under the age of 6. Did this in excel

###################################################################

#Merging the data 
#reading the three datasets
Adilabad_PCA= read.csv("Adilabad_Reduced_PCA.csv")
DCH= read.csv("DCH_Reduced.csv")
Pop_cens= read.csv("Adilabad_Population.csv")

#Removing village code =0 from Adilabad_PCA as we're looking only at village level. village code =0 at Tehsil level
AdilabadVillage=Adilabad_PCA[Adilabad_PCA$Town.Code.Village.code!=0,]

#Merging AdilabadVillage and DCH
DCHAdilabad <- merge(DCH,AdilabadVillage, by.x=c("State.Code","District.Code","Sub.District.Code","Village.Code"), by.y=c("State.Code","District.Code","Tehsil.Code","Town.Code.Village.code"))

#Merging with Population Census Data
Final<- merge(DCHAdilabad,Pop_cens,by.x=c("State.Code","District.Code","Sub.District.Code","Village.Code"),by.y=c("State","District","Subdistt","Town.Village"))


write.csv(data=Final,file="Final.csv",row.names=FALSE)




#############################################################

#Cleaning,feature engineering, and running models on final merged dataset

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

write.csv(Finaln3,"Final_with_factors_3.csv",row.names=FALSE)

###CASE1:TARGET- With.Internet_Computer 
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


####CASE 2: TARGET VARIABLE: Mobile.only 

#Creating 2nd model using Mobile.only as target variable, as it is indicator of digital inclusion
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





####CASE3:TARGET VARIABLE: Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..

#Creating 3rd model using Internet.Cafes...Common.Service.Centre..CSC...Status.A.1..NA.2..as target variable, as it is indicator of digital inclusion
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


 



####CASE4:TARGET VARIABLE: Mobile.Phone.Coverage..Status.A.1..NA.2.. 

#Creating 4th model using Mobile.Phone.Coverage..Status.A.1..NA.2.. as target variable, as it is indicator of digital inclusion
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




#Top25 variable list for different cases mentioned above as follows 
#Final_Variables_For_netcomp

#Final_Variables_For_mobileonly

#Final_Variables_For_landlineonly

#Final_Variables_For_netcafe

#Final_Variables_mobile_coverage



#Putting them into a dataset for side by side comparision
Computers_net_with_cor=Final_Variables_For_netcomp

Mobileonly_with_cor=Final_Variables_For_mobileonly

Landlineonly_with_cor=Final_Variables_For_landlineonly

netcafe_with_cor=Final_Variables_For_netcafe

mobile_coverage_with_cor=Final_Variables_mobile_coverage


Top25Data= data.frame(Computers_net_with_cor,Mobileonly_with_cor,netcafe_with_cor,mobile_coverage_with_cor)
write.csv(Top25Data,"Top25withfactors.csv",row.names=FALSE)


#########################################################

#Above analysis was done only for one district. We later combined all districts by placing their csv files in one folder and running the following code
mydataPCA=ldply(list.files(pattern="csv"),function(filename) { 
  dum=read.csv(filename) 
    dum$filename=filename 
     return(dum) 
 })


#We then created separate data sets for each of the TARGET variables, using the 25 most important variables for each one of the target variables

##########################################################
# Applied correlation to find out the correlated variables
# Here, we've shown only Mobile_Only and its 25 most important variables. But this was done for the rest of the TARGET variables as well

Data <- read.csv("MobileOnlyAP.csv")
attach(Data)
cor <- cor(Data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)])
cor_Computer_Internet <- write.csv(cor, "cor_Top25factors.csv")

##########################################################
# Applied the clustering on the variables 

data <- read.csv("MobileOnlyAP.csv")
mydata <- data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)]
# Determine number of clusters

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster) 
cluster_data <- write.csv(mydata, "cluster_data.csv")

###############################################################################################################
# Applied other models to find the most important variables out of the top 25 variables for the target variable 

mob_only <- read.csv("MobileOnlyAP.csv")
dim(mob_only)
str(mob_only)
summary(mob_only)
attach(mob_only)
mob_only[,prop.table(table(Mobile.only))]
colSums(is.na(mob_only))
# install.packages("ggplot2")
library("ggplot2")
ggplot(mob_only, aes(Mobile.only, fill = None.of.the.assets.specified.in.col..10.to.19)) + geom_bar()

# install.packages("h2o") - installed h2o package
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
mob_only.h2o <- as.h2o(mob_only)
colnames(mob_only.h2o)
#dependent variable (Purchase)
y.dep <- 15
#independent variables (dropping ID variables)
x.indep <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28)

# Multiple Regression in H2O
regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = mob_only.h2o, family = "gaussian")
h2o.performance(regression.model)
# check importance of variables
h2o.varimp(regression.model)

# Random Forest
system.time(rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = mob_only.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122))
h2o.performance(rforest.model)
# check variable importance
h2o.varimp(rforest.model)

# GBM Model
system.time(
  gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = mob_only.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
)
h2o.performance (gbm.model)
# check variable importance
h2o.varimp(gbm.model)

# deep learning models
system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = mob_only.h2o,
                                      epoch = 60,
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)
# check performance
h2o.performance(dlearning.model)

## Using these models we find the top 5 most important variables out of the top 25 variables 
## Next we do the visualizations using these important variables for each target variable in Tableau
