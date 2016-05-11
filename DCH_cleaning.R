#DCH village amenities exploration
#convert file to csv 

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


#Combine all school subtypes 
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

