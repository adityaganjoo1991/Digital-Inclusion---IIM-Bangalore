#PCA_Village/Ward. Removed Main headers and only kept subheaders because there were three line headers 

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


#write file into csv
write.csv(Adilabad_PCA2,file="Adilabad_Reduced_PCA.csv",row.names=FALSE)

