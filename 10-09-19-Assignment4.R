#tapply & merge

#Using the tapply function, find the mean of 'parcel.density.m3' 
#for each transect and assign the outcome to an object

M<-tapply(fish$parcel.density.m3,fish$transect.id, mean)
M

#Convert the object to a data frame

df_M<- as.data.frame(M)
df_M

#Rename the column with the density values to something more descriptive

colnames(df_M) = "mean density"  
df_M

#Assign the row names of the data frame to be the values 
#in a new field "transect"

transect=rownames(df_M)
rownames(df_M)=NULL
M1_df=cbind(transect,df_M)
M1_df

#Repeat the above steps, but this time using the tapply function
#to find the standard deviation of 'parcel.density.m3'

D<-tapply(fish$parcel.density.m3,fish$transect.id, sd)
D

df_D<- as.data.frame(D)
df_D

colnames(df_D) = "sd density"  
df_D

transect=rownames(df_D)
rownames(df_D)=NULL
D1_df=cbind(transect,df_D)
D1_df

#Using the merge function, combine the data frames with the mean 
#and standard deviation to create one, new data frame that has three columns (mean density, sd density, transect)

P1_df<-merge(x=M1_df, y=D1_df, by = "transect")
P1_df

#Repeat the above steps, but this time using the tapply function to find the count of observations for each transect for 'parcel.density.m3''

C<-tapply(fish$parcel.density.m3,fish$transect.id, sum)
C

df_C<- as.data.frame(C)
df_C

colnames(df_C)="count"
df_C

transect=rownames(df_C)
rownames(df_C)=NULL
C1_df=cbind(transect,df_C)
C1_df

#Using the merge function, combine the data frames with the mean and standard deviation to create
#one, new data frame that has three columns (mean density, sd density, count, and transect).

P2_df<-merge(x=P1_df, y=C1_df, by = "transect")
P2_df

#Summarize & join

#Using the group_by and summarise functions (tidyverse package), 
#find the mean of 'parcel.density.m3'for each transect and assign the outcome to an object.

library(tidyverse)
fish %>% group_by(transect.id) %>%
  summarize(parcel.density.m3 = mean(parcel.density.m3, na.rm = TRUE))
MP<- fish %>% group_by(transect.id) %>%
  summarize(parcel.density.m3 = mean(parcel.density.m3, na.rm = TRUE))
MP

#Convert the object to a data frame

df_MP<-as.data.frame(MP)
df_MP

#Rename the column with the density values to something more descriptive

names(df_MP)[names(df_MP) == "parcel.density.m3"] <- "mean density"  
df_MP

#Assign the row names of the data frame to be the values in a 
#new field "transect"

colnames(df_MP)[1]="transect"
df_MP

#Repeat the above steps, but this time using the tapply function 
#to find the standard deviation of'parcel.density.m3'

DP<-fish %>% group_by(transect.id) %>%
  summarize(parcel.density.m3 = sd(parcel.density.m3, na.rm = TRUE))
DP

df_DP<-as.data.frame(DP)
df_DP

names(df_DP)[names(df_DP) == "parcel.density.m3"] <- "sd density"  
df_DP

colnames(df_DP)[1]="transect"
df_DP

