
install.packages('knitr')
install.packages('ggplot2')
install.packages('plyr')
install.packages('dplyr')
install.packages('corrplot')
install.packages('caret')
install.packages('gridExtra')
install.packages('scales')
install.packages('Rmisc')
install.packages('ggrepel')
install.packages('randomForest')
install.packages('psych')
install.packages('xgboost')
install.packages('glmnet')
# 

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(glmnet)






train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)







dim(train)
str(train[,c(1:10, 81)]) #display first 10 variables and the response variable


test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL


test$SalePrice <- NA
alldata <- rbind(train, test)
dim(alldata)




summary(alldata$SalePrice)



##The most important numeric predictors

#The character variables need some work before I can use them. To get a feel for the dataset, I decided to first see which numeric variables have a high correlation with the SalePrice.

###Correlations with SalePrice

#Altogether, there are 10 numeric variables with a correlation of at least 0.5 with SalePrice. Alldata those correlations are positive.

numericVars <- which(sapply(alldata, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

alldata_numVar <- alldata[, numericVars]
cor_numVar <- cor(alldata_numVar, use="pairwise.complete.obs") #correlations of alldata numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


#In the remainder of this section, I will visualize the relation between SalePrice and the two predictors with the highest correlation with SalePrice; Overalldata Quality and the 'Above Grade' Living Area (this is the proportion of the house that is not in a basement; [link](http://www.gimme-shelter.com/above-grade-50066/)).

#It also becomes clear the multicollinearity is an issue. For example: the correlation between GarageCars and GarageArea is very high (0.89), and both have similar (high) correlations with SalePrice. The other 6 six variables with a correlation higher than 0.5 with SalePrice are:
#-TotalBsmtSF: Total square feet of basement area
#-1stFlrSF: First Floor square feet
# -FullBath: Full bathrooms above grade
# -TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
# -YearBuilt: Original construction date
# -YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)


###Overalldata Quality

#Overalldata Quality has the highest correlation with SalePrice among the numeric variables (0.79). It rates the overalldata material and finish of the house on a scale from 1 (very poor) to 10 (very excellent).

ggplot(data=alldata[!is.na(alldata$SalePrice),], aes(x=factor(OveralldataQual), y=SalePrice))+
geom_boxplot(col='blue') + labs(x='Overalldata Quality') +
scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#The positive correlation is certainly there indeed, and seems to be a slightly upward curve. Regarding outliers, I do not see any extreme values. If there is a candidate to take out as an outlier later on, it seems to be the expensive house with grade 4.

###Above Grade (Ground) Living Area (square feet)

#The numeric variable with the second highest correlation with SalesPrice is the Above Grade Living Area. This make a lot of sense; big houses are generalldatay more expensive.

ggplot(data=alldata[!is.na(alldata$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
geom_text_repel(aes(label = ifelse(alldata$GrLivArea[!is.na(alldata$SalePrice)]>4500, rownames(alldata), '')))



#Especialldatay the two houses with realldatay big living areas and low SalePrices seem outliers (houses 524 and 1299, see labels in graph). I will not take them out yet, as taking outliers can be dangerous. For instance, a low score on the Overalldata Quality could explain a low price. However, as you can see below, these two houses actualldatay also score maximum points on Overalldata Quality. Therefore, I will keep houses 1299 and 524 in mind as prime candidates to take out as outliers.

alldata[c(524, 1299), c('SalePrice', 'GrLivArea', 'OveralldataQual')]


#Missing data, label encoding, and factorizing variables

##Completeness of the data

# First of alldata, I would like to see which variables contain missing values.

NAcol <- which(colSums(is.na(alldata)) > 0)
sort(colSums(sapply(alldata[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')


# Of course, the 1459 NAs in SalePrice match the size of the test set perfectly. This means that I have to fix NAs in 34 predictor variables.
# 
# ##Imputing missing data {.tabset}
# 
# In this section, I am going to fix the 34 predictors that contains missing values. I will go through them working my way down from most NAs until I have fixed them alldata. If I stumble upon a variable that actualldatay forms a group with other variables, I will also deal with them as a group. For instance, there are multiple variables that relate to Pool, Garage, and Basement.
# 
# As I want to keep the document as readable as possible, I decided to use the "Tabs" option that knitr provides. You can find a short analysis for each (group of) variables under each Tab. You don't have to go through alldata sections, and can also just have a look at a few tabs. If you do so, I think that especialldatay the Garage and Basement sections are worthwhile, as I have been carefull in determing which houses realldatay do not have a basement or garage.
# 
# Besides making sure that the NAs are taken care off, I have also converted character variables into ordinal integers if there is clear ordinality, or into factors if levels are categories without ordinality. I will convert these factors into numeric later on by using one-hot encoding (using the model.matrix function).

###Pool variables
# 
# **Pool Quality and the PoolArea variable**
#   
#   The PoolQC is the variable with most NAs. The description is as follows:
#   
#   PoolQC: Pool quality
# 
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# NA	No Pool
# 
# So, it is obvious that I need to just assign 'No Pool' to the NAs. Also, the high number of NAs makes sense as normalldatay only a smalldata proportion of houses have a pool.

alldata$PoolQC[is.na(alldata$PoolQC)] <- 'None'

#It is also clear that I can label encode this variable as the values are ordinal. As there a multiple variables that use the same quality levels, I am going to create a vector that I can reuse later on.

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)


#Now, I can use the function 'revalue' to do the work for me.


alldata$PoolQC<-as.integer(revalue(alldata$PoolQC, Qualities))
table(alldata$PoolQC)


# However, there is a second variable that relates to Pools. This is the PoolArea variable (in square feet). As you can see below, there are 3 houses without PoolQC. First, I checked if there was a clear relation between the PoolArea and the PoolQC. As I did not see a clear relation (bigger of smalldataer pools with better PoolQC), I am going to impute PoolQC values based on the Overalldata Quality of the houses (which is not very high for those 3 houses).

alldata[alldata$PoolArea>0 & alldata$PoolQC==0, c('PoolArea', 'PoolQC', 'OveralldataQual')]

alldata$PoolQC[2421] <- 2
alldata$PoolQC[2504] <- 3
alldata$PoolQC[2600] <- 2

# 
# **Please return to the 5.2 Tabs menu to select other (groups of) variables**
#   
#   ###Miscellaneous Feature
#   
#   **Miscellaneous feature not covered in other categories**
#   
#   Within Miscellaneous Feature, there are 2814 NAs. As the values are not ordinal, I will convert MiscFeature into a factor. Values:	
#   
#   Elev	Elevator
# Gar2	2nd Garage (if not described in garage section)
# Othr	Other
# Shed	Shed (over 100 SF)
# TenC	Tennis Court

alldata
# NA	None

alldata$MiscFeature[is.na(alldata$MiscFeature)] <- 'None'
alldata$MiscFeature <- as.factor(alldata$MiscFeature)

ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=MiscFeature, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

table(alldata$MiscFeature)

# When looking at the frequencies, the variable seems irrelevant to me. Having a shed probably means 'no Garage', which would explain the lower sales price for Shed. Also, while it makes a lot of sense that a house with a Tennis court is expensive, there is only one house with a tennis court in the training set.
# 
# **Please return to the 5.2 Tabs menu to select other (groups of) variables**
#   
#   ###Alldataey
#   
#   **Type of alldataey access to property**
#   
#   Within Alldataey, there are 2721 NAs. As the values are not ordinal, I will convert Alldataey into a factor. Values:
#   
#   Grvl	Gravel
# Pave	Paved
# NA 	No alldataey access

alldata$Alley[is.na(alldata$Alley)] <- 'None'
alldata$Alley <- as.factor(alldata$Alley)

ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=Alldataey, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)

table(alldata$Alley)

# **Please return to the 5.2 Tabs menu to select other (groups of) variables**
#   
#   ###Fence
#   
#   **Fence quality**
#   
#   Within Fence, there are 2348 NAs. The values seem to be ordinal. Values:
#   
#   GdPrv	Good Privacy
# MnPrv	Minimum Privacy
# GdWo	Good Wood
# MnWw	Minimum Wood/Wire
# NA	No Fence

alldata$Fence[is.na(alldata$Fence)] <- 'None'
table(alldata$Fence)
alldata[!is.na(alldata$SalePrice),] %>% group_by(Fence) %>% summarise(median = median(SalePrice), counts=n())


#My conclusion is that the values do not seem ordinal (no fence is best). Therefore, I will convert Fence into a factor.

alldata$Fence <- as.factor(alldata$Fence)


# **Please return to the 5.2 Tabs menu to select other (groups of) variables**
#   
#   ###Fireplace variables
#   
#   **Fireplace quality, and Number of fireplaces**
#   
#   Within Fireplace Quality, there are 1420 NAs. Number of fireplaces is complete.
# 
# **Fireplace quality**
#   
#   The number of NAs in FireplaceQu matches the number of houses with 0 fireplaces. This means that I can safely replace the NAs in FireplaceQu with 'no fireplace'. The values are ordinal, and I can use the Qualities vector that I have already created for the Pool Quality. Values:
#   
#   Ex	Excellent - Exceptional Masonry Fireplace
# Gd	Good - Masonry Fireplace in main level
# TA	Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
# Fa	Fair - Prefabricated Fireplace in basement
# Po	Poor - Ben Franklin Stove
# NA	No Fireplace

alldata$FireplaceQu[is.na(alldata$FireplaceQu)] <- 'None'
alldata$FireplaceQu<-as.integer(revalue(alldata$FireplaceQu, Qualities))
table(alldata$FireplaceQu)


#**Number of fireplaces**
  
  # Fireplaces is an integer variable, and there are no missing values.
table(alldata$Fireplaces)
sum(table(alldata$Fireplaces))

# **Please return to the 5.2 Tabs menu to select other (groups of) variables**
  
  ###Lot variables
  
 # 3 variables. One with 1 NA, and 2 complete variables.

#**LotFrontage: Linear feet of street connected to property**
  
 # 486 NAs. The most reasonable imputation seems to take the median per neigborhood.

ggplot(alldata[!is.na(alldata$LotFrontage),], aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (i in 1:nrow(alldata)){
  if(is.na(alldata$LotFrontage[i])){
    alldata$LotFrontage[i] <- as.integer(median(alldata$LotFrontage[alldata$Neighborhood==alldata$Neighborhood[i]], na.rm=TRUE)) 
  }
}


#**LotShape: General shape of property**
  
 # No NAs. Values seem ordinal (Regular=best)

# Reg	Regular	
# IR1	Slightly irregular
# IR2	Moderately Irregular
# IR3	Irregular
alldata$Age


alldata$LotShape<-as.integer(revalue(alldata$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(alldata$LotShape)
sum(table(alldata$LotShape))


# **LotConfig: Lot configuration**
#   
#   No NAs. The values seemed possibly ordinal to me, but the visualization does not show this. Therefore, I will convert the variable into a factor.
# 
# Inside	Inside lot
# Corner	Corner lot
# CulDSac	Cul-de-sac
# FR2	Frontage on 2 sides of property
# FR3	Frontage on 3 sides of property

ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=as.factor(LotConfig), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

alldata$LotConfig <- as.factor(alldata$LotConfig)
table(alldata$LotConfig)
sum(table(alldata$LotConfig))


# **Please return to the 5.2 Tabs menu to select other (groups of) variables**
#   
#   ###Garage variables
#   
#   **Altogether, there are 7 variables related to garages**
#   
#   Two of those have one NA (GarageCars and GarageArea), one has 157 NAs (GarageType), 4 variables have 159 NAs. 
# 
# First of alldata, I am going to replace alldata 159 missing **GarageYrBlt: Year garage was built** values with the values in YearBuilt (this is similar to YearRemodAdd, which also defaults to YearBuilt if no remodeling or additions).

alldata$GarageYrBlt[is.na(alldata$GarageYrBlt)] <- alldata$YearBuilt[is.na(alldata$GarageYrBlt)]

#As NAs mean 'No Garage' for character variables, I now want to find out where the differences between the 157 NA GarageType and the other 3 character variables with 159 NAs come from.

#check if alldata 157 NAs are the same observations among the variables with 157/159 NAs
length(which(is.na(alldata$GarageType) & is.na(alldata$GarageFinish) & is.na(alldata$GarageCond) & is.na(alldata$GarageQual)))

#Find the 2 additional NAs
kable(alldata[!is.na(alldata$GarageType) & is.na(alldata$GarageFinish), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

#The 157 NAs within GarageType alldata turn out to be NA in GarageCondition, GarageQuality, and GarageFinish as well. The differences are found in houses 2127 and 2577. As you can see, house 2127 actualldatay does seem to have a Garage and house 2577 does not. Therefore, there should be 158 houses without a Garage. To fix house 2127, I will imputate the most common values (modes) for GarageCond, GarageQual, and GarageFinish.

#Imputing modes.
alldata$GarageCond[2127] <- names(sort(-table(alldata$GarageCond)))[1]
alldata$GarageQual[2127] <- names(sort(-table(alldata$GarageQual)))[1]
alldata$GarageFinish[2127] <- names(sort(-table(alldata$GarageFinish)))[1]

#display "fixed" house
kable(alldata[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])


# **GarageCars and GarageArea: Size of garage in car capacity and Size of garage in square** 
#   
#   Both have 1 NA. As you can see above, it is house 2577 for both variables. The problem probably occured as the GarageType for this house is "detached", while alldata other Garage-variables seem to indicate that this house has no Garage.

#fixing 3 values for house 2577
alldata$GarageCars[2577] <- 0
alldata$GarageArea[2577] <- 0
alldata$GarageType[2577] <- NA

#check if NAs of the character variables are now alldata 158
length(which(is.na(alldata$GarageType) & is.na(alldata$GarageFinish) & is.na(alldata$GarageCond) & is.na(alldata$GarageQual)))

# Now, the 4 character variables related to garage alldata have the same set of 158 NAs, which correspond to 'No Garage'. I will fix alldata of them in the remainder of this section
# 
# **GarageType: Garage location**
#   
#   The values do not seem ordinal, so I will convert into a factor.
# 
# 2Types	More than one type of garage
# Attchd	Attached to home
# Basment	Basement Garage
# BuiltIn	Built-In (Garage part of house - typicalldatay has room above garage)
# CarPort	Car Port
# Detchd	Detached from home
# NA	No Garage

alldata$GarageType[is.na(alldata$GarageType)] <- 'No Garage'
alldata$GarageType <- as.factor(alldata$GarageType)
table(alldata$GarageType)


# **GarageFinish: Interior finish of the garage**
#   
#   The values are ordinal.
# 
# Fin	Finished
# RFn	Rough Finished	
# Unf	Unfinished
# NA	No Garage       

alldata$GarageFinish[is.na(alldata$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)

alldata$GarageFinish<-as.integer(revalue(alldata$GarageFinish, Finish))
table(alldata$GarageFinish)


# **GarageQual: Garage quality**
#   
#   Another variable than can be made ordinal with the Qualities vector.
# 
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage

alldata$GarageQual[is.na(alldata$GarageQual)] <- 'None'
alldata$GarageQual<-as.integer(revalue(alldata$GarageQual, Qualities))
table(alldata$GarageQual)

# 
# **GarageCond: Garage condition**
#   
#   Another variable than can be made ordinal with the Qualities vector.
# 
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage

alldata$GarageCond[is.na(alldata$GarageCond)] <- 'None'
alldata$GarageCond<-as.integer(revalue(alldata$GarageCond, Qualities))
table(alldata$GarageCond)


# **Please return to the 5.2 Tabs menu to select other (groups of) variables**
#   
#   ###Basement Variables
#   
#   **Altogether, there are 11 variables that relate to the Basement of a house**
#   
#   Five of those have 79-82 NAs, six have one or two NAs.

#check if alldata 79 NAs are the same observations among the variables with 80+ NAs
length(which(is.na(alldata$BsmtQual) & is.na(alldata$BsmtCond) & is.na(alldata$BsmtExposure) & is.na(alldata$BsmtFinType1) & is.na(alldata$BsmtFinType2)))

#Find the additional NAs; BsmtFinType1 is the one with 79 NAs
alldata[!is.na(alldata$BsmtFinType1) & (is.na(alldata$BsmtCond)|is.na(alldata$BsmtQual)|is.na(alldata$BsmtExposure)|is.na(alldata$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

#So altogether, it seems as if there are 79 houses without a basement, because the basement variables of the other houses with missing values are alldata 80% complete (missing 1 out of 5 values). I am going to impute the modes to fix those 9 houses.

#Imputing modes.
alldata$BsmtFinType2[333] <- names(sort(-table(alldata$BsmtFinType2)))[1]
alldata$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(alldata$BsmtExposure)))[1]
alldata$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(alldata$BsmtCond)))[1]
alldata$BsmtQual[c(2218, 2219)] <- names(sort(-table(alldata$BsmtQual)))[1]


# Now that the 5 variables considered agree upon 79 houses with 'no basement', I am going to factorize/hot encode them below.
# 
# **BsmtQual: Evaluates the height of the basement**
#   
#   A variable than can be made ordinal with the Qualities vector.
# 
# Ex	Excellent (100+ inches)	
# Gd	Good (90-99 inches)
# TA	Typical (80-89 inches)
# Fa	Fair (70-79 inches)
# Po	Poor (<70 inches
#          NA	No Basement
#          
         alldata$BsmtQual[is.na(alldata$BsmtQual)] <- 'None'
         alldata$BsmtQual<-as.integer(revalue(alldata$BsmtQual, Qualities))
         table(alldata$BsmtQual)

         
         # **BsmtCond: Evaluates the general condition of the basement**
         #   
         #   A variable than can be made ordinal with the Qualities vector.
         # 
         # Ex	Excellent
         # Gd	Good
         # TA	Typical - slight dampness alldataowed
         # Fa	Fair - dampness or some cracking or settling
         # Po	Poor - Severe cracking, settling, or wetness
         # NA	No Basement
         # 
         alldata$BsmtCond[is.na(alldata$BsmtCond)] <- 'None'
         alldata$BsmtCond<-as.integer(revalue(alldata$BsmtCond, Qualities))
         table(alldata$BsmtCond)

         # **BsmtExposure: Refers to walkout or garden level walldatas**
         #   
         #   A variable than can be made ordinal.
         # 
         # Gd	Good Exposure
         # Av	Average Exposure (split levels or foyers typicalldatay score average or above)	
         # Mn	Mimimum Exposure
         # No	No Exposure
         # NA	No Basement
         # 
         alldata$BsmtExposure[is.na(alldata$BsmtExposure)] <- 'None'
         Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
         
         alldata$BsmtExposure<-as.integer(revalue(alldata$BsmtExposure, Exposure))
         table(alldata$BsmtExposure)

         
         # **BsmtFinType1: Rating of basement finished area**
         #   
         #   A variable than can be made ordinal.
         # 
         # GLQ	Good Living Quarters
         # ALQ	Average Living Quarters
         # BLQ	Below Average Living Quarters	
         # Rec	Average Rec Room
         # LwQ	Low Quality
         # Unf	Unfinshed
         # NA	No Basement
         
         alldata$BsmtFinType1[is.na(alldata$BsmtFinType1)] <- 'None'
         FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
         
         alldata$BsmtFinType1<-as.integer(revalue(alldata$BsmtFinType1, FinType))
         table(alldata$BsmtFinType1)

         
         # **BsmtFinType2: Rating of basement finished area (if multiple types)**
         #   
         #   A variable than can be made ordinal with the FinType vector.
         # 
         # GLQ	Good Living Quarters
         # ALQ	Average Living Quarters
         # BLQ	Below Average Living Quarters	
         # Rec	Average Rec Room
         # LwQ	Low Quality
         # Unf	Unfinshed
         # NA	No Basement
         # 
         alldata$BsmtFinType2[is.na(alldata$BsmtFinType2)] <- 'None'
         FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
         
         alldata$BsmtFinType2<-as.integer(revalue(alldata$BsmtFinType2, FinType))
         table(alldata$BsmtFinType2)

         # **Remaining Basement variabes with just a few NAs**
         #   
         #   I now still have to deal with those 6 variables that have 1 or 2 NAs.
         # 
         #display remaining NAs. Using BsmtQual as a reference for the 79 houses without basement agreed upon earlier
         alldata[(is.na(alldata$BsmtFullBath)|is.na(alldata$BsmtHalfBath)|is.na(alldata$BsmtFinSF1)|is.na(alldata$BsmtFinSF2)|is.na(alldata$BsmtUnfSF)|is.na(alldata$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]

         #It should be obvious that those remaining NAs alldata refer to 'not present'. Below, I am fixing those remaining variables.
         
         # **BsmtFullBath: Basement full bathrooms**
         #   
         #   An integer variable.
         alldata$BsmtFullBath[is.na(alldata$BsmtFullBath)] <-0
         table(alldata$BsmtFullBath)

         

          
         alldata$BsmtHalfBath[is.na(alldata$BsmtHalfBath)] <-0
         table(alldata$BsmtHalfBath)

         # **BsmtFinSF1: Type 1 finished square feet**
         #   
         #   An integer variable.
         alldata$BsmtFinSF1[is.na(alldata$BsmtFinSF1)] <-0

         
         # **BsmtFinSF2: Type 2 finished square feet**
         #   
         #   An integer variable.
         alldata$BsmtFinSF2[is.na(alldata$BsmtFinSF2)] <-0

         
         
         alldata$BsmtUnfSF[is.na(alldata$BsmtUnfSF)] <-0

       
         alldata$TotalBsmtSF[is.na(alldata$TotalBsmtSF)] <-0

         
         # **Please return to the 5.2 Tabs menu to select other (groups of) variables**
         #   
         #   ###Masonry variables
         #   
         #   **Masonry veneer type, and masonry veneer area**
         #   
         #   Masonry veneer type has 24 NAs. Masonry veneer area has 23 NAs. If a house has a veneer area, it should also have a masonry veneer type. Let's fix this one first.
         # 
         #check if the 23 houses with veneer area NA are also NA in the veneer type
         length(which(is.na(alldata$MasVnrType) & is.na(alldata$MasVnrArea)))
         
         #find the one that should have a MasVnrType
         alldata[is.na(alldata$MasVnrType) & !is.na(alldata$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

       
         #fix this veneer type by imputing the mode
         alldata$MasVnrType[2611] <- names(sort(-table(alldata$MasVnrType)))[2] #taking the 2nd value as the 1st is 'none'
         alldata[2611, c('MasVnrType', 'MasVnrArea')]

         
         # This leaves me with 23 houses that realldatay have no masonry.
         # 
         # **Masonry veneer type**
         # 
         # Will check the ordinality below.
         # 
         # BrkCmn	Brick Common
         # BrkFace	Brick Face
         # CBlock	Cinder Block
         # None	None
         # Stone	Stone
         
         alldata$MasVnrType[is.na(alldata$MasVnrType)] <- 'None'
         # 
         alldata[!is.na(alldata$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)
         # 
         # There seems to be a significant difference between "common brick/none" and the other types. I assume that simple stones and for instance wooden houses are just cheaper. I will make the ordinality accordingly.
         
         Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
         alldata$MasVnrType<-as.integer(revalue(alldata$MasVnrType, Masonry))
         table(alldata$MasVnrType)

         
         # **MasVnrArea: Masonry veneer area in square feet**
         # 
         # An integer variable.
         alldata$MasVnrArea[is.na(alldata$MasVnrArea)] <-0

         

         ###MS Zoning
         
         # **MSZoning: Identifies the general zoning classification of the sale**
         # 
         # 4 NAs. Values are categorical.
         # 
         # A	Agriculture
         # C	Commercial
         # FV	Floating Village Residential
         # I	Industrial
         # RH	Residential High Density
         # RL	Residential Low Density
         # RP	Residential Low Density Park 
         # RM	Residential Medium Density
         # 
         #imputing the mode
         alldata$MSZoning[is.na(alldata$MSZoning)] <- names(sort(-table(alldata$MSZoning)))[1]
         alldata$MSZoning <- as.factor(alldata$MSZoning)
         table(alldata$MSZoning)
         sum(table(alldata$MSZoning))

         
         #**Please return to the 5.2 Tabs menu to select other (groups of) variables**
         
         ###Kitchen variables
         
         # **Kitchen quality and numer of Kitchens above grade**
         # 
         # Kitchen quality has 1 NA. Number of Kitchens is complete.
         # 
         # **Kitchen quality**
         # 
         # 1NA. Can be made ordinal with the qualities vector.
         # 
         # Ex	Excellent
         # Gd	Good
         # TA	Typical/Average
         # Fa	Fair
         # Po	Poor
         
         alldata$KitchenQual[is.na(alldata$KitchenQual)] <- 'TA' #replace with most common value
         alldata$KitchenQual<-as.integer(revalue(alldata$KitchenQual, Qualities))
         table(alldata$KitchenQual)
         sum(table(alldata$KitchenQual))

         # **Number of Kitchens above grade**
         # 
         # An integer variable with no NAs.

                table(alldata$KitchenAbvGr)
         sum(table(alldata$KitchenAbvGr))

         
         # **Please return to the 5.2 Tabs menu to select other (groups of) variables**
         
         ###Utilities
         # 
         # **Utilities: Type of utilities available**
         # 
         # 2 NAs. Ordinal as additional utilities is better.
         # 
         # AlldataPub	Alldata public Utilities (E,G,W,& S)	
         # NoSewr	Electricity, Gas, and Water (Septic Tank)
         # NoSeWa	Electricity and Gas Only
         # ELO	Electricity only
         # 
         # However, the table below shows that only one house does not have alldata public utilities. This house is in the train set. Therefore, imputing 'AlldataPub' for the NAs means that alldata houses in the test set will have 'AlldataPub'. This makes the variable useless for prediction. Consequently, I will get rid of it.
         
         table(alldata$Utilities)
         kable(alldata[is.na(alldata$Utilities) | alldata$Utilities=='NoSeWa', 1:9])
         alldata$Utilities <- NULL

         
         # **Please return to the 5.2 Tabs menu to select other (groups of) variables**
         # 
         # ###Home functionality
         # 
         # **Functional: Home functionality**
         # 
         # 1NA. Can be made ordinal (salvage only is worst, typical is best).
         # 
         # Typ	Typical Functionality
         # Min1	Minor Deductions 1
         # Min2	Minor Deductions 2
         # Mod	Moderate Deductions
         # Maj1	Major Deductions 1
         # Maj2	Major Deductions 2
         # Sev	Severely Damaged
         # Sal	Salvage only
         # 
         
         #impute mode for the 1 NA
         alldata$Functional[is.na(alldata$Functional)] <- names(sort(-table(alldata$Functional)))[1]
         
         alldata$Functional <- as.integer(revalue(alldata$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
         table(alldata$Functional)
         sum(table(alldata$Functional))

         
         # **Please return to the 5.2 Tabs menu to select other (groups of) variables**
         # 
         # ###Exterior variables
         # 
         # **There are 4 exterior variables**
         # 
         # 2 variables have 1 NA, 2 variables have no NAs.
         # 
         # **Exterior1st: Exterior covering on house**
         # 
         # 1 NA. Values are categorical.
         # 
         # AsbShng	Asbestos Shingles
         # AsphShn	Asphalt Shingles
         # BrkComm	Brick Common
         # BrkFace	Brick Face
         # CBlock	Cinder Block
         # CemntBd	Cement Board
         # HdBoard	Hard Board
         # ImStucc	Imitation Stucco
         # MetalSd	Metal Siding
         # Other	Other
         # Plywood	Plywood
         # PreCast	PreCast	
         # Stone	Stone
         # Stucco	Stucco
         # VinylSd	Vinyl Siding
         # Wd Sdng	Wood Siding
         # WdShing	Wood Shingles
         # 
         #imputing mode
         
         alldata$Exterior1st[is.na(alldata$Exterior1st)] <- names(sort(-table(alldata$Exterior1st)))[1]
         
         alldata$Exterior1st <- as.factor(alldata$Exterior1st)
         table(alldata$Exterior1st)
         sum(table(alldata$Exterior1st))

         # **Exterior2nd: Exterior covering on house (if more than one material)**
         # 
         # 1 NA. Values are categorical.
         # 
         # AsbShng	Asbestos Shingles
         # AsphShn	Asphalt Shingles
         # BrkComm	Brick Common
         # BrkFace	Brick Face
         # CBlock	Cinder Block
         # CemntBd	Cement Board
         # HdBoard	Hard Board
         # ImStucc	Imitation Stucco
         # MetalSd	Metal Siding
         # Other	Other
         # Plywood	Plywood
         # PreCast	PreCast
         # Stone	Stone
         # Stucco	Stucco
         # VinylSd	Vinyl Siding
         # Wd Sdng	Wood Siding
         # WdShing	Wood Shingles
         # 
         #imputing mode
         alldata$Exterior2nd[is.na(alldata$Exterior2nd)] <- names(sort(-table(alldata$Exterior2nd)))[1]
         
         alldata$Exterior2nd <- as.factor(alldata$Exterior2nd)
         table(alldata$Exterior2nd)
         sum(table(alldata$Exterior2nd))

         # **ExterQual: Evaluates the quality of the material on the exterior** 
         # 
         # No NAs. Can be made ordinal using the Qualities vector.
         # 
         # Ex	Excellent
         # Gd	Good
         # TA	Average/Typical
         # Fa	Fair
         # Po	Poor
         # 

         alldata$ExterQual<-as.integer(revalue(alldata$ExterQual, Qualities))
         table(alldata$ExterQual)
         sum(table(alldata$ExterQual))

         
         
         # **ExterCond: Evaluates the present condition of the material on the exterior**
         # 
         # No NAs. Can be made ordinal using the Qualities vector.
         # 
         # Ex	Excellent
         # Gd	Good
         # TA	Average/Typical
         # Fa	Fair
         # Po	Poor
         # 
         alldata$ExterCond<-as.integer(revalue(alldata$ExterCond, Qualities))
         table(alldata$ExterCond)
         sum(table(alldata$ExterCond))

         
         # **Please return to the 5.2 Tabs menu to select other (groups of) variables**
         
         ###Electrical system
         
         # **Electrical: Electrical system**
         # 
         # 1 NA. Values are categorical.
         # 
         # SBrkr	Standard Circuit Breakers & Romex
         # FuseA	Fuse Box over 60 AMP and alldata Romex wiring (Average)	
         # FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
         # FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
         # Mix	Mixed
         # 
         #imputing mode
         alldata$Electrical[is.na(alldata$Electrical)] <- names(sort(-table(alldata$Electrical)))[1]

         alldata$Electrical <- as.factor(alldata$Electrical)
         table(alldata$Electrical)
         sum(table(alldata$Electrical))

         
         #**Please return to the 5.2 Tabs menu to select other (groups of) variables**
         
         ###Sale Type and Condition
         
         # **SaleType: Type of sale**
         # 
         # 1 NA. Values are categorical.
         # 
         # WD 	Warranty Deed - Conventional
         # CWD	Warranty Deed - Cash
         # VWD	Warranty Deed - VA Loan
         # New	Home just constructed and sold
         # COD	Court Officer Deed/Estate
         # Con	Contract 15% Down payment regular terms
         # ConLw	Contract Low Down payment and low interest
         # ConLI	Contract Low Interest
         # ConLD	Contract Low Down
         # Oth	Other
         # 
         #imputing mode
         alldata$SaleType[is.na(alldata$SaleType)] <- names(sort(-table(alldata$SaleType)))[1]
         
         alldata$SaleType <- as.factor(alldata$SaleType)
         table(alldata$SaleType)
         sum(table(alldata$SaleType))

         
         # **SaleCondition: Condition of sale**
         # 
         # No NAs. Values are categorical.
         # 
         # Normal	Normal Sale
         # Abnorml	Abnormal Sale -  trade, foreclosure, short sale
         # AdjLand	Adjoining Land Purchase
         # Alldataoca	Alldataocation - two linked properties with separate deeds, typicalldatay condo with a garage unit	
         # Family	Sale between family members
         # Partial	Home was not completed when last assessed (associated with New Homes)
         # 
         alldata$SaleCondition <- as.factor(alldata$SaleCondition)
         table(alldata$SaleCondition)
         sum(table(alldata$SaleCondition))

         
         #**Please return to the 5.2 Tabs menu to select other (groups of) variables**
         
         ##Label encoding/factorizing the remaining character variables {.tabset}
         
        # At this point, I have made sure that alldata variables with NAs are taken care of. However, I still need to also take care of the remaining character variables that without missing values. Similar to the previous section, I have created Tabs for groups of variables.
         
         Charcol <- names(alldata[,sapply(alldata, is.character)])
         Charcol
         cat('There are', length(Charcol), 'remaining columns with character values')

         
         # ###Foundation
         # 
         # **Foundation: Type of foundation**
         # 
         # BrkTil          Brick & Tile
         # CBlock	        Cinder Block
         # PConc	        Poured Contrete	
         # Slab	        Slab
         # Stone	        Stone
         # Wood	        Wood
         # 
         #No ordinality, so converting into factors
         alldata$Foundation <- as.factor(alldata$Foundation)
         table(alldata$Foundation)
         sum(table(alldata$Foundation))

         
         # **Please return to the 5.3 Tabs menu to select other (groups of) variables**
         # 
         # ###Heating and airco
         # 
         # There are 2 heating variables, and one that indicates Airco Yes/No.
         # 
         # **Heating: Type of heating**
         # 
         # Floor	Floor Furnace
         # GasA	Gas forced warm air furnace
         # GasW	Gas hot water or steam heat
         # Grav	Gravity furnace	
         # OthW	Hot water or steam heat other than gas
         # Walldata	Walldata furnace
         # 
         
         #No ordinality, so converting into factors
         alldata$Heating <- as.factor(alldata$Heating)
         table(alldata$Heating)
         sum(table(alldata$Heating))

         
         # **HeatingQC: Heating quality and condition**
         # 
         # Ex	Excellent
         # Gd	Good
         # TA	Average/Typical
         # Fa	Fair
         # Po	Poor
         
         #making the variable ordinal using the Qualities vector
         alldata$HeatingQC<-as.integer(revalue(alldata$HeatingQC, Qualities))
         table(alldata$HeatingQC)
         sum(table(alldata$HeatingQC))

         # 
         # **CentralAir: Central air conditioning**
         # 
         # N	No
         # Y	Yes
         
         alldata$CentralAir<-as.integer(revalue(alldata$CentralAir, c('N'=0, 'Y'=1)))
         table(alldata$CentralAir)
         sum(table(alldata$CentralAir))

         
         # **Please return to the 5.3 Tabs menu to select other (groups of) variables**
         # 
         # ###Roof
         # 
         # There are 2 variables that deal with the roof of houses.
         # 
         # **RoofStyle: Type of roof**
         # 
         # Flat	Flat
         # Gable	Gable
         # Gambrel	Gabrel (Barn)
         # Hip	Hip
         # Mansard	Mansard
         # Shed	Shed
         
         #No ordinality, so converting into factors
         alldata$RoofStyle <- as.factor(alldata$RoofStyle)
         table(alldata$RoofStyle)
         sum(table(alldata$RoofStyle))

         
         # **RoofMatl: Roof material**
         # 
         # ClyTile	Clay or Tile
         # CompShg	Standard (Composite) Shingle
         # Membran	Membrane
         # Metal	Metal
         # Roll	Roll
         # Tar&Grv	Gravel & Tar
         # WdShake	Wood Shakes
         # WdShngl	Wood Shingles
         # 
         #No ordinality, so converting into factors
         alldata$RoofMatl <- as.factor(alldata$RoofMatl)
         table(alldata$RoofMatl)
         sum(table(alldata$RoofMatl))

         
         # **Please return to the 5.3 Tabs menu to select other (groups of) variables**
         # 
         # ###Land
         # 
         # 2 variables that specify the flatness and slope of the propoerty.
         # 
         # **LandContour: Flatness of the property**
         # 
         # Lvl	Near Flat/Level	
         # Bnk	Banked - Quick and significant rise from street grade to building
         # HLS	Hillside - Significant slope from side to side
         # Low	Depression
         # 
         #No ordinality, so converting into factors
         alldata$LandContour <- as.factor(alldata$LandContour)
         table(alldata$LandContour)
         sum(table(alldata$LandContour))

         
         # **LandSlope: Slope of property**
         # 
         # Gtl	Gentle slope
         # Mod	Moderate Slope	
         # Sev	Severe Slope
         
         #Ordinal, so label encoding
         alldata$LandSlope<-as.integer(revalue(alldata$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
         table(alldata$LandSlope)
         sum(table(alldata$LandSlope))

         
         # **Please return to the 5.3 Tabs menu to select other (groups of) variables**
         # 
         # ###Dwelling
         # 
         # 2 variables that specify the type and style of dwelling.
         # 
         # **BldgType: Type of dwelling**
         # 
         # 1Fam	Single-family Detached	
         # 2FmCon	Two-family Conversion; originalldatay built as one-family dwelling
         # Duplx	Duplex
         # TwnhsE	Townhouse End Unit
         # TwnhsI	Townhouse Inside Unit
         # 
         # This seems ordinal to me (single family detached=best). Let's check it with visualization.
         # 
         ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=as.factor(BldgType), y=SalePrice)) +
           geom_bar(stat='summary', fun.y = "median", fill='blue')+
           scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
           geom_label(stat = "count", aes(label = ..count.., y = ..count..))

         
         #However, the visualization does not show ordinality.
         
         #No ordinality, so converting into factors
         alldata$BldgType <- as.factor(alldata$BldgType)
         table(alldata$BldgType)
         sum(table(alldata$BldgType))

         # **HouseStyle: Style of dwelling**
         #   
         #   1Story	One story
         # 1.5Fin	One and one-half story: 2nd level finished
         # 1.5Unf	One and one-half story: 2nd level unfinished
         # 2Story	Two story
         # 2.5Fin	Two and one-half story: 2nd level finished
         # 2.5Unf	Two and one-half story: 2nd level unfinished
         # SFoyer	Split Foyer
         # SLvl	Split Level
         # 
         #No ordinality, so converting into factors
         alldata$HouseStyle <- as.factor(alldata$HouseStyle)
         table(alldata$HouseStyle)
         sum(table(alldata$HouseStyle))

         
         # **Please return to the 5.3 Tabs menu to select other (groups of) variables**
         #   
         #   ###Neighborhood and Conditions
         #   
         #   3 variables that specify the physical location, and the proximity of 'conditions'.
         # 
         # **Neighborhood: Physical locations within Ames city limits**
         #   
         #   Note: as the number of levels is realldatay high, I will look into binning later on.
         # 
         # Blmngtn	Bloomington Heights
         # Blueste	Bluestem
         # BrDale	Briardale
         # BrkSide	Brookside
         # ClearCr	Clear Creek
         # CollgCr	College Creek
         # Crawfor	Crawford
         # Edwards	Edwards
         # Gilbert	Gilbert
         # IDOTRR	Iowa DOT and Rail Road
         # MeadowV	Meadow Village
         # Mitchel	Mitchell
         # Names	North Ames
         # NoRidge	Northridge
         # NPkVill	Northpark Villa
         # NridgHt	Northridge Heights
         # NWAmes	Northwest Ames
         # OldTown	Old Town
         # SWISU	South & West of Iowa State University
         # Sawyer	Sawyer
         # SawyerW	Sawyer West
         # Somerst	Somerset
         # StoneBr	Stone Brook
         # Timber	Timberland
         # Veenker	Veenker
         # 
         #No ordinality, so converting into factors
         alldata$Neighborhood <- as.factor(alldata$Neighborhood)
         table(alldata$Neighborhood)
         sum(table(alldata$Neighborhood))

         
         # **Condition1: Proximity to various conditions**
         #   
         #   Artery	Adjacent to arterial street
         # Feedr	Adjacent to feeder street	
         # Norm	Normal	
         # RRNn	Within 200' of North-South Railroad
         # RRAn	Adjacent to North-South Railroad
         # PosN	Near positive off-site feature--park, greenbelt, etc.
         # PosA	Adjacent to postive off-site feature
         # RRNe	Within 200' of East-West Railroad
         # RRAe	Adjacent to East-West Railroad
         # 
         #No ordinality, so converting into factors
         alldata$Condition1 <- as.factor(alldata$Condition1)
         table(alldata$Condition1)
         sum(table(alldata$Condition1))

         
         # **Condition2: Proximity to various conditions (if more than one is present)**
         #   
         #   Artery	Adjacent to arterial street
         # Feedr	Adjacent to feeder street	
         # Norm	Normal	
         # RRNn	Within 200' of North-South Railroad
         # RRAn	Adjacent to North-South Railroad
         # PosN	Near positive off-site feature--park, greenbelt, etc.
         # PosA	Adjacent to postive off-site feature
         # RRNe	Within 200' of East-West Railroad
         # RRAe	Adjacent to East-West Railroad
         # 

         
          #No ordinality, so converting into factors
         alldata$Condition2 <- as.factor(alldata$Condition2)
         table(alldata$Condition2)
         sum(table(alldata$Condition2))

         
         # **Please return to the 5.3 Tabs menu to select other (groups of) variables**
         #   
         #   ###Pavement of Street & Driveway
         #   
         #   2 variables
         # 
         # **Street: Type of road access to property**
         #   
         #   Grvl	Gravel	
         # Pave	Paved
         # 
         
         
         
         #Ordinal, so label encoding
         alldata$Street<-as.integer(revalue(alldata$Street, c('Grvl'=0, 'Pave'=1)))
         table(alldata$Street)
         sum(table(alldata$Street))

         
         # **PavedDrive: Paved driveway**
         #   
         #   Y	Paved 
         # P	Partial Pavement
         # N	Dirt/Gravel
         
         #Ordinal, so label encoding
         alldata$PavedDrive<-as.integer(revalue(alldata$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
         table(alldata$PavedDrive)
         sum(table(alldata$PavedDrive))

         
         # **Please return to the 5.3 Tabs menu to select other (groups of) variables**
         #   
         #   ##Changing some numeric variables into factors
         #   
         #   At this point, alldata variables are complete (No NAs), and alldata character variables are converted into either numeric labels of into factors. However, there are 3 variables that are recorded numeric but should actualldatay be categorical.
         # 
         # ###Year and Month Sold
         # 
         # While oridinality within YearBuilt (or remodeled) makes sense (old houses are worth less), we are talking about only 5 years of sales. These years also include an economic crisis. For instance: Sale Prices in 2009 (after the collapse) are very likely to be much lower than in 2007. I wil convert YrSold into a factor before modeling, but as I need the numeric version of YrSold to create an Age variable, I am not doing that yet.
         # 
         # Month Sold is also an Integer variable. However, December is not "better" than January. Therefore, I will convert MoSold values back into factors.
         
         str(alldata$YrSold)
         str(alldata$MoSold)
         alldata$MoSold <- as.factor(alldata$MoSold)

         
         #Although possible a bit less steep than expected, the effects of the Banking crises that took place at the end of 2007 can be seen indeed. After the highest median prices in 2007, the prices gradualldatay decreased. However, seasonality seems to play a bigger role, as you can see below.
         
         ys <- ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
           geom_bar(stat='summary', fun.y = "median", fill='blue')+
           scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
           geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
           coord_cartesian(ylim = c(0, 200000)) +
           geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
         
         ms <- ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=MoSold, y=SalePrice)) +
           geom_bar(stat='summary', fun.y = "median", fill='blue')+
           scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
           geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
           coord_cartesian(ylim = c(0, 200000)) +
           geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
         
         grid.arrange(ys, ms, widths=c(1,2))

         
         ###MSSubClass
         
         # MSSubClass: Identifies the type of dwelling involved in the sale.	
         # 
         # 20	1-STORY 1946 & NEWER ALLDATA STYLES
         # 30	1-STORY 1945 & OLDER
         # 40	1-STORY W/FINISHED ATTIC ALLDATA AGES
         # 45	1-1/2 STORY - UNFINISHED ALLDATA AGES
         # 50	1-1/2 STORY FINISHED ALLDATA AGES
         # 60	2-STORY 1946 & NEWER
         # 70	2-STORY 1945 & OLDER
         # 75	2-1/2 STORY ALLDATA AGES
         # 80	SPLIT OR MULTI-LEVEL
         # 85	SPLIT FOYER
         # 90	DUPLEX - ALLDATA STYLES AND AGES
         # 120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
         # 150	1-1/2 STORY PUD - ALLDATA AGES
         # 160	2-STORY PUD - 1946 & NEWER
         # 180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
         # 190	2 FAMILY CONVERSION - ALLDATA STYLES AND AGES
         # 
         # These classes are coded as numbers, but realldatay are categories.
         # 
         #revalue for better readability
         alldata$MSSubClass<-revalue(alldata$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story alldata ages', '80'='split/multi level', '85'='split foyer', '90'='duplex alldata style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD alldata', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
         
         str(alldata$MSSubClass)

         
         #Visualization of important variables
         
         #I have now finalldatay reached the point where alldata character variables have been converted into categorical factors or have been label encoded into numbers. In addition, 3 numeric variables have been converted into factors, and I deleted one variable (Utilities). As you can see below, the number of numerical variables is now 56 (including the response variable), and the remaining 23 variables are categorical.
         
         numericVars <- which(sapply(alldata, is.numeric)) #index vector numeric variables
         factorVars <- which(sapply(alldata, is.factor)) #index vector factor variables
         cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

         
         ##Correlations again
         
         #Below I am checking the correlations again. As you can see, the number of variables with a correlation of at least 0.5 with the SalePrice has increased from 10 (see section 4.2.1) to 16. 
         
         alldata_numVar <- alldata[, numericVars]
         cor_numVar <- cor(alldata_numVar, use="pairwise.complete.obs") #correlations of alldata numeric variables
         
         #sort on decreasing correlations with SalePrice
         cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
         #select only high corelations
         CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
         cor_numVar <- cor_numVar[CorHigh, CorHigh]
         
         corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

         ##Finding variable importance with a quick Random Forest
         
         #Although the correlations are giving a good overview of the most important numeric variables and multicolinerity among those variables, I wanted to get an overview of the most important variables including the categorical variables before moving on to visualization.
         
         #I tried to get the relative importance of variables with a quick linear regression model with the calc.relimp function of package , and also tried the boruta function of package boruta which separates the variables into groups that are important or not. However, these method took a long time. As I only want to get an indication of the variable importance, I eventualldatay decided to keep it simple and just use a quick and dirty Random Forest model with only 100 trees. This also does the job for me, and does not take very long as I can specify a (relatively) smalldata number of trees.
         
         set.seed(2018)
         quick_RF <- randomForest(x=alldata[1:1460,-79], y=alldata$SalePrice[1:1460], ntree=100,importance=TRUE)
         imp_RF <- importance(quick_RF)
         imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
         imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
         
         ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")

         
         #Only 3 of those most important variables are categorical according to RF; Neighborhood, MSSubClass, and GarageType. 
         
         ### Above Ground Living Area, and other surface related variables (in square feet)
         
         #As I have already visualized the relation between the Above Ground Living Area and SalePrice in my initial explorations, I will now just display the distribution itself. As there are more 'square feet' surface measurements in the Top 20, I am taking the opportunity to bundle them in this section. Note: GarageArea is taken care of in the Garage variables section.
         
         #I am also adding 'Total Rooms Above Ground' (TotRmsAbvGrd) as this variable is highly correlated with the Above Ground Living Area(0.81).
         
         s1 <- ggplot(data= alldata, aes(x=GrLivArea)) +
           geom_density() + labs(x='Square feet living area')
         s2 <- ggplot(data=alldata, aes(x=as.factor(TotRmsAbvGrd))) +
           geom_histogram(stat='count') + labs(x='Rooms above Ground')
         s3 <- ggplot(data= alldata, aes(x=X1stFlrSF)) +
           geom_density() + labs(x='Square feet first floor')
         s4 <- ggplot(data= alldata, aes(x=X2ndFlrSF)) +
           geom_density() + labs(x='Square feet second floor')
         s5 <- ggplot(data= alldata, aes(x=TotalBsmtSF)) +
           geom_density() + labs(x='Square feet basement')
         s6 <- ggplot(data= alldata[alldata$LotArea<100000,], aes(x=LotArea)) +
           geom_density() + labs(x='Square feet lot')
         s7 <- ggplot(data= alldata, aes(x=LotFrontage)) +
           geom_density() + labs(x='Linear feet lot frontage')
         s8 <- ggplot(data= alldata, aes(x=LowQualFinSF)) +
           geom_histogram() + labs(x='Low quality square feet 1st & 2nd')
         
         layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
         multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout=layout)

         
         # I will investigate several of these variables for outliers later on. For the lot visualization, I have already taken out the lots above 100,000 square feet (4 houses).
         # 
         # GrLivArea seemed to be just the total of square feet 1st and 2nd floor. However, in a later version, I discovered that there is also a variable calldataed: LowQualFinSF: Low quality finished square feet (alldata floors). As you can see above (Low quality square feet 1st and 2nd) almost alldata houses have none of this (only 40 houses do have some). It turns out that these square feet are actualldatay included in the GrLivArea. The correlation between those 3 variables and GrLivArea is exactely 1.
         
         cor(alldata$GrLivArea, (alldata$X1stFlrSF + alldata$X2ndFlrSF + alldata$LowQualFinSF))
         head(alldata[alldata$LowQualFinSF>0, c('GrLivArea', 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF')])

         
         ###The most important categorical variable; Neighborhood
         
         #Th first graph shows the median SalePrice by Neighorhood. The frequency (number of houses) of each Neighborhood in the train set is shown in the labels.
         
         #The second graph below shows the frequencies across alldata data.
         
         n1 <- ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
           geom_bar(stat='summary', fun.y = "median", fill='blue') +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
           geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
           geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
         n2 <- ggplot(data=alldata, aes(x=Neighborhood)) +
           geom_histogram(stat='count')+
           geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
         grid.arrange(n1, n2)

         
         ###Overalldata Quality, and other Quality variables
         
         #I have already visualized the relation between Overalldata Quality and SalePrice in my initial explorations, but I want to visualize the frequency distribution as well. As there are more quality measurements, I am taking the opportunity to bundle them in this section.
         
         q1 <- ggplot(data=alldata, aes(x=as.factor(OveralldataQual))) +
           geom_histogram(stat='count')
         q2 <- ggplot(data=alldata, aes(x=as.factor(ExterQual))) +
           geom_histogram(stat='count')
         q3 <- ggplot(data=alldata, aes(x=as.factor(BsmtQual))) +
           geom_histogram(stat='count')
         q4 <- ggplot(data=alldata, aes(x=as.factor(KitchenQual))) +
           geom_histogram(stat='count')
         q5 <- ggplot(data=alldata, aes(x=as.factor(GarageQual))) +
           geom_histogram(stat='count')
         q6 <- ggplot(data=alldata, aes(x=as.factor(FireplaceQu))) +
           geom_histogram(stat='count')
         q7 <- ggplot(data=alldata, aes(x=as.factor(PoolQC))) +
           geom_histogram(stat='count')
         
         layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
         multiplot(q1, q2, q3, q4, q5, q6, q7, layout=layout)
         

         
        # Overalldata Quality is very important, and also more granular than the other variables. External Quality is also improtant, but has a high correlation with Overalldata Quality (0.73). Kitchen Quality also seems one to keep, as alldata houses have a kitchen and there is a variance with some substance. Garage Quality does not seem to distinguish much, as the majority of garages have Q3. Fireplace Quality is in the list of high correlations, and in the important variables list. The PoolQC is just very sparse (the 13 pools cannot even be seen on this scale). I will look at creating a 'has pool' variable later on.
         
         ###The second most important categorical variable; MSSubClass
         #
        # The first visualization shows the median SalePrice by MSSubClass. The frequency (number of houses) of each MSSubClass in the train set is shown in the labels.
         
         #The histrogram shows the frequencies across alldata data. Most houses are relatively new, and have one or two stories.
         
         ms1 <- ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
           geom_bar(stat='summary', fun.y = "median", fill='blue') +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
           geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
           geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
         ms2 <- ggplot(data=alldata, aes(x=MSSubClass)) +
           geom_histogram(stat='count')+
           geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
         grid.arrange(ms1, ms2)

         
         ###Garage variables
         
         #Several Garage variables have a high correlation with SalePrice, and are also in the top-20 list of the quick random forest. However, there is multicolinearity among them and I think that 7 garage variables is too many anyway. I feel that something like 3 variables should be sufficient (possibly GarageCars, GarageType, and a Quality measurement), but before I do any selection I am visualizing alldata of them in this section.
         
         #correct error
         alldata$GarageYrBlt[2593] <- 2007 #this must have been a typo. GarageYrBlt=2207, YearBuilt=2006, YearRemodAdd=2007.

         
         
         g1 <- ggplot(data=alldata[alldata$GarageCars !=0,], aes(x=GarageYrBlt)) +
           geom_histogram()
         g2 <- ggplot(data=alldata, aes(x=as.factor(GarageCars))) +
           geom_histogram(stat='count')
         g3 <- ggplot(data= alldata, aes(x=GarageArea)) +
           geom_density()
         g4 <- ggplot(data=alldata, aes(x=as.factor(GarageCond))) +
           geom_histogram(stat='count')
         g5 <- ggplot(data=alldata, aes(x=GarageType)) +
           geom_histogram(stat='count')
         g6 <- ggplot(data=alldata, aes(x=as.factor(GarageQual))) +
           geom_histogram(stat='count')
         g7 <- ggplot(data=alldata, aes(x=as.factor(GarageFinish))) +
           geom_histogram(stat='count')
         
         layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
         multiplot(g1, g2, g3, g4, g5, g6, g7, layout=layout)
         

         
         #As already mentioned in section 4.2, GarageCars and GarageArea are highly correlated. Here, GarageQual and GarageCond also seem highly correlated, and both are dominated by level =3. 
         
         ###Basement variables
         
         #Similar the garage variables, multiple basement variables are important in the correlations matrix and the Top 20 RF predictors list. However, 11 basement variables seems an overkill. Before I decide what I am going to do with them, I am visualizing 8 of them below. The 2 "Bathroom" variables are dealt with in Feature Engineering (section 7.1), and the "Basement square feet" is already discussed in section 6.2.1.
         
         b1 <- ggplot(data=alldata, aes(x=BsmtFinSF1)) +
           geom_histogram() + labs(x='Type 1 finished square feet')
         b2 <- ggplot(data=alldata, aes(x=BsmtFinSF2)) +
           geom_histogram()+ labs(x='Type 2 finished square feet')
         b3 <- ggplot(data=alldata, aes(x=BsmtUnfSF)) +
           geom_histogram()+ labs(x='Unfinished square feet')
         b4 <- ggplot(data=alldata, aes(x=as.factor(BsmtFinType1))) +
           geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
         b5 <- ggplot(data=alldata, aes(x=as.factor(BsmtFinType2))) +
           geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
         b6 <- ggplot(data=alldata, aes(x=as.factor(BsmtQual))) +
           geom_histogram(stat='count')+ labs(x='Height of the basement')
         b7 <- ggplot(data=alldata, aes(x=as.factor(BsmtCond))) +
           geom_histogram(stat='count')+ labs(x='Rating of general condition')
         b8 <- ggplot(data=alldata, aes(x=as.factor(BsmtExposure))) +
           geom_histogram(stat='count')+ labs(x='Walkout or garden level walldatas')
         
         layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
         multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout=layout)
         

         
         #So it seemed as if the Total Basement Surface in square feet (TotalBsmtSF) is further broken down into finished areas (2 if more than one type of finish), and unfinished area. I did a check between the correlation of total of those 3 variables, and TotalBsmtSF. The correlation is exactely 1, so that's a good thing (no errors or smalldata discrepancies)!
         
         #Basement Quality is a confusing variable name, as it turns out that it specificalldatay rates the Height of the basement.
         
         #Feature engineering
         
         ##Total number of Bathrooms
         
         #here are 4 bathroom variables. Individualldatay, these variables are not very important. However, I assume that I if I add them up into one predictor, this predictor is likely to become a strong one.
         
        # "A half-bath, also known as a powder room or guest bath, has only two of the four main bathroom components-typicalldatay a toilet and sink." Consequently, I will also count the half bathrooms as half.
         
         alldata$TotBathrooms <- alldata$FullBath + (alldata$HalfBath*0.5) + alldata$BsmtFullBath + (alldata$BsmtHalfBath*0.5)

         
        # As you can see in the first graph, there now seems to be a clear correlation (it's 0.63). The frequency distribution of Bathrooms in alldata data is shown in the second graph.

tb1 <- ggplot(data=alldata[!is.na(alldata$SalePrice),], aes(x=as.factor(TotBathrooms), y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
tb2 <- ggplot(data=alldata, aes(x=as.factor(TotBathrooms))) +
  geom_histogram(stat='count')
grid.arrange(tb1, tb2)


##Adding 'House Age', 'Remodeled (Yes/No)', and IsNew variables

#Altogether, there are 3 variables that are relevant with regards to the Age of a house; YearBlt, YearRemodAdd, and YearSold. YearRemodAdd defaults to YearBuilt if there has been no Remodeling/Addition. I will use YearRemodeled and YearSold to determine the Age. However, as parts of old constructions will always remain and only parts of the house might have been renovated, I will also introduce a Remodeled Yes/No variable. This should be seen as some sort of penalty parameter that indicates that if the Age is based on a remodeling date, it is probably worth less than houses that were built from scratch in that same year.

alldata$Remod <- ifelse(alldata$YearBuilt==alldata$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
alldata$Age <- as.numeric(alldata$YrSold)-alldata$YearRemodAdd


ggplot(data=alldata[!is.na(alldata$SalePrice),], aes(x=Age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)


#As expected, the graph shows a negative correlation with Age (old house are worth less).

cor(alldata$SalePrice[!is.na(alldata$SalePrice)], alldata$Age[!is.na(alldata$SalePrice)])


#As you can see below, houses that are remodeled are worth less indeed, as expected.

ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=as.factor(Remod), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice


#Finalldatay, I am creating the IsNew variable below. Altogether, there are 116 new houses in the dataset.


alldata$IsNew <- ifelse(alldata$YrSold==alldata$YearBuilt, 1, 0)
table(alldata$IsNew)


#These 116 new houses are fairly evenly distributed among train and test set, and as you can see new houses are worth considerably more on average.

ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=as.factor(IsNew), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice


alldata$YrSold <- as.factor(alldata$YrSold) #the numeric version is now not needed anymore


##Binning Neighborhood

nb1 <- ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='Neighborhood', y='Median SalePrice') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
nb2 <- ggplot(alldata[!is.na(alldata$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='Neighborhood', y="Mean SalePrice") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
grid.arrange(nb1, nb2)


#Both the median and mean Saleprices agree on 3 neighborhoods with substantialldatay higher saleprices. The separation of the 3 relatively poor neighborhoods is less clear, but at least both graphs agree on the same 3 poor neighborhoods. Since I do not want to 'overbin', I am only creating categories for those 'extremes'.


alldata$NeighRich[alldata$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
alldata$NeighRich[!alldata$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
alldata$NeighRich[alldata$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

table(alldata$NeighRich)


##Total Square Feet

#As the total living space generalldatay is very important when people buy houses, I am adding a predictors that adds up the living space above and below ground.

alldata$TotalSqFeet <- alldata$GrLivArea + alldata$TotalBsmtSF


ggplot(data=alldata[!is.na(alldata$SalePrice),], aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(alldata$GrLivArea[!is.na(alldata$SalePrice)]>4500, rownames(alldata), '')))


#As expected, the correlation with SalePrice is very strong indeed (0.78). 

cor(alldata$SalePrice, alldata$TotalSqFeet, use= "pairwise.complete.obs")

#The two potential outliers seem to 'outlie' even more than before. By taking out these two outliers, the correlation increases by 5%.

cor(alldata$SalePrice[-c(524, 1299)], alldata$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")


##Consolidating Porch variables

# Below, I listed the variables that seem related regarding porches.
# 
# 
# * WoodDeckSF: Wood deck area in square feet
# 
# * OpenPorchSF: Open porch area in square feet
# 
# * EnclosedPorch: Enclosed porch area in square feet
# 
# * 3SsnPorch: Three season porch area in square feet
# 
# * ScreenPorch: Screen porch area in square feet
# 
# 
# As far as I know, porches are sheltered areas outside of the house, and a wooden deck is unsheltered. Therefore, I am leaving WoodDeckSF alone, and are only consolidating the 4 porch variables.

alldata$TotalPorchSF <- alldata$OpenPorchSF + alldata$EnclosedPorch + alldata$X3SsnPorch + alldata$ScreenPorch


#Although adding up these Porch areas makes sense (there should not be any overlap between areas), the correlation with SalePrice is not very strong.

cor(alldata$SalePrice, alldata$TotalPorchSF, use= "pairwise.complete.obs")


ggplot(data=alldata[!is.na(alldata$SalePrice),], aes(x=TotalPorchSF, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#Preparing data for modeling

##Dropping highly correlated variables

#First of alldata, I am dropping a variable if two variables are highly correlated. To find these correlated pairs, I have used the correlations matrix again (see section 6.1). For instance: GarageCars and GarageArea have a correlation of 0.89. Of those two, I am dropping the variable with the lowest correlation with SalePrice (which is GarageArea with a SalePrice correlation of 0.62. GarageCars has a SalePrice correlation of 0.64).

dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

alldata <- alldata[,!(names(alldata) %in% dropVars)]

##Removing outliers

#For the time being, I am keeping it simple and just remove the two realldatay big houses with low SalePrice manualldatay. However, I intend to investigate this more thorough in a later stage (possibly using the 'outliers' package).

alldata <- alldata[-c(524, 1299),]


##PreProcessing predictor variables

#Before modeling I need to center and scale the 'true numeric' predictors (so not variables that have been label encoded), and create dummy variables for the categorical predictors. Below, I am splitting the dataframe into one with alldata (true) numeric variables, and another dataframe holding the (ordinal) factors.

numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OveralldataQual', 'OveralldataCond'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- alldata[, names(alldata) %in% numericVarNames]
dim(DFnumeric)
DFfactors <- alldata[, !(names(alldata) %in% numericVarNames)]
dim(DFfactors)
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']
dim(DFfactors)
cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')


###Skewness and normalizing of the numeric predictors

# **Skewness**
#   Skewness is a measure of the symmetry in a distribution.  A symmetrical dataset will have a skewness equal to 0.  So, a normal distribution will have a skewness of 0. Skewness essentialldatay measures the relative size of the two tails. As a rule of thumb, skewness should be between -1 and 1. In this range, data are considered fairly symmetrical. In order to fix the skewness, I am taking the log for alldata numeric predictors with an absolute skew greater than 0.8 (actualldatay: log+1, to avoid division by zero issues).

for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

#**Normalizing the data**
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)


###One hot encoding the categorical variables

#The last step needed to ensure that alldata predictors are converted into numeric columns (which is required by most Machine Learning algorithms) is to 'one-hot encode' the categorical variables. This basicalldatay means that alldata (not ordinal) factor values are getting a seperate colums with 1s and 0s (1 basicalldatay means Yes/Present). To do this one-hot encoding, I am using the `model.matrix()` function.

# DFdummies <- as.data.frame(model.matrix(~ ., DFfactors))
# model.matrix(~.-1, DFfactors)
#dim(DFdummies)
dmy <- dummyVars(" ~ .", data = DFfactors)
DFdummies <- data.frame(predict(dmy, newdata = DFfactors))
dim(DFdummies)
###Removing levels with few or no observations in train or test

#In previous versions, I worked with Caret's `Near Zero Variance` function. Although this works, it also is a quick fix and too much information got lost. For instance, by using the defaults, alldata Neighborhoods with less than 146 houses are omitted as (one-hot encoded) variables (frequency ratio higher than 95/5). Therefore, I have taken amore carefull manual approach in this version.

#check if some values are absent in the test set
ZerocolTest <- which(colSums(DFdummies[(nrow(alldata[!is.na(alldata$SalePrice),])+1):nrow(alldata),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] #removing predictors
dim(DFdummies)

#check if some values are absent in the train set
# ZerocolTrain <- which(colSums(DFdummies[1:nrow(alldata[!is.na(alldata$SalePrice),]),])==0)
# colnames(DFdummies[ZerocolTrain])
# DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor

dim(DFdummies)

#Also taking out variables with less than 10 'ones' in the train set.

#fewOnes <- which(colSums(DFdummies[1:nrow(alldata[!is.na(alldata$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
#DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

DFdummies
#Altogether, I have removed 49 one-hot encoded predictors with little or no variance. Altough this may seem a significant number, it is actualldatay much less than the number of predictors that were taken out by using caret's`near zero variance` function (using its default thresholds).

combined <- cbind(DFnorm, DFdummies) #combining alldata (now numeric) predictors into one dataframe 


##Dealing with skewness of response variable

skew(alldata$SalePrice)


qqnorm(alldata$SalePrice)
qqline(alldata$SalePrice)


#The skew of 1.87 indicates a right skew that is too high, and the Q-Q plot shows that sale prices are also not normalldatay distributed. To fix this I am taking the log of SalePrice.

alldata$SalePrice <- log(alldata$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(alldata$SalePrice)


#As you can see,the skew is now quite low and the Q-Q plot is also looking much better.

qqnorm(alldata$SalePrice)
qqline(alldata$SalePrice)


##Composing train and test sets

train1 <- combined[!is.na(alldata$SalePrice),]
test1 <- combined[is.na(alldata$SalePrice),]

test1

train1$

combined.lm=cbind(alldata$SalePrice,combined)
combined.lm
#Modeling

##Lasso regression model

# I have also tried Ridge and Elastic Net models, but since lasso gives the best results of those 3 models I am only keeping the lasso model in the document.
# 
# The elastic-net penalty is controlled by alpha, and bridges the gap between lasso (alpha=1) and ridge (alpha=0). The tuning parameter lambda controls the overalldata strength of the penalty. It is known that the ridge penalty shrinks the coefficients of correlated predictors towards each other while the lasso tends to pick one of them and discard the others.
# 
# Below, I am using caret cross validation to find the best value for lambda, which is the only hyperparameter that needs to be tuned for the lasso model.


combined.lm[!is.na(combined.lm$SalePrice),]


dim(train1)

sum(is.na(train1))
tail(train1)
train1
write.csv(file="train1.csv", x=train1)

which(is.na(train1))
train1[207271]
set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

sum(is.na(train1))
cbind(alldata$SalePrice[!is.na(alldata$SalePrice)],train1)
olm=lm(alldata$SalePrice[1:1458]~.,data=combined.lm[c(1:1458),-1])

combined.lm$NeighRich

plot(exp(alldata$SalePrice[1:1458])-exp(fitted(olm)),main="Residuals of linear model",ylab="")
abline(h=0)
olm



sqrt(mean((alldata$SalePrice[1:1458]-fitted(olm))^2))

sqrt(mean((exp(alldata$SalePrice[1:1458])-exp(fitted(olm)))^2))

names(combined.lm)
combined
?predict.lm
dim(combined)
lm.pred=predict(olm,newdata=test1)
write.csv(x=cbind(test_labels,exp(lm.pred)),file="lmpreds.csv")
lm.pred

length(alldata$SalePrice[!is.na(alldata$SalePrice)])
length(train1$LotFrontage)
lasso_mod <- train(x=train1, y=alldata$SalePrice[!is.na(alldata$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
min(lasso_mod$results$RMSE)

?glmnet
lasso_mod1=glmnet(x=as.matrix(train1),alldata$SalePrice[!is.na(alldata$SalePrice)],alpha=1,lambda=.0025)
plot(exp(alldata$SalePrice[!is.na(alldata$SalePrice)])-exp(fitted(lasso_mod)),main="Residuals of Lasso model",ylab="")
abline(h=0)
sqrt(mean((exp(alldata$SalePrice[1:1458])-exp(fitted(lasso_mod)))^2))


  coef <- data.frame(coef.name = dimnames(coef(lasso_mod$finalModel,s=lasso_mod$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(lasso_mod$finalModel,s=lasso_mod$bestTune$lambda)))

# exclude the (Intercept) term
coef <- coef[-1,]

picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))

cat("Lasso picked",picked_features,"variables and eliminated the other",
    not_picked_features,"variables\n")


# sort coefficients in ascending order
coef <- arrange(coef,-coef.value)

# extract the top 10 and bottom 10 features
imp_coef <- rbind(head(coef,10),
                  tail(coef,10))

ggplot(df.val,aes(x=predictions_lasso,y=exp(SalePrice)))+
  geom_point(size=1 , color = "steelblue")+
  geom_smooth(method = "loess", color="darkred")+
  labs(x="Predicted", y="Actual")+
  theme(plot.margin = unit(c(2,2,2,2), "cm"))

ggplot(imp_coef) +
  geom_bar(aes(x=reorder(coef.name,coef.value),y=coef.value),
           stat="identity") +
  ylim(-1.5,0.6) +
  coord_flip() +
  ggtitle("Coefficents in the Lasso Model") +
  theme(axis.title=element_blank())

fitted(lasso_mod1)
alldata$SalePrice[!is.na(alldata$SalePrice)]-lasso_mod$finalMode

ridgeGrid=expand.grid(alpha=0,lambda=seq(0.001,0.1,by=0.0005))
ridge_mod <- train(x=train1, y=alldata$SalePrice[!is.na(alldata$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=ridgeGrid) 

ridge_mod$bestTune
min(lasso_mod$results$RMSE)
plot(exp(alldata$SalePrice[!is.na(alldata$SalePrice)])-exp(fitted(ridge_mod)),main="Residuals of ridge model")
abline(h=0)


sqrt(mean((exp(alldata$SalePrice[!is.na(alldata$SalePrice)])-exp(fitted(ridge_mod)))^2))
#The documentation of the caret `varImp' function says: for glmboost and glmnet the absolute value of the coefficients corresponding to the tuned model are used.

#Although this means that a real ranking of the most important variables is not stored, it gives me the opportunity to find out how many of the variables are not used in the model (and hence have coefficient 0).

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overalldata!=0))
varsNotSelected <- length(which(lassoImportance$Overalldata==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')

ridgepred=predict(ridge_mod,test1)
predictions_lasso=exp(ridgepred)
write.csv(x=cbind(test_labels,exp(ridgepred)),file="ridgepreds.csv")

#So lasso did what it is supposed to do: it seems to have dealt with multicolinearity well by not using about 45% of the available variables in the model.

LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)

write.csv(file="train1.csv", x=train1)


write.csv(file="lassopreds.csv",x=cbind(test_labels,predictions_lasso))
##XGBoost model

#Initialldatay, I just worked with the XGBoost package directly. The main reason for this was that the package uses its own efficient datastructure (xgb.DMatrix). The package also provides a cross validation function. However, this CV function only determines the optimal number of rounds, and does not support a full grid search of hyperparameters.

#Although caret does not seem to use the (fast) datastructure of the xgb package, I eventualldatay decided to do hyperparameter tuning with it anyway, as it at least supports a full grid search. As far as I understand it, the main parameters to tune to avoid overfitting are max_depth, and min_child_weight (see [XGBoost documentation](http://xgboost.readthedocs.io/en/latest/how_to/param_tuning.html)). Below I am setting up a grid that tunes both these parameters, and also the eta (learning rate).

xgb_grid = expand.grid(
nrounds = 1000,
eta = c(0.1, 0.05, 0.01),
max_depth = c(2, 3, 4, 5, 6),
gamma = 0,
colsample_bytree=1,
min_child_weight=c(1, 2, 3, 4 ,5),
subsample=1
)


#The next step is to let caret find the best hyperparameter values (using 5 fold cross validation).

#xgb_caret <- train(x=train1, y=alldata$SalePrice[!is.na(alldata$SalePrice)], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
#xgb_caret$bestTune


#As expected, this took quite a bit of time (localldatay). As I want to limit the running time on Kaggle, I disabled the code, and am just continuing with the results. According to caret, the 'bestTune' parameters are:

# * Max_depth=3
# * eta=0.05
# * Min_child_weight=4
# 
# In the remainder of this section, I will continue to work with the xgboost package directly. Below, I am starting with the preparation of the data in the recommended format.

label_train <- alldata$SalePrice[!is.na(alldata$SalePrice)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))


#In addition, I am taking over the best tuned values from the caret cross validation.

default_param<-list(
objective = "reg:linear",
booster = "gbtree",
eta=0.05, #default = 0.3
gamma=0,
max_depth=3, #default=6
min_child_weight=4, #default=1
subsample=1,
colsample_bytree=1
)


#The next step is to do cross validation to determine the best number of rounds (for the given set of parameters). 

xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)


#Although it was a bit of work, the hyperparameter tuning definitly paid of, as the cross validated RMSE inproved considerably (from 0.1225 without the caret tuning, to 0.1162 in this version)!

#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 454)


XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
head(predictions_XGB)


#view variable importance plot
library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(train1),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)


##Averaging predictions

#Since the lasso and XGBoost algorithms are very different, averaging predictions likely improves the scores. As the lasso model does better regarding the cross validated RMSE score (0.1121 versus 0.1162), I am weigting the lasso model double.

sub_avg <- data.frame(Id = test_labels, SalePrice = (predictions_XGB+2*predictions_lasso)/3)
head(sub_avg)
write.csv(sub_avg, file = 'average.csv', row.names = F)








