
PPT <- read.csv("G:/Suman/batch34/project2/Datasheet/Property_Price_Train.csv", stringsAsFactors=TRUE)
str(PPT)
view(PPT)
describe(PPT)
PPT$W_Deck_Area_sq<-PPT$W_Deck_Area^2;


library(VIM)
library(psych)
#install.packages("dplyr")
library(dplyr)
PPT<-kNN(PPT) #NA value removed
PPT11<- read.csv("G:/Suman/batch34/project2/Datasheet/Property_Price_Train.csv", stringsAsFactors=TRUE)
PPT11$Brick_Veneer_Area<-kNN(PPT11$Brick_Veneer_Area)

##Column analysis

#Building_Class ## NEED TO CHANGE IT FACTOR VARIABLE
par(mfrow=c(2,1))
bul_class=table(PPT$Building_Class)
barplot(bul_class,col = 'blue',main = "Building_Class",xlab = "Building_Class",ylab = 'Sale_price')
boxplot(PPT$Building_Class,horizontal = T,col = 'blue',main = "Building_Class")
#3 outliers
UB<-quantile(PPT$Building_Class,0.75)+1.5*IQR(PPT$Building_Class)
UB
PPT$Building_Class[PPT$Building_Class>UB]<-UB
describe(PPT$Building_Class)

#Lot_Extent(SQUARE WILL HELP)
Lot_Extent1=table(PPT$Lot_Extent)
barplot(Lot_Extent1)
boxplot(PPT$Lot_Extent,horizontal = T,col = 'blue')

UB<-quantile(PPT$Lot_Extent,0.75)+1.5*IQR(PPT$Lot_Extent)
UB
LB<-quantile(PPT$Lot_Extent,0.25)-1.5*IQR(PPT$Lot_Extent)
LB
PPT$Lot_Extent[PPT$Lot_Extent>UB]<-UB
PPT$Lot_Extent[PPT$Lot_Extent<LB]<-LB



#Lot_Size(Log or Square, oout liers not fixing)
Lot_Size1=table(PPT$Lot_Size)
barplot(Lot_Size1)
boxplot(PPT$Lot_Size,horizontal = T,col = 'blue')

#Overall_Material(square)
Overall_Material1=table(PPT$Overall_Material)
barplot(Overall_Material1)
boxplot(PPT$Overall_Material,horizontal = T,col = 'blue')

#House_Condition(square will increase)

barplot(table(PPT$House_Condition))
boxplot(PPT$Overall_Material,horizontal = T,col = 'blue')

#"Construction_Year",(log or square root)
barplot(table(PPT$Construction_Year))
boxplot(PPT$Construction_Year,horizontal = T,col = 'blue')

LB<-quantile(PPT$Construction_Year,0.25)-1.5*IQR(PPT$Construction_Year)
LB
PPT$Construction_Year[PPT$Construction_Year<LB]<-LB


##"Remodel_Year",(end poit have higher value, squareroot)
barplot(table(PPT$Remodel_Year))
boxplot(PPT$Remodel_Year,horizontal = T,col = 'blue')


#"Brick_Veneer_Area", very less magnitude, can be checked for outliers
barplot(table(PPT$Brick_Veneer_Area))
boxplot(PPT$Brick_Veneer_Area,horizontal = T,col = 'blue')


#"BsmtFinSF1",
barplot(table(PPT$BsmtFinSF1))
boxplot(PPT$BsmtFinSF1,horizontal = T,col = 'blue')


#"BsmtFinSF2",
barplot(table(PPT$BsmtFinSF2))
boxplot(PPT$BsmtFinSF2,horizontal = T,col = 'blue',main="BsmtFinSF2")


#"BsmtUnfSF",
barplot(table(PPT$BsmtUnfSF))
boxplot(PPT$BsmtUnfSF,horizontal = T,col = 'blue')
UB<-quantile(PPT$BsmtUnfSF,0.75)+1.5*IQR(PPT$BsmtUnfSF)
UB
PPT$BsmtUnfSF[PPT$BsmtUnfSF>UB]<-UB

#"Total_Basement_Area"
barplot(table(PPT$Total_Basement_Area))
boxplot(PPT$Total_Basement_Area,horizontal = T,col = 'blue')

UB<-quantile(PPT$Total_Basement_Area,0.75)+1.5*IQR(PPT$Total_Basement_Area)
UB
PPT$Total_Basement_Area[PPT$Total_Basement_Area>UB]<-UB
LB<-quantile(PPT$Construction_Year,0.25)-1.5*IQR(PPT$Construction_Year)
LB
PPT$Construction_Year[PPT$Construction_Year<LB]<-LB

## "First_Floor_Area",(square  )
barplot(table(PPT$First_Floor_Area))
boxplot(PPT$First_Floor_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$First_Floor_Area,0.75)+1.5*IQR(PPT$First_Floor_Area)
UB
PPT$First_Floor_Area[PPT$First_Floor_Area>UB]<-UB


##"Second_Floor_Area", square may help
barplot(table(PPT$Second_Floor_Area))
boxplot(PPT$Second_Floor_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Second_Floor_Area,0.75)+1.5*IQR(PPT$Second_Floor_Area)
UB
PPT$Second_Floor_Area[PPT$Second_Floor_Area>UB]<-UB


#"LowQualFinSF",  squareroot or similar ....
barplot(table(PPT$LowQualFinSF))
boxplot(PPT$LowQualFinSF,horizontal = T,col = 'blue')

#"Grade_Living_Area",
barplot(table(PPT$Grade_Living_Area))
boxplot(PPT$Grade_Living_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Grade_Living_Area,0.75)+1.5*IQR(PPT$Grade_Living_Area)
UB
PPT$Grade_Living_Area[PPT$Grade_Living_Area>UB]<-UB


##"Underground_Full_Bathroom", squarerooot
barplot(table(PPT$Underground_Full_Bathroom))
barplot(table(sqrt(PPT$Underground_Full_Bathroom)))

boxplot(PPT$Underground_Full_Bathroom,horizontal = T,col = 'blue')
UB<-quantile(PPT$Underground_Full_Bathroom,0.75)+1.5*IQR(PPT$Underground_Full_Bathroom)
UB
PPT$Underground_Full_Bathroom[PPT$Underground_Full_Bathroom>UB]<-UB



#"Underground_Half_Bathroom",  need to study later 
barplot(table(PPT$Underground_Half_Bathroom))
boxplot(PPT$Underground_Half_Bathroom,horizontal = T,col = 'blue')


#"Full_Bathroom_Above_Grade", square 
barplot(table(PPT$Full_Bathroom_Above_Grade))
boxplot(PPT$Full_Bathroom_Above_Grade,horizontal = T,col = 'blue')

#"Half_Bathroom_Above_Grade",
barplot(table(PPT$Half_Bathroom_Above_Grade))
boxplot(PPT$Half_Bathroom_Above_Grade,horizontal = T,col = 'blue')


#"Bedroom_Above_Grade", upper md lover limit not required
barplot(table(PPT$Bedroom_Above_Grade))
boxplot(PPT$Bedroom_Above_Grade,horizontal = T,col = 'blue')


#UB<-quantile(PPT$Bedroom_Above_Grade,0.75)+1.5*IQR(PPT$Bedroom_Above_Grade)
UB
#PPT$Bedroom_Above_Grade[PPT$Bedroom_Above_Grade>UB]<-UB
#LB<-quantile(PPT$Bedroom_Above_Grade,0.25)-1.5*IQR(PPT$Bedroom_Above_Grade)
LB
#PPT$Bedroom_Above_Grade[PPT$Bedroom_Above_Grade<LB]<-LB



##"Kitchen_Above_Grade",
barplot(table(PPT$Kitchen_Above_Grade))
boxplot(PPT$Kitchen_Above_Grade,horizontal = T,col = 'blue')


#"Rooms_Above_Grade", perfect , if require can remoe outliers
barplot(table(PPT$Rooms_Above_Grade))
boxplot(PPT$Rooms_Above_Grade,horizontal = T,col = 'blue')



#"Fireplaces",
barplot(table(PPT$Fireplaces))
boxplot(PPT$Fireplaces,horizontal = T,col = 'blue')
UB<-quantile(PPT$Fireplaces,0.75)+1.5*IQR(PPT$Fireplaces)
UB
PPT$Fireplaces[PPT$Fireplaces>UB]<-UB


#"Garage_Built_Year",
barplot(table(PPT$Garage_Built_Year))
boxplot(PPT$Garage_Built_Year,horizontal = T,col = 'blue')

#"Garage_Size",
barplot(table(PPT$Garage_Size))
boxplot(PPT$Garage_Size,horizontal = T,col = 'blue')

UB<-quantile(PPT$Garage_Size,0.75)+1.5*IQR(PPT$Garage_Size)
UB
PPT$Garage_Size[PPT$Garage_Size>UB]<-UB


##"Garage_Area"  need to relook

barplot(table(PPT$Garage_Area))
boxplot(PPT$Garage_Area,horizontal = T,col = 'blue')

##"W_Deck_Area",
barplot(table(PPT$W_Deck_Area))
boxplot(PPT$W_Deck_Area,horizontal = T,col = 'blue')

UB<-quantile(PPT$W_Deck_Area,0.75)+1.5*IQR(PPT$W_Deck_Area)
UB
PPT$W_Deck_Area[PPT$W_Deck_Area>UB]<-UB
LB<-quantile(PPT$W_Deck_Area,0.25)-1.5*IQR(PPT$W_Deck_Area)
LB
PPT$W_Deck_Area[PPT$W_Deck_Area<LB]<-LB




#"Open_Lobby_Area"
barplot(table(PPT$Open_Lobby_Area))
boxplot(PPT$Open_Lobby_Area,horizontal = T,col = 'blue')

UB<-quantile(PPT$Open_Lobby_Area,0.75)+1.5*IQR(PPT$Open_Lobby_Area)
UB
PPT$Open_Lobby_Area[PPT$Open_Lobby_Area>UB]<-UB
LB<-quantile(PPT$Open_Lobby_Area,0.25)-1.5*IQR(PPT$Open_Lobby_Area)
LB
PPT$Open_Lobby_Area[PPT$Open_Lobby_Area<LB]<-LB


#"Enclosed_Lobby_Area",
barplot(table(PPT$Enclosed_Lobby_Area))
boxplot(PPT$Enclosed_Lobby_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Enclosed_Lobby_Area,0.75)+1.5*IQR(PPT$Enclosed_Lobby_Area)
UB
PPT$Enclosed_Lobby_Area[PPT$Enclosed_Lobby_Area>UB]<-UB
LB<-quantile(PPT$Enclosed_Lobby_Area,0.25)-1.5*IQR(PPT$Enclosed_Lobby_Area)
LB
PPT$Enclosed_Lobby_Area[PPT$Enclosed_Lobby_Area<LB]<-LB



#"Three_Season_Lobby_Area",  ... log 
barplot(table(PPT$Three_Season_Lobby_Area))
boxplot(PPT$Three_Season_Lobby_Area,horizontal = T,col = 'blue')

LB<-quantile(PPT$Three_Season_Lobby_Area,0.25)-1.5*IQR(PPT$Three_Season_Lobby_Area)
LB
PPT$Three_Season_Lobby_Area[PPT$Three_Season_Lobby_Area<LB]<-LB



#"Screen_Lobby_Area"   ## need too check
barplot(table(PPT$Screen_Lobby_Area))
boxplot(PPT$Screen_Lobby_Area,horizontal = T,col = 'blue')


#"Pool_Area",

barplot(table(PPT$Pool_Area))
boxplot(PPT$Pool_Area,horizontal = T,col = 'blue')
UB<-quantile(PPT$Pool_Area,0.75)+1.5*IQR(PPT$Pool_Area)
UB
PPT$Pool_Area[PPT$Pool_Area>UB]<-UB



#"Miscellaneous_Value",
barplot(table(PPT$Miscellaneous_Value))
boxplot(PPT$Miscellaneous_Value,horizontal = T,col = 'blue')
UB<-quantile(PPT$Pool_Area,0.75)+1.5*IQR(PPT$Pool_Area)
UB
PPT$Miscellaneous_Value[PPT$Miscellaneous_Value>UB]<-UB



#"Month_Sold",   square may give better 
barplot(table(PPT$Month_Sold))
boxplot(PPT$Month_Sold,horizontal = T,col = 'blue')
UB<-quantile(PPT$Month_Sold,0.75)+1.5*IQR(PPT$Month_Sold)
UB
PPT$Month_Sold[PPT$Month_Sold>UB]<-UB



#"Year_Sold", square
barplot(table(PPT$Year_Sold))
boxplot(PPT$Year_Sold,horizontal = T,col = 'blue')
UB<-quantile(PPT$Year_Sold,0.75)+1.5*IQR(PPT$Year_Sold)
UB
PPT$Year_Sold[PPT$Year_Sold>UB]<-UB


#"Sale_Price"


#anova test for categorical variable
plot(Sale_Price ~ Zoning_Class, data = PPT,col="blue")
zon.bul<-aov(Sale_Price ~ Zoning_Class, data = PPT)
# Summary of the analysis
summary(zon.bul)
library(dplyr)
TukeyHSD(zon.bul)

ab<-lm(Sale_Price~Zoning_Class,data=PPT)
summary(ab)

## "BsmtFinType1","BsmtFinType2"
plot(Sale_Price ~ BsmtFinType2+BsmtFinType1, data = PPT,col="blue")
zon.bul<-aov(Sale_Price ~ BsmtFinType2+BsmtFinType1, data = PPT)
summary(zon.bul)
TukeyHSD(zon.bul)

##Road_Type & Lane_Type
plot(Sale_Price ~ Road_Type+Lane_Type, data = PPT,col="blue")
zon.bul<-aov(Sale_Price ~ Road_Type+Lane_Type, data = PPT)
summary(zon.bul)
TukeyHSD(zon.bul)





##Test for all numeric column
pairs.panels(PPT[c("Lot_Extent","Lot_Size","Overall_Material","House_Condition","Construction_Year","Remodel_Year","Brick_Veneer_Area","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","Total_Basement_Area","First_Floor_Area","Second_Floor_Area","LowQualFinSF","Grade_Living_Area","Underground_Full_Bathroom","Underground_Half_Bathroom","Full_Bathroom_Above_Grade","Half_Bathroom_Above_Grade","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Garage_Size","Garage_Area","Sale_Price")])
pairs.panels(PPT[c("Underground_Full_Bathroom","Underground_Half_Bathroom","Full_Bathroom_Above_Grade","Half_Bathroom_Above_Grade","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Garage_Size","Garage_Area","Sale_Price")])
pairs.panels(PPT[c("W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area","Three_Season_Lobby_Area","Screen_Lobby_Area","Pool_Area","Miscellaneous_Value","Month_Sold","Year_Sold","Sale_Price")])



cor(PPT[c("Lot_Extent","Lot_Size","Overall_Material","House_Condition","Construction_Year","Remodel_Year","Brick_Veneer_Area","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","Total_Basement_Area","First_Floor_Area","Second_Floor_Area","LowQualFinSF","Grade_Living_Area","Underground_Full_Bathroom","Underground_Half_Bathroom","Full_Bathroom_Above_Grade","Half_Bathroom_Above_Grade","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Garage_Size","Garage_Area","W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area","Three_Season_Lobby_Area","Screen_Lobby_Area","Pool_Area","Miscellaneous_Value","Month_Sold","Year_Sold","Sale_Price")])

cor(PPT[c("Sale_Price","Zoning_Class","Road_Type","Lane_Type","Property_Shape","Land_Outline","Utility_Type")])
model3_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold+Road_Type+Utility_Type+Property_Slope+Condition2+BsmtFinType2+Miscellaneous_Feature, data=PPT)

summary(model3_lm)

#catogrical
#Road_Type+Utility_Type+Property_Slope+Condition2+BsmtFinType2+Miscellaneous_Feature

model4_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Underground_Full_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Exterior2nd+House_Design+Zoning_Class+Roof_Quality+Exterior1st+Brick_Veneer_Type+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)

summary(model4_lm) #88%

##
model5_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Garage_Built_Year+Garage_Size+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Exterior2nd+House_Design+Zoning_Class+Roof_Quality+Exterior1st+Brick_Veneer_Type+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)
summary(model5_lm)  #88%

#model 6

model6_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Garage_Built_Year+Garage_Size+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Month_Sold+Year_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Exterior2nd+House_Design+Zoning_Class+Roof_Quality+Exterior1st+Brick_Veneer_Type+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)

summary(model6_lm)#88
vif(model6_lm)


#optimisation

#Lot_Extent+Bedroom_Above_Grade+Garage_Built_Year+Garage_Size+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Bedroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+W_Deck_Area+Screen_Lobby_Area+Pool_Area+Month_Sold+Year_Sold


pairs.panels(PPT[c("Lot_Extent","Bedroom_Above_Grade","Garage_Built_Year","Garage_Size","Full_Bathroom_Above_Grade","Half_Bathroom_Above_Grade","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Garage_Size","W_Deck_Area","Screen_Lobby_Area","Pool_Area","Month_Sold","Year_Sold","Sale_Price")])

##sold year,+W_Deck_Area removed 88.9
model7_lm<-lm(Sale_Price~Lot_Extent+Garage_Size+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Size+Lot_Configuration+Lot_Configuration+Neighborhood+House_Type+Zoning_Class+Roof_Quality+Exterior_Condition+Basement_Height+Exposure_Level+BsmtFinType1+Kitchen_Quality, data=PPT)


summary(model7_lm)
vif(model7_lm)

##
##sold year,+W_Deck_Area removed
model8_lm<-lm(Sale_Price~Lot_Extent+Bedroom_Above_Grade+Garage_Built_Year+Garage_Size+Full_Bathroom_Above_Grade+Bedroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Built_Year+Garage_Size+Screen_Lobby_Area+Pool_Area+Month_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Zoning_Class+Roof_Quality+Brick_Veneer_Type+Pool_Area+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Kitchen_Quality+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)


summary(model8_lm)


##sold year,+W_Deck_Area removed
PPT$Open_Lobby_Area_sq<-PPT$Open_Lobby_Area^2
PPT$Enclosed_Lobby_Area_sq<-PPT$Enclosed_Lobby_Area^2
##+Garage_Built_Year+Screen_Lobby_Area
#Bedroom_Above_Grade+Half_Bathroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+

PPT$areacom<-PPT$Garage_Built_Year*PPT$Rooms_Above_Grade

cor(PPT[c("Sale_Price","Garage_Built_Year","Half_Bathroom_Above_Grade","Rooms_Above_Grade")])
model9_lm<-lm(Sale_Price~areacom+Garage_Built_Year+Garage_Size+Fireplaces+Pool_Area+Month_Sold+Lot_Configuration+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+Condition1+House_Type+Heating_Quality+Zoning_Class+Roof_Quality+Brick_Veneer_Type+Pool_Area+Exterior_Condition+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Finish_Year+Garage_Quality+Garage_Condition+Pavedd_Drive+Fence_Quality+Sale_Type+Sale_Condition, data=PPT)


summary(model9_lm)
View(PPT)

View(PPT)

#PPT$cond<-PPT$Garage_ConditionPPT$Condition1

#PPT$years<-PPT$Garage_Built_Year*PPT$Garage_Finish_Year

cor(PPT[c("Sale_Price","Heating_Quality","Roof_Quality","Garage_Quality","Fence_Quality")])

SUBMODEL20<-lm(Sale_Price~Exterior_Condition+Exterior_Material,data = PPT)
summary(SUBMODEL20)

#27 variable

model9_lm<-lm(Sale_Price~areacom+Garage_Size+Fireplaces+Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Roof_Quality+Brick_Veneer_Type+Pool_Area+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=PPT)


summary(model9_lm)

##Step added  areacom*Garage_Size*Fireplaces*Month_Sold
#26
PPT$allsqr<-(areacom*Garage_Size*Fireplaces*Month_Sold)^2
model11_lm<-lm(Sale_Price~areacom+areacom*Garage_Size*Fireplaces*Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Roof_Quality+Brick_Veneer_Type+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=PPT)
summary(model11_lm)
str(model11_lm)

#25
PPT$allsqr<-(areacom*Garage_Size*Fireplaces*Month_Sold)^2
model11_lm<-lm(Sale_Price~areacom*Fireplaces*Garage_Size*Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Roof_Quality+Brick_Veneer_Type+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=PPT)
summary(model11_lm)
str(model11_lm)

#splitting the data into training and test data
set.seed(2)
#install.packages("caTools")
library(caTools)
split<-sample.split(PPT,SplitRatio = 0.7)
split
train<-subset(PPT,split="TRUE")
test<-subset(PPT,split="FALSE")
train
test
pridict_price<-predict(model11_lm,train)

##Train and predict 
numberrows<-nrow(PPT)
train1<-head(PPT,numberrows*.7)
predict1<-tail(PPT,numberrows*.3)

PPT$allsqr<-(Garage_Size*Fireplaces*Month_Sold)^2 ##Multiple R-squared:  0.8865,	Adjusted R-squared:  0.8709 
model12_lm_train<-lm(Sale_Price~areacom*Fireplaces*Garage_Size*Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Roof_Quality+Brick_Veneer_Type+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=train1)
summary(model12_lm_train)
#str(model11_lm)
PPT$areacom

model13_lm_predict<-lm(Sale_Price~areacom*Fireplaces*Garage_Size*Month_Sold+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Heating_Quality+Roof_Quality+Brick_Veneer_Type+Exterior_Material+Foundation_Type+Basement_Height+Exposure_Level+BsmtFinType1+Heating_Type+Air_Conditioning+Electrical_System+Functional_Rate+Garage+Garage_Quality+Pavedd_Drive+Sale_Type+Sale_Condition, data=predict1)
summary(model13_lm_predict)

pred<-predict(model11_lm,train)

##final - Neighborhood to be added , Brick_Veneer_Area->to be removed
PPT$allsqr<-(Garage_Size*Fireplaces*Month_Sold)^2
model14_lm_train<-lm(Sale_Price~areacom*Fireplaces*Garage_Size+Exterior_Material+Full_Bathroom_Above_Grade+Kitchen_Quality+Brick_Veneer_Area+Foundation_Type+Basement_Height+Garage+Sale_Type, data=train1)
summary(model14_lm_train)

##--House_Type, Heating_Quality, Roof_Quality, Brick_Veneer_Type, to be added ->Exposure_Level, to be added -> BsmtFinType1,Functional_Rate to be added -> Air_Conditioning,Air_Conditioning,Heating_Type,Pavedd_Drive,Garage_Quality,Sale_Condition(less effective ), 
PPT$gr_com<-PPT$Garage_Size*PPT$Brick_Veneer_Area
fit=lm(Sale_Price~areacom*Fireplaces*gr_com+BsmtFinSF1+Exterior_Material+Kitchen_Quality+Fence_Quality+Remodel_Year+Basement_Height+Full_Bathroom_Above_Grade+Rooms_Above_Grade+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area,data = PPT)
summary(fit)

##
train1<-head(PPT,numberrows*.7)
View(train1)
predict1<-tail(PPT,numberrows*.3)
train1$Sale_Price2<-train1$Sale_Price
barplot(table(train1$Sale_Price2))
boxplot(train1$Sale_Price2,horizontal = T,col = 'blue')
UB<-quantile(train1$Sale_Price2,0.75)+1.5*IQR(train1$Sale_Price2)
UB
train1$Sale_Price2[train1$Sale_Price2>UB]<-UB
LB<-quantile(train1$Sale_Price2,0.25)-1.5*IQR(train1$Sale_Price2)
LB
train1$Sale_Price2[train1$Sale_Price2<LB]<-LB

fit=lm(Sale_Price2~areacom*Fireplaces*gr_com+BsmtFinSF1+Exterior_Material+Kitchen_Quality+Fence_Quality+Remodel_Year+Basement_Height+Full_Bathroom_Above_Grade+Rooms_Above_Grade+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+First_Floor_Area+Grade_Living_Area,data = train1)
summary(fit)
##areacom*Fireplaces*gr_com


train1<-head(PPT,1)
View(train1)
###BsmtFinSF1+Exterior_Material+Kitchen_Quality+Remodel_Year+Basement_Height+Fireplaces+Garage_Size+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+Grade_Living_Area
fit1=lm(Sale_Price~BsmtFinSF1+Exterior_Material+Kitchen_Quality+Brick_Veneer_Area+Remodel_Year+Basement_Height+Fireplaces+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area*Grade_Living_Area,data = PPT)
summary(fit1) ###87 WIN normalizePath

fit1=lm(Sale_Price~BsmtFinSF1+Exterior_Material+Kitchen_Quality+Brick_Veneer_Area+Remodel_Year+Basement_Height+Fireplaces+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area*Grade_Living_Area,data = PPT)
summary(fit1)

library(caTools)
split<-sample.split(PPT,SplitRatio = 0.7)
split
train<-subset(PPT,split="TRUE")
test<-subset(PPT,split="FALSE")
train
test
pridict_train<-predict(fit1,train)

summary(pridict_train)

predict_test<-predict(fit1,test)
summary(predict_test)
vif(fit1)

library(tidyverse)
install.packages("HH")
library(HH)

error<-residuals(fit1)
error
summary(error)
hist(error)
boxplot(error)


##################Final model rework


fit1=lm(Sale_Price2~BsmtFinSF1+Exterior_Material+Kitchen_Quality+Brick_Veneer_Area+Remodel_Year+Basement_Height+Fireplaces+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area*Grade_Living_Area,data = PPT)
summary(fit1)

library(caTools)
split<-sample.split(PPT,SplitRatio = 0.7)
split
train<-subset(PPT,split="TRUE")
test<-subset(PPT,split="FALSE")
train
test
pridict_train<-predict(fit1,train)

summary(pridict_train)

predict_test<-predict(fit1,test)
summary(predict_test)


library(tidyverse)
#install.packages("HH")
library(HH)
#Assumption 1: Normality of error

PPT$error<-residuals(fit1)
error
summary(error)
hist(error,main="Normality of error",col="blue")
boxplot(error)

#Assumption 2: Linearity
plot(PPT$Sale_Price,error,main="linearity",col="blue")

#Assumption 3: Independence of error
plot(PPT$Id,PPT$error, main="Independence of error",col="blue")
# Assumption 4: Independence of error X axis: predicted values; Y axis: error

plot(pridict_train,PPT$error, main="Independence of error",col="blue")

#multicolinearity 
vif(fit1)

error<-residuals(fit1)
error
summary(error)
hist(error)
boxplot(error)

## 2nd Final model
###BsmtFinSF1+Exterior_Material+Kitchen_Quality+Remodel_Year+Basement_Height+Fireplaces+Garage_Size+Lot_Size+Overall_Material+Construction_Year+Total_Basement_Area+Grade_Living_Area
fit_final=lm(Sale_Price2~BsmtFinSF1+Exterior_Material+Kitchen_Quality+Remodel_Year+Construction_Year+Basement_Height+Fireplaces+Lot_Size+Overall_Material+Grade_Living_Area/Brick_Veneer_Area,data = PPT)
summary(fit_final) ###87 WIN normalizePath

split<-sample.split(PPT,SplitRatio = 0.7)
split
train<-subset(PPT,split="TRUE")
test<-subset(PPT,split="FALSE")
train
test
pridict_train<-predict(fit_final,train)

summary(pridict_train)

predict_test<-predict(fit_final,test)
summary(predict_test)

#Assumption 1: Normality of error

PPT$error<-residuals(fit_final)
PPT$error
summary(PPT$error)
hist(PPT$error)
boxplot(PPT$error,horizontal = T)

#Assumption 2: Linearity
plot(PPT$Sale_Price,PPT$error,main="linearity",col="blue")

#Assumption 3: Independence of error
plot(PPT$Id,PPT$error, main="Independence of error",col="blue")
# Assumption 4: Independence of error X axis: predicted values; Y axis: error

plot(pridict_train,PPT$error, main="Independence of error",col="blue")

#multicolinearity 

vif(fit_final)
##################################################################################
###Improved model

PPT$Sale_Price2<-PPT$Sale_Price
barplot(table(PPT$Sale_Price2))
boxplot(PPT$Sale_Price2,horizontal = T,col = 'blue')
UB<-quantile(PPT$Sale_Price2,0.75)+1.5*IQR(PPT$Sale_Price2)
UB
PPT$Sale_Price2[PPT$Sale_Price2>UB]<-UB
LB<-quantile(PPT$Sale_Price2,0.25)-1.5*IQR(PPT$Sale_Price2)
LB
PPT$Sale_Price2[PPT$Sale_Price2<LB]<-LB

hist(PPT$Total_Basement_Area)
desc(PPT$Total_Basement_Area)
summary(PPT$Total_Basement_Area)
describe((PPT$Total_Basement_Area))

hist(log(PPT$Lot_Size))##finished
summary(PPT$Lot_Size)
hist((PPT$Lot_Size))
describe(log(PPT$Lot_Size))
PPT$Lot_Size_log<-log(PPT$Lot_Size)

hist(PPT$BsmtFinSF1)
#PPT$BsmtFinSF1[PPT$BsmtFinSF1==0]<-.002
hist(PPT$BsmtFinSF1)
hist(sqrt(PPT$BsmtFinSF1))
PPT$BsmtFinSF1_sqrt<-sqrt(PPT$BsmtFinSF1)
PPT$BsmtFinSF1_log<-log(PPT$BsmtFinSF1)
#PPT$BsmtFinSF1_log[PPT$BsmtFinSF1<1]<-0
hist((PPT$BsmtFinSF1_log))
boxplot(PPT$BsmtFinSF1_log,main="BsmtFinSF1_log")
#PPT$Miscellaneous_Value[PPT$Miscellaneous_Value<1]<-0
#log()

hist(PPT$Remodel_Year)
describe(PPT$Remodel_Year)
hist(sqrt(PPT$Remodel_Year))

##
PPT$Grade_Living_Area_log<-log(PPT$Grade_Living_Area)
hist(sqrt(PPT$Grade_Living_Area))
describe(PPT$Grade_Living_Area)

##basement
hist(PPT$Fireplaces^2)
describe(PPT$Fireplaces^2)
barplot(table((PPT$Fireplaces)))
describe(PPT$Fireplaces)

##years
summary(PPT$Remodel_Year)
describe(PPT$Remodel_Year)
str(PPT$Remodel_Year)
dim(PPT$Remodel_Year)
A<-table(PPT$Remodel_Year)
A
PPT<-within(PPT,{
  Remodel_Year_CAT<-NA
  Remodel_Year_CAT[Remodel_Year>=1950 & Remodel_Year<=1960]<-'1'
  Remodel_Year_CAT[Remodel_Year>1960 & Remodel_Year<=1970]<-'2'
  Remodel_Year_CAT[Remodel_Year>1970 & Remodel_Year<=1980]<-'3'
  Remodel_Year_CAT[Remodel_Year>1980 & Remodel_Year<=1990]<-'4'
  Remodel_Year_CAT[Remodel_Year>1990 & Remodel_Year<=2000]<-'5'
  Remodel_Year_CAT[Remodel_Year>2000 & Remodel_Year<=2010 ]<-'6'
})

barplot(table(PPT$Brick_Veneer_Area))
PPT$Brick_Veneer_Area2<-PPT$Brick_Veneer_Area+1
hist(sqrt(PPT$Brick_Veneer_Area2))


Remodel_Year_CAT
pairs.panels(PPT[c("Sale_Price","BsmtFinSF1","Exterior_Material","Brick_Veneer_Area","Basement_Height","Fireplaces","Lot_Size","Overall_Material","Grade_Living_Area")])
fit_final2=lm(Sale_Price~Exterior_Material+Neighborhood+Remodel_Year+Construction_Year+Basement_Height+Fireplaces+Lot_Size_log+Overall_Material+Grade_Living_Area/Total_Basement_Area+BsmtFinSF1_sqrt+Exposure_Level, data = PPT)
summary(fit_final2) ###87 WIN normalizePath

#training and test

split<-sample.split(PPT,SplitRatio = 0.7)
split
train<-subset(PPT,split="TRUE")
test<-subset(PPT,split="FALSE")
train
test
pridict_train<-predict(fit_final2,train)

summary(pridict_train)

predict_test<-predict(fit_final2,test)
summary(predict_test)

#Assumption 1: Normality of error

error<-residuals(fit_final2)
error
summary(error)
hist(error,col = "blue")
boxplot(error,horizontal = T,col = "blue")
describe(error)

#Assumption 2: Linearity
plot(PPT$Sale_Price,PPT$error,main="linearity",col="blue")

#Assumption 3: Independence of error
plot(PPT$Id,PPT$error, main="Independence of error",col="blue")
# Assumption 4: Constant  of error variance X axis: predicted values; Y axis: error

plot(pridict_train,PPT$error, main="Constant  of error variance",col="blue")

#multicolinearity 

vif(fit_final2)
dim()


###last 
last_lm<-lm(Sale_Price~Lot_Extent+Garage_Size+Full_Bathroom_Above_Grade+Kitchen_Above_Grade+Rooms_Above_Grade+Fireplaces+Garage_Size+Neighborhood+House_Type+Zoning_Class+Roof_Quality+Exterior_Condition+Basement_Height+Kitchen_Quality+Exposure_Level, data=PPT)


summary(last_lm)
vif(last_lm)

fit_final3=lm(Sale_Price~Exterior_Material+Neighborhood+Remodel_Year+Construction_Year+Basement_Height+Fireplaces+Lot_Size_log+Overall_Material+Grade_Living_Area/Total_Basement_Area+BsmtFinSF1_sqrt+Exposure_Level, data = PPT)
summary(fit_final3) ###87 WIN normalizePath
 
