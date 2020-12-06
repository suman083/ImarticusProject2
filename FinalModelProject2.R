
PPT <- read.csv("G:/Suman/batch34/project2/Datasheet/Property_Price_Train.csv", stringsAsFactors=TRUE)
str(PPT)
library(VIM)
PPT<-kNN(PPT) #NA value removed

##Column analysis

#Building_Class
par(mfrow=c(2,1))
bul_class=table(PPT$Building_Class)
barplot(bul_class)
boxplot(PPT$Building_Class,horizontal = T,col = 'blue')
#3 outliers
UB<-quantile(PPT$Building_Class,0.75)+1.5*IQR(PPT$Building_Class)
UB
PPT$Building_Class[PPT$Building_Class>UB]<-UB

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


##Test for all numeric column
pairs.panels(PPT[c("Building_Class","Lot_Extent","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Garage_Size","Garage_Area","W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area","Three_Season_Lobby_Area","Screen_Lobby_Area","Pool_Area","Miscellaneous_Value","Month_Sold","Year_Sold","Sale_Price")])

cor(PPT[c("Building_Class","Lot_Extent","Lot_Size","Overall_Material","House_Condition","Construction_Year","Remodel_Year","Brick_Veneer_Area","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","Total_Basement_Area","First_Floor_Area","Second_Floor_Area","LowQualFinSF","Grade_Living_Area","Underground_Full_Bathroom","Underground_Half_Bathroom","Full_Bathroom_Above_Grade","Half_Bathroom_Above_Grade","Bedroom_Above_Grade","Kitchen_Above_Grade","Rooms_Above_Grade","Fireplaces","Garage_Built_Year","Garage_Size","Garage_Area","W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area","Three_Season_Lobby_Area","Screen_Lobby_Area","Pool_Area","Miscellaneous_Value","Month_Sold","Year_Sold","Sale_Price")])
