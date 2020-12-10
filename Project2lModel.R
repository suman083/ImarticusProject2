Pr_P_Tr<- read.csv("G:/Suman/batch34/project2/Datasheet/Property_Price_Train.csv", stringsAsFactors=TRUE)
View(Pr_P_Tr)
library(psych)
<<<<<<< HEAD
condtn<-table(Pr_P_Tr$Condition2)
plot(table(Pr_P_Tr$Condition2))
=======
>>>>>>> d31483e23907f8cb04cc1188c527fc6d60f40b42


#Pr_P_Tr$Heating_Quality+Pr_P_Tr$Air_Conditioning+Pr_P_Tr$Electrical_System+Pr_P_Tr$First_Floor_Area+Pr_P_Tr$Second_Floor_Area+Pr_P_Tr$Grade_Living_Area+
#Pr_P_Tr$Underground_Half_Bathroom+Pr_P_Tr$Full_Bathroom_Above_Grade+Pr_P_Tr$Kitchen_Above_Grade+Pr_P_Tr$Kitchen_Quality+Pr_P_Tr$Rooms_Above_Grade+
#Pr_P_Tr$Functional_Rate+Pr_P_Tr$Fireplaces+Pr_P_Tr$Fireplace_Quality+Pr_P_Tr$Pool_Area+Pr_P_Tr$Garage+Pr_P_Tr$Gar.+Finish+Year+Pr_P_Tr$Gar.+
#Size+Pr_P_Tr$Gar.+Quality+Pr_P_Tr$Gar.+Condition+Pr_P_Tr$Paved+Drive+Pr_P_Tr$BsmtFinSF1+Pr_P_Tr$BsmtUnfSF+Pr_P_Tr$Total_Basement_Area+Pr_P_Tr$Building_Class+Pr_P_Tr$Zoning_Class+Pr_P_Tr$Lot_Extent+Pr_P_Tr$Lot_Size+Pr_P_Tr$Property_Shape+Pr_P_Tr$Land_Outline+Pr_P_Tr$Lot_Configuration+Pr_P_Tr$Property_Slope+Pr_P_Tr$Neighborhood+Pr_P_Tr$House_Type+Pr_P_Tr$Overall_Material+
#Pr_P_Tr$House_Condition+Pr_P_Tr$Construction_Year+Pr_P_Tr$Remodel_Year+Pr_P_Tr$Roof_Design+Pr_P_Tr$Brick_Veneer_Area

model1_lm<-lm(Sale_Price~Heating_Quality+Air_Conditioning+Electrical_System+First_Floor_Area+Second_Floor_Area+Grade_Living_Area+Underground_Half_Bathroom+Full_Bathroom_Above_Grade+Kitchen_Above_Grade+Kitchen_Quality+Rooms_Above_Grade+Functional_Rate+Fireplaces+Fireplace_Quality+Pool_Area+Garage
              +Garage_Finish_Year+Garage_Quality+Garage_Built_Year+Garage_Condition+Pavedd_Drive
              +BsmtFinSF1+BsmtUnfSF+Total_Basement_Area+Building_Class+Zoning_Class+Lot_Extent+Lot_Size+Property_Shape+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+House_Type+Overall_Material+House_Condition+Construction_Year+Remodel_Year+Roof_Design+Brick_Veneer_Area
              ,data=Pr_P_Tr)
model1_lm


summary(model1_lm)
<<<<<<< HEAD

#Model2

model2_lm<-lm(Sale_Price~Heating_Quality+Air_Conditioning+First_Floor_Area+Second_Floor_Area+Grade_Living_Area+Underground_Half_Bathroom+
                Full_Bathroom_Above_Grade+Kitchen_Above_Grade+Kitchen_Quality+Rooms_Above_Grade+
                Fireplaces+Fireplace_Quality+Pool_Area+Garage
              +Garage_Finish_Year+Garage_Quality+Garage_Condition
              +BsmtUnfSF+Building_Class+Lot_Size+Property_Shape+Land_Outline+Lot_Configuration+Property_Slope+Neighborhood+
                House_Type+Overall_Material+House_Condition+Construction_Year+Brick_Veneer_Area,data=Pr_P_Tr)

summary(model2_lm)


#Model 3 with all the 
model3_lm<-lm(Sale_Price~Building_Class+Zoning_Class+Lot_Extent+Lot_Size+Road_Type+Property_Shape+Land_Outline+Utility_Type+
                Lot_Configuration+Property_Slope+Neighborhood+Condition1+Condition2+House_Type+House_Design+Overall_Material+House_Condition+
                Construction_Year+Remodel_Year+Roof_Design+Roof_Quality+Exterior1st+Exterior2nd+Brick_Veneer_Type+Brick_Veneer_Area+Exterior_Material+
                Exterior_Condition+Foundation_Type+Basement_Height+Basement_Condition+Exposure_Level+BsmtFinType1+BsmtFinSF1+BsmtFinType2+BsmtFinSF2+
                BsmtUnfSF+Total_Basement_Area+Heating_Type+Heating_Quality+Air_Conditioning+Electrical_System+First_Floor_Area+Second_Floor_Area+
                LowQualFinSF+Grade_Living_Area+Underground_Full_Bathroom+Underground_Half_Bathroom+Full_Bathroom_Above_Grade+Half_Bathroom_Above_Grade+
                Bedroom_Above_Grade+Kitchen_Above_Grade+Kitchen_Quality+Rooms_Above_Grade+Functional_Rate+Fireplaces+Fireplace_Quality+Garage+
                Garage_Built_Year+Garage_Finish_Year+Garage_Size+Garage_Area+Garage_Quality+Garage_Condition+Pavedd_Drive+W_Deck_Area+
                Open_Lobby_Area+Enclosed_Lobby_Area+Three_Season_Lobby_Area+Screen_Lobby_Area+Pool_Area+Pool_Quality+Fence_Quality+Miscellaneous_Feature+
                Miscellaneous_Value+Month_Sold+Year_Sold+Sale_Type+Sale_Condition,data=Pr_P_Tr)
summary(model3_lm)



=======
>>>>>>> d31483e23907f8cb04cc1188c527fc6d60f40b42
