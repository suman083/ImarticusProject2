Pr_P_Tr<- read.csv("G:/Suman/batch34/project2/Datasheet/Property_Price_Train.csv", stringsAsFactors=TRUE)
View(Pr_P_Tr)
library(psych)


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
