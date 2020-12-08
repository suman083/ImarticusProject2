## column 31  Basement Height  ### NA 37 fixed
train_set <- read.csv("G:/Suman/batch34/project2/Datasheet/Property_Price_Train.csv", stringsAsFactors=TRUE)

v=table(train_set$Basement_Height)
v
barplot(v,
        col = c('blue'),
        xlab = 'Height of the Basement ',
        ylab='Counts',
        main='Barplot of  Basement Height ')
str(train_set$Basement_Height)

### replacing NA to No Basement
levels <- levels(train_set$Basement_Height)
str(train_set$Basement_Height)

levels[length(levels) + 1] <- "No Basement"

train_set$Basement_Height <- factor(train_set$Basement_Height, levels = levels)
train_set$Basement_Height[is.na(train_set$Basement_Height)] <- "No Basement"
View(train_set)
str(train_set$Basement_Height)

v=table(train_set$Basement_Height)
v
barplot(v,
        col = c('red'),
        xlab = 'Height of the Basement ',
        ylab='Counts',
        main='Barplot of  Basement Height ')
ggplot(data=train_set)+
  geom_point(mapping=aes(x=Id,y=sqrtSP,
                         color=Basement_Height))
