cs2m <- read.csv("G:/Suman/batch34/cs2m.csv", stringsAsFactors=TRUE)

grades <- read.csv("G:/Suman/batch34/grades.csv", header=FALSE, stringsAsFactors=TRUE)
t <- read.csv("G:/Suman/batch34/fwdbasicsofr/t.csv", stringsAsFactors=TRUE)
preg <- read.csv("G:/Suman/batch34/fwdbasicsofr/preg.csv", stringsAsFactors=TRUE)
str(t)
summary(t)
t_complete=t[complete.cases(t),]
summary(t_complete)
dim(t_complete)

library(dplyr)
cs2m_mutate<-mutate(cs2m,chlst_bp=Chlstrl/BP)
cs2m_mutate
cs2m_1<-cs2m
cs2m_1$chlst_bp<-cs2m_1$Chlstrl/cs2m_1$BP
j=m<-cs2m

dim(j)
#stall.packages()
library(reshape)
j=rename(j,c(Drug='Reaction',Prgnt='Pregnant'))
variable.names(j)
names(j)
names(j)[5]="Anxiety"
names(j)[2]="Cholesterol"
names(j)
cs2m_asce<-arrange(cs2m,Age)
?arrange
??arrange

library(psych)
##cs2m_asce<-arrange
apply(cs2m,1,mean)
cs2m_asce<-arrange(cs2m,Age)
cs2m_desc<-arrange(cs2m,desc(Age))
cs2m_asce
grades1<-subset(grades,select = c(quiz1,gpa,final))
grades
names(grades)
grades4<-select(grades,quiz1,gpa,final)
grades4

####
apply(cs2m, 2,sum)
apply(cs2m,1,sum)
##apply(array, margin, ...)
#average all the column by cylinder 
by(mtcars[,-2],mtcars$cyl,colMeans)

#one variable mean across categorical variable
tapply(cs2m$BP, cs2m$Prgnt, mean)
tapply(grades$gpa,grades$ethnicity,mean)
###tapply(vector, index, function)
final_60<-subset(grades,final>60)
final_60
head(final_60)
final_65<-grades[grades$final>65,]
head(final_65)
##box plot
boxplot(grades$final,main="box plot for final",col='red')
boxplot(final_65$final,main="box plot for final>65",col='purple')


#compare correlation between final and gpa
cor.test(grades$gpa,grades$final)
cor.test(final_60$gpa,final_60$final)
cs2m1<-filter(cs2m,Age>20 &Age<30)
ethinicity_white<-subset(grades,ethnicity==4)
grades$ethnicity
grades$sqrtFinal<-sqrt(grades$final)
head(grades)


##compare histogram of final nd sqrtfinal
hist(grades$final, col='blue')
hist(grades$sqrtFinal,col='red')

##convert final into two catagory 
grades$catgryfinal<-ifelse(grades$final<60, 'final<60','final>60')
head(grades)
grades$final_cat<-cut(grades$final,breaks = seq(40,75,5), labels=c('final1','final2','final3','final4','final5','final6','final7'))
head(grades)

##within

library(readr)
m=cs2m
m
m<-within(m,{
            agecat<-NA
            agecat[Age>=15 & Age<=25]<-'Low'
            agecat[Age>=26 & Age<=40]<-'Middle'
            agecat[Age>41]           <-'Hight'
  
  
})

head(m,3)
grades$cateth<-grades$ethnicity
grades$cateth[grades$cateth==1 | grades$cateth==3 | grades$cateth==5]=1
grades$cateth
grades$cateth[grades$cateth==2 | grades$cateth==4]=2
head(grades)


###Take out 20% sample ranidomly''
sam<-sample(x=1:nrow(grades),size = 0.2*nrow(grades))
grades20<-grades[sam,]
grades20
head(grades20)

#summarize

summarise(cs2m, mean_age=mean(Age,na.rm = T),median_age=median(Age,na.rm=T))


#filter 
cs2m_12<-filter(cs2m,Age>20 & Age<32)
cs2m_12$DrugR

##cs2m$Prgnt
##pipe for doing many things in one go 
df<-cs2m %>%
  filter(Prgnt==1)%>%
  select(BP,Age,DrugR)%>%
  mutate(BP_Age=BP/Age)
head(df)
df


##Knowing summary of your choice
grades %>%
   group_by(ethnicity) %>%
    summarise(avg_gpa=mean(gpa),
                avg_final=mean(final),
              avg_total=mean(total))

tapply(grades$gpa,grades$ethnicity, mean)


#average final and average gpa, gender & ethniity wise
grades%>% group_by(ethnicity,gender)%>%
    select(gpa,ethnicity,final,gender)%>%
    summarise(avggpa=mean(gpa),avgfinal=mean(final))

###wanted avg >2.25
grades%>% group_by(ethnicity,gender)%>%
  select(gpa,ethnicity,final,gender)%>%
  summarise(avggpa=mean(gpa),avgfinal=mean(final)) %>%
  filter(avggpa>2.25)

mtcars %>% group_by(cyl,gear) %>%
  select(mpg,cyl,wt,gear,am) %>%
  summarise(avgmpg=mean(mpg),
             avgwt=mean(wt))%>%
             filter(avgmpg>15)




##task write NO for not Pregnent aand Yes for pregnent

w=cs2m
w
w$Prgnt<-ifelse(w$Prgnt==1, "Yes", "No")
head(w)


