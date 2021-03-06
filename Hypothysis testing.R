cs2m <- read.csv("G:/Suman/batch34/cs2m.csv", stringsAsFactors=TRUE)
t.test(cs2m$Age, mu=40,conf.level = .80)
t.test(cs2m$Age, mu=40)
#mean =45
t.test(cs2m$Age,mu=45)

##bp=173
t.test(cs2m$BP,mu=173)

##pair sample t test
grades <- read.csv("G:/Suman/batch34/grades.csv", header=T, stringsAsFactors=TRUE)
grades$quiz1
t.test(grades$quiz1,grades$quiz2,paired = T)
t.test(grades$quiz1,grades$quiz5,paired = T)
t.test(grades$quiz2,grades$quiz3,paired = T)

#independent sample t test 
t.test(cs2m$BP~cs2m$AnxtyLH)##assuming unequal variance
t.test(cs2m$BP~cs2m$AnxtyLH,var.equal=T)##assuming unequal variance


t.test(cs2m$BP~cs2m$DrugR)

##One sample proportion test 
prop.test(50,130,p=0.10,alternative = "two.sided",conf.level = 0.95,correct = F)

##ho-> p=.30

##two proportion test 
prop.test(c(10,22),c(40,50),alternative="two.sided",conf.level = 0.95,correct = F)

##Ho-> p1-p2=0

##one way ANOVA 





































































 
