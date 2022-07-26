install.packages("ggstatsplot")

require(ggstatsplot)
?movies_long
View(movies_long)

attach(movies_long)

#===========================================
#============Group Differences==============
#===========================================

#---------------------------------
#One sample t-test
t.test(budget, mu=30)
gghistostats(movies_long,
             x=budget,
             test.value = 30)

gghistostats(movies_long,
             x=budget,
             test.value = 30,
             type = "np")

#type = "p" or OMITTED for One sample t-Test
#type = "np" for Wilcoxin Test
#---------------------------------


#Two indep sample t-test
summary(budget)
movies_long$bud=ifelse(budget>18,1,0)

t.test(rating~movies_long$bud, 
       var.equal=F)

ggbetweenstats(data=movies_long,
               x=bud,
               y=rating,
               type = "p",
               var.equal = T,
               pairwise.display = "s")

#---------------------------------


#Welch test
ggbetweenstats(data=movies_long,
               x=bud,
               y=rating,
               type = "p",
               var.equal = F,
               pairwise.display = "s")

#---------------------------------


#Mann Whitney U-Test
ggbetweenstats(data=movies_long,
               x=bud,
               y=rating,
               type = "np",
               var.equal = F,
               pairwise.display = "s")

wilcox.test(rating~movies_long$bud)


#---------------------------------
#One-way ANOVA
tapply(rating, mpaa, mean)
a=aov(rating~mpaa)
summary(a)
t=TukeyHSD(a)
t
plot(t)


install.packages("rstantools")
library(rstantools)

ggbetweenstats(data=movies_long,
               x=mpaa,
               y=rating,
               type = "p",
               var.equal = T,
               pairwise.display = "s",
outlier.tagging = T,
outlier.label = title)

#---------------------------------


#Welch-test
ggbetweenstats(data=movies_long,
               x=mpaa,
               y=rating,
               type = "p",
               var.equal = F,
               pairwise.display = "s",
               plot.type = "box")

#---------------------------------


#Kruskal Wallis H-test
kruskal.test(rating~mpaa)

ggbetweenstats(data=movies_long,
               x=mpaa,
               y=rating,
               type = "np",
               var.equal = F,
               pairwise.display = "s",
               plot.type = "box",
outlier.tagging = T,
outlier.label = title)



#===========================================
#=============Correlation================
#===========================================

#---------------------------------
#Correlation between 2 variable
cor(budget,rating)
cor.test(budget,rating)
plot(budget,rating)
install.packages("ggside")
library(ggside)


ggscatterstats(movies_long,
               x=budget,
               y=rating,
               type = "p")
#type = "p" for Karl Pearson Correlation
#type = "np" for Spearman Correlation


#---------------------------------
#Correlation between all vars of a file
attach(mtcars)
round(cor(mtcars),2)
ggcorrmat(mtcars)


require(dplyr)
attach(mtcars)
View(mtcars)
m=mtcars%>%select(mpg,wt,hp,drat,qsec)

x=ggcorrmat(
  data=m,
  type = "bayes",
  output = "dataframe")
x
x[,-c(4,7:14)]




#===========================================
#=========Regression Coefficient================
#===========================================

x=lm(rating~mpaa)
summary(mpaa)

ggcoefstats(x)




#===========================================
#============Categorical Vars=================
#===========================================

#Association between Categorical Vars

nrow(movies_long)
pie(table(genre))
d=subset(movies_long,
         genre=="Drama")
nrow(d)
t=table(d$mpaa)
t
pie(t)
round(t/428,2)
chisq.test(t)

pie(t, init.angle = 90,
    clockwise = T,
    labels = c(round(t/428,2)))


ggpiestats(
  data=d ,
  x = mpaa,
  y = genre,
  label = "both")

table(genre)
#Two genres
data2=subset(movies_long, 
             genre=="Drama" | genre=="Comedy" | genre=="Action")

ggpiestats(
  data = data2,
  x = mpaa,
  y = genre)

#Labels- Count & Percent
ggpiestats(
  data = data2,
  x = mpaa,
  y = genre,
  label = "both")


#--------------------------------------------
#Association between Categorical Vars
b=barplot(table(d$mpaa), las=1, ylim = c(0,400))
b
text(b,table(d$mpaa)+20,table(d$mpaa))

ggbarstats(data = data2,
  x = mpaa,
  y = genre,
  label = "both")



#===========================================
#============Grouped Variants=================
#===========================================

m0=subset(mtcars, am==0)
nrow(m0)
table(as.factor(m0$cyl))/19
pie(table(as.factor(m0$cyl)))

m1=subset(mtcars, am==1)
nrow(m1)
table(as.factor(m1$cyl))/13
pie(table(as.factor(m1$cyl)))


grouped_ggpiestats(data=mtcars,
                   x=cyl,
                   grouping.var = am)

grouped_gghistostats(mtcars,
                     x=mpg,
                     grouping.var = am)

grouped_ggscatterstats(mtcars,
                   x=hp, y=mpg,
                   grouping.var = vs)



#===========================================
#=============Repeat Measure================
#===========================================

#Repeated Measure Equivalent
require(datarium)
data("selfesteem")
head(selfesteem, 3)

self= selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(self, 3)

a= anova_test(data = self, 
              dv = score, 
              wid = id, 
              within = time)

get_anova_table(a)

require(WRS2)
View(WineTasting)
head(WineTasting)
rmanova(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster)
attach(WineTasting)
tapply(Taste, Wine, mean)



ggwithinstats(data = WineTasting,
              x = Wine,
              y = Taste)

