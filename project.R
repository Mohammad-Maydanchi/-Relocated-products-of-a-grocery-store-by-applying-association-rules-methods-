#Data Preprocessing 

#install.packages("arules") 
library(arules)
dataset=read.csv('koorosh_Gilan_Basket.csv', header=FALSE) #ulpoad the dataset

#number of transactions 
n=nrow(dataset)


# create a spares matrix and remove duplicates
dataset=read.transactions('koorosh_Gilan_Basket.csv', sep=',', rm.duplicates=TRUE) #a sparse matrix
### 1,5 means we have 5 transations with 1 duplicates
summary(dataset)
#frequency
itemFrequencyPlot(dataset,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'),main="Absolute Item Frequency Plot")

#Training Apriori on the dataset
##minimum support: for minimum support
##we should set favor frequent number for buying a product in a day
p=7   #period of collecting the data
f=3   #favor frequent in a day for each product
mins=f*p/n #minimum support
###finding rules with length=3
rules=apriori(data=dataset,
              parameter = list(support=mins ,
                               confidence=0.4,target = "rules",
                               maxlen = 3, minlen =3))




insp3=inspect(sort(rules, by='lift'))

s=sort(rules, by='lift')

insp22=inspect(sort(rules, by='lift')[1:4]) #first 2 strongest rules


plot(rules)
quality(rules)
subrules=rules[quality(rules)$confidence >0.4]
## Two-key plot is a scatterplot with shading = "order"
plot(rules, shading="lift", control=list(main = "Two-key plot", col=rainbow(5)))



#export data to csv file
c3=write.csv(insp22,"Rules_length3.csv")
###

##extra Visualizing
library('arulesViz')
itemFrequency(data)







subrules
inspect(subrules)
plot(subrules)
plot(subrules, method = "matrix", measure = "lift")
plot(subrules,method = "graph")
itemFrequencyPlot(dataset,topN=10,type="absolute")
install.packages('rattle')
library(rattle)
install.packages('RColorBrewer')
library(RColorBrewer)
itemFrequencyPlot(dataset,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")
## Two-key plot is a scatterplot with shading = "order"
plot(subrules, shading="lift", control=list(main = "Two-key plot", col=rainbow(5)))
##
plot(s, method = "graph",  engine = "htmlwidget")
