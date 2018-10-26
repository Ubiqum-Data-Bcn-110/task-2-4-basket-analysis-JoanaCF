#### TASK 4 ####

## Name: Discover association between products
## Team: Kim and Joana
## Start date: 26 Oct
## Deadline: 31 Oct

#### Description ####

## General goal: Understand if Electronidex would be an optimal acquisition. 
## Tasks: Identify purchasing patterns that will provide insight into Electronidex's clientele.
## Questions: 

# a) Are there any interesting patterns or item relationships within Electronidex's transactions?
# b) Would Blackwell benefit from selling any of Electronidex's items?
# c) In your opinion, should Blackwell acquire Electronidex?
# d) If Blackwell does acquire Electronidex, do you have any recommendations for Blackwell? (Ex: cross-selling items, sale promotions, should they remove items, etc.)

#### Process ####

#### 0. Get to know data ####

## 0.1 Datasets shares

# a) ElectronidexTransactions - record of one month’s (30 days’ worth) of 9835 online transactions and which items were purchased out of the 125 products Electronidex sells. 
# b) ElectronidexItems -  list of the 125 products that Electronidex sells broken down into 17 product types.

#### 1. Install packages and import data ####

## 1.1 Clean Envirnoment
rm(list = ls())

## 1.2 Install packages
install.packages("arules")
library(arules)
??arules
# arules, a package for analyzing transactional data

install.packages("arulesViz")
library(arulesViz)
install.packages("TSP")
install.packages("caTools")
install.packages("whisker")
install.packages("prabclus")
install.packages("trimcluster")
library(arulesViz)
??arulesViz
# arulesViz, a package that provides visual techniques for the arules package. It has required the installation of otherpackages

## 1.3 Upload data
ElectronidexTransactions <- read.transactions("Desktop/UBIQUM/2. Tasks/Course 2/Task 4/Data/ElectronidexTransactions2017.csv", 
                                              format = "basket", sep=",", rm.duplicates=FALSE)
### WM: incomplete final line found on ... - OK!
### WM: In asMethod(object) : removing duplicated items in transactions - What is it?
??read.transactions

View(ElectronidexTransactions)
str(ElectronidexTransactions)
LIST(ElectronidexTransactions)
### P - shows the different items that are in the basket
inspect(ElectronidexTransactions)
### P - shows composition of each basket
itemLabels(ElectronidexTransactions)
### P - confirm the name of colums
size(ElectronidexTransactions)
### P - size of each basket
itemInfo(ElectronidexTransactions)
### P - list of items

itemFrequency(ElectronidexTransactions, type = "relative")
itemFrequency(ElectronidexTransactions, type = "absolute")
sum(itemFrequency(ElectronidexTransactions))
sum(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the total number of items bought is 43103

max(itemFrequency(ElectronidexTransactions))
### D/C: the most frequent item has a relative frequency/support of 0.25  (- its the imac)
max(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the most frequent item has been bought 2519 times (not the same as the number of transactions)
### Q: How can I know what is it? 
### Q: how do I know in know many transactions it has been purchased?

min(itemFrequency(ElectronidexTransactions))
### D/C: the least frequent item has a relative frequency of 0.0022 - Logitech MK270 Wireless Keyboard and Mouse Combo 
min(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the least frequent item has been bought 22  times 
mean(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the average number of purchases of products is 344.832 / 345
### Q: How can I reffer to a given basket?
??itemFrequency

itemFrequencyPlot(ElectronidexTransactions, topN =10)
itemFrequencyPlot(ElectronidexTransactions, topN =5)
??itemFrequencyPlot
### C: the products with the highest frequency are: iMac, HP Laptop, CYBERPOWER Gamer Desktop, Apple Earpods, Apple MacBook Air. 

image(ElectronidexTransactions)
??image

image(sample(ElectronidexTransactions, 50))
### C: There seems to be a gap around items nr 50 and 100;
### C: there seems to ve a concentration around items 20 and 120. 

image(sample(ElectronidexTransactions, 150))
### C: high frequency around items nr 30, 60 and 120/5
### C: blue ocean between 60 ans 120

image(sample(ElectronidexTransactions, 200))
### C: high frequency around items nr 20, 30,40, 60,80, 100 and 120/5



## to be run:
install.packages("moosaic")
library(mosaic)
plotCumfreq(ElectronidexTransactions)

## to do : 
# itemfrequencyplot - a way to plot using certain metrics? Which plots might provide the most insight?
# sacar un grafico de frecuencias por item set



