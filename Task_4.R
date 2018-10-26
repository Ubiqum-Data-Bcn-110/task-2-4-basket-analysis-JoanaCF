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

#### 2. Exploring dataset ####

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

sum(size(ElectronidexTransactions))
### C/D: It seems that in a month, the company has sold 43104 items
max(size(ElectronidexTransactions))
### C/D: It seems that the maximum number of items bought per transaction is 30

itemFrequency(ElectronidexTransactions, type = "relative")
### P - the relative frequency of each item
itemFrequency(ElectronidexTransactions, type = "absolute")
### P - the absolute frequency of each item
#### C: in that month, 2519 iMacs have been boight
#### C: in that month, 1909 HP Laptops have been bought
#### C: in that month, 1809 CYBER Gamer Desktop have been bought


sum(itemFrequency(ElectronidexTransactions, type = "relative"))
### D/C: why isn't this 1?
sum(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the total number of items bought is 43104

max(itemFrequency(ElectronidexTransactions))
### D/C: the most frequent item has a relative frequency/support of 0.25  (- its the imac)
max(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the most frequent item has been bought 2519 times // this is not the same as the number of transactions

### Q: How can I know what product is it? 
### Q: how do I know in know many transactions it has been purchased?

min(itemFrequency(ElectronidexTransactions))
### D/C: the least frequent item bougtht has a relative frequency of 0.0022 and is Logitech MK270 Wireless Keyboard and Mouse Combo 
min(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the least frequent item bought has been purchased 22  times 
mean(itemFrequency(ElectronidexTransactions, type = "absolute"))
### D/C: the average number of purchases of products is 344.832 / 345

### Q: How can I reffer to a given basket?

??itemFrequencyPlot
itemFrequencyPlot(ElectronidexTransactions, topN =10)
itemFrequencyPlot(ElectronidexTransactions, topN =5)
### C: the products with the highest frequency are: iMac, HP Laptop, CYBERPOWER Gamer Desktop, Apple Earpods, Apple MacBook Air. 

??image
image(sample(ElectronidexTransactions, 50))
### C: There seems to be a gap around items nr 50 and 100;
### C: there seems to ve a concentration around items 20 and 120. 

image(sample(ElectronidexTransactions, 150))
### C: high frequency around items nr 30, 60 and 120/5
### C: blue ocean between 60 ans 120

image(sample(ElectronidexTransactions, 200))
### C: high frequency around items nr 20, 30,40, 60,80, 100 and 120/5
dev.off()
### E: faced an error when tried to rerun this. To fix it, I have run: dev.off() 

itemFrequency(ElectronidexTransactions[, 118:125] )
itemFrequency(ElectronidexTransactions[, 122] )
### C: iMac is item 122

### C: A transaction near transaction 150 seems to have a lot of items
sum(itemFrequency(ElectronidexTransactions[150,], type="absolute" ))
### C: item 153 hasonly been bought twice
sum(itemFrequency(ElectronidexTransactions[149,], type="absolute" ))
### C: item 153 hasonly been bought 6 times
sum(itemFrequency(ElectronidexTransactions[148,],type="absolute" ))
### C: item 153 hasonly been bought 4 times
sum(itemFrequency(ElectronidexTransactions[151,], type="absolute" ))
### C: item 153 hasonly been bought 6 times
sum(itemFrequency(ElectronidexTransactions[152,], type="absolute" ))
### C: item 153 hasonly been bought 7 times
sum(itemFrequency(ElectronidexTransactions[153,], type="absolute" ))
### C: item 153 hasonly been bought once

### C: there seems to be an item around 20 that have been bought several times 
itemFrequency(ElectronidexTransactions[, 18:25] )
### C: item 18 and 19 are also from Apple and have high relative frequencies - Apple MacBook Air and Apple MacBook Pro, 0.16 and 0.11, respectively

itemFrequency(ElectronidexTransactions[, 55:65] )
### C: item 61 - HP Laptop also has a high relative frequencies - HP Laptop, 0.19

itemFrequencyPlot(ElectronidexTransactions, support = 0.2)
### C: Only iMac = > relative frequency 0.2 

itemFrequencyPlot(ElectronidexTransactions, support = 0.19)
### C: Only iMac and Hp Laptop = > relative frequency 0.19

itemFrequencyPlot(ElectronidexTransactions, support = 0.18)
### C: Only iMac, Hp Laptop and CYBERPOWER Gamer Desktop = > relative frequency 0.18

image(sample(ElectronidexTransactions,100))

## to be run:
install.packages("moosaic")
library(mosaic)
plotCumfreq(ElectronidexTransactions)

## to do : 
# itemfrequencyplot - a way to plot using certain metrics? Which plots might provide the most insight?
# sacar un grafico de frecuencias por item set
# image - After plotting your visualizations, do you notice any patterns? Or have any observations? Take notes on your insights and observations, which might be useful to include in your formal report.



