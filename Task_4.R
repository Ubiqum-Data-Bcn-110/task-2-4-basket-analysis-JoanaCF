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
### WM: In asMethod(object) : removing duplicated items in transactions - 

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
sizetransactions <- size(ElectronidexTransactions)
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

#### 3. Applying the algorithm // unclean dataset ####

apriorirules_1 <- apriori(ElectronidexTransactions, parameter = list(supp=0.1, conf=0.8))
inspect(apriorirules_1)
### C: Nothing

apriorirules_2 <- apriori(ElectronidexTransactions, parameter = list(supp=0.1, conf=0.2))
inspect(apriorirules_2)
### C: iMac -   lhs    rhs    support   confidence lift count
### [1] {}  => {iMac} 0.2561261 0.2561261  1    2519 

apriorirules_3 <- apriori(ElectronidexTransactions, parameter = list(supp=0.05, conf=0.2))
inspect(apriorirules_3)
### C: 9 rules: dell desktop -> iMac; iMac -> Dell desktop; CYBERPOWER Gamer Desktop -> iMac; Lenovo Desktop Computer -> iMac; iMac -> Lenovo Desktop Computer; HP Laptop -> iMac; iMac -> HP Laptop
### D: que significa tener las mismas regras pero al reves?

apriorirules_4 <- apriori(ElectronidexTransactions, parameter = list(conf=0.1))
inspect(apriorirules_4)
### D: que significa ser () => algo

apriorirules_5 <- apriori(ElectronidexTransactions, parameter = list(supp=0.05))
inspect(apriorirules_5)
### D: porque no sale nada?

apriorirules_6 <- apriori(ElectronidexTransactions, parameter = list(supp=0.07, conf=0.2))
inspect(apriorirules_6)
### C: no anade nada

apriorirules_7 <- apriori(ElectronidexTransactions, parameter = list(supp=0.04, conf=0.2))
inspect(apriorirules_7)
### C: 19 rules

apriorirules_8 <- apriori(ElectronidexTransactions, parameter = list(supp=0.02, conf=0.2))
inspect(apriorirules_8)
### C: 86 rules; also have count for each rule

apriorirules_9 <- apriori(ElectronidexTransactions, parameter = list(supp=0.02))
inspect(apriorirules_9)
### D/C: Nothing

apriorirules_10 <- apriori(ElectronidexTransactions, parameter = list(conf=0.02))
inspect(apriorirules_10)
### D/C: I don't understand this output / got it support = confidence

apriorirules_11 <- apriori(ElectronidexTransactions, parameter = list(supp=0.01, conf=0.2))
inspect(apriorirules_11)
### D/C: 142 rules; also have count for each rule

apriorirules_12 <- apriori(ElectronidexTransactions, parameter = list(supp=0.07, conf=0.15))
inspect(apriorirules_12)
### D/C: 7 rules / solo dos especifica entre iMAC y HP Laptop

apriorirules_13 <- apriori(ElectronidexTransactions, parameter = list(supp=0.07, conf=0.1))
inspect(apriorirules_13)
### C: same as before - 12

apriorirules_13 <- apriori(ElectronidexTransactions, parameter = list(supp=0.07, conf=0.1))
inspect(apriorirules_13)
### D/C: 7 rules / solo dos especifica entre iMAC y HP Laptop

apriorirules_14 <- apriori(ElectronidexTransactions, parameter = list(supp=0.05, conf=0.1))
inspect(apriorirules_14)
### D/C: 18 rules / similar to rules 3

apriorirules_15 <- apriori(ElectronidexTransactions, parameter = list(supp=0.02, conf=0.1))
inspect(apriorirules_15)

apriorirules_16 <- apriori(ElectronidexTransactions, parameter = list(supp=0.02, conf=0.5))
inspect(apriorirules_16)
### C: only 1 rule confidence =0.5 - hp laptop, lenovo desktop computer and imac 

apriorirules_17 <- apriori(ElectronidexTransactions, parameter = list(supp=0.02, conf=0.4))
inspect(apriorirules_17)
### C: 4 rules with lift >2 and confidence > 0.4 
### C: best one: Viewsonic, imac -> hp laptop

apriorirules_18 <- apriori(ElectronidexTransactions, parameter = list(supp=0.02, conf=0.3))
inspect(apriorirules_18)
### C: 4 rules with lift >2 and confidence > 0.4 
### C: best one: Viewsonic, imac -> hp laptop

??apriori

rules <- apriori(ElectronidexTransactions, 
                 parameter = list(supp = 0.01, conf = 0.3, target = "rules"))
summary(rules)
### C: support max: 0.0755, confidence max: 0.6023, lift max: 3.36

rules1 <- apriori(ElectronidexTransactions, 
                 parameter = list(supp = 0.01, conf = 0.3, target = "rules"))
summary(rules1)
### C: Maximum amounts are the same as in rules

#### 4. Cleaning dataset ####

sizetransactions <- size(ElectronidexTransactions)
sum(sizetransactions == "0")
which(sizetransactions == "0")
### C: transaction 8707 and 9506 tiene 0 productos
hist(sizetransactions)


sum(sizetransactions == "1")
which(sizetransactions == "1")
### C: 2163 transactions only have one product

ElectronidexTransactions_0 <- ElectronidexTransactions[sizetransactions == 0]
summary(ElectronidexTransactions_0)
hist(ElectronidexTransactions_0)

ElectronidexTransactions_1 <- ElectronidexTransactions[sizetransactions == 1]
summary(ElectronidexTransactions_1)
hist(ElectronidexTransactions_1)
### C: Most ferquent products that were bought soo: Apple MacBook Air(383), Apple Earpods (156), iMac (121) and CYBERPOWER Gamer Desktop (109) 

ElectronidexTransactions_new <- ElectronidexTransactions[!size(ElectronidexTransactions)== "0"]
summary(ElectronidexTransactions_new)

ElectronidexTransactions_new_2 <- ElectronidexTransactions_new[!size(ElectronidexTransactions_new)== "1"]
summary(ElectronidexTransactions_new_2)
### C: 7670 rows =  9835 - 2163 - 2

ElectronidexTransactions_clean <- ElectronidexTransactions_new_2

### P: Clean dataset is ElectronidexTransactions_clean

#### 5. Create Product type categories ####
str(ElectronidexTransactions_clean)
Product_type <- ElectronidexTransactions_clean@itemInfo$labels
ElectronidexTransactions_clean@itemInfo$Product_type <- Product_type
str(ElectronidexTransactions_clean)

## 5.1 External Hardrives
grep("Hard Drive", Product_type)
Product_type[grep("Hard Drive", Product_type)]
Product_type[grep("Hard Drive", Product_type)] <- "External Hardrives" 
Product_type
sum(Product_type == "External Hardrives")

## 5.2 Computer Stands
grep("Stand", Product_type)
Product_type[grep("Stand", Product_type)] <- "Computer Stands"
Product_type
grep("Mount", Product_type)
Product_type[grep("Mount", Product_type)] <- "Computer Stands"
Product_type
sum(Product_type == "Computer Stands")

## 5.3 Computer Tablets
grep("iPad", Product_type)
Product_type[grep("iPad", Product_type)] <- "Computer Tablets"
Product_type
grep("HD Tablet", Product_type)
Product_type[grep("HD Tablet", Product_type)] <- "Computer Tablets"
Product_type
grep("Galaxy Tab", Product_type)
Product_type[grep("Galaxy Tab", Product_type)] <- "Computer Tablets"
Product_type
grep("Kindle", Product_type)
Product_type[grep("Kindle", Product_type)] <- "Computer Tablets"
Product_type
sum(Product_type == "Computer Tablets")

## 5.4 Smart Home Devices
grep("Apple TV", Product_type)
Product_type[grep("Apple TV", Product_type)] <- "Smart Home Devices"
Product_type

grep("Google Home", Product_type)
Product_type[grep("Google Home", Product_type)] <- "Smart Home Devices"
Product_type

grep("Smart Light Bulb", Product_type)
Product_type[grep("Smart Light Bulb", Product_type)] <- "Smart Home Devices"
Product_type

grep("Fire TV Stick", Product_type)
Product_type[grep("Fire TV Stick", Product_type)] <- "Smart Home Devices"
Product_type

grep("Roku Express", Product_type)
Product_type[grep("Roku Express", Product_type)] <- "Smart Home Devices"
Product_type

## 5.1.5 Printer Ink
grep("Ink", Product_type)
Product_type[grep("Ink", Product_type)] <- "Printer Ink"
Product_type

grep("Toner", Product_type)
Product_type[grep("Toner", Product_type)] <- "Printer Ink"
Product_type

grep("Labeling Tape", Product_type)
Product_type[grep("Labeling Tape", Product_type)] <- "Printer Ink"
Product_type

sum(Product_type == "Printer Ink")
### C: 5 printer ink

## 5.5 Printers
grep("Epson Printer", Product_type)
Product_type[grep("Epson Printer", Product_type)] <- "Printers"

grep("HP Wireless Printer", Product_type)
Product_type[grep("HP Wireless Printer", Product_type)] <- "Printers"

grep("Canon Office Printer", Product_type)
Product_type[grep("Canon Office Printer", Product_type)] <- "Printers"

grep("Brother Printer", Product_type)
Product_type[grep("Brother Printer", Product_type)] <- "Printers"

grep("DYMO Label Manker", Product_type)
Product_type[grep("DYMO Label Manker", Product_type)] <- "Printers"
Product_type
sum(Product_type == "Printers")

## 5.6 Speakers
grep("Speaker", Product_type)
Product_type[grep("Speaker", Product_type)] <- "Speakers"
Product_type
sum(Product_type == "Speakers")

grep("DOSS Touch", Product_type)
Product_type[grep("DOSS Touch", Product_type)] <- "Speakers"
sum(Product_type == "Speakers")

grep("Cyber Acoustics", Product_type)
Product_type[grep("Cyber Acoustics", Product_type)] <- "Speakers"
sum(Product_type == "Speakers")

grep("Sonos", Product_type)
Product_type[grep("Sonos", Product_type)] <- "Speakers"
sum(Product_type == "Speakers")
Product_type

## 5.7 Laptops
grep("Laptop", Product_type)
Product_type[grep("Laptop", Product_type)] <- "Laptops"
Product_type
sum(Product_type == "Laptops")

grep("Acer Aspire", Product_type)
Product_type[grep("Acer Aspire", Product_type)] <- "Laptops"

grep("ASUS Chromebook", Product_type)
Product_type[grep("ASUS Chromebook", Product_type)] <- "Laptops"

grep("Apple MacBook Pro", Product_type)
Product_type[grep("Apple MacBook Pro", Product_type)] <- "Laptops"

grep("Apple MacBook Air", Product_type)
Product_type[grep("Apple MacBook Air", Product_type)] <- "Laptops"

## 5.8 Mouse and Keyboard Combo
grep("Combo", Product_type)
Product_type[grep("Combo", Product_type)] <- "Mouse and Keyboard Combo"
sum(Product_type == "Mouse and Keyboard Combo")

grep("Keyboard & Mouse", Product_type)
Product_type[grep("Keyboard & Mouse", Product_type)] <- "Mouse and Keyboard Combo"
sum(Product_type == "Mouse and Keyboard Combo")

grep("Keyboard and Mouse", Product_type)
Product_type[grep("Keyboard and Mouse", Product_type)] <- "Mouse and Keyboard Combo"
sum(Product_type == "Mouse and Keyboard Combo")

## 5.9 Desktops
grep("Desktops", Product_type)
Product_type[grep("Desktop", Product_type)] <- "Desktops"
sum(Product_type == "Desktops")

grep("iMac", Product_type)
Product_type[grep("iMac", Product_type)] <- "Desktops"
sum(Product_type == "Desktops")

## 5.10 Computer Cords
grep("Cable", Product_type)
Product_type[grep("Cable", Product_type)] <- "Computer Cords"
sum(Product_type == "Computer Cords")

grep("HDMI Adapter", Product_type)
Product_type[grep("HDMI Adapter", Product_type)] <- "Computer Cords"
sum(Product_type == "Computer Cords")

## 5.11 Monitors
grep("Monitor", Product_type)
Product_type[grep("Monitor", Product_type)] <- "Monitors"
sum(Product_type == "Monitors")

## 5.12 Accessories
grep("Microsoft Office Home and Student 2016", Product_type)
Product_type[grep("Microsoft Office Home and Student 2016", Product_type)] <- "Accessories"
sum(Product_type == "Accessories")

grep("Computer Game", Product_type)
Product_type[grep("Computer Game", Product_type)] <- "Accessories"

grep("Mouse Pad", Product_type)
Product_type[grep("Mouse Pad", Product_type)] <- "Accessories"

## 5.13 Active Headphones
grep("Apple Earpods", Product_type)
Product_type[grep("Apple Earpods", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Monster Beats By Dr Dre", Product_type)
Product_type[grep("Monster Beats By Dr Dre", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Panasonic In-Ear Headphone", Product_type)
Product_type[grep("Panasonic In-Ear Headphone", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Otium", Product_type)
Product_type[grep("Otium", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("APIE", Product_type)
Product_type[grep("APIE", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Philips", Product_type)
Product_type[grep("Philips", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

## 5.14 Computer Headphones
grep("Headset", Product_type)
Product_type[grep("Headset", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Panasonic", Product_type)
Product_type[grep("Panasonic", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Kensington Headphones", Product_type)
Product_type[grep("Kensington Headphones", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Koss Home Headphones", Product_type)
Product_type[grep("Koss Home Headphones", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Ailihen Stereo Headphones", Product_type)
Product_type[grep("Ailihen Stereo Headphones", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

Product_type

## 5.15 Computer Mice
grep("3-Button Mouse", Product_type)
Product_type[grep("3-Button Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Logitech Wireless Mouse", Product_type)
Product_type[grep("Logitech Wireless Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Microsoft Basic Optical Mouse", Product_type)
Product_type[grep("Microsoft Basic Optical Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Logitech 3-button Mouse", Product_type)
Product_type[grep("Logitech 3-button Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Redragon Gaming Mouse", Product_type)
Product_type[grep("Redragon Gaming Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("HP Wireless Mouse", Product_type)
Product_type[grep("HP Wireless Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Generic Black 3-Button", Product_type)
Product_type[grep("Generic Black 3-Button", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Wireless Portable Mouse", Product_type)
Product_type[grep("Wireless Portable Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Gaming Mouse Professional", Product_type)
Product_type[grep("Gaming Mouse Professional", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Slim Wireless Mouse", Product_type)
Product_type[grep("Slim Wireless Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

Product_type

## 5.16 Keyboards

grep("LED", Product_type)
Product_type[grep("LED", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Wireless", Product_type)
Product_type[grep("Wireless", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Magic", Product_type)
Product_type[grep("Magic", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Wired", Product_type)
Product_type[grep("Wired", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Logitech Keyboard", Product_type)
Product_type[grep("Logitech Keyboard", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("HP USB Keyboard", Product_type)
Product_type[grep("HP USB Keyboard", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

Product_type
ElectronidexTransactions_clean@itemInfo$Product_type <- Product_type
str(ElectronidexTransactions_clean)

#### 6. Create brand categories ####

Product_brand <-ElectronidexTransactions_clean@itemInfo$labels
Product_brand

## 6.1 Apple
grep("Apple", Product_brand)
Product_brand[grep("Apple", Product_brand)] <- "Apple"
sum(Product_brand == "Apple")

grep("iPad", Product_brand)
Product_brand[grep("iPad", Product_brand)] <- "Apple"
sum(Product_brand == "Apple")

grep("iPhone", Product_brand)
Product_brand[grep("iPhone", Product_brand)] <- "Apple"
sum(Product_brand == "Apple")

grep("iMac", Product_brand)
Product_brand[grep("iMac", Product_brand)] <- "Apple"
sum(Product_brand == "Apple")

### C: 11 Apple products

## 6.2 Logitech

## 6.3 Lenovo

## 6.4 ASUS

## 6.5 Acer

## 6.6 HP

## 6.7 Samsung

## 6.8 ViewSonic

## 6.9 Microsoft

## 6.10 Others

## 6.11 Rii

Panasonic
DYMO
Canon
Epson










#### 7. Apply model ####

Rules_1 <- apriori(ElectronidexTransactions_clean, parameter = list(supp=0.01, conf=0.01, minlen = 2 ))
inspect(Rules_1)
summary(Rules_1)

install.packages("arulesViz")
library(arulesViz)
install.packages("grid")
plot(Rules_1)

ElectronidexTransactions_clean@itemInfo$labels 
