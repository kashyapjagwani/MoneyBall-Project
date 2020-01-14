#Reading the Batting.csv file
batting <- read.csv('Batting.csv')

#Create a new column called 'BA' which is calculated as H/AB
batting$BA <- batting$H / batting$AB

#Create a new column called 'OBP' which is calculated as
#(H+BB+HBP)/(AB+BB+HBP+SF)
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

#Create a new column called 'X1B' which is calculated as H-2B-3B-HR
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

#Create a new column called 'SLG' which is calculated as
#((1B)+(2*2B)+(3*3B)+(4*HR))/AB
batting$SLG <- ((batting$X1B) + (2*batting$X2B) + (3*batting$X3B) + (4*batting$HR)) / batting$AB

#Read the Salaries.csv file
salaries <- read.csv('Salaries.csv')

#Now, since our batting data contains information from the year 1871, but our salaries data contains information from the year 1985, meaning we need to remove the batting data that occured before 1985
batting.post1985 <- subset(batting, batting$yearID > 1984)

#Now merge the batting.post1985 & salaries data on columns 'playerID' & 'yearID'
combo <- merge(batting.post1985,salaries,by = c('playerID','yearID'))

#Now select the 3 lost players from the combo data
lost.players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01'))

#Since all these players were lost after 2001 in the offseason, let's only concern ourselves with the data from 2001
lost.players <- subset(lost.players,yearID == 2001)

#We are only concerned with the following columns
lost.players <- subset(lost.players, select = c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB'))

#Now we need to find 3 replacement players from the current squad, which can be replaced by the above 3 lost players
#We can make use of the 'combo' data frame to find 3 replacement players with yearID == 2001
available.players <- subset(combo, yearID==2001)

#Now we can use some visualization in order to better understand the data
library(ggplot2)
plt <- ggplot(available.players, aes(x=OBP,y=salary))
print(plt + geom_point())
#plot is in the readme.md file

#As our 1st constraint requires the combined salary of 3 replacement players to be <= $15M
#we can restrict our search to only those players whose salary is below $6.5M
available.players <- subset(available.players, salary<=6500000)

#The 2nd constraint require the replacment players to have combined AB >= AB of lost players
#The combined AB of lost players is 1469, that means each of the replacement players should have AB >= 490 (1469/3 ~= 490)
available.players <- subset(available.players, AB>=490)

#The 3rd constraint places restriction on the mean OBP
#The mean OBP of lost players is around 0.363, thus mean OBP of replacement players should have mean OBP >= 0.363
available.players <- subset(available.players, OBP>=0.363)

#Now we can sort the available players in the descending order of OBP to get the max mean
library(dplyr)
head(arrange(subset(available.players,select=c('playerID','OBP','AB','salary')),desc(OBP)))
#playerID       OBP     AB  salary
#1 giambja01 0.4769001 520 4103333
#2 heltoto01 0.4316547 587 4950000
#3 berkmla01 0.4302326 577  305000
#4 gonzalu01 0.4285714 609 4833333
#5 edmonji01 0.4102142 500 6333333
#6 pujolal01 0.4029630 590  200000

#Thus out of these 6 players we can select players 2,3,4 and these 3 players satisfy all 3 constraints