options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
#install.packages("odbc")
library(RMariaDB)
library(RJDBC)
library(gmodels)
library(ggplot2)
library(leaps)
library(caret)
library(mice)
library(pROC)
library(dplyr)
library(splitstackshape)
library(stringr)
# Connect to a MariaDB version of a MySQL database
con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
                 , dbname="Colts2020"
                 , user="colts2020student", password="dynamicPricing2020")
# list of db tables
dbListTables(con)
# query tables
unsold <- dbGetQuery(con, "select * from 2017_2019_Unsold_Inventory")
primary <- dbGetQuery(con, "select * from 2020_Purdue_Project_Primary")
secondary <- dbGetQuery(con, "select * from 2020_Purdue_Project_Secondary")
opp <- dbGetQuery(con, "select * from Opponent_Data_Variable")
dbDisconnect(con)
rm(con)

#coercing primary dataset
str(primary)
primary$index <- NULL
primary$SaleDate <- as.POSIXct(primary$SaleDate)
primary$TicketingAccountId <- NULL
primary$EventCode <- as.character(primary$EventCode)
primary$EventDesc <- as.character(primary$EventDesc)
primary$SectionName <- as.character(primary$SectionName)
primary$RowName <- as.character(primary$RowName)
primary$FirstSeat <- as.numeric(primary$FirstSeat)
primary$LastSeat <- as.numeric(primary$LastSeat)
primary$TotalRevenue <- NULL
primary$PurchasePrice <- as.numeric(primary$PurchasePrice)
primary$QtySeat <- as.numeric(primary$QtySeat)
primary$EventDate <- as.POSIXct(primary$EventDate)
primary$Season <- as.numeric(primary$Season)
str(primary)

#coercing secondary dataset
str(secondary)
secondary$index <- NULL
secondary$event_name <- as.character(secondary$event_name)
secondary$section_name <- as.character(secondary$section_name)
secondary$row_name <- as.character(secondary$row_name)
secondary$event_date <- as.POSIXct(secondary$event_date)
secondary$seat_num <- as.numeric(secondary$seat_num)
secondary$num_seats <- as.numeric(secondary$num_seats)
secondary$last_seat <- as.numeric(secondary$last_seat)
secondary$Orig_purchase_price <- as.numeric(secondary$Orig_purchase_price)
secondary$add_datetime <- as.POSIXct(secondary$add_datetime)
secondary$te_posting_price <- as.numeric(secondary$te_posting_price)
secondary$te_purchase_price <- as.numeric(secondary$te_purchase_price)
secondary$sales_channel <- as.factor(secondary$sales_channel)
secondary$activity_name <- as.factor(secondary$activity_name)
secondary$season_year <- as.numeric(secondary$season_year)
secondary$acct_id <- NULL
secondary$assoc_acct_id <- NULL
str(secondary)

#coercing unsold dataset
str(unsold)
unsold$index <- NULL
unsold$event_name <- as.factor(unsold$event_name)
unsold$team <- as.character(unsold$team)
unsold$`Time of Game` <- NULL
unsold$event_date <- as.POSIXct(unsold$event_date)
unsold$`Seat Type` <- as.factor(unsold$`Seat Type`)
unsold$RowClass <- as.factor(unsold$RowClass)
unsold$num_seats <- as.numeric(unsold$num_seats)
unsold$section_name <- as.factor(unsold$section_name)
unsold$row_name <- as.factor(unsold$row_name)
unsold$seat_num <- as.numeric(unsold$seat_num)
unsold$last_seat <- as.numeric(unsold$last_seat)
unsold$price_code <- NULL
unsold$`Price Code` <- NULL
unsold$block_full_price <- as.numeric(unsold$block_full_price)
unsold$class_name <- NULL
str(unsold)

#coercing opp dataset
str(opp)
opp$index <- NULL
opp$Season <- as.numeric(opp$Season)
opp$Opponent <- NULL
opp$Team <- NULL
opp$VisitingTeam <- as.character(opp$VisitingTeam)
opp$Conc  <- NULL
opp$OppWin <- as.numeric(opp$OppWin)
opp$ColtsWin <- as.numeric(opp$ColtsWin)
opp$RoadAttendance <- as.numeric(opp$RoadAttendance)
opp$FacebookFans <- as.numeric(opp$FacebookFans)
opp$Distance <- as.numeric(opp$Distance)
opp$`Home Opener` <- as.integer(opp$`Home Opener`)
opp$`Temp at Kick` <- as.numeric(opp$`Temp at Kick`)
opp$`Rain/Snow` <- as.integer(opp$`Rain/Snow`)
opp$`Colts Out of Contention` <- as.integer(opp$`Colts Out of Contention`)
opp$LastVisitYears <- as.numeric(opp$LastVisitYears)
opp$OppScoredLY <- as.numeric(opp$OppScoredLY)
opp$OppDefGivenLY <- as.numeric(opp$OppDefGivenLY)
opp$`OppPlayoff(Prev Bin)` <- as.integer(opp$`OppPlayoff(Prev Bin)`)
opp$`Top25Jersey(Ordinal)` <- as.integer(opp$`Top25Jersey(Ordinal)`)
opp$`OffMVP(Interval)` <- as.numeric(opp$`OffMVP(Interval)`)
opp$`DefMVP(Interval)` <- as.numeric(opp$`DefMVP(Interval)`)
opp$OddsFeb <- as.integer(opp$OddsFeb)
opp$GAindyL10 <- as.numeric(opp$GAindyL10)
str(opp)

#changing names
names(primary)
names(primary)[3]<-"OppTeam"

names(secondary)
names(secondary)[1]<-"EventCode"
names(secondary)[2]<-"SectionName"
names(secondary)[3]<-"RowName"
names(secondary)[4]<-"EventDate"
names(secondary)[5]<-"FirstSeat"
names(secondary)[6]<-"QtySeat"
names(secondary)[7]<-"LastSeat"
names(secondary)[8]<-"OrigPurchasePrice"
names(secondary)[9]<-"SaleDate"
names(secondary)[10]<-"TePostingPrice"
names(secondary)[11]<-"TePurchasePrice"
names(secondary)[12]<-"SalesChannel"
names(secondary)[13]<-"ActivityName"
names(secondary)[14]<-"Season"

names(unsold)
names(unsold)[1]<-"EventCode"
names(unsold)[2]<-"OppTeam"
names(unsold)[3]<-"EventDate"
names(unsold)[4]<-"SeatType"
names(unsold)[5]<-"RowClass"
names(unsold)[6]<-"QtySeat"
names(unsold)[7]<-"SectionName"
names(unsold)[8]<-"RowName"
names(unsold)[9]<-"FirstSeat"
names(unsold)[10]<-"LastSeat"
names(unsold)[11]<-"BlockFullPrice"

unsold$BlockFullPrice<- unsold$BlockFullPrice/unsold$QtySeat
names(unsold)[11]<-"BlockPerPrice"

names(opp)
names(opp)[2]<-"OppTeam"
names(opp)[8]<-"HomeOpener"
names(opp)[9]<-"TempAtKick"
names(opp)[10]<-"RainOrSnow"
names(opp)[11]<-"ColtsOutOfContention"
names(opp)[15]<-"OppPlayoffPrevBin"
names(opp)[16]<-"Top25JerseyOrdinal"
names(opp)[17]<-"OffMVPInterval"
names(opp)[18]<-"DefMVPInterval"

#Separating column OppTeam by "vs." in Primary
primary$OppTeam<-str_replace(primary$OppTeam, " vs ", " vs. ")
teams<-str_split(primary$OppTeam, " vs. ", simplify = TRUE)

#Making all our names unanimus
teams[teams[,2]=="Indianapolis Colts",2] = "Colts"
teams[teams[,2]=="Indianpolis Colts",2] = "Colts"

#Changing values to opponent team if not already
teams[teams[,1]=="Colts",1]<-teams[teams[,1]=="Colts",2]
primary$OppTeam<-teams[,1]
rm(teams)

#Making opponent names unanimus in Primary
primary$OppTeam[primary$OppTeam=="Atlanta Falcons"]="Falcons"
primary$OppTeam[primary$OppTeam=="Baltimore Ravens"]="Ravens"
primary$OppTeam[primary$OppTeam=="Buffalo Bills"]="Bills"
primary$OppTeam[primary$OppTeam=="Carolina"]="Panthers"
primary$OppTeam[primary$OppTeam=="Cincinnati"]="Bengals"
primary$OppTeam[primary$OppTeam=="Cincinnati Bengals"]="Bengals"
primary$OppTeam[primary$OppTeam=="Cleveland Browns"]="Browns"
primary$OppTeam[primary$OppTeam=="Denver"]="Broncos"
primary$OppTeam[primary$OppTeam=="Denver Broncos"]="Broncos"
primary$OppTeam[primary$OppTeam=="Detroit"]="Lions"
primary$OppTeam[primary$OppTeam=="Green Bay Packers"]="Packers"
primary$OppTeam[primary$OppTeam=="Houston"]="Texans"
primary$OppTeam[primary$OppTeam=="Houston Texans"]="Texans"
primary$OppTeam[primary$OppTeam=="Jacksonville"]="Jaguars"
primary$OppTeam[primary$OppTeam=="Jacksonville Jaguars"]="Jaguars"
primary$OppTeam[primary$OppTeam=="Miami"]="Dolphins"
primary$OppTeam[primary$OppTeam=="Miami Dolphins"]="Dolphins"
primary$OppTeam[primary$OppTeam=="Minnesota Vikings"]="Vikings"
primary$OppTeam[primary$OppTeam=="New England Patriots"]="Patriots"
primary$OppTeam[primary$OppTeam=="New Orleans Saints"]="Saints"
primary$OppTeam[primary$OppTeam=="New York Giants"]="Giants"
primary$OppTeam[primary$OppTeam=="Oakland"]="Raiders"
primary$OppTeam[primary$OppTeam=="Oakland Raiders"]="Raiders"
primary$OppTeam[primary$OppTeam=="Philadelphia Eagles"]="Eagles"
primary$OppTeam[primary$OppTeam=="Seattle Seahawks"]="Seahawks"
primary$OppTeam[primary$OppTeam=="St. Louis Rams"]="Rams"
primary$OppTeam[primary$OppTeam=="Tennessee"]="Titans"
primary$OppTeam[primary$OppTeam=="Tennessee Titans"]="Titans"
primary$OppTeam[primary$OppTeam=="Washington Redskins"]="Redskins"

#checking if no repeated team names are present in Primary
unique(primary$OppTeam)

#Repeating the same process for unsold dataset
unsold$OppTeam[unsold$OppTeam=="Atlanta Falcons"]="Falcons"
unsold$OppTeam[unsold$OppTeam=="Baltimore Ravens"]="Ravens"
unsold$OppTeam[unsold$OppTeam=="Buffalo Bills"]="Bills"
unsold$OppTeam[unsold$OppTeam=="Carolina Panthers"]="Panthers"
unsold$OppTeam[unsold$OppTeam=="Cincinnati"]="Bengals"
unsold$OppTeam[unsold$OppTeam=="Chicago Bears"]="Bears"
unsold$OppTeam[unsold$OppTeam=="Cincinnati Bengals"]="Bengals"
unsold$OppTeam[unsold$OppTeam=="Cleveland Browns"]="Browns"
unsold$OppTeam[unsold$OppTeam=="Denver"]="Broncos"
unsold$OppTeam[unsold$OppTeam=="Denver Broncos"]="Broncos"
unsold$OppTeam[unsold$OppTeam=="Detroit"]="Lions"
unsold$OppTeam[unsold$OppTeam=="Green Bay Packers"]="Packers"
unsold$OppTeam[unsold$OppTeam=="Houston"]="Texans"
unsold$OppTeam[unsold$OppTeam=="Houston Texans"]="Texans"
unsold$OppTeam[unsold$OppTeam=="Jacksonville"]="Jaguars"
unsold$OppTeam[unsold$OppTeam=="Jacksonville Jaguars"]="Jaguars"
unsold$OppTeam[unsold$OppTeam=="Miami"]="Dolphins"
unsold$OppTeam[unsold$OppTeam=="Miami Dolphins"]="Dolphins"
unsold$OppTeam[unsold$OppTeam=="Minnesota Vikings"]="Vikings"
unsold$OppTeam[unsold$OppTeam=="New England Patriots"]="Patriots"
unsold$OppTeam[unsold$OppTeam=="Arizona Cardinals"]="Cardinals"
unsold$OppTeam[unsold$OppTeam=="New York Giants"]="Giants"
unsold$OppTeam[unsold$OppTeam=="Oakland"]="Raiders"
unsold$OppTeam[unsold$OppTeam=="Oakland  Raiders"]="Raiders"
unsold$OppTeam[unsold$OppTeam=="Philadelphia Eagles"]="Eagles"
unsold$OppTeam[unsold$OppTeam=="Carolina Panthers"]="Panthers"
unsold$OppTeam[unsold$OppTeam=="Dallas Cowboys"]="Cowboys"
unsold$OppTeam[unsold$OppTeam=="Detroit Lions"]="Lions"
unsold$OppTeam[unsold$OppTeam=="Tennessee Titans"]="Titans"
unsold$OppTeam[unsold$OppTeam=="Pittsburgh Steelers"]="Steelers"
unsold$OppTeam[unsold$OppTeam=="San Francisco 49ers"]="49ers"

#checking if no repeated team names are present in Primary
unique(unsold$OppTeam)

#Repeating the same process for Opp dataset
opp$OppTeam[opp$OppTeam=="Atlanta Falcons"]="Falcons"
opp$OppTeam[opp$OppTeam=="Baltimore Ravens"]="Ravens"
opp$OppTeam[opp$OppTeam=="Buffalo Bills"]="Bills"
opp$OppTeam[opp$OppTeam=="Carolina Panthers"]="Panthers"
opp$OppTeam[opp$OppTeam=="Cincinnati"]="Bengals"
opp$OppTeam[opp$OppTeam=="Chicago Bears"]="Bears"
opp$OppTeam[opp$OppTeam=="Cincinnati Bengals"]="Bengals"
opp$OppTeam[opp$OppTeam=="Cleveland Browns"]="Browns"
opp$OppTeam[opp$OppTeam=="Denver"]="Broncos"
opp$OppTeam[opp$OppTeam=="Denver Broncos"]="Broncos"
opp$OppTeam[opp$OppTeam=="Detroit"]="Lions"
opp$OppTeam[opp$OppTeam=="Green Bay Packers"]="Packers"
opp$OppTeam[opp$OppTeam=="Houston"]="Texans"
opp$OppTeam[opp$OppTeam=="Houston Texans"]="Texans"
opp$OppTeam[opp$OppTeam=="Jacksonville"]="Jaguars"
opp$OppTeam[opp$OppTeam=="Jacksonville Jaguars"]="Jaguars"
opp$OppTeam[opp$OppTeam=="Miami"]="Dolphins"
opp$OppTeam[opp$OppTeam=="Miami Dolphins"]="Dolphins"
opp$OppTeam[opp$OppTeam=="Minnesota Vikings"]="Vikings"
opp$OppTeam[opp$OppTeam=="New England Patriots"]="Patriots"
opp$OppTeam[opp$OppTeam=="Arizona Cardinals"]="Cardinals"
opp$OppTeam[opp$OppTeam=="New York Giants"]="Giants"
opp$OppTeam[opp$OppTeam=="Oakland"]="Raiders"
opp$OppTeam[opp$OppTeam=="Oakland  Raiders"]="Raiders"
opp$OppTeam[opp$OppTeam=="Philadelphia Eagles"]="Eagles"
opp$OppTeam[opp$OppTeam=="Carolina Panthers"]="Panthers"
opp$OppTeam[opp$OppTeam=="Dallas Cowboys"]="Cowboys"
opp$OppTeam[opp$OppTeam=="Detroit Lions"]="Lions"
opp$OppTeam[opp$OppTeam=="Tennessee Titans"]="Titans"
opp$OppTeam[opp$OppTeam=="Pittsburgh Steelers"]="Steelers"
opp$OppTeam[opp$OppTeam=="St.Louis Rams"]="Rams"
opp$OppTeam[opp$OppTeam=="Tampa Bay Buccaneers"]="Buccaneers"
opp$OppTeam[opp$OppTeam=="Kansas City Chiefs"]="Chiefs"
opp$OppTeam[opp$OppTeam=="New York Jets"]="Jets"
opp$OppTeam[opp$OppTeam=="San Diego Chargers"]="Chargers"

#checking if no repeated team names are present in Primary
unique(opp$OppTeam)



#Forming new column in Primary for all seats, so 14 to 18 becomes 14,15,16,17,18
for(i in 1:nrow(primary))
{
  primary[i,"Seat"]<-paste(c(primary$FirstSeat[i]:primary$LastSeat[i]),collapse = ",")
}

#Splitting based on Seats
primary<-cSplit(primary, "Seat" , direction = "long")

#Removing Unecessary Features
primary$FirstSeat<-NULL
primary$LastSeat<-NULL
primary$QtySeat<-NULL



#Same process for Secondary data
for(i in 1:nrow(secondary))
{
  secondary[i,"Seat"]<-paste(c(secondary$FirstSeat[i]:secondary$LastSeat[i]),collapse = ",")
}

secondary<-cSplit(secondary, "Seat" , direction = "long")

secondary$FirstSeat<-NULL
secondary$LastSeat<-NULL
secondary$QtySeat<-NULL



#Same process for Unsold data
for(i in 1:nrow(unsold))
{
  unsold[i,"Seat"]<-paste(c(unsold$FirstSeat[i]:unsold$LastSeat[i]),collapse = ",")
}

unsold<-cSplit(unsold, "Seat" , direction = "long")

unsold$FirstSeat<-NULL
unsold$LastSeat<-NULL
unsold$QtySeat<-NULL



#Removing all values with no Original Purchase Price from Secondary
secondary<-subset(x = secondary,!is.na(secondary$OrigPurchasePrice))



#checking each dataset for total NAs
sum(sapply(primary, function(x) sum(is.na(x))))
sum(sapply(secondary, function(x) sum(is.na(x))))
sum(sapply(unsold, function(x) sum(is.na(x))))
sum(sapply(opp, function(x) sum(is.na(x))))

#checking NA distribution in Secondary
colnames(secondary)[colSums(is.na(secondary)) > 0] #missing values do not need imputation
rownames(secondary)[rowSums(is.na(secondary)) > 0]

#checking NA distribution in Opp
colnames(opp)[colSums(is.na(opp)) > 0] #missing values are values of the future
rownames(opp)[rowSums(is.na(opp)) > 0]



#checking to see if all events present in all datasets
g_event<-data.frame(c("Primary", "Secondary", "Unsold"),
                    c(length(unique(primary$EventCode)),length(unique(secondary$EventCode)),length(unique(unsold$EventCode))))

ggplot() +
  geom_point(data = g_event, aes(x = g_event[,1],y = g_event[,2])) +
  labs(title = "No. of Unique Events in each Dataset",x="Dataset",y="No. of Unique Events")

rm(g_event)

#primary has 67, sec 70 events, unsold 29


#cheking to see if data has only 1 opp. team per event code
EventCodeP<-as.data.frame(unique(primary$EventCode))

for(i in 1:nrow(EventCodeP))
{
  EventCodeP[i,2]<- unique(primary$OppTeam[primary$EventCode==EventCodeP[i,1]])
}
rm(EventCodeP)
# Row no. 4 gets an error because we see 2 teams for a event, so assuming that is happening throughout, we match
#event code and event date to identify an unique event



#Checking no. of total tickets in combination datasets per event
#primary
cp<-as.data.frame(primary %>%
                    group_by(EventCode) %>%
                    summarise(n=n()))

ggplot() +
  geom_point(data = cp, aes(x = EventCode, y=n)) +
  scale_y_continuous()+
  labs(title = "No. of Total Tickets per Event in Primary Dataset",x="Event Code",y="No. of Tickets") +
  theme(axis.text.x = element_text(angle = 90))

#secondary
cs<-as.data.frame(secondary %>%
                    group_by(EventCode) %>%
                    summarise(n=n()))

ggplot() +
  geom_point(data = cs, aes(x = EventCode, y=n)) +
  scale_y_continuous()+
  labs(title = "No. of Total Tickets per Event in Secondary Dataset",x="Event Code",y="No. of Tickets") +
  theme(axis.text.x = element_text(angle = 90))

#unsold
cu<-as.data.frame(unsold %>%
                    group_by(EventCode) %>%
                    summarise(n=n()))

ggplot() +
  geom_point(data = cu, aes(x = EventCode, y=n)) +
  scale_y_continuous()+
  labs(title = "No. of Total Tickets per Event in Unsold Dataset",x="Event Code",y="No. of Tickets") +
  theme(axis.text.x = element_text(angle = 90))

#combined
count<- as.data.frame(bind_rows(cp,cs,cu) %>%
                        group_by(EventCode) %>%
                        summarise_all(funs(sum(.,na.rm=TRUE))))

ggplot() +
  geom_point(data = count, aes(x = EventCode, y=n)) +
  scale_y_continuous()+
  labs(title = "No. of Total Tickets per Event in Primary + Secondary + Unsold",x="Event Code",y="No. of Tickets") +
  theme(axis.text.x = element_text(angle = 90))




#checking if tickets of event "CLT0903" from primary repeats in first secondary secondary
uni_pri<-subset(unique(primary[,c(2,4,5,7,8,9)]),EventCode=="CLT0903")
uni_sec<-subset(unique(secondary[,c(1,2,3,4,11,12)]),EventCode=="CLT0903")

#Duplicates=0
#for (i in 1:nrow(uni_pri))
#{
#  for (j in 1:nrow(uni_sec))
#  {
#    if (sum(uni_pri[i,]==uni_sec[j,])==6)
#    {
#      Duplicates = Duplicates + 1
#    }
#  }
#}
#1 duplicate found for event "CLT0903"of of 180,000 combinations, which if generalized
#across population of events, would mean the primary and secondary are mutually exclusive.


#Finding and saving first sale date of event and opp in primary
primary$FirstSale<-as.POSIXct(c("2012-08-12"))

for(i in 1:nrow(primary))
{
  primary$FirstSale[i]<- min(primary$SaleDate[primary$EventCode==primary$EventCode[i]
                                              & primary$OppTeam==primary$OppTeam[i]])
}

#same for unsold
unsold$FirstSale<-as.POSIXct(c("2012-08-12"))
for(i in 1:nrow(unsold))
{
  unsold$FirstSale[i]<- min(primary$SaleDate[unsold$EventCode[i]==primary$EventCode
                                             & unsold$OppTeam[i]==primary$OppTeam])
}

#feature creation: Day of Sale and Days to Event
primary$DayofSale<- as.Date(primary$SaleDate)-as.Date(primary$FirstSale)
primary$DaystoEvent<- as.Date(primary$EventDate)-as.Date(primary$SaleDate)

#making these features in unsold as well, where day of sale = max(day of sale for same event in primmary)
# and days to event = 0
unsold$DaystoEvent<- 0
unsold$DayofSale<-as.Date(unsold$EventDate)-as.Date(unsold$FirstSale)


#Y variable creation and clean up of both primary and unsold
primary$Season<- NULL
primary$SaleDate<-NULL
unsold$SeatType<-NULL
unsold$RowClass<-NULL
names(unsold)[6]<-"PurchasePrice"
primary$y<-1
unsold$y<-0

#rearanging in same order
primary<-primary[,c(11,1:10)]
unsold<-unsold[,c(11,1,2,4,5,6,3,7,8,10,9)]

#final coercing before merging
str(primary)
primary$EventCode<-as.factor(primary$EventCode)
primary$OppTeam<-as.factor(primary$OppTeam)
primary$SectionName<-as.factor(primary$SectionName)
primary$RowName<-as.factor(primary$RowName)
primary$DayofSale<-as.numeric(primary$DayofSale)
primary$Seat<-as.numeric(primary$Seat)
primary$DaystoEvent<-as.numeric(primary$DaystoEvent)

str(unsold)
unsold$OppTeam<-as.factor(unsold$OppTeam)
unsold$Seat<-as.numeric(unsold$Seat)
unsold$DayofSale<-as.numeric(unsold$DayofSale)

#combining datasets
tickets<-rbind(primary,unsold)

#creating new feature in tickets for secondary market avg price previous day
secondary<-secondary[is.na(secondary$TePurchasePrice)==FALSE,]
AvgSecPri<-as.data.frame(secondary %>%
                           group_by(EventCode,EventDate,SectionName) %>%
                           summarise(AvgSecPrice=mean(TePurchasePrice)))

tickets$AvgSecPrice<-0
for (i in 1:nrow(tickets))
{
  tickets$AvgSecPrice[i]<-mean(AvgSecPri$AvgSecPrice[AvgSecPri$EventCode==tickets$EventCode[i] &
                                                       AvgSecPri$EventDate==tickets$EventDate[i] &
                                                       AvgSecPri$SectionName==tickets$SectionName[i]])
}


#Imputing Values
imputedValues <- mice(data=d # just using some features
                      , seed=2016     # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)

tickets_Imp <- complete(imputedValues,1)
tickets$AvgSecPrice<-tickets_Imp$AvgSecPrice

#Now merging opp into tickets
tickets$Season<- format(as.Date(tickets$EventDate, format="%d/%m/%Y"),"%Y")
str(tickets)
str(opp)
tickets$OppTeam<-as.character(tickets$OppTeam)
tickets$Season<-as.numeric(tickets$Season)
opp$OppTeam<-as.character(opp$OppTeam)
opp$Season<-as.numeric(opp$Season)

d<-merge(x = tickets, y = opp, by = c("OppTeam","Season"), all.x = TRUE)

#Checking for NA in tickets dataset
sum(sapply(d, function(x) sum(is.na(x))))
colnames(d)[colSums(is.na(d)) > 0] #missing values are values of the future
rownames(d)[rowSums(is.na(d)) > 0]

#removing NAs and season column
d<-na.omit(d)

#checking distribution on y variable
table(d$y)
table(d$EventCode)
table(d$SectionName) #some with negligible no. of entries
table(d$RowName) #some with negligible no. of entries

#rearranging d so y is 1st col and reveling
d<-d[,c(3,1,2,4:31)]
d$y<-ifelse(d$y==1,"Y","N")
d$y<-as.factor(d$y)
d$y <- relevel(d$y,"Y")

#dummy variable creation
dummy <- dummyVars( ~ OppTeam, data = d)
d<-cbind(d,predict(dummy, d))
d$OppTeam<-NULL

dummy <- dummyVars( ~ SectionName, data = d)
d<-cbind(d,predict(dummy, d))
d$SectionName<-NULL

dummy <- dummyVars( ~ RowName, data = d)
d<-cbind(d,predict(dummy, d))
d$RowName<-NULL

d$Seat<-as.factor(d$Seat)
dummy <- dummyVars( ~ Seat, data = d)
d<-cbind(d,predict(dummy, d))
d$Seat<-NULL


#fixing non-coerced cols for linear combos
str(d)
d$Division<-as.numeric(d$Division)
d$HomeOpener<-as.numeric(d$HomeOpener)
d$RainOrSnow<-as.numeric(d$RainOrSnow)
d$ColtsOutOfContention<-as.numeric(d$ColtsOutOfContention)
d$OppPlayoffPrevBin<-as.numeric(d$OppPlayoffPrevBin)
d$Top25JerseyOrdinal<-as.numeric(d$Top25JerseyOrdinal)
d$OddsFeb<-as.numeric(d$OddsFeb)

#removing all non-numeric cols before correlation matrix
Backup<-d[,c("EventCode","EventDate","FirstSale")]
d<-d[,-c("EventCode","EventDate","FirstSale")]
d$Distance<-NULL

# Find if any linear combinations exist and which column combos they are.
# Below I add a vector of 1s at the beginning of the dataset. This helps ensure
# the same features are identified and removed.

# first save response
y <- d$y

# create a column of 1s. This will help identify all the right linear combos
d <- cbind(rep(1, nrow(d)), d[,2:ncol(d)])
names(d)[1] <- "ones"

# identify the columns that are linear combos
comboInfo <- findLinearCombos(d)
comboInfo

# remove columns identified that led to linear combos
#d <- d[,-comboInfo$remove]
d<-d[,-c(47,48,49,140,254,268,314,355)]

# remove the "ones" column in the first column
d <- d[,2:ncol(d)]

# Add the target variable back to our data.frame
d <- as.data.frame(cbind(y, d))

rm(y, comboInfo)  # clean up


#removing features with near zero frequency
nzv <- nearZeroVar(d, freqCut = 99.9/0.1, saveMetrics = TRUE)
head(nzv)
d <- d[, c(TRUE,!nzv$zeroVar[2:ncol(d)])] #No feautres had zero variance
d <- d[, c(TRUE,!nzv$nzv[2:ncol(d)])] #Over 50% had nzv = False

#numeric feature z-score standardization

#saving statistics before standardizing
dstat<-as.data.frame(c(0))
dstat[1,1]= "Mean"
dstat[2,1]="SD"
for(i in 2:ncol(d))
{
  dstat[1,i]<-mean(d[,i])
  dstat[2,i]<-sd(d[,i])
  colnames(dstat)[i]<-colnames(d)[i]
}

#standardizing
preProcValues <- preProcess(d[,3:ncol(d)], method = c("center","scale"))
d <- predict(preProcValues, d)


#adding dates back to dataset
d<-cbind(d,Backup[,c(2,3)])

#Performing forward selection
# using the regsubsets() function from the leaps library on the 'd' data.frame.
mlrF <- regsubsets(y ~ .
                   , data = d
                   , nbest = 1           # 1 best model for each number of predictors
                   , intercept = T       # should model have intercept term (B0)?
                   , method = "forward" # forward, backward, sequential, exhaustive
                   , nvmax = 30        # NULL for no limit on number of variables
)
summary(mlrF)

# capture the set of features to include
vars <- data.frame(vars = summary(mlrF)$which[which.max(summary(mlrF)$adjr2),])
vars <- data.frame(vars=row.names(vars), keep=vars$vars)
vars <- vars[which(vars$keep==T & vars$vars!="(Intercept)"),"vars"]

# automatically create the model formula for me to put into the lm() function
# or caret train() function

mod_formulaF <- as.formula(paste("y ~ ",paste(vars, collapse="+"),sep = ""))
mod_formulaF

#since PurchasePrice doesn't come into the formula, removing AvgSecPrice
#feature since it is a result of PurchasePrice and may be causing this
d$AvgSecPrice<-NULL

#Performing forward selection again
# using the regsubsets() function from the leaps library on the 'd' data.frame.
mlrF <- regsubsets(y ~ .
                   , data = d
                   , nbest = 1           # 1 best model for each number of predictors
                   , intercept = T       # should model have intercept term (B0)?
                   , method = "forward" # forward, backward, sequential, exhaustive
                   , nvmax = 30        # NULL for no limit on number of variables
)
summary(mlrF)

# capture the set of features to include
vars <- data.frame(vars = summary(mlrF)$which[which.max(summary(mlrF)$adjr2),])
vars <- data.frame(vars=row.names(vars), keep=vars$vars)
vars <- vars[which(vars$keep==T & vars$vars!="(Intercept)"),"vars"]

# automatically create the model formula for me to put into the lm() function
# or caret train() function

mod_formulaF <- as.formula(paste("y ~ ",paste(vars, collapse="+"),sep = ""))
mod_formulaF

#since no ForwaRD formula contains PurchasePrice, making one manually
mod_formulaP<- as.formula(paste("y ~ ",paste(vars, collapse="+"),"+ PurchasePrice",sep = ""))

#Performing backward selection
# using the regsubsets() function from the leaps library on the 'd' data.frame.
mlrB <- regsubsets(y ~ .
                   , data = d
                   , nbest = 1           # 1 best model for each number of predictors
                   , intercept = T       # should model have intercept term (B0)?
                   , method = "backward" # forward, backward, sequential, exhaustive
                   , nvmax = 50        # NULL for no limit on number of variables
)
summary(mlrB)

# capture the set of features to include
vars <- data.frame(vars = summary(mlrB)$which[which.max(summary(mlrB)$adjr2),])
vars <- data.frame(vars=row.names(vars), keep=vars$vars)
vars <- vars[which(vars$keep==T & vars$vars!="(Intercept)"),"vars"]

# automatically create the model formula for me to put into the lm() function
# or caret train() function

mod_formulaB <- as.formula(paste("y ~ ",paste(vars, collapse="+"),sep = ""))
mod_formulaB



###creating train, test and evaluate dataset###

#checking for the latest match date in dataset
max(d$EventDate)

#which team played latest match
unique(primary$OppTeam[primary$EventDate=="2019-12-21 19:00:00"])

#Seperating Panthers last match for eval set from d
evaluate<-d[d$OppTeamPanthers>=0 & d$EventDate=="2019-12-21 19:00:00",]
d<-d[!(d$OppTeamPanthers>=0 & d$EventDate=="2019-12-21 19:00:00"),]

#creating data partition
set.seed(1234)
inTrain <- createDataPartition(y = d$y,    # outcome variable
                               p = .70,         # % of train data you want
                               list = FALSE)

# training and test data sets
train <- d[inTrain,]
test  <- d[-inTrain,]

#trainig first 3 models using logistic regression
m1 <- train(mod_formulaF
            , method = "glm"
            , data = train
            , family = "binomial")
summary(m1)

defaultSummary(data=data.frame(obs=train$y
                               , pred=predict(m1, newdata=train))
               , model=m1)
defaultSummary(data=data.frame(obs=test$y
                               , pred=predict(m1, newdata=test))
               , model=m1)

m2 <- train(mod_formulaP
            , method = "glm"
            , data = train
            , family = "binomial")
summary(m2)

defaultSummary(data=data.frame(obs=train$y
                               , pred=predict(m2, newdata=train))
               , model=m2)
defaultSummary(data=data.frame(obs=test$y
                               , pred=predict(m2, newdata=test))
               , model=m2)

m3 <- train(mod_formulaB
            , method = "glm"
            , data = train
            , family = "binomial")
summary(m3)

defaultSummary(data=data.frame(obs=train$y
                               , pred=predict(m3, newdata=train))
               , model=m3)
defaultSummary(data=data.frame(obs=test$y
                               , pred=predict(m3, newdata=test))
               , model=m3)

m4 <- train(y ~ Season +  ColtsWin + FacebookFans +
              TempAtKick + ColtsOutOfContention + OppScoredLY + OppTeamBears +
              OppTeamBuccaneers + OppTeamChargers + OppTeamChiefs + OppTeamCowboys +
              OppTeamFalcons + OppTeamGiants + OppTeamLions + OppTeamPanthers +
              OppTeamPatriots + SectionName.401 + SectionName.402 + SectionName.403 +
              SectionName.404 + SectionName.406 + SectionName.407 + SectionName.420 +
              SectionName.422 + SectionName.424 + SectionName.429 + SectionName.431 +
              SectionName.446 + SectionName.447 + SectionName.449 + SectionName.450 +
              SectionName.451 + SectionName.452 + SectionName.520 + SectionName.610 +
              SectionName.611 + SectionName.612 + SectionName.615 + SectionName.616 +
              SectionName.617 + SectionName.619 + SectionName.642 + RowName.1 +
              RowName.11 + RowName.2 + RowName.3 + EventDate + FirstSale +
              PurchasePrice
            , method = "glm"
            , data = train
            , family = "binomial")
summary(m4)

defaultSummary(data=data.frame(obs=train$y
                               , pred=predict(m4, newdata=train))
               , model=m4)
defaultSummary(data=data.frame(obs=test$y
                               , pred=predict(m4, newdata=test))
               , model=m4)

#trainig next 1 models using neural networks
m5 <- train(y ~ Season +  ColtsWin + FacebookFans +
              TempAtKick + ColtsOutOfContention + OppScoredLY + OppTeamBears +
              OppTeamBuccaneers + OppTeamChargers + OppTeamChiefs + OppTeamCowboys +
              OppTeamFalcons + OppTeamGiants + OppTeamLions + OppTeamPanthers +
              OppTeamPatriots + SectionName.401 + SectionName.402 + SectionName.403 +
              SectionName.404 + SectionName.406 + SectionName.407 + SectionName.420 +
              SectionName.422 + SectionName.424 + SectionName.429 + SectionName.431 +
              SectionName.446 + SectionName.447 + SectionName.449 + SectionName.450 +
              SectionName.451 + SectionName.452 + SectionName.520 + SectionName.610 +
              SectionName.611 + SectionName.612 + SectionName.615 + SectionName.616 +
              SectionName.617 + SectionName.619 + SectionName.642 + RowName.1 +
              RowName.11 + RowName.2 + RowName.3 + EventDate + FirstSale +
              PurchasePrice
            , method = "nnet"
            , data = train
            , family = "binomial")
summary(m5)

defaultSummary(data=data.frame(obs=train$y
                               , pred=predict(m5, newdata=train))
               , model=m5)
defaultSummary(data=data.frame(obs=test$y
                               , pred=predict(m5, newdata=test))
               , model=m5)

#trainig 1 model using ordinal Random Forest
m6 <- train(y ~ Season +  ColtsWin + FacebookFans +
              TempAtKick + ColtsOutOfContention + OppScoredLY + OppTeamBears +
              OppTeamBuccaneers + OppTeamChargers + OppTeamChiefs + OppTeamCowboys +
              OppTeamFalcons + OppTeamGiants + OppTeamLions + OppTeamPanthers +
              OppTeamPatriots + SectionName.401 + SectionName.402 + SectionName.403 +
              SectionName.404 + SectionName.406 + SectionName.407 + SectionName.420 +
              SectionName.422 + SectionName.424 + SectionName.429 + SectionName.431 +
              SectionName.446 + SectionName.447 + SectionName.449 + SectionName.450 +
              SectionName.451 + SectionName.452 + SectionName.520 + SectionName.610 +
              SectionName.611 + SectionName.612 + SectionName.615 + SectionName.616 +
              SectionName.617 + SectionName.619 + SectionName.642 + RowName.1 +
              RowName.11 + RowName.2 + RowName.3 + EventDate + FirstSale +
              PurchasePrice
            , method = "adaboost"
            , data = train
            , family = "binomial")
summary(m6)

defaultSummary(data=data.frame(obs=train$y
                               , pred=predict(m6, newdata=train))
               , model=m6)
defaultSummary(data=data.frame(obs=test$y
                               , pred=predict(m6, newdata=test))
               , model=m6)

# logit model F preds
trP_logitF <- predict(m1 , newdata=train, type='prob')[,1]
trC_logitF <- predict(m1 , newdata=train)
teP_logitF <- predict(m1 , newdata=test, type='prob')[,1]
teC_logitF <- predict(m1 , newdata=test)
# logit model P preds
trP_logitP <- predict(m2 , newdata=train, type='prob')[,1]
trC_logitP <- predict(m2 , newdata=train)
teP_logitP <- predict(m2 , newdata=test, type='prob')[,1]
teC_logitP <- predict(m2 , newdata=test)
# logit model B preds
trP_logitB <- predict(m3 , newdata=train, type='prob')[,1]
trC_logitB <- predict(m3 , newdata=train)
teP_logitB <- predict(m3 , newdata=test, type='prob')[,1]
teC_logitB <- predict(m3 , newdata=test)
# logitt model P preds
trP_logit4 <- predict(m4 , newdata=train, type='prob')[,1]
trC_logit4 <- predict(m4 , newdata=train)
teP_logit4 <- predict(m4 , newdata=test, type='prob')[,1]
teC_logit4 <- predict(m4 , newdata=test)
# nnet model P preds
trP_nnet <- predict(m5 , newdata=train, type='prob')[,1]
trC_nnet <- predict(m5 , newdata=train)
teP_nnet <- predict(m5 , newdata=test, type='prob')[,1]
teC_nnet <- predict(m5 , newdata=test)
# ada model P preds
trP_adaboost <- predict(m6 , newdata=train, type='prob')[,1]
trC_adaboost <- predict(m6 , newdata=train)
teP_adaboost <- predict(m6 , newdata=test, type='prob')[,1]
teC_adaboost <- predict(m6 , newdata=test)


plot(roc(test$y, teP_logitF), col = "red")
plot(roc(test$y, teP_logitP), col = "blue",add=TRUE,lwd=1)
plot(roc(test$y, teP_logitB), col = "green",add=TRUE,lwd=1)
plot(roc(test$y, teP_logit4), col = "pink",add=TRUE,lwd=1)
plot(roc(test$y, teP_nnet), col = "orange",add=TRUE,lwd=1)
plot(roc(test$y, teP_adaboost), col = "yellow",add=TRUE,lwd=1)
legend(0, 1, legend=c("LogitF","LogitP","LogitB","Logit4","nnet","adaboost")
       , col=c("red","blue","green","pink","orange","yellow"), lty=1:5,cex=1)

AUCtable = rbind(auc(roc(test$y, teP_logitF)),
                 auc(roc(test$y, teP_logitP)),
                 auc(roc(test$y, teP_logitB)),
                 auc(roc(test$y, teP_logit4)),
                 auc(roc(test$y, teP_nnet)),
                 auc(roc(test$y, teP_adaboost)))
rownames(AUCtable) <- c("LogitF_test","LogitP_test","LogitB_test","Logit4_test"
                        ,"nnet_test","adaboost_test")
colnames(AUCtable) = "AUC"
AUCtable


#creating Expected Value Maximization code
eval_price<-evaluate

#putting all possible prices from 1 to 255
eval_price$PurchasePrice<-paste(c(1:255),collapse=",")
eval_price<-cSplit(eval_price, "PurchasePrice" , direction = "long")

#zscore standardization using the mean and sd of old set
eval_price$PurchasePrice<- (eval_price$PurchasePrice-dstat$PurchasePrice[1])/dstat$PurchasePrice[2]

#saving prices
ExP<-as.data.frame(eval_price$PurchasePrice)

#predicting sale or not for each price per ticket
ExP[,2]<-predict(m2 , newdata=eval_price, type = "prob")[,1]

#calculating Expected Value after making price normal
ExP[,3]<-ExP[,1]*ExP[,2]

#finding  maximum expected value and saving price in new column in evaluation
for(i in 1:nrow(evaluate))
{
  a<- (255*i-254):(255*i) #going from 1 to 255, then 256 to 510...
  #now taking the price of max expected value per ticket and saving in set
  evaluate$RecPrice[i]<-ExP$`eval_price$PurchasePrice`[ExP$V3==max(ExP$V3[a])]
  evaluate$RecY[i]<-ExP$V2[ExP$V3==max(ExP$V3[a])]
}

CompPrice<-as.data.frame(evaluate$PurchasePrice)
CompPrice[,2]<-as.data.frame(evaluate$y)
CompPrice[,3]<-evaluate$RecPrice
CompPrice[,4]<-evaluate$RecY

#converting y/n to 1/0
CompPrice[,2]<-ifelse(CompPrice[,2]=="Y",1,0)

#converting RecY to 1/0
CompPrice[,4]<-ifelse(CompPrice[,4]<0.5,0,1)

#getting standardized features back to normal
CompPrice[,c(1,3)]<-CompPrice*dstat$PurchasePrice[2]+dstat$PurchasePrice[1]

#Creating Revenue Columns
CompPrice[,5]<-CompPrice[,1]*CompPrice[,2]
CompPrice[,6]<-CompPrice[,3]*CompPrice[,4]

#saving day of sale
CompPrice[,7]<-evaluate$DayofSale

#getting standardized features back to normal
CompPrice[,7]<-CompPrice[,7]*dstat$DayofSale[2]+dstat$DayofSale[1]

table(CompPrice$`evaluate$y`)
table(CompPrice$V4)

sum(CompPrice$V5)
sum(CompPrice$V6)