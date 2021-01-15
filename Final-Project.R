#Loading my libraries
library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(magrittr)
library(gt)
library(pander)
library(rmarkdown)
library(ggplot2)

#Loading my data
calls_service <- read_csv("calls_service.csv")
View(calls_service)
calls.frame <- as.data.frame(calls_service) #making data frame
class(calls.frame)

#Exploring my data
sum(is.na(calls.frame))
as.data.frame(calls.frame)
nrow(calls.frame)
typeof(calls.frame)
length(calls.frame$district)
unique(calls.frame$district)

#putting date and time in different columns, making them in a new data frame
date.calls <- as.data.frame(str_split(calls.frame$callDateTime," ", simplify = TRUE)) 
colnames(date.calls)[1] <- "wrong.date" #changing col name

#changing to date notation for R, putting it in new column
date.calls$date <- as.Date(date.calls$wrong.date, format = "%m/%d/%Y")

#combining the column of this data set into the one i'm working with
calls.table <- as.data.frame(cbind(calls.frame, date.calls$date))

#renaming the column of the date
colnames(calls.table) [9] <- "date" 

#I want to do the same for time, i currently have them separated with time
#AM/PM in different columns, so i want to unite them
#I will concatenate them
date.calls$full.time <- paste0(date.calls$V2, " ", date.calls$V3)

#changing to time format
date.calls$new.time <- parse_time(date.calls$full.time, "%I:%M:%S %p")

#now adding new.time as a column to calls.table
calls.table <- as.data.frame(cbind(calls.frame, date.calls$date, date.calls$new.time))

#changing column names to date and time
colnames(calls.table) [9] <-"call.date"
colnames(calls.table) [10] <- "call.time"

#I'm choosing to only analyze data from 2017
calls.2017 <- dplyr::filter(calls.table, call.date >= as.Date("2017-01-01"), 
                            call.date <= as.Date("2017-12-31"))
#Priority Calls ####
#How many high, low, medium, emergency, non-emergency,
#out of service calls were there per district? 
types.calls <- as.data.frame(calls.2017)

#Finding missing values in my priority and district column
sum(is.na(types.calls$priority))
sum(is.na(types.calls$district))

#Finding types of priorities
unique(types.calls$priority)

#I want to know the frequency of each priority type
sum(types.calls$priority == "Out of Service")
sum(types.calls$priority == "Non-Emergency")
sum(types.calls$priority == "Emergency")
sum(types.calls$priority == "Medium")
sum(types.calls$priority == "High")
sum(types.calls$priority == "Low")

#I will make a binary flag to know which rows have
#out of service in them
types.calls$out.service <- as.factor(ifelse(types.calls$priority == "Out of Service", "1", "0"))
sum(types.calls$out.service == 1) #verifying binary flag matches the priority column

#deleting rows with out of service
priority.calls <- subset(types.calls, types.calls$priority != "Out of Service")
sum(priority.calls$priority == "Out of Service") #verifying my work

#now time to sort everything using dplyr
new.priority <- priority.calls %>%
  arrange(district) %>% #arranging by neighborhood name
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n()) #make column that counts priority per neighborhood

#making priority a variable
clean.priority <- new.priority %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)

#In this case an NA will equal there were no calls w/that priority type
#Therefore I will make NA = 0
clean.priority[is.na(clean.priority)] <- 0
colnames(clean.priority) [1] <- "District"

#Making a final column to sum the amount of calls
clean.priority$Total <- rowSums(clean.priority[2:6])

#Eliminating extra rows
TRU.no <- clean.priority[clean.priority$District != "TRU",]
CW.no <- TRU.no[TRU.no$District != "CW",]
EV.no <- CW.no[CW.no$District != "EVT1",]
final.priority <- EV.no[EV.no$District != "SS",]

#Month Calls####
#copying my 2017 data frame to work on a new data frame for this question
months.calls <- as.data.frame(calls.2017)

#finding NA in dates
sum(is.na(months.calls$date)) #There are no NA

#subset by months
january.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-01-01"), 
                               call.date <= as.Date("2017-01-31"))
february.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-02-01"), 
                                call.date <= as.Date("2017-02-28"))
march.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-03-01"), 
                             call.date <= as.Date("2017-03-31"))
april.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-04-01"), 
                             call.date <= as.Date("2017-04-30"))
may.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-05-01"), 
                           call.date <= as.Date("2017-05-31"))
june.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-06-01"), 
                            call.date <= as.Date("2017-06-30"))
july.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-07-01"), 
                            call.date <= as.Date("2017-07-31"))
august.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-08-01"), 
                              call.date <= as.Date("2017-08-31"))
september.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-09-01"), 
                                 call.date <= as.Date("2017-09-30"))
october.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-10-01"), 
                               call.date <= as.Date("2017-10-31"))
november.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-11-01"), 
                                call.date <= as.Date("2017-11-30"))
december.calls <- dplyr::filter(months.calls, call.date >= as.Date("2017-12-01"), 
                                call.date <= as.Date("2017-12-31"))

#Arrange each month subset by district

#January
jan.new <- january.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
jan.clean <- jan.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
jan.clean[is.na(jan.clean)] <- 0 #make NA values 0

#I will delete the out of service calls since those were not completed
jan.clean[7] <- NULL
jan.clean$jan.total <- rowSums(jan.clean[2:6])

#February
feb.new <- february.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
feb.clean <- feb.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)

feb.clean[is.na(feb.clean)] <- 0 #Making NA values 0

#Delete out of service calls
feb.clean[7] <- NULL

#make a column that counts total of calls
feb.clean$feb.total <- rowSums(feb.clean[2:6])

#March
mar.new <- march.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
mar.clean <- mar.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
mar.clean[is.na(mar.clean)] <- 0 #make NA = 0

#I will delete the out of service calls since those were not completed
mar.clean[7] <- NULL

#make column that counts total of calls
mar.clean$mar.total <- rowSums(mar.clean[2:6])

#April
apr.new <- april.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
apr.clean <- apr.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
apr.clean[is.na(apr.clean)] <- 0 #make NA = 0

#I will delete the out of service calls since those were not completed
apr.clean[7] <- NULL

#make a column that counts the total of calls
apr.clean$apr.total <- rowSums(apr.clean[2:6]) 

#May
may.new <- may.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
may.clean <- may.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
may.clean[is.na(may.clean)] <- 0 #make NA = 0

#I will delete the out of service calls since those were not completed
may.clean[7] <- NULL

#make a column that counts the total of calls
may.clean$may.total <- rowSums(may.clean[2:6])

#June
june.new <- june.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
june.clean <- june.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
june.clean[is.na(june.clean)] <- 0 #making NA = 0

#I will delete the out of service calls since those were not completed
june.clean[7] <- NULL

#make a column that counts the total of calls
june.clean$june.total <- rowSums(june.clean[2:6])

#July
july.new <- july.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
july.clean <- july.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
july.clean[is.na(july.clean)] <- 0 #change NA to 0

#I will delete the out of service calls since those were not completed
july.clean[7] <- NULL

#make a column that counts the total of calls
july.clean$july.total <- rowSums(july.clean[2:6])

#August
aug.new <- august.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
aug.clean <- aug.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
aug.clean[is.na(aug.clean)] <- 0

#I will delete the out of service calls since those were not completed
aug.clean[7] <- NULL

#make a column that counts the total of calls
aug.clean$aug.total <- rowSums(aug.clean[2:6])


#September
sept.new <- september.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
sept.clean <- sept.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
sept.clean[is.na(sept.clean)] <- 0 #make NA values = 0

#I will delete the out of service calls since those were not completed
sept.clean[7] <- NULL

#make a column that counts the total of calls
sept.clean$sept.total <- rowSums(sept.clean[2:6])

#October
oct.new <- october.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
oct.clean <- oct.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
oct.clean[is.na(oct.clean)] <- 0 #make Na values = 0

#I will delete the out of service calls since those were not completed
oct.clean[7] <- NULL

#make a column that counts the total of calls
oct.clean$oct.total <- rowSums(oct.clean[2:6])

#November
nov.new <- november.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
nov.clean <- nov.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
nov.clean[is.na(nov.clean)] <- 0 #make NA values equal 0

#I will delete the out of service calls since those were not completed
nov.clean[7] <- NULL

#make a column that counts the total of calls
nov.clean$nov.total <- rowSums(nov.clean[2:6])

#December
dec.new <- december.calls %>%
  arrange(district) %>%
  group_by(district, priority) %>% #grouping by neighborhood and priority
  summarize(prioritycounts = n())

#make priority a variable
dec.clean <- dec.new %>%
  pivot_wider(names_from = priority, values_from = prioritycounts)
dec.clean[is.na(dec.clean)] <- 0 #make NA = 0

#I will delete the out of service calls since those were not completed
dec.clean[7] <- NULL

#make a column that counts the total of calls
dec.clean$dec.total <- rowSums(dec.clean[2:6])

#Combining month total calls ####
total.calls <- cbind(jan.clean$district, jan.clean$jan.total, feb.clean$feb.total, 
                     mar.clean$mar.total, apr.clean$apr.total,
                     may.clean$may.total, june.clean$june.total, 
                     july.clean$july.total, aug.clean$aug.total, 
                     sept.clean$sept.total, oct.clean$oct.total,
                     nov.clean$nov.total, dec.clean$dec.total)
total.calls <- as.data.frame(total.calls)
#Changing column names
colnames(total.calls) <- c("District", "January", "February", 
                           "March", "April", "May", "June", 
                           "July", "August", "September", "October",
                           "November", "December")
#Making the columns with numbers integer
total.calls[2:13] <- lapply(total.calls[2:13], as.integer)

lapply(total.calls, typeof) #revising my work

#Making a final column with total amount of calls
total.calls$Year <- rowSums(total.calls[2:13])
total.calls$District

#I will only include the main 9 Baltimore City Districts
total.calls <- total.calls[-c(13), ]

#Eliminating extra
no.TRU <- total.calls[total.calls$District != "TRU",]
no.CW <- no.TRU[no.TRU$District != "CW",]
final.total.calls <- no.CW[no.CW$District != "SS",]


#Tables ####
#Table for Priority calls
table.priority <- as.data.frame(final.priority)

#renaming
names(table.priority)[6] <- "Non.Emergency"
table.priority$District[table.priority$District == "CD"] <- "Central"
table.priority$District[table.priority$District == "ED"] <- "Eastern"
table.priority$District[table.priority$District == "ND"] <- "Northern"
table.priority$District[table.priority$District == "NE"] <- "Northeast"
table.priority$District[table.priority$District == "NW"] <- 'Northwest'
table.priority$District[table.priority$District == "SD"] <- "Southern"
table.priority$District[table.priority$District == "SE"] <- "Southeast"
table.priority$District[table.priority$District == "SW"] <- 'Southwest'
table.priority$District[table.priority$District == "WD"] <- "Western"

#Table with titles
table.priority %>%
  gt() %>%
  tab_header(
    title = "911 Police Service Call Priority in Baltimore City for 2017",
    subtitle = "Number of Calls by District") %>%
  tab_spanner(
    label = "Call Priority",
    columns = vars(Emergency, High, Low, Medium, Non.Emergency)
  )

#Table for monthly calls
table.months <- as.data.frame(final.total.calls)
table.months$District[table.months$District == "CD"] <- "Central"
table.months$District[table.months$District == "ED"] <- "Eastern"
table.months$District[table.months$District == "ND"] <- "Northern"
table.months$District[table.months$District == "NE"] <- "Northeast"
table.months$District[table.months$District == "NW"] <- 'Northwest'
table.months$District[table.months$District == "SD"] <- "Southern"
table.months$District[table.months$District == "SE"] <- "Southeast"
table.months$District[table.months$District == "SW"] <- 'Southwest'
table.months$District[table.months$District == "WD"] <- "Western"

#Table with titles
table.months %>%
  gt() %>%
  tab_header(
    title = "Monthly 911 Police Service Calls in Baltimore City",
    subtitle = "Number of Calls by District") %>%
  tab_spanner(
    label = "Month",
    columns = vars(January, February, March, April, May, June, July, August,
                   September, October, November, December))

#Graphs ####
#Arranging dataframe
new.months.calls <- months.calls
bye1 <- new.months.calls[new.months.calls$district != "TRU",]
bye2 <- bye1[bye1$district != "CW",]
bye3 <- bye2[bye2$district != "EVT1",]
fix.months.calls <- bye3[bye3$district != "SS",]

#Making col for month
fix.months.calls$month <- unlist(lapply(str_split(fix.months.calls$call.date,"-"), `[`, c(2)))
fix.months.calls$month <- as.integer(fix.months.calls$month)

#Renaming
fix.months.calls$district[fix.months.calls$district == "CD"] <- "Central"
fix.months.calls$district[fix.months.calls$district == "ED"] <- "Eastern"
fix.months.calls$district[fix.months.calls$district == "ND"] <- "Northern"
fix.months.calls$district[fix.months.calls$district == "NE"] <- "Northeast"
fix.months.calls$district[fix.months.calls$district == "NW"] <- 'Northwest'
fix.months.calls$district[fix.months.calls$district == "SD"] <- "Southern"
fix.months.calls$district[fix.months.calls$district == "SE"] <- "Southeast"
fix.months.calls$district[fix.months.calls$district == "SW"] <- 'Southwest'
fix.months.calls$district[fix.months.calls$district == "WD"] <- "Western"

#New Data Frame
grouped.month <- fix.months.calls %>%
  dplyr::select(district, month) %>%
  dplyr::group_by(district, month) %>%
  dplyr::summarise(count = n())

#Making Month an integer
grouped.month$month <- as.integer(grouped.month$month)
names(grouped.month) [1] <- "District"

#Month Graph
ggplot(data = grouped.month,
       aes(x = month, y = count,
           color = District)) +
  scale_x_discrete(name = "Month",
                   limits = c("Jan.", "Feb.", "March", "April", "May", "June",
                              "July", "Aug.", "Sep.", 
                              "Oct.", "Nov.", "Dec.")) +
  geom_line(size = 1) +
  xlab("Month") +
  ylab("Amount of Calls") +
  ggtitle("911 Police Service Calls in Baltimore City for 2017") +
  theme_light()

#By District
ggplot(data = grouped.month,
       aes(x = month, y = count,
           color = District)) +
  scale_x_discrete(name = "2017",
                   limits = c(" ", " ", " ", " ", " ", " ",
                              " ", " ", " ", 
                              " ", " ", " ")) +
  geom_line(size = 1) +
  xlab("2017") +
  ylab("Amount of Calls") +
  ggtitle("911 Police Service Calls in Baltimore City") +
  theme_light() +
  facet_wrap(~ District)

#Priority Calls

#Arranging dataframe
graph.priority <- new.priority
adios1 <- graph.priority[graph.priority$district != "TRU",]
adios2 <- adios1[adios1$district != "CW",]
adios3 <- adios2[adios2$district != "EVT1",]
fix.graph.priority <- adios3[adios3$district != "SS",]

#Renaming
fix.graph.priority$district[fix.graph.priority$district == "CD"] <- "Central"
fix.graph.priority$district[fix.graph.priority$district == "ED"] <- "Eastern"
fix.graph.priority$district[fix.graph.priority$district == "ND"] <- "Northern"
fix.graph.priority$district[fix.graph.priority$district == "NE"] <- "Northeast"
fix.graph.priority$district[fix.graph.priority$district == "NW"] <- 'Northwest'
fix.graph.priority$district[fix.graph.priority$district == "SD"] <- "Southern"
fix.graph.priority$district[fix.graph.priority$district == "SE"] <- "Southeast"
fix.graph.priority$district[fix.graph.priority$district == "SW"] <- 'Southwest'
fix.graph.priority$district[fix.graph.priority$district == "WD"] <- "Western"
names(fix.graph.priority)[1] <- "District"
names(fix.graph.priority) [2] <- "Priority"

#Making the actual graph
ggplot(data=fix.graph.priority, 
       aes(x= District, y=prioritycounts, fill=Priority)) +
  geom_bar(stat="identity")+
  xlab("District") +
  ylab("Amount of Calls") +
  ggtitle("Priority of 911 Police Service Calls in Baltimore for 2017") +
  theme_light() +
  coord_flip()

write.csv(final.priority,"C:/Users/krist/Documents/GitHub/DDD-Final-Project/final-2017-priority.csv", row.names = FALSE)
write.csv(final.total.calls, "C:/Users/krist/Documents/GitHub/DDD-Final-Project/month-2017-calls.csv", row.names = FALSE)

#obpd.crime.ad.list <- unlist(lapply(str_split(obpd.crime.10$location," "), `[`, c(2)))
#https://lubridate.tidyverse.org/reference/month.html
