#Set working directory of the project
setwd("C:/Users/SAJAL/Desktop/CSC")

#Install & load all the libraries required
install.packages("ggplot2")
install.packages("data.table")
install.packages("zoo")
install.packages("data.table")
install.packages("scales")
library(dplyr)
library(zoo)
library(ggplot2)
library(data.table)
library(scales)

#Load the telemetry data set and EDA on it

telemetry <- read.csv("telemetry.csv" , header = TRUE ,  stringsAsFactors = FALSE)

# format datetime field which comes in as.character

telemetry$datetime <- as.POSIXct(telemetry$datetime,
                                 format="%m/%d/%Y %I:%M:%S %p",
                                 tz="UTC")

cat("Total Number of telemetry records:", nrow(telemetry))
range(telemetry$datetime)
head(telemetry,10)
tail(telemetry,10)
summary(telemetry)

#Sample Plot on the telemetry data set

theme_set(theme_bw())  # theme for figures
options(repr.plot.width = 8, repr.plot.height = 6)

ggplot(data = telemetry %>% filter(machineID %in% 1:2, 
                                   datetime > as.POSIXct("2015-01-01"),
                                   datetime < as.POSIXct("2015-02-01")),
       aes(x = datetime, y = volt, col = factor(machineID))) +
  geom_line(alpha = 0.5) +
  labs(y = "voltage", color = "machineID") +
  facet_wrap(~machineID, ncol=1)

#Load the error file and EDA on it

errors <- read.csv("errors.csv" , header = TRUE ,  stringsAsFactors = FALSE)

# format datetime and errorID fields

errors$datetime <- as.POSIXct(errors$datetime,
                              format="%m/%d/%Y %I:%M:%S %p", 
                              tz="UTC")
errors$errorID <- as.factor(errors$errorID)

cat("Total Number of error records:",nrow(errors))
errors[c(1:5, nrow(errors)-4:1),]

#Sample plots for errors
options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(errors, aes(x = errorID)) + geom_bar(fill = "orange") + labs(title = "Errors by type", x = "error types")

options(repr.plot.width = 6, repr.plot.height = 5)
ggplot(errors %>% filter(machineID < 4), aes(x = errorID, fill = factor(machineID))) + geom_bar(color = "black") + 
labs(title = "MachineID errors by type", x = "error types", fill="MachineID")+facet_wrap(~machineID, ncol = 1)

options(repr.plot.width = 7, repr.plot.height = 5)
ggplot(errors %>% filter(machineID == 4), aes(y = errorID, x = datetime)) + geom_point(color = "black", alpha = 0.5) + 
labs(title = "MachineID 4 errors", x = "Date")

#Load the maintenance data and EDA on it

maint <- read.csv("maint.csv" , header = TRUE ,  stringsAsFactors = FALSE)

# format datetime and comp fields

maint$datetime <- as.POSIXct(maint$datetime,
                             format="%m/%d/%Y %I:%M:%S %p", 
                             tz="UTC")
maint$comp <- as.factor(maint$comp)

cat("Total number of maintenance records:", nrow(maint))
range(maint$datetime)
maint[c(1:5, nrow(maint)-4:0),]

#Sample plots of the maintance cmp replace

options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(maint, aes(x = comp)) + geom_bar(fill= "magenta") +labs(title = "Component replacements", x = "component types")

options(repr.plot.width = 6, repr.plot.height = 8)
ggplot(maint %>% filter(machineID < 4), aes(x = comp, fill = factor(machineID))) + geom_bar(color = "black") +
labs(title = "Component replacements", x = "component types", fill = "Machine ID")+facet_wrap(~machineID, ncol = 1)

options(repr.plot.width = 7, repr.plot.height = 5)
ggplot(maint %>% filter(machineID == 4), aes(y = comp, x = datetime)) + geom_point(color = "black", alpha = 0.5) + labs(title = "MachineID 4 component replacements", x = "Date")

#Load the machine data and EDA on it

machines  <- read.csv("machines.csv" , header = TRUE ,  stringsAsFactors = FALSE)

# format model field

machines$model <- as.factor(machines$model)

cat("Total number of machines:", nrow(machines))
machines[c(1:5, nrow(machines)-4:0),]

#Sample plots of the data

options(repr.plot.width = 8, repr.plot.height = 6)
ggplot(machines, aes(x = age, fill = model)) + geom_histogram(color = "black") + labs(title = "Machines", x = "age (years)") +facet_wrap(~model)
summary(machines)

#Load the machine data and EDA on it

failures  <- read.csv("failures.csv" , header = TRUE ,  stringsAsFactors = FALSE)

# format datetime and failure fields

failures$datetime <- as.POSIXct(failures$datetime,
                                format="%m/%d/%Y %I:%M:%S %p", 
                                tz="UTC")
failures$failure <- as.factor(failures$failure)

cat("Total number of failures:", nrow(failures))
failures[c(1:5, nrow(failures)-4:0),]

#Sample plots of the failures data

options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(failures, aes(x = failure)) + geom_bar(fill = "red") + labs(title = "Failure distribution", x = "component type")

options(repr.plot.width = 6, repr.plot.height = 6)
ggplot(failures %>% filter(machineID < 4),
       aes(x = failure, fill = factor(machineID))) + geom_bar(color = "black") + labs(title = "Failure distribution", x = "component type", fill = "MachineID") +
facet_wrap(~machineID, ncol=1)

#Feauture Engineering
#Time stamp lag feature calculation
# calculate the rolling mean and rolling standard deviation 
# on the last 24 hour lag window (width=24), for every 3 hours (by=3)
# for each machine ID.
# Rolling mean caluclation
telemetrymean_24hrs <- telemetry %>% arrange(machineID, datetime) %>% group_by(machineID) %>%
         mutate(voltmean_24hrs = rollapply(volt, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean_24hrs = rollapply(rotate, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean_24hrs = rollapply(pressure, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean_24hrs = rollapply(vibration, width = 24, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean_24hrs, rotatemean_24hrs, pressuremean_24hrs, vibrationmean_24hrs) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()

head(telemetrymean_24hrs)

#Std Dev calculation
telemetrysd_24hrs <- telemetry %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd_24hrs = rollapply(volt, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd_24hrs = rollapply(rotate, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd_24hrs = rollapply(pressure, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd_24hrs = rollapply(vibration, width = 24, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd_24hrs, rotatesd_24hrs, pressuresd_24hrs, vibrationsd_24hrs) %>%
  filter(!is.na(voltsd_24hrs)) %>%
  ungroup()

head(telemetrysd_24hrs)

# calculate the rolling mean and rolling standard deviation 
# on the last 3 hour lag window (width=3), for every 3 hours (by=3)
# for each machine ID.
telemetrymean <- telemetry %>%
  arrange(machineID, datetime) %>% 
  group_by(machineID) %>%
  mutate(voltmean = rollapply(volt, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean = rollapply(rotate, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean = rollapply(pressure, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean = rollapply(vibration, width = 3, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean, rotatemean, pressuremean, vibrationmean) %>%
  filter(!is.na(voltmean))%>% 
  ungroup()

head(telemetrymean)

#Std Dev over 3 hr window

telemetrysd <- telemetry %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd = rollapply(volt, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd = rollapply(rotate, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd = rollapply(pressure, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd = rollapply(vibration, width = 3, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd, rotatesd, pressuresd, vibrationsd) %>%
  filter(!is.na(voltsd)) %>%
  ungroup()

head(telemetrysd)

# merge columns of feature sets for final set of Telemetry

telemetryfeat <- data.frame(telemetrymean, telemetrysd[,-c(1:2)]) 
telemetryfeat_24hrs <- data.frame(telemetrymean_24hrs, telemetrysd_24hrs[,-c(1:2)])
telemetryfeat <- telemetryfeat %>%
  left_join(telemetryfeat_24hrs, by = c("datetime", "machineID")) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()

head(telemetryfeat)
summary(telemetryfeat)

#Errors Feauture Engineering
# create a column for each error type

errorcount <- errors %>% select(datetime, machineID, errorID) %>% 
  mutate(error1 = as.integer(errorID == "error1"), 
         error2 = as.integer(errorID == "error2"),
         error3 = as.integer(errorID == "error3"),
         error4 = as.integer(errorID == "error4"),
         error5 = as.integer(errorID == "error5"))

# sum the duplicate errors in an hour

errorcount <- errorcount %>% 
  group_by(machineID,datetime)%>%
  summarise(error1sum = sum(error1), 
            error2sum = sum(error2), 
            error3sum = sum(error3), 
            error4sum = sum(error4), 
            error5sum = sum(error5)) %>%
  ungroup()

head(errorcount)

# align errors with telemetry datetime field

errorfeat <- telemetry %>% 
  select(datetime, machineID) %>%
  left_join(errorcount, by = c("datetime", "machineID"))

# replace missing values
errorfeat[is.na(errorfeat)] <- 0

head(errorfeat)
summary(errorfeat)

# count the number of errors of different types in the last 24 hours,  for every 3 hours

errorfeat <- errorfeat %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(error1count = rollapply(error1sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error2count = rollapply(error2sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error3count = rollapply(error3sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error4count = rollapply(error4sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error5count = rollapply(error5sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, error1count, error2count, error3count, error4count, error5count) %>%
  filter(!is.na(error1count)) %>% 
  ungroup()

head(errorfeat)
summary(errorfeat)

#Component Replacement/Maintance engineering
# create a binary column for each component. 1 if replacement occured, 0 if not.

comprep <- maint %>% 
  select(datetime, machineID, comp) %>% 
  mutate(comp1 = as.integer(comp == "comp1"), 
         comp2 = as.integer(comp == "comp2"),
         comp3 = as.integer(comp == "comp3"),
         comp4 = as.integer(comp == "comp4")) %>%
  select(-comp)

head(comprep)

#Convert to a tabular form  and sort it according to the attributes via setkey()

comprep <- as.data.table(comprep)
setkey(comprep, machineID, datetime)

# seperate different component type replacements into different tables

comp1rep <- comprep[comp1 == 1, .(machineID, datetime, lastrepcomp1 = datetime)]# component 1 replacements
comp2rep <- comprep[comp2 == 1, .(machineID, datetime, lastrepcomp2 = datetime)]# component 2 replacements
comp3rep <- comprep[comp3 == 1, .(machineID, datetime, lastrepcomp3 = datetime)]# component 3 replacements
comp4rep <- comprep[comp4 == 1, .(machineID, datetime, lastrepcomp4 = datetime)]# component 4 replacements

# use telemetry feature table datetime and machineID to be matched with replacements
compdate <- as.data.table(telemetryfeat[,c(1:2)]) 
setkey(compdate, machineID, datetime)

# data.table rolling match will attach the latest record from the component replacement tables 
# to the telemetry date time and machineID
comp1feat <- comp1rep[compdate[,.(machineID, datetime)],roll = TRUE] 
comp1feat$sincelastcomp1 <- as.numeric(difftime(comp1feat$datetime, comp1feat$lastrepcomp1, units = "days"))
comp2feat <- comp2rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp2feat$sincelastcomp2 <- as.numeric(difftime(comp2feat$datetime, comp2feat$lastrepcomp2, units = "days"))
comp3feat <- comp3rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp3feat$sincelastcomp3 <- as.numeric(difftime(comp3feat$datetime, comp3feat$lastrepcomp3, units="days"))
comp4feat <- comp4rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp4feat$sincelastcomp4 <- as.numeric(difftime(comp4feat$datetime, comp4feat$lastrepcomp4, units = "days"))

# merge all tables
compfeat <-data.frame(compdate, comp1feat[,.(sincelastcomp1)], comp2feat[,.(sincelastcomp2)],
                      comp3feat[,.(sincelastcomp3)],comp4feat[,.(sincelastcomp4)])

head(compfeat,10)

#Joining in the data with machine data
# telemetry and error features have the same datetime and total feautures 

finalfeat <- data.frame(telemetryfeat, errorfeat[,-c(1:2)])

# merge with component features and machine features lastly

finalfeat <- finalfeat %>% 
  left_join(compfeat, by = c("datetime","machineID")) %>% 
  left_join(machines, by = c("machineID"))

head(finalfeat, 10)
cat("The final set of features are:",paste0(names(finalfeat), ","))