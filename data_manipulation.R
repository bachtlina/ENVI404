# basic stats on Beitbridge data
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

file <- file.choose()
beit <- read_csv(file, skip = 7)
head(beit)
tail(beit)
beit <- beit[1:28,]

mean(beit$`Flow (cumec)`)
sd(beit$`Flow (cumec)`) # this is sample deviation

m <- max(beit$`Flow (cumec)`)
i <- which(beit$`Flow (cumec)` == m) # asking a question: which one? answer is a location on the data chart
beit[which.max(beit$`Flow (cumec)`),]
beit[which.min(beit$`Flow (cumec)`),]

# logical operators: ==, <, <=, >, >=, !=
# types of brackets
# () parantheses, input for functions
# [] locations
# {} loop commands :
# for (i in 1:10) {
#    loop commands
# }

# variables

single.variables <- 7
vectors_or_lists <- c(5+6, single.variables, 1:3)
first <- c(1:3)
second <- c(4:8)
third <- cbind(first, second)
fourth <- rbind(first, second)
df <- data.frame("col1" = c("A", "b", "C", "D", "e"),
                 "col2" = vectors_or_lists,
                 "col3" = c(1:5))

# Manipulate some data!
beit.f <- select(beit, Year, `Flow (cumec)`)
y2k <- filter(beit,Year == 2000)
beit.f <- mutate(beit.f, flow_lps = 1000*`Flow (cumec)`) # dplyr method to add column - useful when combining with other commands
beit.f$flow_lps <- beit.f$`Flow (cumec)`*1000 # old way to add column

# wide versus long:
beit.l <- beit %>%
     mutate(flow_lps = 1000*`Flow (cumec)`) %>%
     select(Year, `Level (m)`, `flow_lps`) %>%
     pivot_longer(cols = c(`Level (m)`, flow_lps),
                  names_to = "Variable", 
                  values_to = "Value")
     
beit.w <- beit.l %>%
     pivot_wider(names_from = Variable, 
                 values_from= Value)

# Dates and huge data sets
file <- file.choose()
mutale <- read_csv(file)
head(mutale)
str(mutale)

mutale1 <- mutale %>%
     mutate(datetime = ymd_hm(paste(YEAR, MONT, DAYN, HOUR, MINU))) %>%
     na_if(-9999) %>%
     na_if(-8888) %>%
     na_if(-7777) %>%
     select(-YEAR, -MONT, -DAYN, -HOUR, -MINU) %>%
     rename(precip_mm = PRCP,
            airtemp_C = TEMP,
            humid_p = RHMD, 
            solRad_Wm2 = SRAD, 
            AirPres_kPa = APRS, 
            Wind_sp = WSPD,
            WindDir = WDIR, 
            RivStage_m = RIVS, 
            watertemp = WTMP, 
            Cond_uS_cm = COND,
            Turbidity = TRBD)

file <- file.choose()
disasters <- read_csv("https://duq.box.com/shared/static/48g71l89th4prmlnmp5wupkksaf29gzy.csv", skip = 6, col_names = TRUE, col_types = "ciifffffcfffffccfffccnnfcccciiiiiiiiiiinnnn")  

events <- disasters %>%
     filter(Year <= 2013) %>%
     group_by(Continent) %>%
     summarize(Events = length(Continent))

deaths <- disasters %>%
     group_by(Continent) %>%
     summarize(Deaths = sum(`Total Deaths`, na.rm = TRUE))
