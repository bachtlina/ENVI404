library(tidyverse) # includes ggplot and readr commands
library(RColorBrewer) # used for wind rose with color code by speed
library(lubridate) # used for dates, specifically month separation


## Heinz Field (HFP)
hfp <- read_csv("https://duq.box.com/shared/static/2cs6xi81xtcmq4mmi46t0v0ev4f2mehs.csv")
#hfp$date <- as_date(hfp$Timestamp)
#hfp$month <- month(hfp$date)
hfp$dir <- hfp$`Wind Vane` # degrees
hfp$spd <- 0.44704 * hfp$Anemometer # converted to m/s from mph, per https://allegheny.weatherstem.com/pitt

## NEW METHOD - with speed binning
speed.bins <- 7
dir.bins <- 36

wind <- array(0, dim = c(speed.bins, dir.bins))
for (t in 1:nrow(hfp)) {
     j <- ceiling(hfp$dir[t]/10)
     i <- ceiling(hfp$spd[t])
     # if (i > 3) { 
     #      i <- 3
     # }
     wind[i,j] <- wind[i,j] + 1
} 

wind.long <- array(NA, dim = dir.bins*speed.bins) #using NA because we will be filling in every single bin, if there are any NAs left over, now it didnt work properly
speeds <- c(rep("0-1",dir.bins), rep("1-2",dir.bins), rep("2-3",dir.bins), rep("3-4",dir.bins), rep("4-5",dir.bins), rep("5-6",dir.bins), rep("6-7",dir.bins)) # be sure to fill in as many as the wind bins in "wind" allocation
directions <- rep(5+10*(c(0:35)), speed.bins) 
for (i in 1:speed.bins) {
     for(j in 1:dir.bins){
          wind.long[(dir.bins*(i-1))+j] <- wind[i,j] #how we define each bin, setting up long table
     }
     
}
rose <- data.frame(directions, speeds, wind.long)
ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) +
     labs(caption = paste("Heinz Field")) +
     geom_bar(position="stack", stat="identity") +
     scale_fill_brewer("Speed (m/s)", palette = "Greens") +
     coord_polar(theta = "x", start = 0) +
     scale_x_continuous(breaks = seq(0, 360, 45)) +
     theme_linedraw() +
     theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank())
