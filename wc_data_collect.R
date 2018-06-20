library(fcscrapR)

### Dates of World Cup
gameIDs <- vector()
dates <- c(paste0("201806", 14:30), paste0("2018070", 1:9), paste0("201807", 10:15))

### Extract Game IDs from ESPN Scoreboard
for(date in dates) {
  print(paste0("Getting IDs from ", substring(date,5,6), "/", 
               substring(date,7,8), "/", substring(date,1,4)))  
  url <- paste0("http://www.espnfc.us/fifa-world-cup/4/scores?date=", date)
  x <- scan(url, sep = "\n",  what = "")
  x <- x[grep("gameId", x)]
  nchar("\t\t\t\t<a  name=\"&lpos=scoreboard:")
  x <- suppressWarnings(as.numeric(substring(x, 32, 37)))
  x <- x[!is.na(x)]
  gameIDs <- unique(c(gameIDs, x))
}

### Scrape all data together
for(i in 1:length(gameIDs)) {
  print(paste0("Getting Game ", i, "/64"))
  y <- scrape_commentary(gameIDs[i])
  
  # Data Succesfully Scraped
  if(class(y) == "data.frame") {
    if(i == 1) {
      wc_data <- y
    }
    else {
     wc_data <- rbind(wc_data, y) 
    }
  }
}


### Save Data
write.csv(wc_data, "wc_data.csv", row.names = F)