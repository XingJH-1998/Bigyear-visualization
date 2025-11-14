library(rebird)
library(dplyr)

df <- read.csv("MyEBirdData.csv")
df$Time_24h <- format(as.POSIXct(df$Time, format="%I:%M %p"), format="%H:%M")
df$ObserverName <- ""
loc <- df$Location.ID
date <- df$Date
time <- df$Time_24h
obserlist <- ""

for(i in 1:nrow(df))
{
  checklist <- ebirdchecklistfeed(loc = loc[i], date = date[i], key = "65cf025nr786") 
  #if(nrow(checklist)==0){next}
  if(!is.null(checklist$obsTime))
  {
  checklist <- checklist %>% filter(obsTime==time[i])
  }
  df$ObserverName[i] <- paste(checklist$userDisplayName, collapse = ",")
  obserlist <- c(obserlist, checklist$userDisplayName) %>% unique()
  print(i)
}
write.csv(df, "df_withname.csv")

