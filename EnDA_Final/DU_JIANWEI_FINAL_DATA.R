### load packages
library(rjson)

### read associated data
playerstats <- read.csv("Player Regular 16-17 Stats.csv", stringsAsFactors = FALSE)
playerID <- playerstats$X.Player.ID[playerstats$X.Team.Name == "Warriors"]

### data collecting
l <- list()
for (i in 1:length(playerID)){
  shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2016-17&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID[i],"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
  shotData <- fromJSON(file = shotURL, method="C")
  shotdf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))
  l[[i]] <- shotdf
}

### data cleaning
df <- do.call(rbind, l)
colnames(df) <- shotData$resultSets[[1]][[2]]

# remove unnessary data
df <- subset(df, select = -c(1,2,3,6,7,9,10,15,23,24))
df <- df[-which(df$SHOT_ZONE_BASIC == "Backcourt"),]
df <- df[-which(df$SHOT_ZONE_RANGE == "Back Court Shot"),]

# massage general data
df$SHOT_TYPE <- as.character(df$SHOT_TYPE)
df$SHOT_TYPE[df$SHOT_TYPE == "2PT Field Goal"] <- "2PT"
df$SHOT_TYPE[df$SHOT_TYPE == "3PT Field Goal"] <- "3PT"

df$SHOT_ZONE_BASIC <- as.character(df$SHOT_ZONE_BASIC)
df$SHOT_ZONE_BASIC[df$SHOT_ZONE_BASIC == "Above the Break 3"] <- "Above Break 3"
df$SHOT_ZONE_BASIC[df$SHOT_ZONE_BASIC == "In The Paint (Non-RA)"] <- "Paint (Non-RA)"

df$SHOT_ZONE_RANGE <- as.character(df$SHOT_ZONE_RANGE)
df$SHOT_ZONE_RANGE[df$SHOT_ZONE_RANGE == "Less Than 8 ft."] <- "8- ft."

# make complex Action_Type data easier
df$ACTION_TYPE<-as.character(df$ACTION_TYPE)
for (i in 1:nrow(df)){
  if (grepl("Fadeaway", df$ACTION_TYPE[i])){
    df$ACTION_TYPE[i] <- "Fadeaway"
  }else if (grepl("Hook", df$ACTION_TYPE[i])){
    df$ACTION_TYPE[i] <- "Hook Shot"
  }else if (grepl("Bank", df$ACTION_TYPE[i])){
    df$ACTION_TYPE[i] <- "Bank Shot"
  }else if (grepl("Dunk", df$ACTION_TYPE[i])){
    df$ACTION_TYPE[i] <- "Dunk"
  }else if (grepl("Layup", df$ACTION_TYPE[i])){
    df$ACTION_TYPE[i] <- "Layup"
  }else{
    df$ACTION_TYPE[i] <- "Jump Shot"
  }
}

# convert Game_Date data to type of Date
df$GAME_DATE <- as.character(df$GAME_DATE)
for (i in 1:nrow(df)){
  df$GAME_DATE[i] <- paste(substring(df$GAME_DATE[i],c(1,5,7),c(4,6,8)),collapse="-")
}
df$GAME_DATE <- as.Date(df$GAME_DATE)

### save file for analysis
write.csv(df, "Shotlog_GSW.csv", row.names = F)
