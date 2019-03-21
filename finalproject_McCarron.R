library(plyr)

##set working directory
wd <- setwd("/Users/Owen/Desktop/Classes/Data Mining/project/Data/Final Project/Raw CSV Files")


##Import the Kaggle data files
  RS.data <- read.csv("RegularSeasonDetailedResults.csv")
  TS.data <- read.csv("TourneySeeds.csv")
  TDR.data <- read.csv("TourneyDetailedResults.csv")

##Step 1: Create a single row per team per game in the regular season
  col.labels <- c("Season","team","score","score.opp",
                  "fgm","fga","fgm3","fga3","ftm",
                  "fta","or","dr","ast","to","stl","blk","pf")
  
  Winners <- c("Season","Wteam","Wscore","Lscore",
               "Wfgm","Wfga","Wfgm3","Wfga3","Wftm",
               "Wfta","Wor","Wdr","Wast","Wto","Wstl","Wblk","Wpf")
  RS.WinningTeam <- RS.data[Winners]
  colnames(RS.WinningTeam) <- col.labels
  
  Losers <- c("Season","Lteam","Lscore","Wscore",
              "Lfgm","Lfga","Lfgm3","Lfga3","Lftm",
              "Lfta","Lor","Ldr","Last","Lto","Lstl","Lblk","Lpf")
  RS.LosingTeam <- RS.data[Losers]
  colnames(RS.LosingTeam) <- col.labels
  
  RS.WL <- rbind(RS.WinningTeam,RS.LosingTeam)

##Step 2: Create new variables
  RS.WL$tr <- RS.WL$or+RS.WL$dr
  RS.WL$pt.diff <- RS.WL$score-RS.WL$score.opp
  RS.WL$fg.per <- RS.WL$fgm/RS.WL$fga
  RS.WL$fg3.per <- RS.WL$fgm3/RS.WL$fga3
  RS.WL$ft.per <- RS.WL$ftm/RS.WL$fta
  RS.WL$fgm2 <- RS.WL$fgm - RS.WL$fgm3
  RS.WL$fga2 <- RS.WL$fga - RS.WL$fga3
  RS.WL$fg2.per <- RS.WL$fgm2/RS.WL$fga2

##Step 3: Average the game statistics by team and season
  RS.WL.agg <- aggregate(RS.WL,by=list(RS.WL$Season,RS.WL$team),FUN=mean,na.rm=TRUE)

##Step 4: Aggregate Wins and Losses for the season
  RS.W <- RS.WinningTeam[c("Season","team")]
  RS.W$result <- rep("W",nrow(RS.W))
  RS.W.agg <- ddply(RS.W,c("Season","team"),summarise,Wins=length(result))
  RS.L <- RS.LosingTeam[c("Season","team")]
  RS.L$result <- rep("L",nrow(RS.L))
  RS.L.agg <- ddply(RS.L,c("Season","team"),summarise,Losses=length(result))

  RS.ST <- RS.WL.agg[c("Season","team")]
  RS.ST.mergeW <- merge(x=RS.ST,y=RS.W.agg,by=c("Season","team"),all.x=TRUE)
  RS.ST.merge <- merge(x=RS.ST.mergeW,y=RS.L.agg,by=c("Season","team"),all.x=TRUE)
  RS.ST.merge[is.na(RS.ST.merge)] <- 0
  RS.ST.merge$games <- RS.ST.merge$Wins+RS.ST.merge$Losses
  RS.ST.merge$Wperc <- RS.ST.merge$Wins/RS.ST.merge$games
  RS.ST.merge$Lperc <- RS.ST.merge$Losses/RS.ST.merge$games

## Step 5: Parse Seed and Region data, merge to Regular Season data  
  TS.data$region <- substr(TS.data$Seed,1,1)
  TS.data$seed.numeric <- as.numeric(substr(TS.data$Seed,2,3))
  TS.data.sort <- TS.data[c("Season","Team","Seed","region","seed.numeric")]
  colnames(TS.data.sort) <- c("Season","team","Seed","region","seed")
  RS.WL.Per.merge <- merge(x=RS.WL.agg,y=RS.ST.merge,by=c("Season","team"),all.x=TRUE)
  RS.TS.merge <- merge(x=RS.WL.Per.merge,y=TS.data.sort, by = c("Season","team"),all.x=TRUE)


TDR.WinningTeam <- TDR.data[Winners]
colnames(TDR.WinningTeam) <- col.labels

TDR.LosingTeam <- TDR.data[Losers]
colnames(TDR.LosingTeam) <- col.labels

TDR.WL <- rbind(TDR.WinningTeam,TDR.LosingTeam)

TDR.WL$tr <- TDR.WL$or+TDR.WL$dr

TDR.WL.agg <- aggregate(TDR.WL,by=list(TDR.WL$Season,TDR.WL$team),FUN=mean,na.rm=TRUE)

TDR.W <- TDR.WinningTeam[c("Season","team")]
TDR.W$result <- rep("W",nrow(TDR.W))
TDR.W.agg <- ddply(TDR.W,c("Season","team"),summarise,Wins=length(result))

TDR.L <- TDR.LosingTeam[c("Season","team")]
TDR.L$result <- rep("L",nrow(TDR.L))
TDR.L.agg <- ddply(TDR.L,c("Season","team"),summarise,Losses=length(result))

##Step 6: Aggregate wins in the tournament to derive the champion each season.
  TDR.ST <- TDR.WL.agg[c("Season","team")]
  TDR.ST.mergeW <- merge(x=TDR.ST,y=TDR.W.agg,by=c("Season","team"),all.x=TRUE)
  TDR.ST.merge <- merge(x=TDR.ST.mergeW,y=TDR.L.agg,by=c("Season","team"),all.x=TRUE)
  TDR.ST.merge[is.na(TDR.ST.merge)] <- 0
  TDR.ST.merge$games <- TDR.ST.merge$Wins+TDR.ST.merge$Losses
  TDR.ST.merge$Wperc <- TDR.ST.merge$Wins/TDR.ST.merge$games
  TDR.ST.merge$Lperc <- TDR.ST.merge$Losses/TDR.ST.merge$games
  
  TS.data$region <- substr(TS.data$Seed,1,1)
  TS.data$seed.numeric <- as.numeric(substr(TS.data$Seed,2,3))
  TS.data.sort <- TS.data[c("Season","Team","Seed","region","seed.numeric")]
  colnames(TS.data.sort) <- c("Season","team","Seed","region","seed")
  
  TDR.WL.Per.merge <- merge(x=TDR.WL.agg,y=TDR.ST.merge,by=c("Season","team"),all.x=TRUE)
  TDR.TS.merge <- merge(x=TDR.WL.Per.merge,y=TS.data.sort, by = c("Season","team"),all.x=TRUE)
  
  TDR.TS.merge$champ[TDR.TS.merge$Wins == 6] <- "W"
  TDR.TS.merge$champ[TDR.TS.merge$Wins < 6] <- "L"



TDR.unique <- TDR.TS.merge[c("Season","team")]
TDR.RSavg <- merge(x=TDR.unique,y=RS.TS.merge,by=c("Season","team"),all.x=TRUE)
TDR.data <- TDR.TS.merge[c("Season","team","Wins","champ")]
TDR.weka.merge <- merge(x=TDR.RSavg,y=TDR.data,by=c("Season","team"),all.x=TRUE)
TDR.weka.unsort <- TDR.weka.merge[-c(3:4)]

colnames(TDR.weka.unsort) <- c("Season","team","pts.RS","pts.opp.RS",
                        "fgm.RS","fga.RS","fgm3.RS","fga3.RS",
                        "ftm.RS","fta.RS","or.RS","dr.RS","ast.RS","to.RS","stl.RS","blk.RS","pf.RS",
                        "tr.RS","pt.diff.RS","fg.per.RS","fg3.per.RS","ft.per.RS",
                        "fgm2.RS","fga2.RS","fg2.per.RS",
                        "Wins.RS","Losses.RS","games.RS", 
                        "wperc.RS","lperc.RS","seed.desc.TS","region.TS","seed.num.TS","wins.TS","champ.TS")

TDR.weka <- TDR.weka.unsort[c("Season","team","pts.RS","pts.opp.RS","pt.diff.RS",
                              "fgm.RS","fga.RS","fg.per.RS","fgm2.RS","fga2.RS","fg2.per.RS","fgm3.RS","fga3.RS","fg3.per.RS",
                              "ftm.RS","fta.RS","ft.per.RS","or.RS","dr.RS","tr.RS",
                              "ast.RS","to.RS","stl.RS","blk.RS","pf.RS",
                              "wperc.RS","lperc.RS","region.TS","seed.num.TS","champ.TS")]

write.csv(TDR.weka, file = "TDR.weka.csv",row.names = FALSE)
