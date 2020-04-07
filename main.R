# Clear console (Can be done by pressing ctrl/cmd + L)
cat("\014")
# Clear environment
rm(list=ls())
# Get working directory
getwd()

#install.packages("dplyr")
#install.packages("gridExtra")

library(dplyr)
library(gridExtra)
library(ggplot2)

# Read datasets
f16 <- read.csv("Dataset/players_16.csv")
f17 <- read.csv("Dataset/players_17.csv")
f18 <- read.csv("Dataset/players_18.csv")
f19 <- read.csv("Dataset/players_19.csv")
f20 <- read.csv("Dataset/players_20.csv")

# Get unnecessary columns
unnecessaryColumns <- c("sofifa_id","player_url","dob","short_name",
                        "relaease_clause_eur","real_face","nation_position",
                        "nation_jersey_number","mentality_composure")

# Remove unnecessary columns
f16 <- f16[,!(names(f16) %in% unnecessaryColumns)]
f17 <- f17[,!(names(f17) %in% unnecessaryColumns)]
f18 <- f18[,!(names(f18) %in% unnecessaryColumns)]
f19 <- f19[,!(names(f19) %in% unnecessaryColumns)]
f20 <- f20[,!(names(f20) %in% unnecessaryColumns)]

# Get top 5 leagues
top5leagues <- c("Arsenal","Manchester United","Manchester City","Liverpool","Tottenham Hotspur","Chelsea",
                 "FC Barcelona","AtlÃ©tico Madrid","Real Madrid","AtlÃ©tico Madrid","Sevilla","Valencia CF",
                 "Napoli","Juventus","Inter","Lazio","Milan","Atalanta","Roma",
                 "Borussia Dortmund","FC Bayern MÃ¼nchen","RB Leipzig","Bayer 04 Leverkusen","Borussia MÃ¶nchengladbach","FC Schalke 04",
                 "Paris Saint-Germain","Olympique Lyonnais","LOSC Lille","Stade Rennais FC","AS Monaco")

# Keep top 5 leagues only
top2016 <- f16[f16$club %in% top5leagues,]
top2017 <- f17[f17$club %in% top5leagues,]
top2018 <- f18[f18$club %in% top5leagues,]
top2019 <- f19[f19$club %in% top5leagues,]
top2020 <- f20[f20$club %in% top5leagues,]

########################################################################

addPositionColumn <- function(df){
  df$player_positions <- gsub(" ", "", substr(df$player_positions, 1, 3))
  df$player_positions <- gsub(",", "", substr(df$player_positions, 1, 3))
  x <- as.factor(df$player_positions)
  levels(x) <- list(GK  = c("GK"), 
                    DEF = c("LWB", "LB", "CB", "RB", "RWB"), 
                    MID = c("LM","CDM","CM","CAM","RM"), 
                    FWD = c("CF", "ST","LW","RW"))
  df <- mutate(df, Position = x)
  return(df)
}


#Factorise player positions:
f16 <- addPositionColumn(f16)
f17 <- addPositionColumn(f17)
f18 <- addPositionColumn(f18)
f19 <- addPositionColumn(f19)
f20 <- addPositionColumn(f20)
#Age Statistics:

# Fifa 16:
g_age <- ggplot(data = f16, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 16:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 16:Distribution based on Age and Position")

# Fifa 17:
g_age <- ggplot(data = f17, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 17:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 17:Distribution based on Age and Position")

# Fifa 18:
g_age <- ggplot(data = f18, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 18:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 18:Distribution based on Age and Position")

# Fifa 19:
g_age <- ggplot(data = f19, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 19:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 19:Distribution based on Age and Position")

# Fifa 20:
g_age <- ggplot(data = f20, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 20:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 20:Distribution based on Age and Position")


