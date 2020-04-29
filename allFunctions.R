#install.packages(c("dplyr","gridExtra","rworldmap",
#                   "randomForest","reshape2","stringi",
#                   "class","tidyr","sf","rnaturalearth",
#                   "rgeos","rnaturalearthdata","jpeg,
#                   "ggpubr", "magritter","ggimage"))
#
library(randomForest)
library(grid)
library(plyr)
library(caret)
library(e1071)
library(ROCR)
library(stringi)
library(dplyr)
library(gridExtra)
library(rworldmap)
library(ggplot2)
library(reshape2)

library(class)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(jpeg)
library(png)
library(magrittr)
library(ggpubr)
library(ggimage)
########################################################################
# Function Definitions:

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



graphTopCountries <- function(df){
  countries_count <- count(df, nationality)
  top10 <- top_n(countries_count, 10, n)
  top10 <- as.data.frame(top10)
  ggplot(top10, aes(x=nationality,y = n)) +geom_bar(stat = "identity", col = "orange", aes(fill = n))+
    ggtitle("Distribution based on Nationality of Players (Top 10 Countries)")
}


addWageandValueLevels <- function(df){
  
  wage_breaks <- c(0, 100000, 200000, 300000, 400000, 500000, Inf)
  wage_labels <- c("0-100k", "100k-200k", "200k-300k", "300k-400k", "400k-500k", "500k+")
  wage_brackets <- cut(x=df$wage_eur, breaks=wage_breaks, 
                       labels=wage_labels, include.lowest = TRUE)
  df <- mutate(df, wage_brackets)
  
  value_breaks <- c(0, 10000000, 20000000, 30000000, 40000000, 50000000, 60000000, 70000000, 80000000, 90000000, 100000000, Inf)
  value_labels <- c("0-10M", "10-20M", "20-30M", "30-40M", "40-50M","50-60M", "60-70M", "70-80M", "80-90M","90-100M","100M+")
  value_brackets <- cut(x=df$value_eur, breaks=value_breaks, 
                        labels=value_labels, include.lowest = TRUE)
  df <-mutate(df, value_brackets)
  return(df)
}

plotAgevsOverall <- function(df){
  g_age_overall <- ggplot(df, aes(age, overall))
  g_age_overall + 
    geom_point(aes(color=wage_brackets), size=3) + geom_smooth(color="darkblue") + 
    ggtitle("Distribution between Age and Overall of players based  on Wages")
}


plotWagesMoreThan100k <- function(df){
  
  not0To100K <- filter(df, wage_brackets != "0-100k")
  ggplot(not0To100K, aes(x = wage_brackets)) + 
    geom_bar(aes(fill = ..count..)) + 
    ggtitle("Distribution of top Wage between 100K-500K+")
  
}

plotValueAbove30M <- function(df){
  moreThan30M <- filter(df, value_eur>30000000)
  ggplot(moreThan30M, aes(x = value_brackets)) + 
    geom_bar(aes(fill = ..count..)) + 
    ggtitle("Distribution of value between 30M-100M+")
}


plotTopClubValue <- function(df){
  group_clubs <- group_by(df, club)
  club_value <- summarise(group_clubs, total_val = sum(value_eur))
  top_10_valuable_clubs <- top_n(club_value, 10, total_val)
  
  top_10_valuable_clubs$Club <-as.factor(top_10_valuable_clubs$club)
  
  ggplot(top_10_valuable_clubs, aes(x = club, y = total_val)) + 
    geom_bar(stat = "identity", aes(fill=total_val)) +
    coord_flip() + ggtitle("Top 10 valuable clubs")
}


removeGKColumns <- function(df){
  
  removedColumns <- c("nationality","value_eur","wage_eur","player_positions",
                      "international_reputation","work_rate",
                      "body_type","release_clause_eur","player_tags",
                      "team_position","team_jersey_number","joined","contract_valid_until",
                      "player_traits","value_brackets","loaned_from",
                      "age","long_name","club","overall","potential",
                      "ls","st","rs","rw","lw","lf","cf","rf","lam",
                      "cam","ram","lm","rm","cm","lcm","rcm","cdm",
                      "ldm","rdm","lwb","rwb","lb","lcb","cb","rcb","rb",
                      "wage_brackets","preferred_foot","gk_diving","gk_handling",
                      "gk_kicking","gk_reflexes","gk_speed","gk_positioning",
                      "goalkeeping_diving","goalkeeping_handling","goalkeeping_kicking",
                      "goalkeeping_positioning","goalkeeping_reflexes")
  
  temp <- df[,!(names(df) %in% removedColumns)]
  temp
  temp <- filter(temp, Position != "GK")
  return(temp) 
}


predictSVM <- function(df){
  
  svmData <- removeGKColumns(df)  
  svmTest <- svmData[1:1000,]
  svmData <- svmData[1000:16242,]
  
  model <- svm(Position~. ,data=svmData,kernel = "linear")
  predictionSVM <- predict(model,svmTest)
  cfmSVM<-confusionMatrix(predictionSVM,svmTest$Position)
  return(cfmSVM)
  
}

predictRandomForrest <- function(df){
  
  tempDF <- removeGKColumns(df)
  testSet <- tempDF[1:1000,]
  trainSet <- tempDF[1000:16242,]
  testSet$Position <- factor(testSet$Position)
  trainSet$Position <- factor(trainSet$Position)
  # To generate same results:
  set.seed(1)
  
  ranforrest= randomForest(Position ~., data= trainSet,
                           mtry= 7, ntree= 250,na.action = na.omit,
                           importance=TRUE)
  rfpredict<- predict(ranforrest, testSet, type="class")
  # To Calculate Accuracy
  mean(rfpredict == testSet$Position)
  return(table(predict=rfpredict, truth=testSet$Position))
  
}

plotCorrelationHeatMap <- function(df){
  
  tempF <- removeGKColumns(df)
  tempF$Position <- as.numeric(as.factor(tempF$Position))
  cormat <- round(cor(tempF),2)
  # If we want to order the correlation map:
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
  #cormat[upper.tri(cormat)] <- NA
  
  melted_cormat <- melt(cormat, na.rm = TRUE)
  
  ggplot(data = melted_cormat, aes(reorder(Var2), reorder(Var1), fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0, 
                                     size = 11, hjust = 1))+
    coord_fixed(ratio= 1)+
    xlab("Attributes")+ylab(" ")
  
  
}


helperFun <- function(column){
  column <- as.numeric(unlist(stri_split_regex(column, "\\+|-", n_max = 1))[1])
  
}

handleNonNumericAttributes <- function(df){
  
  df[34:92] <- apply(df[34:92],MARGIN = 2 ,helperFun)
  return(df)
}

generateDistributionGraph <- function(df, val2){
  f20_att <- removeGKColumns(df)
  f20_att <- f20_att[1:38,]
  f20_att$names <- colnames(f20_att[, 1:38])
  ggbarplot(f20_att, x = "names", y = val2,
            fill = "Position",               # change fill color by cyl
            color = "white",            # Set bar border colors to white
            palette = "jco",            # jco journal color palett. see ?ggpar
            sort.val = "asc",          # Sort the value in dscending order
            sort.by.groups = TRUE,     # Don't sort inside each group
            x.text.angle = 90,           # Rotate vertically x axis texts
            ggtheme = theme_pubclean()
  )+
    font("x.text", size = 8, vjust = 0.5)
}


prepareCountriesData <- function(df){
  df <- df %>% dplyr::select(player_positions, nationality)
  df <- df[!(is.na(df$nationality) | df$nationality==""), ]
  return(df)
}

getCountByNationality <- function(df){
  tmp <- df %>%
    dplyr::group_by(nationality) %>% # or: group_by_at(vars(-score))
    dplyr::summarise(RW = sum(player_positions == "RW"), GK = sum(player_positions == "GK"),
                     LWB = sum(player_positions == "LWB"), LB = sum(player_positions == "LB"),
                     CB = sum(player_positions == "CB"), RB = sum(player_positions == "RB"),
                     RWB = sum(player_positions == "RWB"), LM = sum(player_positions == "LM"),
                     CDM = sum(player_positions == "CDM"), CM = sum(player_positions == "CM"),
                     CAM = sum(player_positions == "CAM"), RM = sum(player_positions == "RM"),
                     CF = sum(player_positions == "CF"), ST = sum(player_positions == "ST"),
                     LW = sum(player_positions == "LW"), RW = sum(player_positions == "RW"))
  return(tmp)
}

drawWorldMapWithPositions <- function(df){
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world$color <- ifelse(world$name %in% df$nationality, df$largest, "Didnt particpate in this fifa")
  ggplot(data = world) +
    geom_sf(aes(fill = as.factor(color))) 
}
getWorldPlot <- function(df){
  temp <- getCountByNationality(df)
  temp$largest <- colnames(temp[,2:ncol(temp)])[apply(temp[,2:ncol(temp)],1,which.max)]
  drawWorldMapWithPositions(temp)
}



#########################################

best_team <- function(df, input){
  
  team <- tibble()
  team_copy <- df %>% arrange(-overall)
  
  tac442 <- c("GK","RB", "LCB", "RCB", "LB", "RM", "CM", "CM", "LM", "ST", "ST")
  tac352 <- c("GK","CB", "LCB", "RCB", "LM", "CDM", "CAM", "CDM", "RM", "ST", "ST")
  tac433 <- c("GK", "RB", "LCB","RCB", "LB", "CAM", "CDM", "CAM", "RW", "ST", "LW")
  
  tactic <- if(input == "4-4-2"){
    tac442
  }else if(input == "3-5-2"){
    tac352
  }else{
    tac433
  }
  
  for (i in tactic) {
    
    team %<>%  bind_rows(team_copy %>% filter(team_position %in% i) %>% head(1))
    team_copy %<>% filter(!short_name %in% (team %>% pull(short_name)))
    
  }
  
  return(team)
  
}


plotBest442 <- function(df_name){
  
  s <- sprintf("Dataset/players_%s.csv", df_name)
  df <- read.csv(s)
  tempDF <- df  %>% select(short_name, team_position, overall)
  
  
  formation442 <- data.frame(Position = as.factor(c("GK","RB", "CB", "CB", "LB", "RM", "CM", "CM", "LM", "ST", "ST")),
                             X = c(5, 17, 17, 17, 17, 45, 35, 35, 45, 60, 60), 
                             Y = c(50 ,16 ,41, 58, 83, 16, 58, 41, 83, 33, 66))
  
  tac442 <- best_team(tempDF, input = "4-4-2")
  F442 <- cbind(tac442, formation442 %>% select(-Position))
  options(repr.plot.width = 12, repr.plot.height = 8)
  
  img <- readJPEG("football_pitch.jpg")
  
  ggplot(F442, aes(X, Y, label = short_name))+
    background_image(img)+
    geom_text(size = 7.5, angle = 5, color = "white", nudge_y = 5)+
    theme_void()+
    xlim(0,70)+
    geom_point(shape = 21, colour = "black", fill = "red", size = 4, stroke = 3)
  
}

plotBest433 <- function(df_name){
  
  s <- sprintf("Dataset/players_%s.csv", df_name)
  df <- read.csv(s)
  tempDF <- df  %>% select(short_name, team_position, overall)
  
  formation433 <- data.frame(Position = as.factor(c("GK", "LB", "CB","CB", "RB", "RM", "CM", "LM", "LW", "ST", "RW")), 
                             X = c(5, 17, 17, 17, 17, 40, 35, 40, 55, 60, 55), 
                             Y = c(50 ,16 ,41, 58, 83, 25, 50, 75, 25, 50, 75))
  
  tac433 <- best_team(tempDF, input = "4-3-3")
  
  F433 <- cbind(tac433, formation433 %>% select(-Position))
  
  options(repr.plot.width = 12, repr.plot.height = 8)
  
  img <- readJPEG("football_pitch.jpg")
  
  ggplot(F433, aes(X, Y, label = short_name))+
    background_image(img)+
    geom_text(size = 7.5, angle = 8, color = "white", nudge_y = 4)+
    theme_void()+
    xlim(0,70)+
    geom_point(shape = 21, colour = "black", fill = "red", size = 4, stroke = 3)

}

plotBest352 <- function(df_name){
  
  s <- sprintf("Dataset/players_%s.csv", df_name)
  df <- read.csv(s)
  
  tempDF <- df  %>% select(short_name, team_position, overall)
  tempDF
  
  formation352 <- data.frame(Position = as.factor(c("GK","CB", "CB", "CB", "RM", "CDM","CAM", "CDM", "LM", "ST", "ST")), 
                             X = c(5, 17, 17, 17, 45, 35, 45, 35, 45, 60, 60), 
                             Y = c(50 ,25 ,50, 75, 16, 35, 50, 65, 83, 33, 66))
  
  
  tac352 <- best_team(tempDF, input = "3-5-2")
  
  F352 <- cbind(tac352, formation352 %>% select(-Position))
  
  options(repr.plot.width = 12, repr.plot.height = 8)
  
  img <- readJPEG("football_pitch.jpg")
  
  ggplot(F352, aes(X, Y, label = short_name))+
    background_image(img)+
    geom_text(size = 7.5, angle = 5, color = "white", nudge_y = 5)+
    theme_void()+
    xlim(0,70)+
    geom_point(shape = 21, colour = "black", fill = "red", size = 4, stroke = 3)
}




getBestTeamForFormation <- function(df_name,input){
  
  s <- sprintf("Dataset/players_%s.csv", df_name)
  df <- read.csv(s)
  tempDF <- df  %>% select(short_name, team_position, overall)
  team <- best_team(tempDF, input)
  return(data.frame(team))
}


getBestTeamByOverall <- function(firstTeam, secondTeam, thirdTeam){
  
  firstTeam <- sum(firstTeam$overall)
  secondTeam <- sum(secondTeam$overall)
  thirdTeam <- sum(thirdTeam$overall)
  allTeams <- c(firstTeam, secondTeam, thirdTeam)
  
  return(which.max(allTeams))
  
}


plotMessiVsCristiano <- function(){
  
  
  MC16 <- f16  %>% select(short_name, overall)
  MC17 <- f17  %>% select(short_name, overall)
  MC18 <- f18  %>% select(short_name, overall)
  MC19 <- f19  %>% select(short_name, overall)
  MC20 <- f20  %>% select(short_name, overall)
  
  
  MC16 <- MC16[MC16$short_name %in% c("L. Messi", "Cristiano Ronaldo"),]
  MC17 <- MC17[MC17$short_name %in% c("L. Messi", "Cristiano Ronaldo"),]
  MC18 <- MC18[MC18$short_name %in% c("L. Messi", "Cristiano Ronaldo"),]
  MC19 <- MC19[MC19$short_name %in% c("L. Messi", "Cristiano Ronaldo"),]
  MC20 <- MC20[MC20$short_name %in% c("L. Messi", "Cristiano Ronaldo"),]
  
  MC <- rbind.fill(MC16,MC17,MC18, MC19, MC20)
  
  year <- c("2016","2016","2017","2017","2018","2018","2019","2019","2020","2020")
  
  messi_image <- readPNG("Images/messi.png")
  cristiano_image <- readPNG("Images/cristiano.png")
  
  P <- ggplot(data=MC, aes(x=year, y=overall, group=short_name)) +
    geom_line(aes(colour=short_name), size=5)+
    ylim(92, 95)+
    labs(color = "Player's Name")
  
  plot(P)
  
  grid.raster(messi_image, 0.7, 0.7, width=.2) # print homer in ll conrner
  grid.raster(cristiano_image, 0.7, 0.4, width=.12) # print homer in ll conrner
  
  
}


logisticRegression_Position <- function(df){
  
  
  tempTest <- removeGKColumns(df)
  
  x <- as.factor(tempTest$Position)
  levels(x) <- list(DEF = c("DEF"), 
                    ATT = c("FWD","MID"))
  tempTest <- mutate(tempTest, factor = x)
  
  tempTest <- tempTest[,!(names(tempTest) %in% c("Position"))]
  
  testSet <- tempTest[1:1000,]
  tempTest <- tempTest[1000:16242,]
  
  as.factor(tempTest$factor)
  as.factor(testSet$factor)
  
  mylogit <- glm(factor ~ weight_kg + weak_foot + skill_moves + 
                   pace + shooting + passing + dribbling + defending + physic, 
                 family = binomial(link = "logit"), data = tempTest, na.action = na.omit)
  
  summary(mylogit)
  pred = predict(mylogit,newdata = testSet, type="response")
  pred = round(pred)
  pred
  
  table(pred)
  testSet$factor <- as.numeric(as.factor(testSet$factor)) - 1
  table(testSet$factor)
  
  
  # Area Under Curve:
  predObj = prediction(pred, testSet$factor)
  rocObj = performance(predObj, measure="tpr", x.measure="fpr")
  aucObj = performance(predObj, measure="auc")
  auc = aucObj@y.values[[1]]
  auc
  plot(rocObj, main = paste("Area under the curve:", auc))
  
  
  cfmLR = confusionMatrix(
    factor(pred, levels = 0:1),
    factor(testSet$factor, levels = 0:1)
  )
  
  return(cfmLR)
  
  
  
}

