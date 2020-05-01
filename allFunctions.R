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

require(purrr)
require(ggpubr)
require(nnet)
require(forcats)
require(rvest)
require(xml2)
library(leaflet)
library(ggimage)
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
library(caTools)
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
  club_value <- dplyr::summarise(group_clubs, total_val = sum(value_eur))
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
  
  df[34:92] <- apply(df[34:92],MARGIN = c(1,2) ,helperFun)
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
  team_copy <- df %>% dplyr::arrange(-overall)
  
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
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Finding the most valuable teams -----------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotValuableTeams <- function(df)
{
  #  fifa_grouped_by_club <- group_by(df,club)
  #  fifa_grouped_by_club_summarized <- summarise(fifa_grouped_by_club, club.squad.value = round(sum(value_eur)/1000000))
  #  fifa_grouped_by_club_summarized_arranged <- arrange(fifa_grouped_by_club_summarized,-club.squad.value)
  #  fifa_grouped_by_club_summarized_arranged <- head(fifa_grouped_by_club_summarized_arranged,10)
  df %>% group_by(club) %>%
    dplyr::summarise(club.squad.value = round(sum(value_eur)/1000000)) %>%
    dplyr::arrange(-club.squad.value) %>%
    head(10) %>%
    
    ggplot(aes(
      x = fct_reorder(as.factor(club),club.squad.value), y = club.squad.value, label = club.squad.value))+
    geom_text(hjust = 0.01,inherit.aes = T, position = "identity")+
    geom_bar(stat = "identity", fill = "violetred1",alpha=.6, width=.4)+
    coord_flip()+
    xlab("Club")+
    ylab("Squad Value in Million")+
    labs(title = paste("Fifa",df$season ,"Most Valuable Teams")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Finding the Top Wage Bills teams ----------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotWageBills <- function(df)
{
  df %>% group_by(club) %>%
    dplyr::summarise(total_wage = round(sum(wage_eur)/1000000 , digits = 2)) %>%
    dplyr::arrange(-total_wage) %>%
    head(10) %>%
    
    ggplot(aes(
      x = fct_reorder(as.factor(club),total_wage), y = total_wage, label = total_wage))+
    geom_text(hjust = 0.01,inherit.aes = T, position = "identity")+
    geom_bar(stat = "identity", fill = "violetred1",alpha=.6, width=.4)+
    coord_flip()+
    xlab("Club")+
    ylab( "Squad Wages in Million")+
    labs(title =  paste("Fifa",df$season ,"Top Wage Bills")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Finding count of Superstars in teams ------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotSuperStars <- function (df)
{
  df %>%
    mutate(super_star = ifelse(overall> 86, "super star","non-super star"))%>%
    group_by(club)%>%
    filter(super_star=="super star")%>%
    dplyr::summarise(players_count = n())%>%
    dplyr::arrange(-players_count)%>%
    ggplot(aes(x = as.factor(club) %>%
                 fct_reorder(players_count), y = players_count, label = players_count))+
    geom_text(hjust = 0.01,inherit.aes = T, position = "identity")+
    geom_bar(stat = "identity", fill = "palegreen2",alpha=.6, width=.4)+
    coord_flip()+
    xlab("Club")+
    ylab( "Number of Superstars") +
    labs(title =  paste("Fifa",df$season,"Number of Superstars per top 10 clubs")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Age Distribution among the Top Valued Clubs -----------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotAgesForTopValuedClubs <- function(df)
{
  most_valued_clubs <- df %>%
    group_by(club)%>%
    dplyr::summarise(club_squad_value = round(sum(value_eur)/1000000))%>%
    dplyr::arrange(-club_squad_value)%>%
    head(10)
  
  clubs_and_ages <- df[df$club %in% most_valued_clubs[[1]], c("club", "age")]
  
  ggplot(clubs_and_ages, aes(x = age, y = club, fill = club)) +
    geom_violin(trim = F)+
    geom_boxplot(width = 0.1)+
    theme(axis.text.x = element_text(angle = 90), legend.position = "none")+
    ylab("Age distribution amongst Clubs") +
    labs(title = paste("Fifa",df$season,"Age Distribution among the Top Valued Clubs")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Clubs with the youngest Squad -------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotClubsWithYoungstPlayers <- function(df)
{
  df %>%
    group_by(club)%>%
    dplyr::summarise(club_age_average = round(sum(age)/length(age),digits = 2))%>%
    dplyr::arrange(club_age_average)%>%
    head(10)%>%
    
    ggplot(aes(y = fct_reorder(as.factor(club),club_age_average), x = club_age_average, label = club_age_average))+
    geom_bar(stat = "identity", fill = "turquoise4",alpha=.6, width=.4)+
    geom_text(inherit.aes = T, nudge_y = 0.5)+
    xlab("Club")+
    theme(axis.text.x = element_text(angle = 90))+
    ylab("Average Squad Age")+
    labs(title = paste("Fifa",df$season,"Clubs with the youngest Squad")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Dominant Nationalities in Fifa ------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotDominantNationalities <-function(df)
{
  df %>%
    group_by(nationality)%>%
    dplyr::summarise(players_count = n()) %>%
    dplyr::arrange(-players_count) %>%
    head(10) %>%
    
    ggplot(aes(y = fct_reorder(as.factor(nationality),players_count), x = players_count, label = players_count))+
    geom_bar(stat = "identity", fill = "turquoise4",alpha=.6, width=.4)+
    geom_text(inherit.aes = T, nudge_y = 0.5)+
    xlab("Number of Players")+
    theme(axis.text.x = element_text(angle = 90))+
    ylab("Nationality")+
    labs(title = paste("Fifa",df$season,"Dominant Nationalities")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  
}



#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Nationalities with highest overall --------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotNationlaitiesWithHighestOverall <- function(df)
{
  nationality_filter <- df %>%
    group_by(nationality)%>%
    dplyr::summarise(players_count = n()) %>%
    dplyr::arrange(-players_count) %>%
    head(30)
  
  df[df$nationality %in% nationality_filter$nationality,] %>%
    group_by(nationality)%>%
    dplyr::summarise(nationality_overall_average = round(sum(overall)/length(overall),digits = 2))%>%
    dplyr::arrange(-nationality_overall_average)%>%
    head(10)%>%
    
    ggplot(aes(x = fct_reorder(as.factor(nationality),nationality_overall_average), y = nationality_overall_average, label = nationality_overall_average))+
    geom_bar(stat = "identity", fill="#f68060", alpha=.6, width=.4)+
    geom_text(inherit.aes = T, nudge_y = 0.8)+
    xlab("Nationality")+
    coord_flip()+
    theme(axis.text.x = element_text(angle = 90))+
    ylab("Average Natinality Overall")+
    labs(title = paste("Fifa",df$season,"Nationalities with highest overall")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))+
    theme_bw()
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Clubs with highest number of players ------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotClubsWithHighestPlayerCount <- function(df)
{
  df %>%
    group_by(club)%>%
    dplyr::summarise(players_count = n())%>%
    dplyr::arrange(-players_count)%>%
    head(10)%>%
    ggplot(aes(x = as.factor(club) %>%
                 fct_reorder(players_count), y = players_count, label = players_count,
               fill = factor(ifelse(club=="Arsenal","Arsenal","Others"))))+
    geom_text(hjust = 0.01,inherit.aes = T, position = "identity")+
    geom_bar(stat = "identity", alpha=.6, width=.4)+
    scale_fill_manual(name = "club", values=c("red","palegreen2"))+
    coord_flip()+
    xlab("Club")+
    ylab( "Number of players") +
    labs(title =  paste("Fifa",df$season,"Number of players per top 10 clubs")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Clubs with highest number of left foot players --------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotClubsWithHighestLeftFootPlayers <-function(df)
{
  df %>%
    group_by(club)%>%
    filter(preferred_foot=="Left")%>%
    dplyr::summarise(players_count = n())%>%
    dplyr::arrange(-players_count)%>%
    head(10)%>%
    ggplot(aes(x = as.factor(club) %>%
                 fct_reorder(players_count), y = players_count, label = players_count))+
    geom_text(hjust = 0.01,inherit.aes = T, position = "identity")+
    geom_bar(stat = "identity", fill = "palegreen2",alpha=.6, width=.4)+
    coord_flip()+
    xlab("Club")+
    ylab( "Number of Left Foot PLayers") +
    labs(title =  paste("Fifa",df$season,"Number of left foot players per top 10 clubs")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Best free kick takers in the game ---------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotBestFreeKickTakers <- function(df)
{
  df %>%
    dplyr::arrange(-skill_fk_accuracy, -skill_curve)%>%
    select(short_name,club,skill_fk_accuracy,skill_curve,image_url)%>%
    head(10) %>%
    
    ggplot(aes(x = club, y = skill_fk_accuracy))+
    geom_point(aes(size = skill_curve), color = "violetred1")+
    geom_text(inherit.aes = T, nudge_y = -0.5, aes(label = short_name))+
    geom_image(aes(image = image_url), nudge_y=1.1, size = 0.11)+
    xlab("Club")+
    ylab("Free Kick Accuracy")+
    labs(title = paste("Fifa",df$season,"Clubs with best free-kick takers")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylim(85,97)
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Extract player image using webpage (too slow fashkh) --------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
AddImageColumnByWeb <- function(df)
{
  players_URLs_list <- as.character(df$player_url)
  image_data_list <- lapply(players_URLs_list, function(x)
  {
    webpage <- read_html(x)
    webpage %>%
      html_nodes(xpath = "/html/body/div[2]/div/div/div[1]/div/div[1]/div/img") %>%
      html_attr("data-src")
  })
}

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Extract player image using soffia_id ------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
AddImageColumn <-function(df)
{
  # extract players images
  players_id <- df$sofifa_id
  players_id <- formatC(players_id, format = "d", digits = 5, flag = "0")
  players_id <- as.data.frame(players_id)
  
  image_url <- rep("https://cdn.sofifa.com/players/",NROW(players_id))
  image_url <- as.data.frame(image_url)
  
  image_url$image_url <- paste(image_url$image_url,substr(players_id$players_id,1,3),"/",
                               substr(players_id$players_id,4,6),"/",as.character(df$season[1]),"_120.png",sep ="")
  return(mutate(df, image_url = image_url$image_url))
  
}
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------- Height V.s. Dribbiling --------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------


# there was no direct corelation between hight and dribbling
# plotting a graph between height and dribbling for all the players was a total mess and did not indicate anything
# so instead, i have took the average dribbling value for each height range 
addHeightLevels <- function(df)
{
  
  height_breaks <- c(0, 160, 170, 180, 190, 200, Inf)
  height_labels <- c("-160", "160-170", "170-180", "180-190", "190-200", "200+")
  height_brackets <- cut(x=df$height_cm, breaks=height_breaks, 
                         labels=height_labels, include.lowest = TRUE)
  df <- mutate(df, height_brackets)
  return (df)
}

removeGoalKeapers <- function (df)
{
  temp <- subset(df , df$player_positions != "GK")
  return(temp)
}

getDribblingAverage <- function (df , c)
{
  return (colSums(select(df[df$height_brackets == c,], dribbling )) / NROW(df[df$height_brackets == c,]))
}

AddDribblingAverageToDataframe <- function (df , array)
{
  height_breaks <- c(0, 160, 170, 180, 190, 200, Inf)
  dribbling_average_labels <- array
  dribbling_average <- cut(x=df$height_cm, breaks=height_breaks, 
                           labels=dribbling_average_labels, include.lowest = TRUE)
  df <- mutate(df, dribbling_average)
  
  df$dribbling_average <-  as.numeric(as.character(df$dribbling_average))
  
  return(df)
  
}
PlotConcatenatedDribblingAverage <- function (fifa_without_GK_vector , average_dribbling_matrix)
{
  p1 <- AddDribblingAverageToDataframe(fifa_without_GK_vector[[1]] , average_dribbling_matrix[1,])
  p2 <- AddDribblingAverageToDataframe(fifa_without_GK_vector[[2]] , average_dribbling_matrix[2,])
  p3 <- AddDribblingAverageToDataframe(fifa_without_GK_vector[[3]] , average_dribbling_matrix[3,])
  p4 <- AddDribblingAverageToDataframe(fifa_without_GK_vector[[4]] , average_dribbling_matrix[4,])
  p5 <- AddDribblingAverageToDataframe(fifa_without_GK_vector[[5]] , average_dribbling_matrix[5,])
  
  visuals <- rbind(p1,p2,p3,p4,p5)
  ggplot(visuals,
         aes(y = dribbling_average,  x = height_brackets, group = season)) + geom_line(aes(color = factor(season)))  +
    ggtitle("average dribbling for each height range")+theme(plot.title = element_text(hjust = 0.5, size = 14))
  
}
# creating a matrix that will contain average dribbling skill for each height range per each fifa season
creatingCellsForMatrix <- function (fifa_without_GK_vector)
{
  height_levels <- c("-160","160-170","170-180","180-190","190-200","200+")
  cells <- c()
  for( fifa in fifa_without_GK_vector )
  {
    for (level in height_levels)
    {
      cells <-c(cells, getDribblingAverage(fifa,level))
    }
  }
  return(cells)
}
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------- Player Position V.s Wage ------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PLotPLayerPositionWithWage <- function(df,exclude_less_than_100k = FALSE)
  {
    if (exclude_less_than_100k){
        data <-df %>%
        group_by(wage_brackets,Position)%>%
        filter(wage_brackets!="0-100k")%>%
        dplyr::summarise(players_count = n())
    }
  else{
      data <-df %>%
      group_by(wage_brackets,Position)%>%
      dplyr::summarise(players_count = n())
  }
    
    ggplot(data,aes(x = Position, y = players_count, label = players_count))+
    geom_bar(stat = "identity",alpha=.6, width=.4,aes(fill = wage_brackets))+
    coord_flip()+
    xlab("Position")+
    ylab( "Number of PLayers") +
    labs(title =  paste("Fifa",df$season,"Number of players per each wage range")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  }
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------- Preferred-foot V.s wage -------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PLotWageForEachFoot <- function(df)
  {
    df %>%
    group_by(wage_brackets,preferred_foot)%>%
    filter(wage_brackets!="0-100k")%>%
    dplyr::summarise(players_count = n()) %>%
    
    ggplot(aes(x = preferred_foot, y = players_count, label = players_count))+
    geom_bar(stat = "identity",alpha=.6, width=.4,aes(fill = wage_brackets))+
    coord_flip()+
    xlab("Preferred foot")+
    ylab( "Number of PLayers") +
    labs(title =  paste("Fifa",df$season,"Wages for different preferred-foot players")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  }

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Predict Value using linear Regression -----------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
ValuePredictionPreProcesssing <- function (df)
{
  removedColumns <- c("nationality","player_positions","season", "sofifa_id",
                      "international_reputation","work_rate",
                      "body_type","player_tags",
                      "team_position","team_jersey_number","joined","contract_valid_until",
                      "player_traits","value_brackets","loaned_from",
                      "long_name","club","potential",
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
GetMostCorrelatedToValue <- function(df)
  {
    fifa_preprocessed <- ValuePredictionPreProcesssing(df)
    fifa_int <- fifa_preprocessed[,map_lgl(fifa_preprocessed,is.numeric)]

    correlation_table<- as.data.frame(cor(fifa_int, use = "complete.obs"))

    value_correlation_column <- correlation_table["value_eur"]

    value_correlation_column <- subset(value_correlation_column, value_eur > 0.48)

    attributes_correleted_to_value <- rownames(value_correlation_column)
    attributes_correleted_to_value <- as.vector(attributes_correleted_to_value)
    attributes_correleted_to_value <- attributes_correleted_to_value[attributes_correleted_to_value != "value_eur"]
    return(attributes_correleted_to_value)
  }

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Polynomial Regression Model to predict wage -----------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

WagePredictionPreProcesssing <- function (df)
{
  removedColumns <- c("nationality","player_positions","season", "sofifa_id",
                      "international_reputation","work_rate",
                      "body_type","player_tags",
                      "team_position","team_jersey_number","joined","contract_valid_until",
                      "player_traits","value_brackets","loaned_from",
                      "long_name","club","potential",
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
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- The Most correlated attributes to wage  ---------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PrintMostCorrelatedToWage <- function(df)
  {
    fifa_preprocessed <- WagePredictionPreProcesssing(df)
    fifa_int <- fifa_preprocessed[,map_lgl(fifa_preprocessed,is.numeric)]

    correlation_table<- as.data.frame(cor(fifa_int, use = "complete.obs"))

    value_correlation_column <- correlation_table["wage_eur"]
    value_correlation_column <- subset(value_correlation_column, wage_eur > 0.32)
    attributes_correleted_to_value <- rownames(value_correlation_column)
    attributes_correleted_to_value <- as.vector(attributes_correleted_to_value)
    return(attributes_correleted_to_value)
  }
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Linear regression Model to predict wage ---------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#--------------------------------------- Second Trial -------------------------------------------------------------
LinearRegressionPreProcessing <- function (df)
  {
    temp <- select (df, wage_eur, age,potential,value_eur,
                        dribbling,defending,power_stamina,
                        power_long_shots,mentality_interceptions,mentality_positioning,
                        mentality_vision,mentality_penalties,defending_marking,
                        defending_sliding_tackle) 
    return(temp)
  }