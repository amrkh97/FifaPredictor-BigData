#install.packages(c("dplyr","gridExtra","rworldmap",
#                   "randomForest","reshape2","stringi",
#                   "class","tidyr","sf","rnaturalearth",
#                   "rgeos","rnaturalearthdata"))

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