# Clear console (Can be done by pressing ctrl/cmd + L)
cat("\014")
# Clear environment
rm(list=ls())
# Get working directory
getwd()

#install.packages(c("dplyr","gridExtra","rworldmap",
#                   "randomForest","reshape2","stringi","ggpubr", "nnet" , "forcats", "rvest", "xml2", "leaflet", "ggimage"))


require(randomForest)
require(caret)
require(e1071)
require(ROCR)
require(stringi)
require(dplyr)
require(gridExtra)
require(rworldmap)
require(ggplot2)
require(reshape2)
require(ggpubr)
require(nnet)
require(forcats)
require(rvest)
require(xml2)
library(leaflet)
library(ggimage)



# Read datasets
f16_complete <- read.csv("Dataset/players_16.csv")
f17_complete <- read.csv("Dataset/players_17.csv")
f18_complete <- read.csv("Dataset/players_18.csv")
f19_complete <- read.csv("Dataset/players_19.csv")
f20_complete <- read.csv("Dataset/players_20.csv")



# Get unnecessary columns
unnecessaryColumns <- c("sofifa_id","player_url","dob","short_name",
                        "real_face","nation_position",
                        "nation_jersey_number","mentality_composure","loaned_from",
                        "team_position","team_jersey_number","joined","contact_valid_until")

# Remove unnecessary columns
f16 <- f16_complete[,!(names(f16_complete) %in% unnecessaryColumns)]
f17 <- f17_complete[,!(names(f17_complete) %in% unnecessaryColumns)]
f18 <- f18_complete[,!(names(f18_complete) %in% unnecessaryColumns)]
f19 <- f19_complete[,!(names(f19_complete) %in% unnecessaryColumns)]
f20 <- f20_complete[,!(names(f20_complete) %in% unnecessaryColumns)]
# Get top 5 leagues
top5leagues <- c("Arsenal","Manchester United","Manchester City","Liverpool","Tottenham Hotspur","Chelsea",
                 "FC Barcelona","AtlÃ©tico Madrid","Real Madrid","AtlÃ©tico Madrid","Sevilla","Valencia CF",
                 "Napoli","Juventus","Inter","Lazio","Milan","Atalanta","Roma",
                 "Borussia Dortmund","FC Bayern MÃ¼nchen","RB Leipzig","Bayer 04 Leverkusen","Borussia MÃ¶nchengladbach","FC Schalke 04",
                 "Paris Saint-Germain","Olympique Lyonnais","LOSC Lille","Stade Rennais FC","AS Monaco")

# WILL BE USED LATER
# Keep top 5 leagues only
#top2016 <- f16[f16$club %in% top5leagues,]
#top2017 <- f17[f17$club %in% top5leagues,]
#top2018 <- f18[f18$club %in% top5leagues,]
#top2019 <- f19[f19$club %in% top5leagues,]
#top2020 <- f20[f20$club %in% top5leagues,]
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
  cormat[upper.tri(cormat)] <- NA
  
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

####################################################################
# Handle String attributes that caused errors:
# For Example: attacking_crossing, ls and similar attributes.
f16 <- handleNonNumericAttributes(f16)
f17 <- handleNonNumericAttributes(f17)
f18 <- handleNonNumericAttributes(f18)
f19 <- handleNonNumericAttributes(f19)
f20 <- handleNonNumericAttributes(f20)

####################################################################
#re add short_name, sofifa_id
f16 <- cbind(short_name = f16_complete$short_name,f16)
f16 <- cbind(sofifa_id = f16_complete$sofifa_id,f16)
f17 <- cbind(short_name = f17_complete$short_name,f17)
f17 <- cbind(sofifa_id = f17_complete$sofifa_id,f17)
f18 <- cbind(short_name = f18_complete$short_name,f18)
f18 <- cbind(sofifa_id = f18_complete$sofifa_id,f18)
f19 <- cbind(short_name = f19_complete$short_name,f19)
f19 <- cbind(sofifa_id = f19_complete$sofifa_id,f19)
f20 <- cbind(short_name = f20_complete$short_name,f20)
f20 <- cbind(sofifa_id = f20_complete$sofifa_id,f20)


####################################################################
#add season columns
f16 <- cbind(season = 16,f16)
f17 <- cbind(season = 17,f17)
f18 <- cbind(season = 18,f18)
f19 <- cbind(season = 19,f19)
f20 <- cbind(season = 20,f20)

####################################################################
# Factorise player positions:
f16 <- addPositionColumn(f16)
f17 <- addPositionColumn(f17)
f18 <- addPositionColumn(f18)
f19 <- addPositionColumn(f19)
f20 <- addPositionColumn(f20)

####################################################################
#Age Statistics:

# Fifa 16:
g_age <- ggplot(data = f16, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 16:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 16:Distribution based on Age and Position")
ggplot(f16, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 16:Distribution based on General Playing Position")


# Fifa 17:
g_age <- ggplot(data = f17, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 17:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 17:Distribution based on Age and Position")
ggplot(f17, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 17:Distribution based on General Playing Position")

# Fifa 18:
g_age <- ggplot(data = f18, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 18:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 18:Distribution based on Age and Position")
ggplot(f18, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 18:Distribution based on General Playing Position")

# Fifa 19:
g_age <- ggplot(data = f19, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 19:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 19:Distribution based on Age and Position")
ggplot(f19, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 19:Distribution based on General Playing Position")

# Fifa 20:
g_age <- ggplot(data = f20, aes(age))
g_age + geom_histogram(bins = 30,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 20:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 20:Distribution based on Age and Position")
ggplot(f20, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 20:Distribution based on General Playing Position")


########################################################################
# Top 10 Countries:

graphTopCountries(f16)
graphTopCountries(f17)
graphTopCountries(f18)
graphTopCountries(f19)
graphTopCountries(f20)

countries_count <- count(f20, nationality)
top10 <- top_n(countries_count, 10, n)
top10 <- as.data.frame(top10)
# Work-Around to graph UK
levels(top10$nationality) <- c(levels(top10$nationality), "UK") 
top10$nationality[top10$nationality == "England"]  <- "UK"

fr <- joinCountryData2Map(dF = top10,joinCode = "NAME",nameJoinColumn = "nationality",verbose=F) # Prepare data to plot

mapCountryData(mapToPlot = fr,nameColumnToPlot = "nationality",
               catMethod = "fixedWidth",
               oceanCol = "steelblue1",
               missingCountryCol = "white",
               mapTitle = "Number of players per Country",
               colourPalette = "heat",
               aspect = "variable",
               addLegend = F)

########################################################################
# Add Wage and value Labels:

f16 <- addWageandValueLevels(f16)
f17 <- addWageandValueLevels(f17)
f18 <- addWageandValueLevels(f18)
f19 <- addWageandValueLevels(f19)
f20 <- addWageandValueLevels(f20)

#######################################################################
# Wage Above 100k:

plotWagesMoreThan100k(f16)
plotWagesMoreThan100k(f17)
plotWagesMoreThan100k(f18)
plotWagesMoreThan100k(f19)
plotWagesMoreThan100k(f20)

######################################################################
# Age vs Overall:

plotAgevsOverall(f16)
plotAgevsOverall(f17)
plotAgevsOverall(f18)
plotAgevsOverall(f19)
plotAgevsOverall(f20)

######################################################################
# Market Value:

plotValueAbove30M(f16)
plotValueAbove30M(f17)
plotValueAbove30M(f18)
plotValueAbove30M(f19)
plotValueAbove30M(f20)

#####################################################################
# Club Values:

plotTopClubValue(f16)
plotTopClubValue(f17)
plotTopClubValue(f18)
plotTopClubValue(f19)
plotTopClubValue(f20)


#####################################################################
# Predict Position based on attributes:

tempTest <- removeGKColumns(f20)

x <- as.factor(tempTest$Position)
levels(x) <- list(DEF = c("DEF"), 
                  ATT = c("FWD","MID"))
tempTest <- mutate(tempTest, factor = x)

tempTest <- tempTest[,!(names(tempTest) %in% c("Position"))]

testSet <- tempTest[1:1000,]
tempTest <- tempTest[1000:16242,]

as.factor(tempTest$factor)
as.factor(testSet$factor)

mylogit <- glm(factor~.,data =tempTest, family=binomial(link="logit"),
               na.action=na.omit)

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

cfmLR

############################################################################
# Predict using SVM:

CMSVM_F16 <- predictSVM(f16)
CMSVM_F17 <- predictSVM(f17)
CMSVM_F18 <- predictSVM(f18)
CMSVM_F19 <- predictSVM(f19)
CMSVM_F20 <- predictSVM(f20)

CMSVM_F16
CMSVM_F17
CMSVM_F18
CMSVM_F19
CMSVM_F20

############################################################################
# Predict using Random Forrest:

CMRF_F16 <- predictRandomForrest(f16)
CMRF_F17 <- predictRandomForrest(f17)
CMRF_F18 <- predictRandomForrest(f18)
CMRF_F19 <- predictRandomForrest(f19)
CMRF_F20 <- predictRandomForrest(f20)

CMRF_F16
CMRF_F17
CMRF_F18
CMRF_F19
CMRF_F20


############################################################################
# Correlation:

plotCorrelationHeatMap(f16)
plotCorrelationHeatMap(f17)
plotCorrelationHeatMap(f18)
plotCorrelationHeatMap(f19)
plotCorrelationHeatMap(f20)

#-------------------------------------- General Functions----------------------------------------------------------
Plot <- function (df, x_axis, y_axis, g = FALSE, c = NULL)
  {
    ggplot(df,
              aes(y = y_axis, x = x_axis, group = g, color = c)) + geom_point(shape=21, fill="#69b3a2", size=1)
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
           aes(y = dribbling_average,  x = height_brackets, group = season)) + geom_line(aes(color = factor(season)))  + ggtitle("average dribbling for each height range")

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

# removing all goal keapers rows as they have an NA in the driblling cloumns
f16_without_GK <- removeGoalKeapers(f16)
f17_without_GK <- removeGoalKeapers(f17)
f18_without_GK <- removeGoalKeapers(f18)
f19_without_GK <- removeGoalKeapers(f19)
f20_without_GK <- removeGoalKeapers(f20)

# adding height levels column i.e. between 160 cm and 170 cm and so on
f16_without_GK <- addHeightLevels(f16_without_GK)
f17_without_GK <- addHeightLevels(f17_without_GK)
f18_without_GK <- addHeightLevels(f18_without_GK)
f19_without_GK <- addHeightLevels(f19_without_GK)
f20_without_GK <- addHeightLevels(f20_without_GK)

#correlation between height and dribbling
cor(f16_without_GK$height_cm , f16_without_GK$skill_dribbling)
cor(f17_without_GK$height_cm , f17_without_GK$skill_dribbling)
cor(f18_without_GK$height_cm , f18_without_GK$skill_dribbling)
cor(f19_without_GK$height_cm , f19_without_GK$skill_dribbling)
cor(f20_without_GK$height_cm , f20_without_GK$skill_dribbling)

# rows names and columns names for creating the matrix of average dribbling value per each height level for each fifa season
rnames <- c("f16","f17","f18","f19","f20")
cnames <- c("less_than_160_dribbling","between_160_170_dribbling","between_170_180_dribbling"
            ,"between_180_190_dribbling","between_190_200_dribbling","more_than_200_dribbling")


fifa_without_GK_vector <- list(f16_without_GK,f17_without_GK,f18_without_GK,f19_without_GK,f20_without_GK)
average_dribbling_matrix <- matrix(creatingCellsForMatrix(fifa_without_GK_vector), nrow = 5, ncol = 6, 
                                   byrow =TRUE, dimnames = list(rnames, cnames))

#plotting average dribbling value per each height level (this is done for each fifa season and all of them will be plotted on the same graph)
PlotConcatenatedDribblingAverage(fifa_without_GK_vector,average_dribbling_matrix )

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------- Player Position V.s Wage ------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

Plot(f16, y_axis =  log10(f16$wage_eur), x_axis =f16$Position ) # not really useful
table(f16$Position, f16$wage_brackets)
table(f17$Position, f17$wage_brackets)
table(f18$Position, f18$wage_brackets)
table(f19$Position, f19$wage_brackets)
table(f20$Position, f20$wage_brackets)

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------- Preferred-foot V.s wage --------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

ggplot(f20,
           aes(y = wage_eur,  x = preferred_foot)) +
            geom_point(shape=21, size=4, aes(color = preferred_foot)) + 
              ggtitle("wages for each preferred foot players")

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Linear regression Model to predict wage ---------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

# TODO build a regression model

# first i will build a linear regression model for all type of players but Goalkeapers
# the model will be trained on fifa16, fifa17 and fifa18 then will be tested on fifa19 and fifa20
# due to incosistency that may occur; a second case will be handeled were the model will train on a subset of each season then predict the other

# first case (train on fifa16, fifa17 and fifa18)
# preprocessing (bind all of fifa16 till 18 datasets)
f16_till_18 <- rbind(f16_without_GK, f17_without_GK, f18_without_GK)
NROW( f16_till_18[f16_till_18$long_name == "Lionel Andrés Messi Cuccittini"]) # just to check that lionel Messi exists in 3 rows
# remove some columns with NA 
f16_till_18 <- f16_till_18[, !(colnames(f16_till_18) %in% c("gk_diving", "gk_handling", "gk_kicking", "gk_reflexes",
                                                            "gk_speed", "gk_positioning", "release_clause_eur"))]

apply(f16_till_18, 2, function(x) any(is.na(x))) # chech if there is any NA
training = data.frame ( f16_till_18$wage_eur, f16_till_18$age, f16_till_18$height_cm , f16_till_18$weight_kg ,f16_till_18$overall ,
                        f16_till_18$potential ,f16_till_18$value_eur, f16_till_18$skill_moves ,f16_till_18$pace , 
                        f16_till_18$shooting , f16_till_18$passing ,f16_till_18$dribbling , f16_till_18$defending , 
                        f16_till_18$physic ,f16_till_18$attacking_crossing ,f16_till_18$attacking_finishing , 
                        f16_till_18$attacking_heading_accuracy ,f16_till_18$attacking_short_passing , 
                        f16_till_18$attacking_volleys , f16_till_18$skill_dribbling ,f16_till_18$skill_curve , 
                        f16_till_18$skill_fk_accuracy , f16_till_18$skill_long_passing ,f16_till_18$skill_ball_control , 
                        f16_till_18$movement_acceleration , f16_till_18$movement_sprint_speed ,
                        f16_till_18$movement_agility , f16_till_18$movement_reactions , f16_till_18$movement_balance ,
                        f16_till_18$power_shot_power , f16_till_18$power_jumping , f16_till_18$power_stamina ,
                        f16_till_18$power_strength , f16_till_18$power_long_shots , f16_till_18$mentality_aggression ,
                        f16_till_18$mentality_interceptions , f16_till_18$mentality_positioning , 
                        f16_till_18$mentality_vision ,f16_till_18$mentality_penalties , f16_till_18$defending_marking , 
                        f16_till_18$defending_standing_tackle ,f16_till_18$defending_sliding_tackle)
colnames(training) <- c("wage_eur", "age","height_cm","weight_kg","overall","potential","value_eur","skill_moves","pace",
                        "shooting","passing","dribbling","defending","physic","attacking_crossing","attacking_finishing",
                        "attacking_heading_accuracy","attacking_short_passing","attacking_volleys","skill_dribbling",
                        "skill_curve","skill_fk_accuracy","skill_long_passing","skill_ball_control","movement_acceleration",
                        "movement_sprint_speed","movement_agility","movement_reactions","movement_balance","power_shot_power",
                        "power_jumping","power_stamina","power_strength","power_long_shots","mentality_aggression",
                        "mentality_interceptions","mentality_positioning","mentality_vision","mentality_penalties",
                        "defending_marking","defending_standing_tackle","defending_sliding_tackle")

wage_model <- lm (wage_eur ~ age +  height_cm  +  weight_kg  + overall  + 
                  potential  + value_eur +  skill_moves  + pace  +  
                  shooting  +  passing  + dribbling  +  defending  +  
                  physic  + attacking_crossing  + attacking_finishing  +  
                  attacking_heading_accuracy  + attacking_short_passing  +  
                  attacking_volleys  +  skill_dribbling  + skill_curve  +  
                  skill_fk_accuracy  +  skill_long_passing  + skill_ball_control  +  
                  movement_acceleration  +  movement_sprint_speed  + 
                  movement_agility  +  movement_reactions  +  movement_balance  + 
                  power_shot_power  +  power_jumping  +  power_stamina  + 
                  power_strength  +  power_long_shots  +  mentality_aggression  + 
                  mentality_interceptions  +  mentality_positioning  +  
                  mentality_vision  + mentality_penalties  +  defending_marking  +  
                  defending_standing_tackle  + defending_sliding_tackle, data = training)

summary(wage_model)
# predict the same data
wage_predict <- predict(wage_model)
wage_df <- data.frame(wage_predict, f16_till_18$wage_eur)
colnames(wage_df) <- c("predicted_wage", "actual_wage")
wage_df <- transform(wage_df, new.col = actual_wage-predicted_wage)
colnames(wage_df) <- c("predicted_wage", "actual_wage", "difference")
# plotting both of actual and predicted wages
ggplot(wage_df,
       aes(y = difference, x = actual_wage)) + geom_point(shape=21, fill="#69b3a2", size=1)
# Calculate the mean squared error (MSE)of the training data.
sm <- summary(wage_model)
MSE <- mean(sm$residuals^2)
MSE
max(wage_df$difference)
# omitting variables with high p value ( weight, power_shot, ) and with incorrect sign coeff (skill_moves, dribbling, physic, passing, attacking_crossing till the end)
# will try to restore one by one
training = data.frame (f16_till_18$wage_eur, f16_till_18$age, f16_till_18$height_cm, f16_till_18$weight_kg, 
                    f16_till_18$overall , f16_till_18$potential , f16_till_18$value_eur , f16_till_18$skill_moves, 
                    f16_till_18$pace , f16_till_18$shooting , 
                    f16_till_18$defending)
colnames(training) <- c("wage_eur", "age","height_cm","weight_kg","overall","potential","value_eur","skill_moves","pace",
                        "shooting","defending")


wage_model <- lm (wage_eur ~ age + height_cm + weight_kg +
                    overall + potential + value_eur + skill_moves +
                    pace + shooting + 
                    defending , data = training)
summary(wage_model)
# predict the same data
wage_predict <- predict(wage_model)
wage_predict[wage_predict < 0] = 0
wage_df <- data.frame(wage_predict, f16_till_18$wage_eur)
colnames(wage_df) <- c("predicted_wage", "actual_wage")
wage_df <- transform(wage_df, new.col = actual_wage-predicted_wage)
colnames(wage_df) <- c("predicted_wage", "actual_wage", "difference")
# plotting both of actual and predicted wages
ggplot(wage_df,
       aes(y = predicted_wage, x = actual_wage)) + geom_point(shape=21, fill="#69b3a2", size=1)
# Calculate the mean squared error (MSE)of the training data.
sm <- summary(wage_model)
MSE <- mean(sm$residuals^2)
MSE_after_zero_addition <- mean(wage_df$difference^2)
MSE
MSE_after_zero_addition
max(wage_df$difference)

#it might looks like there it has a big mean squared error , however the maximum difference between the actual and the
# predicted wage is around 200k which is really small comparing to the minimum wage range 0-100k

# now try to predict fifa19 and fifa20 wages
f19_and_20 <- rbind(f19_without_GK, f20_without_GK)
#exctract only needed columns
f19_and_20_test <- select(f19_and_20, age, height_cm, weight_kg, overall, potential, value_eur, skill_moves, pace, shooting, passing, dribbling, defending,
       physic, attacking_crossing, attacking_finishing, attacking_heading_accuracy, attacking_short_passing, attacking_volleys, skill_dribbling,
       attacking_short_passing, attacking_volleys, skill_dribbling, skill_curve, skill_fk_accuracy, skill_long_passing,
       skill_ball_control, movement_acceleration, movement_sprint_speed, movement_agility, movement_reactions, movement_balance, power_jumping,
       power_stamina, power_strength, power_long_shots, mentality_aggression, mentality_interceptions, mentality_positioning,
       mentality_vision, mentality_penalties, defending_marking, defending_standing_tackle, defending_sliding_tackle)
f19_and_20_test <- as.data.frame(f19_and_20_test)
wage_predict <- predict(wage_model, newdata =  f19_and_20_test)
wage_df <- data.frame(wage_predict, f19_and_20$wage_eur)
colnames(wage_df) <- c("predicted_wage", "actual_wage")
wage_df <- transform(wage_df, new.col = actual_wage-predicted_wage)
colnames(wage_df) <- c("predicted_wage", "actual_wage", "difference")
# plotting both of actual and predicted wages
ggplot(wage_df,
       aes(y = predicted_wage, x = actual_wage)) + geom_point(shape=21, fill="#69b3a2", size=1)
# Calculate the mean squared error (MSE)of the training data.
sm <- summary(wage_model)
MSE <- mean(sm$residuals^2)
MSE_after_zero_addition <- mean(wage_df$difference^2)
MSE
MSE_after_zero_addition
max(wage_df$difference)
min(wage_df$difference)

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
pre_processed_f19 <- LinearRegressionPreProcessing(f19_without_GK)
pre_processed_f20 <- LinearRegressionPreProcessing(f20_without_GK)
training <- pre_processed_f19
test <- pre_processed_f20


wage_model <- lm (wage_eur ~ age   + 
                  potential  + value_eur  +  
                  dribbling  +  defending  + power_stamina  + 
                  power_long_shots +  mentality_interceptions  +  mentality_positioning  +  
                  mentality_vision  + mentality_penalties  +  defending_marking  +  
                  defending_sliding_tackle, data = training)
summary(wage_model)
wage_predict <- predict(wage_model)
any(wage_predict < 0)
wage_predict[wage_predict < 0] = 0
wage_df <- data.frame(wage_predict, training$wage_eur)
colnames(wage_df) <- c("predicted_wage", "actual_wage")
wage_df <- transform(wage_df, new.col = actual_wage-predicted_wage)
colnames(wage_df) <- c("predicted_wage", "actual_wage", "difference")
# plotting both of actual and predicted wages
ggplot(wage_df,
       aes(y = predicted_wage, x = actual_wage)) + geom_point(shape=21, fill="#69b3a2", size=1)
# Calculate the mean squared error (MSE)of the training data.
sm <- summary(wage_model)
MSE <- mean(sm$residuals^2)
MSE_after_zero_addition <- mean(wage_df$difference^2)
MSE
MSE_after_zero_addition
max(wage_df$difference)

# test wage model

wage_predict <- predict(wage_model, newdata =  test)
any(wage_predict < 0)
wage_predict[wage_predict < 0] = 0
wage_df <- data.frame(wage_predict, test$wage_eur)
colnames(wage_df) <- c("predicted_wage", "actual_wage")
wage_df <- transform(wage_df, new.col = actual_wage-predicted_wage)
colnames(wage_df) <- c("predicted_wage", "actual_wage", "difference")
# plotting both of actual and predicted wages
ggplot(wage_df,
       aes(y = predicted_wage, x = actual_wage)) + geom_point(shape=21, fill="#69b3a2", size=1)
# Calculate the mean squared error (MSE)of the training data.
sm <- summary(wage_model)
MSE <- mean(sm$residuals^2)
MSE_after_zero_addition <- mean(wage_df$difference^2)
MSE
MSE_after_zero_addition


#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Logistic regression Model to predict wage -------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
LogisticRegressionPreProcessing <- function (df)
  {
    # temp <- select (df, potential,value_eur,
    #                     dribbling,defending,power_stamina,
    #                     power_long_shots,mentality_interceptions,mentality_positioning,
    #                     mentality_vision,mentality_penalties,defending_marking,
    #                     defending_sliding_tackle,wage_brackets)
    temp <- select (df, height_cm, weight_kg, overall, potential,value_eur, skill_moves, pace,shooting, passing, 
                        dribbling,defending, physic,  attacking_crossing, attacking_finishing, attacking_heading_accuracy, attacking_short_passing, attacking_volleys, skill_dribbling,
       attacking_short_passing, attacking_volleys, skill_dribbling, skill_curve, skill_fk_accuracy, skill_long_passing,
       skill_ball_control, movement_acceleration, movement_sprint_speed, movement_agility, movement_reactions, movement_balance, power_jumping,
       power_stamina, power_strength, power_long_shots, mentality_aggression, mentality_interceptions, mentality_positioning,
       mentality_vision, mentality_penalties, defending_marking, defending_standing_tackle, defending_sliding_tackle, wage_brackets) 
    return(temp)
  }

Pre_processed_f19_without_GK <- LogisticRegressionPreProcessing(f19_without_GK)
Pre_processed_f20_without_GK <- LogisticRegressionPreProcessing(f20_without_GK)


test_set <- Pre_processed_f20_without_GK
training_set <- Pre_processed_f19_without_GK
table(test_set$wage_brackets)

# training_set$wage_brackets2 <- relevel(training_set$wage_brackets, ref = "0-100k")
wage_logit <- multinom(wage_brackets ~., data = training_set)

z <- summary(wage_logit)$coefficients/summary(wage_logit)$standard.errors
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

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
      summarise(club.squad.value = round(sum(value_eur)/1000000)) %>%
          arrange(-club.squad.value) %>%
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
      summarise(total_wage = round(sum(wage_eur)/1000000 , digits = 2)) %>%
          arrange(-total_wage) %>%
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
#---------------------------------- Finding count of Superstars in teams -----------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotSuperStars <- function (df)
  {
    df %>%
      mutate(super_star = ifelse(overall> 86, "super star","non-super star"))%>%
        group_by(club)%>%
          filter(super_star=="super star")%>%
            summarise(players_count = n())%>%
              arrange(-players_count)%>%
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
#---------------------------------- Age Distribution among the Top Valued Clubs ---------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

PlotAgesForTopValuedClubs <- function(df)
  {
    most_valued_clubs <- df %>%
      group_by(club)%>%
        summarise(club_squad_value = round(sum(value_eur)/1000000))%>%
          arrange(-club_squad_value)%>%
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
        summarise(club_age_average = round(sum(age)/length(age),digits = 2))%>%
          arrange(club_age_average)%>%
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
#---------------------------------- Dominant Nationalities in Fifa --------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotDominantNationalities <-function(df)
  {
    df %>%
     group_by(nationality)%>%
       summarise(players_count = n()) %>%
         arrange(-players_count) %>%
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
                              summarise(players_count = n()) %>%
                                arrange(-players_count) %>%
                                  head(30)

    df[df$nationality %in% nationality_filter$nationality,] %>%
      group_by(nationality)%>%
          summarise(nationality_overall_average = round(sum(overall)/length(overall),digits = 2))%>%
            arrange(-nationality_overall_average)%>%
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
            summarise(players_count = n())%>%
              arrange(-players_count)%>%
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
            summarise(players_count = n())%>%
              arrange(-players_count)%>%
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
      arrange(-skill_fk_accuracy, -skill_curve)%>%
        select(short_name,club,skill_fk_accuracy,skill_curve)%>%
          head(10) %>%
    
    ggplot(aes(x = club, y = skill_fk_accuracy))+
    geom_point(aes(size = skill_curve), color = "violetred1")+
    geom_text(inherit.aes = T, nudge_y = 0.5, aes(label = short_name))+
    geom_image(aes(image = df$image_url))
    xlab("Club")+
    ylab("Free Kick Accuracy")+
    labs(title = paste("Fifa",df$season,"Clubs with best free-kick takers")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
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
#---------------------------------- Extract player image using soffia_id (too slow fashkh) ------------------------
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

###############################################
# add image column
f16 <- AddImageColumn(f16)
f17 <- AddImageColumn(f17)
f18 <- AddImageColumn(f18)
f19 <- AddImageColumn(f19)
f20 <- AddImageColumn(f20)
##################################################

PlotValuableTeams(f16)
PlotWageBills(f16)

PlotValuableTeams(f20)
PlotWageBills(f20)

PlotSuperStars(f16)
PlotSuperStars(f20)

PlotAgesForTopValuedClubs(f16)
PlotAgesForTopValuedClubs(f20)

PlotClubsWithYoungstPlayers(f16)
PlotClubsWithYoungstPlayers(f20)


PlotNationlaitiesWithHighestOverall(f20)

PlotClubsWithHighestPlayerCount(f16)
PlotClubsWithHighestPlayerCount(f17)
PlotClubsWithHighestPlayerCount(f18)
PlotClubsWithHighestPlayerCount(f19)
PlotClubsWithHighestPlayerCount(f20)

PlotClubsWithHighestLeftFootPlayers(f16)
PlotClubsWithHighestLeftFootPlayers(f20)

#############################doll ya 3amr
PlotBestFreeKickTakers(f16)
PlotBestFreeKickTakers(f17)
PlotBestFreeKickTakers(f18)
PlotBestFreeKickTakers(f19)
PlotBestFreeKickTakers(f20)

PlotDominantNationalities(f16)



