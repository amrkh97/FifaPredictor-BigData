# Clear console (Can be done by pressing ctrl/cmd + L)
cat("\014")
# Clear environment
rm(list=ls())
# Get working directory
getwd()

#install.packages(c("dplyr","gridExtra","rworldmap",
#                   "randomForest","reshape2","stringi"))
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
library(dplyr)
library(arules)
library(arulesViz)
library(tm)
library(tmap)
library(wordcloud)
library(class)
library(tidyr)
library(UpSetR)

# Read datasets
f16 <- read.csv("Dataset/players_16.csv")
f17 <- read.csv("Dataset/players_17.csv")
f18 <- read.csv("Dataset/players_18.csv")
f19 <- read.csv("Dataset/players_19.csv")
f20 <- read.csv("Dataset/players_20.csv")

# Get unnecessary columns
unnecessaryColumns <- c("sofifa_id","player_url","dob","short_name",
                        "relaease_clause_eur","real_face","nation_position",
                        "nation_jersey_number","mentality_composure","loaned_from",
                        "team_position","team_jersey_number","joined","contact_valid_until")

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

prepareFootData <- function(df){
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
                      "goalkeeping_positioning","goalkeeping_reflexes", "Position")
  
  temp <- df[,!(names(df) %in% removedColumns)]
  temp
  temp <- temp[complete.cases(temp), ]
  return(temp)
}

addFootColumn <- function(df){
  df$player_positions <- gsub(" ", "", substr(df$player_positions, 1, 2))
  df$player_positions <- gsub(",", "", substr(df$player_positions, 1, 2))
  x <- as.factor(df$preferred_foot)
  levels(x) <- list(LEFT  = c("Left"), 
                    RIGHT = c("Right"))
  df <- mutate(df, foot = x)
  return(df)
}

helperFun <- function(column){
  column <- as.numeric(unlist(stri_split_regex(column, "\\+|-", n_max = 1))[1])
  
}

handleNonNumericAttributes <- function(df){
  
  df[34:92] <- apply(df[34:92],MARGIN = 2 ,helperFun)
  return(df)
}

prepareAssociationDataForPosition <- function(df){
  temp <- df[!(df$Position =="GK"),]
  temp <- temp[ , colSums(is.na(temp)) == 0]
  temp <- temp[complete.cases(temp), ]
  temp <- temp %>%select(Position, pace, shooting, passing, dribbling, defending, physic)
  temp[,2] <- factor(temp[,2])
  temp[,3] <- factor(temp[,3])
  temp[,4] <- factor(temp[,4])
  temp[,5] <- factor(temp[,5])
  temp[,6] <- factor(temp[,6])
  temp[,7] <- factor(temp[,7])
  transaction <- as(temp,"transactions")
  return(transaction)
}
checkTopRulesForPositions <- function(transaction){
  inspect(transaction[1:10])
  itemFrequency(transaction[1:10])
  itemFrequencyPlot(transaction, topN = 5)
  associationRules <- apriori(data=transaction, parameter=list (supp=0.001,conf = 0.08), 
                              appearance = list (default="lhs",rhs= c("Position=GK", "Position=FWD",
                                                                      "Position=MID", "Position=DEF")), 
                              control = list (verbose=F))
  inspect(associationRules)
  support <- sort(associationRules, by = "support")[1:6]
  inspect(support)
  confidence <- sort(associationRules, by = "confidence")[1:6]
  inspect(confidence)
  lift <- sort(associationRules, by = "lift")[1:6]
  inspect(lift)
  plot(associationRules, jitter = 0, engine = "plotly")
}
prepareDataForTagsPositions <- function(df){
  #df <- unite(df, player_tags, c(player_traits, player_tags), remove=FALSE)
  df <- df %>% select(player_positions, player_tags)
  df <- df[!(is.na(df$player_tags) | df$player_tags==""), ]
  return(df)
}
cleanCorpus <- function(df){
  corp <- Corpus(VectorSource(df$player_tags)) 
  corp <- tm_map(corp,removePunctuation)
  corp <- tm_map(corp,stripWhitespace)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp,removeNumbers)
  corp <- tm_map(corp,removeWords,stopwords("en"))
  return(corp)
}
generateTDM <- function(corp){
  s.tdm <- TermDocumentMatrix(corp)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  return(s.tdm)
}
generateTextDf <- function(lis){
  s.mat <- t(data.matrix(lis[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  s.df <- cbind(s.df, rep(lis[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetPositions"
  return(s.df)
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
startTextMiningToGetTopPositions <- function(df){
  f20_mod <- prepareDataForTagsPositions(df)
  corp.f20 <- cleanCorpus(f20_mod)
  s.tdm <- generateTDM(corp.f20)
  findFreqTerms(s.tdm, 65)
  normalmat <- as.matrix(s.tdm)
  normalmat <- t(normalmat)
  pfrequency <- colSums(normalmat)
  freq <- data.frame(sort(pfrequency, decreasing=TRUE))
  head(freq, n=5)
  pwords <- names(pfrequency[1:5]) 
  wordcloud(pwords[1:5], pfrequency[1:5], random.color=TRUE, 
            colors=c("red","green","blue","orange","yellow","pink"))
  lis <- list(name=f20_mod$player_positions, tdm=s.tdm)
  s.df <- generateTextDf(lis)
  s.df$row_sum <- rowSums(s.df[,1:ncol(s.df)-1])
  head(s.df[order(s.df$row_sum, decreasing=TRUE),c(ncol(s.df)-1, ncol(s.df))], n=5)
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

step(mylogit, direction = "backward")

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

############################################################################
# Association Rules For Position:
f16_transaction <- prepareAssociationDataForPosition(f16)
checkTopRulesForPositions(f16_transaction)
f17_transaction <- prepareAssociationDataForPosition(f17)
checkTopRulesForPositions(f17_transaction)
f18_transaction <- prepareAssociationDataForPosition(f18)
checkTopRulesForPositions(f18_transaction)
f19_transaction <- prepareAssociationDataForPosition(f19)
checkTopRulesForPositions(f19_transaction)
f20_transaction <- prepareAssociationDataForPosition(f20)
checkTopRulesForPositions(f20_transaction)
############################################################################
# Text Minning Positionand Tags:
# F20
startTextMiningToGetTopPositions(f20)
# F19
startTextMiningToGetTopPositions(f19)
# F18
startTextMiningToGetTopPositions(f18)
# F17
startTextMiningToGetTopPositions(f17)
# F16
startTextMiningToGetTopPositions(f16)
############################################################################
# Plot Attributes
library(ggpubr)
# F20
generateDistributionGraph(f20, "pace")
generateDistributionGraph(f20, "Position")
generateDistributionGraph(f20, "shooting")
generateDistributionGraph(f20, "defending")
generateDistributionGraph(f20, "physic")
generateDistributionGraph(f20, "passing")
generateDistributionGraph(f20, "dribbling")
# F19
generateDistributionGraph(f19, "pace")
generateDistributionGraph(f19, "Position")
generateDistributionGraph(f19, "shooting")
generateDistributionGraph(f19, "defending")
generateDistributionGraph(f19, "physic")
generateDistributionGraph(f19, "passing")
generateDistributionGraph(f19, "dribbling")
# F18
generateDistributionGraph(f18, "pace")
generateDistributionGraph(f18, "Position")
generateDistributionGraph(f18, "shooting")
generateDistributionGraph(f18, "defending")
generateDistributionGraph(f18, "physic")
generateDistributionGraph(f18, "passing")
generateDistributionGraph(f18, "dribbling")
# F17
generateDistributionGraph(f17, "pace")
generateDistributionGraph(f17, "Position")
generateDistributionGraph(f17, "shooting")
generateDistributionGraph(f17, "defending")
generateDistributionGraph(f17, "physic")
generateDistributionGraph(f17, "passing")
generateDistributionGraph(f17, "dribbling")
# F16
generateDistributionGraph(f16, "pace")
generateDistributionGraph(f16, "Position")
generateDistributionGraph(f16, "shooting")
generateDistributionGraph(f16, "defending")
generateDistributionGraph(f16, "physic")
generateDistributionGraph(f16, "passing")
generateDistributionGraph(f16, "dribbling")
