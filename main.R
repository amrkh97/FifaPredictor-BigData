# Clear console (Can be done by pressing ctrl/cmd + L)
cat("\014")
# Clear environment
rm(list=ls())
# Get working directory
getwd()

source("allFunctions.R")

set.seed(101)


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
# Logistic Regression For Defense:
logisticRegression_Position(f16)
logisticRegression_Position(f17)
logisticRegression_Position(f18)
logisticRegression_Position(f19)
logisticRegression_Position(f20)

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
g_age + geom_histogram(bins = 25,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 19:Distribution based on Age")
g_age +
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Fifa 19:Distribution based on Age and Position")
ggplot(f19, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Fifa 19:Distribution based on General Playing Position")

# Fifa 20:
g_age <- ggplot(data = f20, aes(age))
g_age + geom_histogram(bins = 25,col="orange", aes(fill = ..count..)) + ggtitle("Fifa 20:Distribution based on Age")
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
# Best Team In Each Year:

# FIFA 20:
plotBest442("20")
plotBest352("20")
plotBest433("20")

F20_442 <- getBestTeamForFormation("20", "4-4-2")
F20_352 <- getBestTeamForFormation("20", "3-5-2")
F20_433 <- getBestTeamForFormation("20", "4-3-3")

bestTeams20 <- list(F20_442, F20_352, F20_433)
bestTeams20[getBestTeamByOverall(F20_442, F20_352, F20_433)]

# FIFA 19:
plotBest442("19")
plotBest352("19")
plotBest433("19")

F19_442 <- getBestTeamForFormation("19", "4-4-2")
F19_352 <- getBestTeamForFormation("19", "3-5-2")
F19_433 <- getBestTeamForFormation("19", "4-3-3")

bestTeams19 <- list(F19_442, F19_352, F19_433)
bestTeams19[getBestTeamByOverall(F19_442, F19_352, F19_433)]

# FIFA 18:
plotBest442("18")
plotBest352("18")
plotBest433("18")

F18_442 <- getBestTeamForFormation("18", "4-4-2")
F18_352 <- getBestTeamForFormation("18", "3-5-2")
F18_433 <- getBestTeamForFormation("18", "4-3-3")

bestTeams18 <- list(F18_442, F18_352, F18_433)
bestTeams18[getBestTeamByOverall(F18_442, F18_352, F18_433)]

# It can be observed that teams with messi and cristiano are always the best formation:
# F20 Team is better than that of other years:
getBestTeamByOverall(F20_433, F19_433, F18_433)

########################################################
# Lionel Messi VS Cristiano Ronaldo:

plotMessiVsCristiano()

# F20
f20_nation <- prepareCountriesData(f20)
getWorldPlot(f20_nation)
# F19
f19_nation <- prepareCountriesData(f19)
getWorldPlot(f19_nation)
# F18
f18_nation <- prepareCountriesData(f18)
getWorldPlot(f18_nation)
# F17
f17_nation <- prepareCountriesData(f17)
getWorldPlot(f17_nation)
# F16
f16_nation <- prepareCountriesData(f16)
getWorldPlot(f16_nation)


###########################################################################
###########################################################################
####################### KNN wage prediction ###############################
###########################################################################


  
levels(f20$Position)
fifa20.knn <- prepareKNNPredictionData(f20)
fifa20.knn.n <- as.data.frame(lapply(fifa20.knn[,1:ncol(fifa20.knn)-1], knnNormalize))
knnPositionPrediction(fifa20.knn, fifa20.knn.n)
#---------------------------------------------------
# testing knn model for wages
  
levels(f20$wage_brackets)
fifa20.wages.knn <- prepareKNNPredictionDataWages(f20)
fifa20.wages.knn.n <- as.data.frame(lapply(fifa20.wages.knn[,1:ncol(fifa20.wages.knn)-1], knnNormalize))
knnWagesPrediction(fifa20.wages.knn, fifa20.wages.knn.n)


###########################################################################
###########################################################################
###########################################################################
############################# Abdelgawad ##################################
###########################################################################
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

# add image column
f16 <- AddImageColumn(f16)
f17 <- AddImageColumn(f17)
f18 <- AddImageColumn(f18)
f19 <- AddImageColumn(f19)
f20 <- AddImageColumn(f20)
f20[f20$sofifa_id == 251691, "image_url"] = "https://i.pinimg.com/564x/0d/36/e7/0d36e7a476b06333d9fe9960572b66b9.jpg"
##################################################

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

PLotPLayerPositionWithWage(f16)
PLotPLayerPositionWithWage(f16,exclude_less_than_100k=TRUE)
PLotPLayerPositionWithWage(f17)
PLotPLayerPositionWithWage(f17,exclude_less_than_100k=TRUE)
PLotPLayerPositionWithWage(f18)
PLotPLayerPositionWithWage(f18,exclude_less_than_100k=TRUE)
PLotPLayerPositionWithWage(f19)
PLotPLayerPositionWithWage(f19,exclude_less_than_100k=TRUE)
PLotPLayerPositionWithWage(f20)
PLotPLayerPositionWithWage(f20,exclude_less_than_100k=TRUE)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------- Preferred-foot V.s wage -------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PLotWageForEachFoot(f16)
PLotWageForEachFoot(f17)
PLotWageForEachFoot(f18)
PLotWageForEachFoot(f19)
PLotWageForEachFoot(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Finding the most valuable teams -----------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotValuableTeams(f16)
PlotValuableTeams(f17)
PlotValuableTeams(f18)
PlotValuableTeams(f19)
PlotValuableTeams(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Finding the Top Wage Bills teams ----------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotWageBills(f16)
PlotWageBills(f17)
PlotWageBills(f18)
PlotWageBills(f19)
PlotWageBills(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Finding count of Superstars in teams ------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotSuperStars(f16)
PlotSuperStars(f17)
PlotSuperStars(f18)
PlotSuperStars(f19)
PlotSuperStars(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Age Distribution among the Top Valued Clubs -----------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotAgesForTopValuedClubs(f16)
PlotAgesForTopValuedClubs(f17)
PlotAgesForTopValuedClubs(f18)
PlotAgesForTopValuedClubs(f19)
PlotAgesForTopValuedClubs(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Clubs with the youngest Squad -------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotClubsWithYoungstPlayers(f16)
PlotClubsWithYoungstPlayers(f17)
PlotClubsWithYoungstPlayers(f18)
PlotClubsWithYoungstPlayers(f19)
PlotClubsWithYoungstPlayers(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Nationalities with highest overall --------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotNationlaitiesWithHighestOverall(f16)
PlotNationlaitiesWithHighestOverall(f17)
PlotNationlaitiesWithHighestOverall(f18)
PlotNationlaitiesWithHighestOverall(f19)
PlotNationlaitiesWithHighestOverall(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Clubs with highest number of players ------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotClubsWithHighestPlayerCount(f16)
PlotClubsWithHighestPlayerCount(f17)
PlotClubsWithHighestPlayerCount(f18)
PlotClubsWithHighestPlayerCount(f19)
PlotClubsWithHighestPlayerCount(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Clubs with highest number of left foot players --------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotClubsWithHighestLeftFootPlayers(f16)
PlotClubsWithHighestLeftFootPlayers(f17)
PlotClubsWithHighestLeftFootPlayers(f18)
PlotClubsWithHighestLeftFootPlayers(f19)
PlotClubsWithHighestLeftFootPlayers(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Best free kick takers in the game ---------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotBestFreeKickTakers(f16)
PlotBestFreeKickTakers(f17)
PlotBestFreeKickTakers(f18)
PlotBestFreeKickTakers(f19)
PlotBestFreeKickTakers(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Dominant Nationalities in Fifa ------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
PlotDominantNationalities(f16)
PlotDominantNationalities(f17)
PlotDominantNationalities(f18)
PlotDominantNationalities(f19)
PlotDominantNationalities(f20)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Linear regression Model to predict wage ---------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#--------------------------------------- Second Trial -------------------------------------------------------------

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
       aes(y = predicted_wage, x = actual_wage)) + geom_point(shape=21, fill="#69b3a2", size=1)+geom_smooth(color = "red")
# Calculate the mean squared error (MSE)of the training data.
sm <- summary(wage_model)
MSE <- mean(sm$residuals^2)
MSE_after_zero_addition <- mean(wage_df$difference^2)
MSE #130820612
MSE_after_zero_addition #130562942
max(wage_df$difference) #200967.6


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
       aes(y = predicted_wage, x = actual_wage)) + geom_point(shape=21, fill="#69b3a2", size=1)+ geom_smooth(color = "red")
        
# Calculate the mean squared error (MSE)of the training data.
sm <- summary(wage_model)
MSE <- mean(sm$residuals^2)
MSE_after_zero_addition <- mean(wage_df$difference^2)
MSE #130820612
MSE_after_zero_addition #122856893
max(wage_df$difference) #239382.6

wage_df$accuracy <- ifelse(wage_df$difference > 0.25 * wage_df$actual_wage , "No",ifelse(wage_df$difference < -(0.25 * wage_df$actual_wage),"No", "Yes"))
accuracy <- 100*(table(wage_df$accuracy)[2] / (table(wage_df$accuracy)[1]+ 
                                                              table(wage_df$accuracy)[2]))
accuracy #24.81837 

#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Predict Value using linear Regression -----------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

attributes_correleted_to_value <- GetMostCorrelatedToValue(f19)
sample = sample.split(f20, SplitRatio = 0.6)
training_set <- subset(f20, sample == TRUE)
test_set <- subset(f20, sample == FALSE)

#######################################
# Training the model
value_linera_predictor <- lm( paste("value_eur ~", paste(attributes_correleted_to_value, collapse = "+"), sep = ""),
           data = training_set, na.action = na.omit)

mean(summary(value_linera_predictor)$residuals^2) #396798076374

#####################################
# Predicting Market value
test_fit <- predict(value_linera_predictor, newdata = test_set)
test_fit <- round(test_fit,0)
test_set$predicted_value <- test_fit
test_set_name_values <- test_set[c("short_name","value_eur","predicted_value")]
test_set_name_values <- test_set_name_values %>%
  mutate(difference = value_eur - predicted_value )
test_set_name_values <- na.omit(test_set_name_values)

####################################
# plotting the actual value vs predicted ones
ggplot(test_set_name_values,
                aes(x=value_eur, y=predicted_value)) + geom_point()+
                  xlab("Actual market value")+
                  ylab( "Predicted market value") +
                  labs(title =  paste("Fifa",f19$season,"Players Market value prediction")) +
                  theme(plot.title = element_text(hjust = 0.5, size = 14))


test_set_name_values$accuracy <- ifelse(test_set_name_values$difference > 0.20 * test_set_name_values$value_eur , "No",
                                ifelse(test_set_name_values$difference < -(0.20 * test_set_name_values$value_eur),"No", "Yes"))
accuracy <- 100*(table(test_set_name_values$accuracy)[2] / (table(test_set_name_values$accuracy)[1]+ 
                                                          table(test_set_name_values$accuracy)[2]))
accuracy #69.16569 


#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------- Polynomial Regression Model to predict wage -----------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

correlated_attributes <- PrintMostCorrelatedToWage(f20)

#########################################################################
# plot each attribute vs wage to see the best fit curve, skill dribbling will be omitted because it is correlated to dribbling
# 1- market value
ggplot(f19, aes(y= wage_eur, x= value_eur)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#2- passing
ggplot(f19, aes(y= wage_eur, x= passing)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#3- dribbling
ggplot(f19, aes(y= wage_eur, x= dribbling)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#4- attacking_short_passing
ggplot(f19, aes(y= wage_eur, x= attacking_short_passing)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#5-skill_long_passing
ggplot(f19, aes(y= wage_eur, x= skill_long_passing)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#6- skill_ball_control
ggplot(f19, aes(y= wage_eur, x= skill_ball_control)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#7- movement_reactions
ggplot(f19, aes(y= wage_eur, x= movement_reactions)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#8- power_shot_power
ggplot(f19, aes(y= wage_eur, x= power_shot_power)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#9- mentality_vision
ggplot(f19, aes(y= wage_eur, x= mentality_vision)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#10- overall
ggplot(f19, aes(y= wage_eur, x= overall)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")

#11- release_clause_eur
ggplot(f19, aes(y= wage_eur, x= release_clause_eur)) + geom_point(color ="turquoise4", alpha=.7)+ geom_smooth(color = "red")


#it is clear that almost all features have a degree 2 relation with wage, except for market value


#########################################################################
#################### Wage prediction using polynomial model####################################
pre_prcessed_data <- ValuePredictionPreProcesssing(f20)
### shuffling the data
pre_prcessed_data<- pre_prcessed_data[sample(nrow(pre_prcessed_data)),]
#######################
sample = sample.split(pre_prcessed_data, SplitRatio = 0.7)
training_set <- subset(pre_prcessed_data,sample = TRUE)
test_set <- subset(pre_prcessed_data,sample = FALSE)


wage_predictor <- lm(wage_eur~ value_eur+release_clause_eur+ I(passing^2) + I(dribbling^2) +I(attacking_short_passing^2)+
                      I(skill_ball_control^2)+ I(movement_reactions^2)+ I(overall^2)+ I(power_shot_power^2)+
                       I(mentality_vision^2),data = training_set, na.action = na.omit)
summary(wage_predictor)

wage_prediction <- predict(wage_predictor, newdata = test_set)
test_set$predicted_wage <- wage_prediction
test_set<- test_set[,c("wage_eur","predicted_wage")]
test_set <- mutate(test_set,difference = wage_eur - predicted_wage)

test_set$accuracy <- ifelse(test_set$difference > 0.20 * test_set$wage_eur , "No",ifelse(test_set$difference < -(0.20 * test_set$wage_eur),"No", "Yes"))
accuracy <- 100*(table(test_set$accuracy)[2] / (table(test_set$accuracy)[1]+ 
                                                              table(test_set$accuracy)[2]))

accuracy #21.90091  




#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#----------------------------------- Failure Attempts -------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
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
NROW( f16_till_18[f16_till_18$long_name == "Lionel Andrés Messi Cuccittini",]) # just to check that lionel Messi exists in 3 rows
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
