rm(list=ls())
#display work items
ls()
#get working directory
getwd()

f15 <- read.csv("Dataset/players_15.csv")
f16 <- read.csv("Dataset/players_16.csv")
f17 <- read.csv("Dataset/players_17.csv")
f18 <- read.csv("Dataset/players_18.csv")
f19 <- read.csv("Dataset/players_19.csv")
f20 <- read.csv("Dataset/players_20.csv")


unnecessaryColumns <- c("sofifa_id","player_url","dob","long_name",
                        "relaease_clause_eur","real_face","nation_position",
                        "nation_jersey_number","mentality_composure")


f15 <- f15[,!(names(f15) %in% unnecessaryColumns)]
f16 <- f16[,!(names(f16) %in% unnecessaryColumns)]
f17 <- f17[,!(names(f17) %in% unnecessaryColumns)]
f18 <- f18[,!(names(f18) %in% unnecessaryColumns)]
f19 <- f19[,!(names(f19) %in% unnecessaryColumns)]
f20 <- f20[,!(names(f20) %in% unnecessaryColumns)]

