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


unnecessaryColumns <- c("sofifa_id","player_url","dob","short_name",
                        "relaease_clause_eur","real_face","nation_position",
                        "nation_jersey_number","mentality_composure")


f15 <- f15[,!(names(f15) %in% unnecessaryColumns)]
f16 <- f16[,!(names(f16) %in% unnecessaryColumns)]
f17 <- f17[,!(names(f17) %in% unnecessaryColumns)]
f18 <- f18[,!(names(f18) %in% unnecessaryColumns)]
f19 <- f19[,!(names(f19) %in% unnecessaryColumns)]
f20 <- f20[,!(names(f20) %in% unnecessaryColumns)]

top5leagues <- c("Arsenal","Manchester United","Manchester City","Liverpool","Tottenham Hotspur","Chelsea",
                 "FC Barcelona","AtlÃ©tico Madrid","Real Madrid","AtlÃ©tico Madrid","Sevilla","Valencia CF",
                 "Napoli","Juventus","Inter","Lazio","Milan","Atalanta","Roma",
                 "Borussia Dortmund","FC Bayern MÃ¼nchen","RB Leipzig","Bayer 04 Leverkusen","Borussia MÃ¶nchengladbach",
                 "Paris Saint-Germain","Olympique Lyonnais","LOSC Lille","Stade Rennais FC","AS Monaco")


top2015 <- f15[f15$club %in% top5leagues,]
top2016 <- f16[f16$club %in% top5leagues,]
top2017 <- f17[f17$club %in% top5leagues,]
top2018 <- f18[f18$club %in% top5leagues,]
top2019 <- f19[f19$club %in% top5leagues,]
top2020 <- f20[f20$club %in% top5leagues,]
