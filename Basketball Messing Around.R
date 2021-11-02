unique(agdata$player)

players <- c("T. Williams","Z. Edey","J. Ivey","S. Stefanovic","E. Hunter Jr.")

wheeler <- agdata%>%
  filter(player %in% players)

wheeler<-wheeler%>%
  mutate(OfEf = PTS/MIN)

wheeler%>%
  ggplot(aes(x = gamedate, y = OfEf, color = player))+stat_smooth(se = FALSE, size = 1.2)+labs(
    title = 'Offensive Efficiency Rating, By Game',
    subtitle = 'Aaron Wheeler & Brandon Newman, 2020-2021 Season',
    x = 'Month',
    y = 'Points Per Minute'
  )+theme_hc()

g<-agdata%>%
  select(player,threepct,gamedate,location)%>%
  group_by(gamedate)%>%
  summarise(Avg3 = mean(threepct))

g$location <- sched$location

sched<-head(sched, -1)

spline_int <- as.data.frame(spline(as.Date(g$gamedate), g$Avg3))

g%>%
  ggplot(aes(x = gamedate, y = Avg3,color = location))+stat_smooth(se = FALSE, size = 1.2, color = 'hotpink')+geom_point()+
  geom_line(data = spline_int, aes(x =x, y = y))

esquisser(g)

ggplot(g) +
  aes(x = gamedate, weight = Avg3) +
  geom_bar(fill = "#fdc926") +
  labs(x = "Game Date", y = "Team 3 Pt Shooting Percentage", title = "Purdue 3 Pt Shooting by Game") +
  theme_gray() +
  ylim(0, 0.45)

sched<-sched%>%
  filter(location == 'H' | location == 'A')

ggplot(g) +
  aes(x = opponent, fill = location, weight = Avg3) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "Date", y = "3pt %", title = "Purdue 3Pt Percentage by Game", caption = "2020-2021 Season", fill = "Home/Away") +
  theme_gray() +
  theme(legend.position = "bottom")

g$opponent <- sched$opponent


ggplot(g) +
  aes(x = opponent, fill = location, weight = Avg3) +
  geom_bar(position = "dodge",alpha = 0.7, color = 'black') +
  scale_fill_hue() +
  labs(x = "Opponent", y = "3pt%", title = "Purdue 3pt% Per Opponent", 
       subtitle = 'Opponents Played More Than Once',
       fill = "Home/Away") +
  theme_hc() +
  theme(legend.position = "bottom")+scale_fill_viridis_d(option = "inferno")

teams <- c('Maryland','Michgan State','Minnesota','Ohio State','Penn State','Indiana')

twice <- purduedata%>%
  filter(opponent %in% teams)

g<-twice%>%
  select(player,threepct,gamedate,location)%>%
  group_by(gamedate)%>%
  summarise(Avg3 = mean(threepct))

sched<-get_schedule('Purdue')

dat <- g$gamedate

g$opponent <- sched$opponent[sched$date %in% g$gamedate]

sched$date %in% dat

esquisser(g)

g$location <- sched$location[sched$date %in% g$gamedate]








duke_data <- function(...){
  
  library(ncaahoopR)
  library(tidyverse)
  library(lubridate)
  
  
  box_scores_duke<-function(game_id){
    
    sched <- get_schedule('Duke')  
    
    box_scores<-get_boxscore(game_id)
    
    box_duke<-box_scores$Duke
    
    box_duke<-box_duke%>%
      mutate(threepct = round((`3PTM`/`3PTA`),digits = 2))%>%
      mutate(FTpct = round((FTM/FTA), digits = 2))%>%
      unite('3s',`3PTM`:`3PTA`,sep = '-')%>%
      unite(FTs,FTM:FTA,sep = '-')%>%
      mutate(FGpct = round((FGM/FGA),digits = 2))%>%
      unite(FGs,FGM:FGA,sep = '-')%>%
      select(player,PTS,MIN,FGs,FGpct,'3s',threepct,FTs,FTpct,OREB,DREB,REB,AST,STL,BLK,TO,PF)%>%
      mutate(threepct = replace_na(threepct,0))%>%
      mutate(FTpct = replace_na(FTpct,0))%>%
      mutate(gamedate =sched$date[sched$game_id == game_id] )%>%
      mutate(location = sched$location[sched$game_id == game_id])%>%
      mutate(opponent = sched$opponent[sched$game_id == game_id])
  }
  
  sched <- get_schedule('Duke')
  
  filter_date <- Sys.Date()
  
  sched <- sched%>%
    filter(date < filter_date)
  
  agdata<-data.frame()
  
  for (gameid in sched$game_id)
  {
    gamedata <- box_scores_duke(gameid)
    agdata <- rbind(agdata, gamedata)
    
  }
  
  data_duke <<- agdata%>%
    filter(player != 'TEAM')
}
duke_data()

unique(agdata$player)

williams <- data_duke%>%
  select(player, PTS, MIN,gamedate)%>%
  filter(player == "M. Williams")%>%
  mutate(PPM = PTS/MIN)

edey <- agdata%>%
  select(player, PTS, MIN,gamedate)%>%
  filter(player == "Z. Edey")%>%
  mutate(PPM = PTS/MIN)

edey <- head(edey, -5)

c <- c(1:25)

edey$gamenum <- c(1L)
williams$gamenum <- c(1:20)

data2 <- rbind(edey,williams)

esquisser(data2)

ggplot(data2) +
  aes(x = gamenum, y = PPM, fill = player) +
  geom_col(size = 1L, position = 'dodge', alpha = .5, color = 'black') +
  scale_color_hue() +labs(
    title = "Comparison of Offensive Efficency Between Purdue's Zach Edey vs Duke's 5 * Mark Williams",
    subtitle = 'First 20 Games',
    x = 'Game Number',
    y = 'PPM',
    fill = 'Player',
    caption = 'Edey average ppm - .5198
    Williams average ppm - .3050')
  )

edey %>%
  summarise(avgef = mean(PPM))

williams %>%
  summarise(avgef = mean(PPM))



library(tidyverse)

unique(purduedata$player)

rm(purdue_data)

purdue_freshmen <- c("M. Gillis" ,"Z. Edey","J. Ivey","B. Newman","E. Morton")

purduedata <- purduedata%>%
  filter(player %in% purdue_freshmen)

a<-purduedata %>%
  group_by(gamedate)%>%
  summarise(ppg = mean(PTS/MIN))

a$gamenum <- c(1:25)

esquisser(a)

unique(iudata$player)

iu_freshmen <- c("A. Leal","K. Lander","J. Geronimo","T. Galloway")

b <- iudata%>%
  group_by(gamedate)%>%
  summarise(ppg = mean(PTS/MIN))

b$gamenum <- c(1:25)

c <- full_join(a,b, b = "gamenum")

library(esquisse)
library(ggthemes)

esquisser(c)

a$team <- 'Purdue'

b$team <- 'IU'

c <- rbind(a,b)

b$ppg <- ifelse(is.na(b$ppg),0,b$ppg)

esquisser(c)


ggplot(c) +
  aes(x = gamenum, y = ppg, fill = team) +
  geom_col(size = 1L, colour = "black", position = 'dodge') +
  scale_fill_hue() +
  theme_hc()+ scale_fill_manual(values=c("#990000","#000000"))+labs(
    title = 'Offensive Efficiency',
    subtitle = 'Purdue Freshmen Class vs IU Freshmen Class by Game',
    x = 'Game Number',
    y = 'PPM',
    caption = 'Purdue Avg PPM - 0.3223
    IU Avg PPM - 0.2853',
    fill = 'Team'
  )

mean(a$ppg)

mean(b$ppg)

ncaa_colors

ncaa_colors[ncaa_colors$ncaa_name == 'Purdue']

view(ncaa_colors)



tjd <-get_team_data('Indiana')%>%
  filter(player == "T. Jackson-Davis")

unique(tjd$player)

tjd <-tjd%>%
  filter(player == "T. Jackson-Davis")

sched <- get_schedule('Indiana')

tjd$opponent <- ifelse(sched$opponent == 'Purdue','Purdue','Not Purdue')

tjd%>%
  group_by(opponent)%>%
  summarise(avgppg = mean(PTS))

library(tidyverse)

wheeler <- get_team_data('Purdue')%>%
  filter(player == 'A. Wheeler')

wheeler

wheeler$ortg <- c(114,72,123,87,28,122,126,70,111,26,0,199,72,77,0,94,42,90,24,57,56,103,95,187,181,124,109,96)
wheeler$gamenum <- c(1:28)

wheeler%>%
  ggplot(aes(x = gamenum, y = ortg))+geom_col(color = 'black',fill = '#CEB888')+theme_hc()+labs(
    title = "Aaron Wheeler's Offensive Rating per Game",
    subtitle = '2020-2021 Season',
    x = 'Game #',
    y = 'ORtg'
  )

wheeler%>%
  filter(gamenum <= 20) %>%
  summarise(avgrtg = mean(ortg))

wheeler%>%
  filter(gamenum > 20) %>%
  summarise(avgrtg = mean(ortg))
  

library(esquisse)
library(ggthemes)

esquisser(wheeler)




sched <- get_schedule('Purdue')
sched <- sched%>%
  filter(date <= Sys.Date())

sched <- sched[-3,]
scoring<-get_team_data('Purdue')

scoring%>%
  group_by(player)%>%
  summarise(ppg = mean(PTS))%>%
  arrange(ppg)%>%
  filter(row_number()==n())


#get list of top scorers and their ppgs

opponenttopscorers <- data.frame()

for (opponent in unique(sched$opponent))
try({
  df <- get_team_data(opponent)%>%
    group_by(player)%>%
    summarise(ppg = mean(PTS))%>%
    arrange(ppg)%>%
    filter(row_number()==n())
  
  opponenttopscorers <- rbind(opponenttopscorers,df)
  
  
})

get_team_data('Maryland')

opponenttopscorers$opponent

opponenttopscorers$opponent <- unique(sched$opponent)


againstpurdue<-data.frame()

df <- data.frame()

rm(i)

for(i in opponenttopscorers$player)
 try({
  
  df<-get_team_data(opponenttopscorers$opponent[opponenttopscorers$player == i])%>%
    select(player,PTS,opponent)%>%
    filter(player == opponenttopscorers$player[opponenttopscorers$player == i])%>%
    filter(opponent == 'Purdue')%>%
    mutate(ppgpurdue = mean(PTS))
  
  againstpurdue<-rbind(df,againstpurdue)
  
  
})

againstpurdue$ppgpurdue[opponenttopscorers$player == unique(againstpurdue$player)]


againstpurdue$player <- unique(againstpurdue$player)

againstpurdue[!duplicated(againstpurdue$player)]

againstpurdue <-againstpurdue%>%
  select(player, ppgpurdue)

againstpurdue <- againstpurdue%>%
  distinct(pla)


unique(sched$opponent)

df <- get_team_data('Oakland')%>%
  group_by(player)%>%
  summarise(ppg = mean(PTS))%>%
  arrange(ppg)%>%
  filter(row_number()==n())

againsgpurdue<- againstpurdue%>%
  distinct(player,ppgpurdue,)

opponenttopscorers[-10,]

againsgpurdue$purdue <- 'Against Purdue'
opponenttopscorers <- opponenttopscorers%>%
  select(player,ppg)

opponenttopscorers <- opponenttopscorers[-10,]
opponenttopscorers$purdue <- 'Season Average'

againsgpurdue <- againsgpurdue%>%
  rename(ppg = ppgpurdue)

final_df <- rbind(opponenttopscorers,againsgpurdue)

final_df %>%
  group_by(purdue)%>%
  summarise(mean(ppg))

esquisser(final_df)


ggplot(final_df) +
  aes(x = player, fill = purdue, weight = ppg) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Player", y = "PPG", title = "Opposing Team's Highest Scorer Performance", 
       subtitle = "Against Purdue vs Season Average", 
       fill = "Season/Against Purdue",
       caption = "Season Average PPG - 16.2
       Average PPG Against Purdue - 13.5") +
  ggthemes::theme_igray() +
  theme(legend.position = "bottom")


get_schedule('Purdue','2019-20')


library(readr)
data <- read_csv("C:/Users/Frank/Desktop/defense.csv")

library(esquisse)


esquisser(data)

ggplot(data) +
  aes(x = reorder(Player, -DRtg), weight = DRtg) +
  geom_bar(fill = "#ed7953") +
  coord_flip() +
  theme_minimal()+labs(
    title = 'Purdue Defensive Efficiency',
    subtitle = '2020-21 Season',
    x = 'Points Allowed Per 100 Posessions',
    y = 'Player'
  )

ggplot(data) +
  aes(x = reorder(Player, -ORtg), weight = ORtg) +
  geom_bar(fill = "#ed7953") +
  coord_flip() +
  theme_minimal()+labs(
    title = 'Purdue Offensive Efficiency',
    subtitle = '2020-21 Season',
    x = 'Points Produced Per 100 Posessions',
    y = 'Player'
  )

ggplot(data) +
  aes(x = reorder(Player, -PTS), weight = PTS) +
  geom_bar(fill = "#ed7953") +
  coord_flip() +
  theme_minimal()+labs(
    title = 'Purdue Points Per 100 Posessions',
    subtitle = '2020-21 Season',
    x = 'Points Scored Per 100 Posessions',
    y = 'Player'
  )

data%>%
  select(Player,DRtg,MP)%>%
  mutate(PointsAllowedPerPosession = DRtg/100)%>%
  arrange(PointsAllowedPerPosession)

data$PosLen <- 18.5  



rm(seasons)

teams <- c('Indiana','Iowa','Maryland','Michigan State','Ohio State', 'Penn State','Rutgers',
           'Illinois','Minnesota','Northwestern','Wisconsin','Nebraska','Michigan')
seasons <- c('2017-18','2018-19','2019-20','2020-21')

teams %in% ids$team

rm(team)
rm(season)

sched <- data.frame()


  
  
 

  for (season in seasons){
  
    df<-  get_schedule('Purdue',season)%>%
      filter(opponent %in% teams)%>%
      mutate(season = season)%>%
      select(opponent,team_score,opp_score,season)%>%
      mutate(margin = team_score - opp_score)
    
    
    sched <- rbind(sched,df)
  

 }
  
  


sched <- na.omit(sched)

sched%>%
  filter(opponent == 'Purdue')%>%
  summarise(AvgPPGAgainstPurdue = mean(team_score))



sched%>%
  filter(opponent != 'Purdue')%>%
  summarise(AvgPPGAgainstPurdue = mean(team_score))

install.packages("kableExtra")

sched%>%
  group_by(AgainstPurdue, team)%>%
  summarise(PPG = mean(team_score))%>%
  group_by


sched%>%
  group_by(AgainstPurdue, team)%>%
  summarise(PPG = mean(team_score))%>%
  ggplot(aes(x = team,weight = PPG, fill = AgainstPurdue))+geom_bar(position = 'dodge')+coord_flip()+labs(
    title = 'Big Ten Conference PPG Comparison',
    subtitle = 'Against Purdue vs Season Average (Conference Games Only)',
    x = 'Points Per Game',
    y = 'Team',
    caption = 'Last Four Seasons',
    fill = ''
  )+scale_fill_manual(values = c('#CEB888','black'))


seasons <- c('2004-05','2005-06','2006-07','2007-08','2008-09','2009-10','2010-11','2011-12','2012-13',
             '2013-14','2014-15','2015-16',
             '2017-18','2018-19','2019-20','2020-21')

sched <- data.frame()
for (season in seasons){
  
 
 df<-  get_schedule('Purdue',season)%>%
   filter(opponent == "Indiana")%>%
   mutate(season = season)%>%
   select(opponent,team_score,opp_score,season)%>%
   mutate(margin = team_score - opp_score)
     
 
 sched <- rbind(sched,df)
}


sched%>%
  summarise(mean(margin))

teams <- c('Indiana','Iowa','Maryland','Michigan State','Ohio State', 'Penn State','Rutgers',
           'Illinois','Minnesota','Northwestern','Wisconsin','Nebraska','Michigan')

seasons <- c('2016-2017','2017-18','2018-19','2019-20','2020-21')

for (season in seasons){
  
  df<-  get_schedule('Purdue',season)%>%
    filter(opponent %in% teams)%>%
    mutate(season = season)%>%
    select(opponent,team_score,opp_score,season)%>%
    mutate(margin = team_score - opp_score)
  
  
  sched <- rbind(sched,df)
  
  
}


sched%>%
  group_by(opponent)%>%
  summarise(AvgMargin = round(mean(margin),digits = 2))%>%
  ggplot(aes(x = opponent, y = AvgMargin,label = AvgMargin))+geom_col(fill = '#CEB888',color = 'Black')+
  coord_flip()+geom_text(aes(x = opponent,y = AvgMargin),hjust = 1.2)+labs(
    title = 'Average Scoring Margin Against Big Ten Teams',
    subtitle = 'Last 5 Seasons',
    x = 'Avg Margin',
    y = 'Opponent'
  )

sched%>%
  group_by(opponent)%>%
  summarise(AvgMargin = round(mean(margin),digits = 2))%>%
  summarise(mean(AvgMargin))


for (season in seasons){
  
  df<-  get_schedule('Purdue',season)%>%
    filter(opponent %in% teams)%>%
    mutate(season = season)%>%
    select(opponent,team_score,opp_score,season)%>%
    mutate(margin = team_score - opp_score)
  
  
  sched <- rbind(sched,df)
  
  
}

sched%>%
  group_by(opponent)%>%
  summarise(AvgMargin = round(mean(margin),digits = 2))%>%
  ggplot(aes(x = opponent, y = AvgMargin,label = AvgMargin))+geom_col(fill = '#CEB888',color = 'Black')+
  coord_flip()+geom_text(aes(x = opponent,y = AvgMargin),hjust = 1.2)+labs(
    title = 'Average Scoring Margin Against Big Ten Teams',
    subtitle = 'Under Matt Painter',
    x = 'Avg Margin',
    y = 'Opponent'
  )

sched%>%
  group_by(opponent)%>%
  summarise(AvgMargin = round(mean(margin),digits = 2))%>%
  summarise(mean(AvgMargin))

sched <- data.frame()

for (season in seasons){
  
  df<-  get_schedule('Purdue',season)%>%
    filter(opponent %in% teams)%>%
    mutate(season = season)%>%
    select(opponent,team_score,opp_score,season,location)%>%
    filter(location == 'H' | location == 'A')%>%
    mutate(margin = team_score - opp_score)
  
  
  sched <- rbind(sched,df)
  
  
}

sched%>%
  group_by(opponent,location)%>%
  summarise(AvgMargin = round(mean(margin),digits = 2))%>%
  ggplot(aes(x = opponent, y = AvgMargin,label = AvgMargin,fill = location))+geom_col(color = 'Black',
                                                                                      position = 'dodge')+
  coord_flip()+geom_text(aes(x = opponent,y = AvgMargin),hjust = 1.2,vjust = 1)+labs(
    title = 'Average Scoring Margin Against Big Ten Teams',
    subtitle = 'Under Matt Painter',
    x = 'Avg Margin',
    y = 'Opponent'
  )+scale_fill_manual(values = c('#CEB888','black'))+theme_hc()

sched%>%
  group_by(opponent,location)%>%
  summarise(AvgMargin = round(mean(margin),digits = 2))%>%
  group_by(location)%>%
  summarise(mean(AvgMargin))

sched$home <- ifelse(sched$location == "H",1,0)

t1<-t.test(sched$margin~sched$location)

t1%>%
  tidy%>%
  as.data.frame()%>%
  kable()

sched <- data.frame()

for (season in seasons){
  
  
}


wheeler <- get_team_data('Purdue','2020-21')%>%
  filter(player == 'A. Wheeler')

df <- get_team_data('Purdue','2018-19')%>%
  filter(player == 'A. Wheeler')

df$gamenum <- c(1:36)
df$ortg <- c(40,110,168,32,131,62,0,161,138,61,92,113,159,196,36,173,104,72,112,269,
             112,24,72,54,135,64,158,0,157,0,73,156,171,0,123,119)

wheeler <- rbind(wheeler,df)

wheeler$ortg <- c(114,72,123,87,28,122,126,70,111,26,0,199,72,77,0,94,42,90,24,57,56,103,95,187,181,124,109,96,
                  111,69,88,117,139,44,42,89,9,62,64,125,40,91,0,70,74,0,104,18,0,95,202,260,0,0,150,
                  0,0,0,27)
a<-c(1:28)
b<-c(1:31)
c<-c(1:36)
wheeler$gamenum <- c(a,b)

esquisser(wheeler)

wheeler%>%
  ggplot(aes(x = gamenum, y = ortg))+geom_col(color = 'black',fill = '#CEB888')+theme_hc()+labs(
    title = "Aaron Wheeler's Offensive Rating per Game",
    subtitle = '2020-2021 Season',
    x = 'Game #',
    y = 'ORtg'
  )

ggplot(wheeler) +
  aes(x = gamenum, y = ortg) +
  geom_col(color = 'black',fill = '#CEB888') +
  theme_hc() +
  facet_grid(vars(), vars(season))+labs(
    title = 'Aaron Wheeler Offensive Rating Per Game',
    x = 'Game',
    y = 'Offensive Rating'
  )


wheeler%>%
  group_by(season)%>%
  summarise(AvgOrtg = mean(ortg))
