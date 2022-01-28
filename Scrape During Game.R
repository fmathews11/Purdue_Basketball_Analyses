library(tidyverse)
library(ncaahoopR)

append_real_time_data <- function(game_id,df){
  
  append_agg_boxscores<- function(game_id){
    test <- get_boxscore(game_id)
    
    #Get team names
    team_a <- names(test)[1]
    team_b <- names(test)[2]
    
    #Create a dataframe for the first team
    df_agg_a <- test[[team_a]]%>%
      filter(player != "TEAM")%>%
      summarise_at(vars(MIN:PTS),sum)%>%
      mutate(TEAM = team_a)%>%
      select(TEAM,FGM,FGA,'3PTM','3PTA',FTM,FTA,OREB,DREB,REB,AST,STL,BLK,TO,PF,PTS)%>%
      mutate(POS = round((FGA-OREB)+TO+(0.475*FTA),digits = 0))%>%
      mutate(PTS_POS =  round(PTS/POS,digits = 3))%>%
      mutate(TO_PCT = round(100*TO/(FGA+0.475*FTA+TO),digits = 2))%>%
      mutate(TRUE_SP = round(100*PTS/(2*(FGA+0.475*FTA)),digits = 2))%>%
      mutate(FG_PCT = round(100*(FGM/FGA),digits = 2))%>%
      mutate(FT_PCT = round(100*(FTM/FTA),digits = 2))
    
    #Create a dataframe for the second team
    df_agg_b <- test[[team_b]]%>%
      filter(player != "TEAM")%>%
      summarise_at(vars(MIN:PTS),sum)%>%
      mutate(TEAM = team_b)%>%
      select(TEAM,FGM,FGA,'3PTM','3PTA',FTM,FTA,OREB,DREB,REB,AST,STL,BLK,TO,PF,PTS)%>%
      mutate(POS = round((FGA-OREB)+TO+(0.475*FTA),digits = 0))%>%
      mutate(PTS_POS =  round(PTS/POS,digits = 3))%>%
      mutate(TO_PCT = round(100*TO/(FGA+0.475*FTA+TO),digits = 2))%>%
      mutate(TRUE_SP = round(100*PTS/(2*(FGA+0.475*FTA)),digits = 2))%>%
      mutate(FG_PCT = round(100*(FGM/FGA),digits = 2))%>%
      mutate(FT_PCT = round(100*(FTM/FTA),digits = 2))
    
    #Create a variable of the number of defensive rebounds for each team
    team_a_rebs <- df_agg_a$DREB
    team_b_rebs <- df_agg_b$DREB
    
    #Create offensive rebounding percentage for each dataframe
    df_agg_a <- df_agg_a%>%
      mutate(OR_PCT = round(100*OREB/(OREB+team_b_rebs),digits = 2))
    
    df_agg_b <- df_agg_b%>%
      mutate(OR_PCT = round(100*OREB/(OREB+team_a_rebs),digits = 2))
    
    #Merge the dataframes and return the merged dataframe
    final_df <- data.frame(rbind(df_agg_a,df_agg_b))
    return(final_df)
  }
  
  #Get Timestamp
  base_url <- "https://www.espn.com/mens-college-basketball/playbyplay/_/gameId/"
  url <- paste(base_url, game_id, sep = "")
  tmp <- try(XML::readHTMLTable(RCurl::getURL(url)), silent = T)
  if(length(tmp) == 3){
    bingo <-tmp[2]
    current_half <- 1
  } else{
    bingo <- tmp[2]
    current_half <- 2
  }
  bingo <- c(bingo[[1]][1])
  bingo <- bingo[[1]]
  current_time <<- bingo[2]
  
  if(current_time != last_reported_timestamp){

    #Create the temporary dataframe
    temp_df <- append_agg_boxscores(game_id)
    temp_df$timestamp <- current_time
    temp_df$half <- current_half
    last_reported_timestamp <<- current_time
    final_df <- (rbind(df,temp_df))
    return(final_df)
    
  } else{
    return(df)
  }
  
  
  
}

harvard_v_penn <- data.frame()

last_reported_timestamp <- 0


harvard_v_penn <- append_real_time_data(401369747,harvard_v_penn)

i <- 1
while(TRUE){
  if (i == 5){
    write_csv(harvard_v_penn,'testing_this_out.csv')
    break
  }
  print(i)
  harvard_v_penn <<- append_real_time_data(401369747,harvard_v_penn)
  ran_num <- sample(30:60,1)
  Sys.sleep(time = ran_num)
  i = i +1
}


########################################################################
url <- "https://www.espn.com/mens-college-basketball/playbyplay?gameId=401369747"
tmp <- try(XML::readHTMLTable(RCurl::getURL(url)), silent = T)
length(tmp)
