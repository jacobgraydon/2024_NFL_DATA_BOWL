library(dplyr)


Week_1 <-read.csv("tracking_week_1.csv") %>% 
  left_join(Players %>% select(nflId,position), by = "nflId")

GameIds_Week_1 <- unique(Week_1$gameId)
data <- data.frame()

PlayLoop <- function() {
  
  
  gid <- GameIds_Week_1[[x]]
  
  game <- Week_1 %>%
    filter(gameId == gid) %>%
    select(everything())
  
  # Loop through unique playIds
  
  unique_playIds <- unique(game$playId)
  
  for (pid in unique_playIds) {
    #for (pid in 1126) {
    play <- filter(game, playId == pid)
    
    # Loop through unique frameIds within the play
    unique_frameIds <- unique(play$frameId)
    #fid = 2
    for (fid in unique_frameIds) {
      
      frame <- filter(play, frameId == fid) %>%
        mutate(nflId = ifelse(is.na(nflId),"football",nflId))
      
      
      # Calculate distqnce
      dist_matrix <- as.matrix(dist(frame %>% select(x, y)))
      dist_matrix <- as.data.frame(dist_matrix)
      dist_matrix$nflId <- frame$nflId
      dist_matrix <- dist_matrix %>%
        select(nflId,everything())
      # Create unique positions to avoid duplicating columns
      frame2 <- frame %>%
        group_by(position) %>%
        mutate(
          position = ifelse(is.na(position),"football",position),
          pos_unique = ifelse(position == "football", "football",paste0(
            position,
            row_number() %>%
              as.character() %>%
              sub("\\.0", "", .) %>%
              sub("0", "", .)
          )
          )
        )
      colnames(dist_matrix) <-c("nflId",frame2$pos_unique)
      Final_Frame <- frame2 %>% left_join(dist_matrix, by = "nflId")
      # Concatenate new results into the output dataframe
      data <- bind_rows(data, Final_Frame)
      
    }
  }
  return(data)
}


result_df <- PlayLoop()

# Show distance in play 56 of when pass was caught
result_df %>% 
  filter(playId == 56,
         event == 'pass_outcome_caught')

