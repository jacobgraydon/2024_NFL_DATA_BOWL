# Load Packages
library(dplyr)
library(nflfastR)

# Read in data
Games <- read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/games.csv")
Players <- read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/players.csv")
Plays <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/plays.csv")
Tackles <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tackles.csv")
Week_1 <-read.csv("Downloads/2024 NFL Data Bowl/nfl-big-data-bowl-2024/tracking_week_1.csv")
# Tack on player positions to week 1 set
Week_1 <- Week_1 %>% 
  left_join(Players %>% select(nflId,position), by = "nflId")

# To loop through entire week apply same for loop logic that is being used for
# frames. The code below is isolating just one game

# Create Empty DF
df <- data.frame()
# Assign gameIds
gid = 2022091103 
game <- Week_1 %>%
  filter(gameId == gid) %>%
  select(everything()) 

# Loop through unique playIds
unique_playIds <- unique(game$playId)
#pid <- 1126
#rm(pid)
#fid <- 1
#rm(fid)

for (pid in unique_playIds) {
  #for (pid in 1126) {
  play <- filter(game, playId == pid)
  
  # Loop through unique frameIds within the play
  unique_frameIds <- unique(play$frameId)
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
    df <- bind_rows(df, Final_Frame)
  }
}

# Exploratory Analysis
def_pos <- c('CB', 'SS', 'NT', 'DE', 'FS', 'DT', 'OLB', 'ILB')
df_agg <- df[df$position %in% def_pos, c('nflId', 'displayName', 'position', 'football')]
df_agg <- aggregate(football ~ nflId + displayName + position, data = df_agg, FUN = mean)
df_agg <- df_agg[order(df_agg$football), ]
head(df_agg, 20)



