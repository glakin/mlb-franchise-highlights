library(Lahman)
library(dplyr)

data(Batting)
data(People)
data(TeamsFranchises)

battingSeasons <- left_join(Batting, Teams, 
                     by = c("teamID" = "teamID", "yearID" = "yearID") ) %>%
  left_join(TeamsFranchises, by = c("franchID" = "franchID")) %>%
  left_join(People, by = c("playerID" = "playerID")) %>%
  filter(active == "Y") %>%
  select(yearID, franchID, franchName, playerID, nameFirst, nameLast, AB.x, H.x,
         HR.x, RBI, SB.x) %>%
  rename(AB = AB.x, H = H.x, HR = HR.x, SB = SB.x)

battingSeasons$nameFull <- paste(seasons$nameFirst, seasons$nameLast)

avg <- battingSeasons %>% 
  group_by(nameFull, franchName) %>% 
  summarize(ABs = sum(AB), Hs = sum(H))

avg$avg <- avg$Hs / avg$ABs

avg_10 <- avg %>%
  group_by(franchName) %>%
  filter(ABs >= 1000) %>%
  slice_max(order_by = avg, n = 10) %>%
  select(nameFull, franchName, avg)

h_10 <- avg %>%
  group_by(franchName) %>%
  slice_max(order_by = Hs, n = 10) %>%
  select(nameFull, franchName, Hs)

hrs <- battingSeasons %>% 
  group_by(nameFull, franchName) %>% 
  summarize(HRs = sum(HR))

hrs_10 <- hrs %>%
  group_by(franchName) %>%
  slice_max(order_by = HRs, n = 10)

rbis <- battingSeasons %>% 
  group_by(nameFull, franchName) %>% 
  summarize(RBIs = sum(RBI))

rbis_10 <- rbis %>%
  group_by(franchName) %>%
  slice_max(order_by = RBIs, n = 10)

sbs <- battingSeasons %>% 
  group_by(nameFull, franchName) %>% 
  summarize(SBs = sum(SB))

sb_10 <- sbs %>%
  group_by(franchName) %>%
  slice_max(order_by = SBs, n = 10)

