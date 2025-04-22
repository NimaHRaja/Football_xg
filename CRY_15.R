#### CRY's 2015-16 was a season of "two-halves". More so in pts than in xpts.

library(understatr)
library(dplyr)


EPL15 <- 
  get_league_teams_stats(league_name = "EPL", year = 2015) 

EPL15 %>% 
  filter(team_name == "Crystal Palace") %>% 
  mutate(half = if_else(date < "2016-01-01", "first_half", "second_half")) %>% 
  group_by(half) %>% 
  summarise(xpts = sum(xpts), pts = sum(pts))


inner_join(
  EPL15 %>% filter(team_name == "Crystal Palace") %>% select(h_a,xpts,date,pts,xG,xGA,scored),
  EPL15 %>% select(date,xG,xGA,team_name,scored) %>% rename(xG = xGA, xGA = xG),
  by = c("date","xG", "xGA")) %>%
  mutate(pts_luck = pts-xpts) %>% 
  arrange(desc(abs(pts_luck))) %>% 
  mutate(result = paste(scored.x, "-", scored.y,sep = "")) %>% 
  mutate(x_result = paste(round(xG,2), "-", round(xGA,2),sep = "")) %>% 
  select(date, team_name, h_a,xpts,pts,pts_luck,x_result,result) %>% 
  mutate(half = if_else(date < "2016-01-01", "first_half", "second_half")) %>% 
  head(10)
