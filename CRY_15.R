#### CRY's 2015-16 was a season of "two-halves". More so in pts than in xpts.

library(understatr)
library(dplyr)


understatr::get_league_teams_stats(league_name = "EPL", year = 2015) %>% 
  filter(team_name == "Crystal Palace") %>% 
  mutate(half = if_else(date < "2016-01-01", "first_half", "second_half")) %>% 
  group_by(half) %>% 
  summarise(xpts = sum(xpts), pts = sum(pts))
