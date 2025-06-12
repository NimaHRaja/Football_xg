library(understatr)
library(dplyr)
library(ggplot2)
library(reshape2)


DF_xg <- 
  rbind(
    get_league_teams_stats(league_name = "EPL", year = 2023),
    get_league_teams_stats(league_name = "EPL", year = 2022),
    get_league_teams_stats(league_name = "EPL", year = 2021),
    get_league_teams_stats(league_name = "EPL", year = 2020),
    get_league_teams_stats(league_name = "EPL", year = 2019),
    get_league_teams_stats(league_name = "EPL", year = 2018),
    get_league_teams_stats(league_name = "EPL", year = 2017),
    get_league_teams_stats(league_name = "EPL", year = 2016),
    get_league_teams_stats(league_name = "EPL", year = 2015),
    get_league_teams_stats(league_name = "EPL", year = 2014)
  )

DF_table_summary <- 
  inner_join(
    DF_xg %>% select(xpts,date,pts,xG,xGA,scored,team_name,year),
    DF_xg %>% select(date,xG,xGA,scored,team_name,year) %>% rename(xG = xGA, xGA = xG),
    by = c("date","xG", "xGA", "year")) %>%
  group_by(team_name.x, year) %>%
  rename(team_name = team_name.x) %>% 
  summarise(xpts = sum(xpts), pts = sum(pts), GD = sum(scored.x - scored.y), GF = sum(scored.x), .groups = "drop") %>% 
  mutate(points = pts + GD/1000 + GF/1000000) 


DF_table_summary %>% 
  group_by(year) %>% 
  mutate(table_rank = rank(-points)) %>% 
  mutate(xpoints_rank = rank(-xpts)) %>% 
  filter(table_rank <= 4) %>%
  filter(xpoints_rank > 4) %>% 
  select(team_name, year, xpts, xpoints_rank, pts, table_rank)

DF_table_summary %>% 
  group_by(year) %>% 
  mutate(table_rank = rank(-points)) %>% 
  mutate(xpoints_rank = rank(-xpts)) %>% 
  filter(xpoints_rank <= 4) %>%
  filter(table_rank > 4) %>% 
  select(team_name, year, xpts, xpoints_rank, pts, table_rank)


cbind(
  DF_table_summary %>% 
    group_by(year) %>% 
    mutate(table_rank = rank(-points)) %>% 
    mutate(xpoints_rank = rank(-xpts)) %>% 
    ungroup() %>% 
    filter(table_rank <= 4) %>%
    filter(xpoints_rank > 4) %>% 
    select(team_name, year, xpts, xpoints_rank, pts, table_rank) %>% 
    arrange(year) %>% 
    select(year, Team1 = team_name, Team1_xrank = xpoints_rank, Team1_trank = table_rank),
  
  DF_table_summary %>% 
    group_by(year) %>% 
    mutate(table_rank = rank(-points)) %>% 
    mutate(xpoints_rank = rank(-xpts)) %>% 
    ungroup() %>% 
    filter(xpoints_rank <= 4) %>%
    filter(table_rank > 4) %>% 
    select(team_name, year, xpts, xpoints_rank, pts, table_rank) %>% 
    arrange(year) %>% 
    select(Team2 = team_name, Team2_xrank = xpoints_rank, Team2_trank = table_rank)
) %>%  
  write.csv("Medium/xpoints_ranks_vs_table_ranks/top4_mismatches.csv", row.names = FALSE)


DF_table_summary %>% 
  group_by(year) %>% 
  mutate(table_rank = rank(-points)) %>% 
  mutate(xpoints_rank = rank(-xpts)) %>% 
  filter(xpoints_rank >= 18) %>%
  filter(table_rank < 18) %>% 
  select(team_name, year, xpts, xpoints_rank, pts, table_rank)

DF_table_summary %>% 
  group_by(year) %>% 
  mutate(table_rank = rank(-points)) %>% 
  mutate(xpoints_rank = rank(-xpts)) %>% 
  filter(table_rank >= 18) %>%
  filter(xpoints_rank < 18) %>% 
  select(team_name, year, xpts, xpoints_rank, pts, table_rank) 


cbind(
  DF_table_summary %>% 
    group_by(year) %>% 
    mutate(table_rank = rank(-points)) %>% 
    mutate(xpoints_rank = rank(-xpts)) %>% 
    ungroup() %>% 
    filter(xpoints_rank >= 18) %>%
    filter(table_rank < 18) %>% 
    select(team_name, year, xpts, xpoints_rank, pts, table_rank) %>% 
    arrange(year) %>% 
    select(year, Team1 = team_name, Team1_xrank = xpoints_rank, Team1_trank = table_rank),
  
  DF_table_summary %>% 
    group_by(year) %>% 
    mutate(table_rank = rank(-points)) %>% 
    mutate(xpoints_rank = rank(-xpts)) %>% 
    ungroup() %>% 
    filter(table_rank >= 18) %>%
    filter(xpoints_rank < 18) %>% 
    select(team_name, year, xpts, xpoints_rank, pts, table_rank) %>% 
    arrange(year) %>% 
    select(Team2 = team_name, Team2_xrank = xpoints_rank, Team2_trank = table_rank)
) %>% 
  arrange(year, Team2_trank) %>% 
  write.csv("Medium/xpoints_ranks_vs_table_ranks/relegation_mismatches.csv", row.names = FALSE)


jpeg("Medium/xpoints_ranks_vs_table_ranks/xptsTable_vs_pointsTable_freq_table.jpg", width = 1200, height = 600)
DF_table_summary %>% 
  group_by(year) %>% 
  mutate(table_rank = rank(-points)) %>% 
  mutate(xpoints_rank = rank(-xpts)) %>% 
  ungroup() %>%
  mutate(one = 1) %>%
  dcast(xpoints_rank ~ table_rank, value.var = "one", fun.aggregate = sum) %>%
  melt(id.vars = "xpoints_rank") %>%
  rename(table_rank = variable) %>% 
  ggplot(aes(x = table_rank, y = xpoints_rank, label = value, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text() + 
  scale_y_reverse() +
  theme(legend.position = "none") +
  ggtitle("Premier League 2014/15-2023/24")
dev.off()


jpeg("Medium/xpoints_ranks_vs_table_ranks/xptsTable_vs_pointsTable_histogram.jpg", width = 1200, height = 600)
DF_table_summary %>% 
  group_by(year) %>% 
  mutate(table_rank = rank(-points)) %>% 
  mutate(xpoints_rank = rank(-xpts)) %>% 
  mutate(rank_diff = table_rank - xpoints_rank) %>% 
  ggplot(aes(x = rank_diff)) +
  geom_histogram(binwidth = 1, fill = "blue") +
  ggtitle("difference between x-rank and table-rank")
dev.off()

DF_table_summary %>% 
  group_by(year) %>% 
  mutate(table_rank = rank(-points)) %>% 
  mutate(xpoints_rank = rank(-xpts)) %>% 
  mutate(rank_diff = table_rank - xpoints_rank) %>% 
  filter(abs(rank_diff)> 5) %>% 
  select(team_name, year, xpts, xpoints_rank, pts, table_rank, rank_diff) %>%
  arrange(desc(abs(rank_diff))) 