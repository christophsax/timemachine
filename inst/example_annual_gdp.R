library(timemachine)

# Benchmark Preparation: First Value by OFS

bench0 <- 
  swiss_history %>% 

  # filter variable of interest (annual GDP)
  filter(var == "GDP.CH") %>% 
  group_by(pub_date, 
           ref_date = as.Date(paste0(data.table::year(ref_date), "-1-1"))
           ) %>% 
  # drop incomplete years
  filter(n() == 4) %>% 
  summarize(value = sum(value)) %>% 

  # calculate percentage change rate
  # need to sort and group by pub data, to get proper lags
  arrange(ref_date) %>% 
  group_by(pub_date) %>% 
  mutate(value = value / lag(value) - 1) %>% 
  ungroup() %>% 
  filter(!is.na(value)) 


bench_bfs <- 
  bench0 %>% 
  # 1st BFS value is available in Oct
  filter(as.POSIXlt(pub_date)$mon + 1 == 7) %>% 
  arrange(pub_date) %>% 
  group_by(ref_date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(ref_date, bench = value)

bench_seco <- 
  bench0 %>% 
  # 1st SECO value is the first aailable value
  arrange(pub_date) %>% 
  group_by(ref_date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(ref_date, bench_seco = value)

