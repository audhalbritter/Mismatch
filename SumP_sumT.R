

dat <- AllPred %>% 
  left_join(Date_snowmelt, by = c("stage", "siteID" = "site")) %>% 
  select(-Snowmelt_date, -first.poll, -last.poll, -first.fl, -last.fl, -peak.diff) %>% 
  rename(SM = doy)



dat %>% 
  rowwise() %>% 
  do({
    dd <- .
    fl <- Weather %>% 
      filter(between(doy, dd$SM, dd$peak.fl)) %>% 
      summarise(sumT.fl = sum(temperature, na.rm = TRUE), sumP.fl = sum(precipitation, na.rm = TRUE))
    
    res <- Weather %>% 
      filter(between(doy, dd$SM, dd$peak.poll)) %>% 
      summarise(sumT.poll = sum(temperature, na.rm = TRUE), sumP.poll = sum(precipitation, na.rm = TRUE)) %>% 
      bind_cols(fl)
    
    dd %>% 
      bind_cols(res)
      })


