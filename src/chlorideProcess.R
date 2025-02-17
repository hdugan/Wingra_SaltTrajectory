# Chloride data

dcph = read_csv('InputData/Lakes and Outfalls - Chloride Data.csv') |> 
  select(Lake, Date = Date_Sampled, cl = Results)
table(dcph$Lake)

ntl = read_csv('InputData/ntl2_v10.csv') |> 
  filter(depth == 0) |> 
  select(Lake = lakeid, Date = sampledate, cl) |> 
  filter(!is.na(cl)) |> 
  filter(Lake %in% c('ME', 'MO', 'WI', 'KE', 'WA')) |> 
  mutate(Lake = recode(Lake, 
                       'KE' = 'Kegonsa', 
                       'ME' = 'Mendota', 
                       'MO' = 'Monona', 
                       'WA' = 'Waubesa', 
                       'WI' = 'Wingra'))

ggplot(dcph) +
  geom_point(aes(x = Date, y = cl), size = 0.3) +
  geom_point(data = ntl, aes(x = Date, y = cl), col = 'blue', size = 0.3) +
  facet_wrap(~Lake)

ggplot(dcph |> filter(Lake == 'Wingra')) +
  geom_point(aes(x = Date, y = cl), size = 0.3) +
  geom_point(data = ntl|> filter(Lake == 'Wingra'), aes(x = Date, y = cl), col = 'blue', size = 0.3) +
  geom_path(aes(x = Date, y = cl), size = 0.3) +
  geom_path(data = ntl|> filter(Lake == 'Wingra'), aes(x = Date, y = cl), col = 'blue', size = 0.3)
