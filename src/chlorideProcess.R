# Chloride data
library(tidyverse)

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

older = read_csv('InputData/MadisonLakes_chloride[38].csv') |> 
  mutate(Date = mdy(Date), cl = Chloride.mgL) |> 
  filter(Date <= as.Date('1975-01-01'))

# Plot all lakes
ggplot(dcph) +
  geom_point(aes(x = Date, y = cl), size = 0.3) +
  geom_point(data = ntl, aes(x = Date, y = cl), col = 'blue', size = 0.3) +
  geom_point(data = older, aes(x = Date, y = cl), col = 'red', size = 0.3) +
  facet_wrap(~Lake)

ggplot(dcph |> filter(Lake == 'Mendota')) +
  geom_point(aes(x = Date, y = cl), size = 0.3) +
  geom_point(data = ntl |> filter(Lake == 'Mendota'), aes(x = Date, y = cl), col = 'blue', size = 0.3) +
  facet_wrap(~Lake)

ggplot(dcph |> filter(Lake == 'Wingra')) +
  geom_point(aes(x = Date, y = cl), size = 0.3) +
  geom_point(data = ntl |> filter(Lake == 'Wingra'), aes(x = Date, y = cl), col = 'blue', size = 0.3) +
  facet_wrap(~Lake)


#Join data
cl.join = dcph |> bind_rows(older) |> arrange(Lake, Date) 

# remove outliers
cl.outliers = cl.join %>%
  group_by(Lake) %>%
  mutate(pred = fitted(lm(cl ~ Date))) %>%
  mutate(residuals = cl - pred,
         z_score = (residuals - mean(residuals, na.rm = TRUE)) / sd(residuals, na.rm = TRUE),
         outlier = abs(z_score) > 2) |> 
  # Remove outliers and reconstruct cleaned data
  mutate(
    residuals = if_else(outlier, NA_real_, residuals),
    cl.clean = pred + residuals  # Reconstruct the cleaned data
  ) %>%
  ungroup()  # Ungroup after operation

# Plot all lakes
ggplot(cl.outliers) +
  geom_point(aes(x = Date, y = cl.clean), size = 0.3) +
  facet_wrap(~Lake)


# Look at seasonality of Lake Wingra data
cl.change = cl.outliers |> filter(Lake == 'Wingra') |> 
  mutate(month = month(Date)) |> 
  mutate(wateryear = if_else(month(Date) >= 7, year(Date) + 1, year(Date))) |> 
  group_by(wateryear) |> 
  mutate(cl.change = cl.clean - first(cl.clean)) |> 
  filter(any(month == 2) & any(month == 4))

ggplot(cl.change |> filter(month <=5)) +
  geom_point(aes(x = month, y = cl.change), size = 0.3) +
  geom_path(aes(x = month, y = cl.change, group = wateryear), size = 0.3) +
  facet_wrap(~Lake)
