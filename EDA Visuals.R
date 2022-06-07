# packages
library(tidyverse)
library(hutils)

uni_clean_visual <- read_csv(file = "data/processed/uni_clean.csv")

# bar plot for industries
uni_clean_visual %>% 
  group_by(industry) %>%
  count() %>%
  filter(n > 20) %>%
  ggplot(aes(x = reorder(industry, -n), y = n)) + 
  geom_bar(stat = "identity", color = "red", fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Industry", 
    y = "Count"
  ) 

# total raised and valuation
uni_clean_visual %>% 
ggplot(aes(total_raised, valuation)) +
  geom_point() +
  geom_smooth() +
  labs(
    x = "Total Raised",
    y = "Valuation"
  )

# country pie chart
uni_clean_visual %>% 
  mutate(country = fct_lump(country %>% as.factor, n=5)) %>% 
  group_by(country) %>%
  count() %>%
  ggplot(aes(x = "", y = n, fill = country)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  coord_polar(theta = "y") +
  labs(
    x = NULL,
    y = NULL
  )

