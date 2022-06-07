# Experimenting with NLP

check <- uni_clean_2 %>%
  mutate(
    select_inverstors = tolower(select_inverstors),
    select_inverstors = str_replace(select_inverstors, pattern = '^[ \t]+', ''),
    select_inverstors = str_replace(select_inverstors, pattern = '[ \t]+$', ''),
    
  ) %>% 
  group_by(select_inverstors) %>% 
  summarise(
    count = n()
  ) %>% 
  arrange(desc(count))


v <- c('SoftBank  Group', 'SoftBank Capital', 'Accel', 'Sequoia Capital China', 
'Sequoia Capital', ' Sequoi Capital')

# lower case
v = str_to_lower(v)

str_replace(v, pattern = '^[ \t]+', '')
