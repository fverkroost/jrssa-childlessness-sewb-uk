# Function to return string of observed ages
observed_ages = function(x){ return(paste0(unique(sort(x$Age)), collapse = " & ")) }

# Count number of individuals by their observations
tot_counts = df %>%
  group_by(ID, Sex) %>%
  do(data.frame(ages_obs = observed_ages(.))) %>%
  ungroup() %>%
  group_by(ages_obs, Sex) %>%
  summarise(age_obs_counts = n())

# Plot and save bar plot of observed ages
g = ggplot(data = tot_counts, aes(x = reorder(ages_obs, age_obs_counts), y = age_obs_counts, ymax = ceiling(age_obs_counts/1e3)*1e3)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex) +
  coord_flip() +
  theme_flo() + 
  labs(y = "Number of individuals", x = "Ages at which observed") +
  geom_text(aes(label=age_obs_counts), position=position_dodge(width=0.9), hjust=-0.25) 
ggsave(paste0(output_folder, 'ages_observations.png'), g, dpi = 600, height = 10, width = 8)
print(g)

# Functions to check when individuals are observed
check_for_observation_at_age_42 = function(col){ return(42 %in% col) }
check_for_observation_at_age_46 = function(col){ return(46 %in% col) }
check_for_observation_at_all_ages = function(col){ return(all(seq(26, 46, 4) %in% col)) }

# Count how many individuals are observed at either age 42 or 46 or both
df %>%
  group_by(ID, Sex) %>% 
  mutate(val_42 = check_for_observation_at_age_42(Age)) %>%
  mutate(val_46 = check_for_observation_at_age_46(Age)) %>%
  distinct(ID, Sex, val_42, val_46) %>%
  ungroup() %>%
  group_by(Sex, val_42, val_46) %>%
  summarise(counts = n())

# Count how many total observations there are for those who are observed at either age 42 or 46 or both
df %>%
  group_by(ID, Sex) %>% 
  mutate(val_42 = check_for_observation_at_age_42(Age)) %>%
  mutate(val_46 = check_for_observation_at_age_46(Age)) %>%
   ungroup() %>%
  group_by(Sex, val_42, val_46) %>%
  summarise(counts = n())

# Add variables that indicate whether someone was observed at age 42, 46, or all ages (26-46)
final = df %>%
  group_by(ID, Sex) %>% 
  mutate(val_42 = check_for_observation_at_age_42(Age)) %>%
  mutate(val_46 = check_for_observation_at_age_46(Age)) %>%
  mutate(val_all = check_for_observation_at_all_ages(Age))
  # distinct() %>%
  # ungroup() %>%
  # group_by(Sex, val_42, val_46) %>%
  # filter(val_42 == TRUE | val_46 == TRUE)

# Save uncensored versions of the data
write.csv(subset(final, Sex == "Female" & (val_42 == TRUE | val_46 == TRUE)), 'womenBCS_obs_42_or_46.csv')
write.csv(subset(final, Sex == "Male" & (val_42 == TRUE | val_46 == TRUE)), 'menBCS_obs_42_or_46.csv')
write.csv(subset(final, Sex == "Female" & val_all == TRUE), 'womenBCS_obs_all_ages.csv')
write.csv(subset(final, Sex == "Male" & val_all == TRUE), 'menBCS_obs_all_ages.csv')

