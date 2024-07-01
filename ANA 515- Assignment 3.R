
library(dplyr)
library(stringr)
library(ggplot2)


file_path <- "C:/Documents/banud/StormEvents_details-ftp_v1.0_d1992_c20220425.csv.gz"  # Update with the actual file name

df <- read.csv(file_path)

head(df)

write.csv(df, "storm_events_raw.csv", row.names = FALSE)

columns_to_keep <- c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")
df <- df %>% select(all_of(columns_to_keep))

df <- df %>% arrange(STATE)

df <- df %>% mutate(STATE = str_to_title(STATE), CZ_NAME = str_to_title(CZ_NAME))

df <- df %>% filter(CZ_TYPE == "C")
df <- df %>% select(-CZ_TYPE)

df <- df %>% mutate(STATE_FIPS = str_pad(STATE_FIPS, 3, pad = "0"), CZ_FIPS = str_pad(CZ_FIPS, 3, pad = "0"))
df <- df %>% mutate(FIPS = paste0(STATE_FIPS, CZ_FIPS)) %>% select(-STATE_FIPS, -CZ_FIPS)

df <- df %>% rename_all(tolower)

data("state")
state_info <- data.frame(
  state = state.name,
  area = state.area,
  region = as.character(state.region)
)

events_per_state <- df %>% 
  count(state) %>% 
  rename(events = n)

state_data <- left_join(state_info, events_per_state, by = c("state" = "state")) %>%
  filter(!is.na(events))

state_data <- state_data %>%
  mutate(events_per_area = events / area)

ggplot(state_data, aes(x = reorder(state, events_per_area), y = events_per_area, fill = region)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "State", 
       y = "Number of Storm Events per Square Mile of Land Area", 
       title = "Number of Storm Events per Square Mile by State") +
  theme_minimal()

ggsave("storm_events_plot.png", width = 10, height = 8)

