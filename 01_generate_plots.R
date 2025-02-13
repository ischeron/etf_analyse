library(tidyverse)
library(lubridate)
library(quantmod)


# load data ---------------------------------------------------------------
getSymbols("ISAC.L")
getSymbols("XDWD.L")


# MSCI ACWI ---------------------------------------------------------------
msci_acwi <- ISAC.L
msci_acwi <- msci_acwi %>% 
  as.data.frame() %>% 
  select(ISAC.L.Adjusted) %>% 
  rownames_to_column(var = "date") %>% 
  rename(kurs = ISAC.L.Adjusted) %>% 
  mutate(date = as.Date(date),
         year = year(date),
         date_md = paste0(month(date),"-",day(date)) %>% as.Date(format="%m-%d")) %>% 
  as_tibble()

# ergänze fehlende Daten (z.B. Wochende)
msci_acwi <- tibble(date = seq(as.Date("2011-10-21"), Sys.Date(), by="days")) %>%
  left_join(msci_acwi) %>% 
  mutate(
    kurs = ifelse(is.na(kurs), lag(kurs), kurs),
    kurs = ifelse(is.na(kurs), lag(kurs), kurs),
    kurs = ifelse(is.na(kurs), lag(kurs), kurs),
    kurs = ifelse(is.na(kurs), lag(kurs), kurs)
  )

# anpassung namen
names(msci_acwi) <- names(msci_acwi) %>% str_replace("ISAC.L.", "") %>% tolower()

# volativität
msci_acwi %>% 
  mutate(per = 100*(kurs - lag(kurs, n=1))/lag(kurs, n=1)) %>% 
  filter(date > Sys.Date()-years(1),
         !is.na(year)) %>% 
  mutate(vol = per - mean(per)) %>% 
  summarise(vol = sqrt(mean(vol^2)))

## plot totaler verlauf ---------------------------------------------------------
msci_acwi %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(x = date,
             y = kurs)) +
  geom_line() +
  geom_smooth(formula = y ~ poly(x, 2), col = "grey") +
  labs(title = "iShares MSCI ACWI UCITS ETF USD (Acc)") +
  theme_minimal()

ggsave("www/msci_acwi_total.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)

## plot verlauf nach anlagehorizont ---------------------------------------------------------
msci_acwi %>% 
  mutate(
    # percent_t1 = (kurs - lag(kurs, n=1))/lag(kurs, n=1),
    # percent_t30 = (kurs - lag(kurs, n=30))/lag(kurs, n=30),
    # percent_t90 = (kurs - lag(kurs, n=90))/lag(kurs, n=90),
    # percent_t180 = (kurs - lag(kurs, n=180))/lag(kurs, n=180),
    percent_t360 = (kurs - lag(kurs, n=360))/lag(kurs, n=360),
    percent_t720 = (kurs - lag(kurs, n=720))/lag(kurs, n=720),
    percent_t1080 = (kurs - lag(kurs, n=1080))/lag(kurs, n=1080),
    percent_t1440 = (kurs - lag(kurs, n=1440))/lag(kurs, n=1440),
    percent_t1800 = (kurs - lag(kurs, n=1800))/lag(kurs, n=1800),
    percent_t2160 = (kurs - lag(kurs, n=2160))/lag(kurs, n=2160),
    percent_t2520 = (kurs - lag(kurs, n=2520))/lag(kurs, n=2520),
    percent_t2880 = (kurs - lag(kurs, n=2880))/lag(kurs, n=2880),
    percent_t3240 = (kurs - lag(kurs, n=3240))/lag(kurs, n=3240),
    percent_t3600 = (kurs - lag(kurs, n=3600))/lag(kurs, n=3600)
    ) %>% 
  summarise(
    across(percent_t360:percent_t3600, ~min(.x, na.rm = T), .names = "min_{.col}"),
    across(percent_t360:percent_t3600, ~max(.x, na.rm = T), .names = "max_{.col}"),
    across(percent_t360:percent_t3600, ~median(.x, na.rm = T), .names = "med_{.col}")
  ) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "val"
  ) %>% 
  mutate(
    fun = substring(name, 1,3),
    jahr = parse_number(name)/360,
    val = val / jahr
  ) %>% 
  ggplot(aes(x = jahr, y = val, col = fun)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, alpha = 0.4) +
  labs(col = "",
       title = "iShares MSCI ACWI UCITS ETF USD (Acc) p.a.") +
  scale_color_manual(values = c(max = "darkred",
                                med = "grey",
                                min = "steelblue")) +
  scale_x_continuous(labels = c("0", "1/2", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                     breaks = c(0, 180, 360, 720, 1080, 1440, 1800, 2160, 2520, 2880, 3240, 3600)/360) +
  scale_y_continuous(labels = scales::percent, name = "") +
  theme_minimal()

ggsave("www/msci_acwi_horizont.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)

## plot verlauf nach anlagehorizont p.a. ---------------------------------------------------------
msci_acwi %>% 
  mutate(
    # percent_t1 = (kurs - lag(kurs, n=1))/lag(kurs, n=1),
    # percent_t30 = (kurs - lag(kurs, n=30))/lag(kurs, n=30),
    # percent_t90 = (kurs - lag(kurs, n=90))/lag(kurs, n=90),
    # percent_t180 = (kurs - lag(kurs, n=180))/lag(kurs, n=180),
    percent_t360 = (kurs - lag(kurs, n=360))/lag(kurs, n=360),
    percent_t720 = (kurs - lag(kurs, n=720))/lag(kurs, n=720),
    percent_t1080 = (kurs - lag(kurs, n=1080))/lag(kurs, n=1080),
    percent_t1440 = (kurs - lag(kurs, n=1440))/lag(kurs, n=1440),
    percent_t1800 = (kurs - lag(kurs, n=1800))/lag(kurs, n=1800),
    percent_t2160 = (kurs - lag(kurs, n=2160))/lag(kurs, n=2160),
    percent_t2520 = (kurs - lag(kurs, n=2520))/lag(kurs, n=2520),
    percent_t2880 = (kurs - lag(kurs, n=2880))/lag(kurs, n=2880),
    percent_t3240 = (kurs - lag(kurs, n=3240))/lag(kurs, n=3240),
    percent_t3600 = (kurs - lag(kurs, n=3600))/lag(kurs, n=3600)
  ) %>% 
  summarise(
    across(percent_t360:percent_t3600, ~min(.x, na.rm = T), .names = "min_{.col}"),
    across(percent_t360:percent_t3600, ~max(.x, na.rm = T), .names = "max_{.col}"),
    across(percent_t360:percent_t3600, ~median(.x, na.rm = T), .names = "med_{.col}")
  ) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "val"
  ) %>% 
  mutate(
    fun = substring(name, 1,3),
    jahr = parse_number(name)/360,
    val = val / jahr
  ) %>% 
  ggplot(aes(x = jahr, y = val, col = fun)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, alpha = 0.4) +
  labs(col = "",
       title = "iShares MSCI ACWI UCITS ETF USD (Acc) p.a.") +
  scale_color_manual(values = c(max = "darkred",
                                med = "grey",
                                min = "steelblue")) +
  scale_x_continuous(labels = c("0", "1/2", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                     breaks = c(0, 180, 360, 720, 1080, 1440, 1800, 2160, 2520, 2880, 3240, 3600)/360) +
  scale_y_continuous(labels = scales::percent, name = "") +
  theme_minimal()

ggsave("www/msci_acwi_horizont_pa.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)

# MSCI WORLD ---------------------------------------------------------------
msci_world <- XDWD.L
msci_world <- msci_world %>% 
  as.data.frame() %>% 
  select(XDWD.L.Adjusted) %>% 
  rownames_to_column(var = "date") %>% 
  rename(kurs = XDWD.L.Adjusted) %>% 
  mutate(date = as.Date(date),
         year = year(date),
         date_md = paste0(month(date),"-",day(date)) %>% as.Date(format="%m-%d")) %>% 
  as_tibble()

# ergänze fehlende Daten (z.B. Wochende)
msci_world <- tibble(date = seq(as.Date("2014-07-22"), Sys.Date(), by="days")) %>%
  left_join(msci_world) %>% 
  mutate(
    kurs = ifelse(is.na(kurs), lag(kurs), kurs),
    kurs = ifelse(is.na(kurs), lag(kurs), kurs),
    kurs = ifelse(is.na(kurs), lag(kurs), kurs),
    kurs = ifelse(is.na(kurs), lag(kurs), kurs)
  )

# anpassung namen
names(msci_world) <- names(msci_world) %>% str_replace("ISAC.L.", "") %>% tolower()

# volativität
msci_world %>% 
  mutate(per = 100*(kurs - lag(kurs, n=1))/lag(kurs, n=1)) %>% 
  filter(date > Sys.Date()-years(1),
         !is.na(year)) %>% 
  mutate(vol = (per - mean(per))^2) %>% 
  summarise(vol = sqrt(mean(vol)))


## plot totaler verlauf ---------------------------------------------------------
msci_world %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(x = date,
             y = kurs)) +
  geom_smooth(formula = y ~ poly(x, 2), col = "grey") +
  geom_line() +
  labs(title = "Xtrackers MSCI World UCITS ETF 1C") +
  theme_minimal()

ggsave("www/msci_world_total.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)

## plot verlauf nach anlagehorizont ---------------------------------------------------------
msci_world %>% 
  mutate(
    percent_t1 = (kurs - lag(kurs, n=1))/lag(kurs, n=1),
    percent_t30 = (kurs - lag(kurs, n=30))/lag(kurs, n=30),
    percent_t90 = (kurs - lag(kurs, n=90))/lag(kurs, n=90),
    percent_t180 = (kurs - lag(kurs, n=180))/lag(kurs, n=180),
    percent_t360 = (kurs - lag(kurs, n=360))/lag(kurs, n=360),
    percent_t720 = (kurs - lag(kurs, n=720))/lag(kurs, n=720),
    percent_t1080 = (kurs - lag(kurs, n=1080))/lag(kurs, n=1080),
    percent_t1440 = (kurs - lag(kurs, n=1440))/lag(kurs, n=1440),
    percent_t1800 = (kurs - lag(kurs, n=1800))/lag(kurs, n=1800),
    percent_t2160 = (kurs - lag(kurs, n=2160))/lag(kurs, n=2160),
    percent_t2520 = (kurs - lag(kurs, n=2520))/lag(kurs, n=2520),
    percent_t2880 = (kurs - lag(kurs, n=2880))/lag(kurs, n=2880),
    percent_t3240 = (kurs - lag(kurs, n=3240))/lag(kurs, n=3240),
    percent_t3600 = (kurs - lag(kurs, n=3600))/lag(kurs, n=3600)
  ) %>% 
  summarise(
    across(percent_t1:percent_t3600, ~min(.x, na.rm = T), .names = "min_{.col}"),
    across(percent_t1:percent_t3600, ~max(.x, na.rm = T), .names = "max_{.col}"),
    across(percent_t1:percent_t3600, ~median(.x, na.rm = T), .names = "med_{.col}")
  ) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "val"
  ) %>% 
  mutate(
    fun = substring(name, 1,3),
    jahr = parse_number(name)/360
  ) %>% 
  ggplot(aes(x = jahr, y = val, col = fun)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, alpha = 0.4) +
  labs(col = "",
       title = "Xtrackers MSCI World UCITS ETF 1C") +
  scale_color_manual(values = c(max = "darkred",
                                med = "grey",
                                min = "steelblue")) +
  scale_x_continuous(labels = c("0", "1/2", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                     breaks = c(0, 180, 360, 720, 1080, 1440, 1800, 2160, 2520, 2880, 3240, 3600)/360) +
  scale_y_continuous(labels = scales::percent, name = "") +
  theme_minimal()

ggsave("www/msci_world_horizont.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)

## plot verlauf nach anlagehorizont p.a. ---------------------------------------------------------
msci_world %>% 
  mutate(
    # percent_t1 = (kurs - lag(kurs, n=1))/lag(kurs, n=1),
    # percent_t30 = (kurs - lag(kurs, n=30))/lag(kurs, n=30),
    # percent_t90 = (kurs - lag(kurs, n=90))/lag(kurs, n=90),
    # percent_t180 = (kurs - lag(kurs, n=180))/lag(kurs, n=180),
    percent_t360 = (kurs - lag(kurs, n=360))/lag(kurs, n=360),
    percent_t720 = (kurs - lag(kurs, n=720))/lag(kurs, n=720),
    percent_t1080 = (kurs - lag(kurs, n=1080))/lag(kurs, n=1080),
    percent_t1440 = (kurs - lag(kurs, n=1440))/lag(kurs, n=1440),
    percent_t1800 = (kurs - lag(kurs, n=1800))/lag(kurs, n=1800),
    percent_t2160 = (kurs - lag(kurs, n=2160))/lag(kurs, n=2160),
    percent_t2520 = (kurs - lag(kurs, n=2520))/lag(kurs, n=2520),
    percent_t2880 = (kurs - lag(kurs, n=2880))/lag(kurs, n=2880),
    percent_t3240 = (kurs - lag(kurs, n=3240))/lag(kurs, n=3240),
    percent_t3600 = (kurs - lag(kurs, n=3600))/lag(kurs, n=3600)
  ) %>% 
  summarise(
    across(percent_t360:percent_t3600, ~min(.x, na.rm = T), .names = "min_{.col}"),
    across(percent_t360:percent_t3600, ~max(.x, na.rm = T), .names = "max_{.col}"),
    across(percent_t360:percent_t3600, ~median(.x, na.rm = T), .names = "med_{.col}")
  ) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "val"
  ) %>% 
  mutate(
    fun = substring(name, 1,3),
    jahr = parse_number(name)/360,
    val = val/jahr
  ) %>% 
  ggplot(aes(x = jahr, y = val, col = fun)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, alpha = 0.4) +
  labs(col = "",
       title = "Xtrackers MSCI World UCITS ETF 1C p.a.") +
  scale_color_manual(values = c(max = "darkred",
                                med = "grey",
                                min = "steelblue")) +
  scale_x_continuous(labels = c("0", "1/2", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                     breaks = c(0, 180, 360, 720, 1080, 1440, 1800, 2160, 2520, 2880, 3240, 3600)/360) +
  scale_y_continuous(labels = scales::percent, name = "") +
  theme_minimal()

ggsave("www/msci_world_horizont_pa.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)



