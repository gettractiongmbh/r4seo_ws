# Daten von APIs abfragen ---------------------------------------------

# Packages installieren; nur einmal nötig
# install.packages(c("googleAuthR",
#                    "googleAnalyticsR",
#                    "searchConsoleR",
#                    "lubridate",
#                    "jsonlite"))

library(googleAuthR)
library(googleAnalyticsR)
library(searchConsoleR)
library(lubridate)
library(jsonlite)


options(googleAuthR.client_id = "XXX.apps.googleusercontent.com",
        googleAuthR.client_secret = "XXX",
        googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics.readonly",
                                        "https://www.googleapis.com/auth/webmasters.readonly"))

gar_auth()


# Konfiguration -------------------------------------------------------

# Google Analytics View-ID ermitteln und eintragen
# ga_account_list() %>% View()
GA_VIEW_ID <- "174272300"

# GSC Property ermitteln und eintragen
# list_websites() %>% View()
GSC_PROP <- "https://dein-trueffel.de/"

# Sisitrix
SISTRIX_DOMAIN <- "XXX"
SISTRIX_API_KEY <- "XXX"

# Start- und Endzeitpunkt der Datenabfragen definieren
# Hier: Sechs Monate rückwirkend vom Beginn des aktuellen Monats bis zum
# aktuellen Datum
START_DATE <- floor_date(today(), "month") %m-% months(6)
END_DATE <- today()


# Google Analytics ----------------------------------------------------

# Organic Search / Direct Sessions abfragen

channel_filter_organic <- dim_filter(dimension = "channelGrouping",
                                     operator = "EXACT", 
                                     expressions = "Organic Search")

channel_filter_direct <- dim_filter(dimension = "channelGrouping",
                                    operator = "EXACT", 
                                    expressions = "Direct")

filter_clause <- filter_clause_ga4(list(channel_filter_organic, 
                                        channel_filter_direct),
                                   operator = "OR")

ga_sessions <- google_analytics(viewId = GA_VIEW_ID,
                                date_range = c(START_DATE, END_DATE),
                                metrics = c("sessions"),
                                dimensions = c("date", "channelGrouping"),
                                dim_filters =  filter_clause,
                                anti_sample = TRUE)


# GSC -----------------------------------------------------------------

# GSA: Date

gsa_dim_date <- search_analytics(siteURL = GSC_PROP,
                                 startDate = START_DATE,
                                 endDate = END_DATE,
                                 searchType = "web",
                                 dimensions = "date")

# GSA: Date ~ Query

gsa_dim_date_query <- search_analytics(siteURL = GSC_PROP,
                                       startDate = START_DATE,
                                       endDate = END_DATE,
                                       searchType = "web",
                                       dimensions = c("date", "query"))

# GSA: Date ~ Page

gsa_dim_date_page <- search_analytics(siteURL = GSC_PROP,
                                      startDate = START_DATE,
                                      endDate = END_DATE,
                                      searchType = "web",
                                      dimensions = c("date", "page"))


# Sistrix -------------------------------------------------------------

# API-URLs zusammenbauen
si_url_desktop <- paste0("https://api.sistrix.com/domain.sichtbarkeitsindex?history=true&format=json&api_key=", 
                         SISTRIX_API_KEY, 
                         "&domain=", 
                         SISTRIX_DOMAIN)

si_url_mobile <- paste0("https://api.sistrix.com/domain.sichtbarkeitsindex?history=true&format=json&api_key=", 
                        SISTRIX_API_KEY, 
                        "&domain=", 
                        SISTRIX_DOMAIN, 
                        "&mobile=true")

# API abfragen
api_response_desktop <- fromJSON(si_url_desktop)
api_response_mobile <- fromJSON(si_url_mobile)

# JSON-Antwort in DataFrame überführen
si_desktop <- api_response_desktop$answer$sichtbarkeitsindex[[1]]
si_mobile <- api_response_mobile$answer$sichtbarkeitsindex[[1]]





# Daten aufräumen 01 --------------------------------------------------

# Packages installieren; nur einmal nötig
# install.packages(c("tidyverse",
#                    "zoo",
#                    "padr"))

library(tidyverse)
library(zoo)
library(padr)


# GSC -----------------------------------------------------------------

# Impressions / Clicks

# Fehlende Datumspunkte einfügen, Impressions und
# Clicks mit 0 auffüllen
gsa_dim_date <- gsa_dim_date %>%
  pad(start_val = START_DATE,
      end_val = END_DATE) %>%
  replace_na(list(clicks = 0,
                  impressions = 0))


# Gleitenden Mittelwert berechnen
gsa_dim_date <- gsa_dim_date %>%
  mutate(rollmean_clicks = rollmean(x = clicks,
                                    k = 7,
                                    fill = NA,
                                    align = "right"),
         rollmean_impressions = rollmean(x = impressions,
                                         k = 7,
                                         fill = NA,
                                         align = "right"))


# Tabelle transponieren
gsa_dim_date <- gsa_dim_date %>%
  select(date, rollmean_clicks, rollmean_impressions) %>%
  gather(key = "metric",
         value = "value",
         -date)


# Google Analytics ----------------------------------------------------

# Fehlende Datumspunkte einfügen und Sessions mit 0 auffüllen
ga_sessions <- ga_sessions %>%
  pad(start_val = START_DATE,
      end_val = END_DATE,
      group = "channelGrouping") %>%
  replace_na(list(sessions = 0))


# Gleitenden Mittelwert berechnen
ga_sessions <- ga_sessions %>%
  group_by(channelGrouping) %>%
  mutate(rollmean_sessions = rollmean(x = sessions,
                                      k = 7,
                                      fill = NA,
                                      align = "right"))





# Daten aufräumen 02 --------------------------------------------------


# GSC -----------------------------------------------------------------

# Berechnung des Montags und Sonntags der Vorwoche
MONDAY_PREVIOUS_WEEK <- floor_date(today(), "week", 1) - 7
SUNDAY_PREVIOUS_WEEK <- MONDAY_PREVIOUS_WEEK + 6


# Page: Berechnung der Top 10 Seiten nach Clicks der Vorwoche
gsa_top_10_pages <- gsa_dim_date_page %>%
  
  filter(between(date, MONDAY_PREVIOUS_WEEK, SUNDAY_PREVIOUS_WEEK)) %>%
  
  group_by(page) %>%
  summarise(sum_clicks = sum(clicks),
            sum_impressions = sum(impressions),
            ctr  = sum_clicks / sum_impressions,
            weight_position = sum(impressions * position) / sum(impressions)) %>%
  ungroup() %>%
  
  top_n(n = 10,
        wt = sum_clicks) %>%
  
  arrange(desc(sum_clicks)) %>%
  
  mutate(page = str_replace(string = page, 
                            pattern = GSC_PROP, 
                            replacement = "/"),
         ctr = round(x = ctr,
                     digits = 3),
         weight_position = round(weight_position, 2)) %>%
  
  rename("Page" = page,
         "Clicks" = sum_clicks,
         "Impressions" = sum_impressions,
         "CTR" = ctr,
         "Position" = weight_position)


# Query: Berechnung der Top 10 Queries nach Clicks der Vorwoche
gsa_top_10_queries <- gsa_dim_date_query %>%
  filter(between(date, MONDAY_PREVIOUS_WEEK, SUNDAY_PREVIOUS_WEEK)) %>%
  group_by(query) %>%
  summarise(sum_clicks = sum(clicks),
            sum_impressions = sum(impressions),
            ctr  = sum_clicks / sum_impressions,
            weight_position = sum(impressions * position) / sum(impressions)) %>%
  ungroup() %>%
  top_n(10, sum_clicks) %>%
  arrange(desc(sum_clicks)) %>%
  mutate(ctr = round(ctr, 3),
         weight_position = round(weight_position, 2)) %>%
  rename("Query" = query,
         "Clicks" = sum_clicks,
         "Impressions" = sum_impressions,
         "CTR" = ctr,
         "Position" = weight_position)


# Sistrix -------------------------------------------------------------

si <- si_desktop %>%
  mutate(date = date(date)) %>%
  select(-domain) %>%
  rename("Desktop" = value) %>%
  
  left_join(si_mobile %>%
              mutate(date = date(date)) %>%
              select(-domain) %>%
              rename("Mobile" = value),
            by = "date") %>%
  
  replace_na(list(Mobile = 0)) %>%
  
  filter(between(date, START_DATE, END_DATE)) %>%
  
  gather(key = "platform",
         value = "value",
         -date)





# Visualisierung ------------------------------------------------------


# Konfiguration -------------------------------------------------------

COLOR_A <- "#377EB8"
COLOR_B <- "#FF7F00"


# GA ------------------------------------------------------------------

ggplot(data = ga_sessions, 
       aes(x = date,
           y = rollmean_sessions,
           color = channelGrouping)) +
  geom_line(size = 1) +
  geom_smooth(method = lm, 
              se = TRUE,
              size = 0.5) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#666666")) +
  labs(title = "Google Analytics - Sessions",
       subtitle = "Gleitender Mittelwert der Sessions je Channel-Grouping",
       x = "Kalenderwoche",
       y = "Sessions",
       color = "Channel Grouping") +
  scale_color_manual(values = c(COLOR_A, COLOR_B)) +
  scale_x_date(date_breaks = "2 weeks", 
               date_labels = "%W\n'%y")


# GSC -----------------------------------------------------------------

ggplot(gsa_dim_date, aes(date, value, color = metric)) +
  geom_line(size = 1) +
  geom_smooth(method = loess,
              size = 0.5, 
              se = FALSE) +
  facet_grid(metric~., 
             scales = "free_y", 
             labeller = labeller(metric = c(rollmean_clicks = "Clicks",
                                            rollmean_impressions = "Impressions"))) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#666666")) +
  labs(title = "GSC - Traffic",
       subtitle = "Gleitender Mittelwert der Impressions / Clicks",
       x = "",
       y = "") +
  scale_color_manual(values = c(COLOR_A, COLOR_B)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n'%y") +
  guides(color = F)


# SI ------------------------------------------------------------------

ggplot(si, 
       aes(date, 
           value, 
           color = platform, 
           fill = platform)) +
  geom_area(alpha = 0.5) +
  geom_point() +
  facet_grid(platform~., scales = "free_y") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "#666666")) +
  labs(title = "Sistrix - Sichtbarkeitsindex",
       subtitle = "nach Platform",
       x = "",
       y = "SI") +
  scale_color_manual(values = c(COLOR_A, COLOR_B)) +
  scale_fill_manual(values = c(COLOR_A, COLOR_B)) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%W\n'%y") +
  guides(color = F,
         fill = F)
