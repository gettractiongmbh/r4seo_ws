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
GA_VIEW_ID <- "XXX"

# GSC Property ermitteln und eintragen
# list_websites() %>% View()
GSC_PROP <- "XXX"

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
