## Organisations ##
library(readr)
library(jsonlite)
library(tidyverse)

# Map
library(plotly)

all_countries <- read_csv("all_countries.csv")

merged_countries <- all_countries %>%
  group_by(tolower(name)) %>%
  count()


country <- c()
countries <- c()

for (i in 1:length(claims)) {
  country <- claims[[i]]$contentLocations
  countries <- c(countries, country)
}

countries <- as.data.frame(unlist(countries))

df_countries <- merge(countries, 
                       all_countries, 
                       by.x = "unlist(countries)", 
                       by.y = "@id")

count_by_country <- df_countries %>%
  group_by(name) %>%
  count()

library(countrycode)


count_by_country_weird <- count_by_country
weird <- c("Africa", 
           "Baltic states", 
           "Central Europe", 
           "Chechnya", 
           "CIS", 
           "Daesh", 
           "David Icke", 
           "Eastern Europe", 
           "EU", "Europe", "GDR", "International", "Islam", "Kirgizstan", 
           "Kosovo", "Kurdistan", "Middle East", 
           "Middle East and Africa", "The West", "West", 
           "Yugoslavia")

count_by_country_weird <- subset(count_by_country_weird, count_by_country_weird$name %in% weird)

africa <- data.frame(continents$Three_Letter_Country_Code[continents$Continent_Name=="Africa"], 8)
europe <- data.frame(continents$Three_Letter_Country_Code[continents$Continent_Name=="Europe"], 362)
baltic <- data.frame(c("LV", "LT", "EE"), 116)
eastern <- data.frame(c("BGR", "HUN", "CZE", "UKR", "MDA", "BLR", "RUS", "SVK", "ROu", "POL"), 48)
eu <- data.frame(c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", 
                   "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT",
                   "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR"), 924)

names(africa) <- c("name", "n")
names(europe) <- c("name", "n")
names(baltic) <- c("name", "n")
names(eastern) <- c("name", "n")
names(eu) <- c("name", "n")

continents <- rbind(africa, europe, baltic, eastern, eu)
continents_count <- continents %>%
  group_by(name) %>%
  summarise(sum(n))

countries_2 <- subset(count_by_country, !(count_by_country$name %in% weird))
countries_2$name <- countrycode(countries_2$name, "country.name", "iso3c")

all_the_countries <- merge(continents_count, countries_2, by = "name", all = T)
all_the_countries[is.na(all_the_countries)] = 0
all_the_countries$all <- all_the_countries$`sum(n)` + all_the_countries$n


# light grey boundaries
l <- list(color = toRGB("black"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = T,
  projection = list(type = 'Mercator')
)

p <- plot_geo(all_the_countries) %>%
  add_trace(
    z = ~all, color = ~all, colors = 'Reds',
    text = ~name, locations = ~name, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of articles') %>%
  layout(
    title = 'Number of articles targeting each country or region',
    geo = g
  )

p





# Total
all_news_articles <- read_csv("all_news_articles.csv")
all_organizations <- read_csv("all_organisations.csv")

merged_a_o <- merge(all_news_articles, 
                                all_organizations, 
                                by.x = "author",
                                by.y = "@id")
total_org <- length((unique(merged_a_o$author)))
total_org





# Most active plot

articles_organisations <- 
  count_by_org <- merged_a_o %>%
  group_by(tolower(name.y)) %>%
  count()

top10 <- head(articles_organisations[order(-articles_organisations$n),], n = 10)
colnames(top10) <- c("Organisation", "Total")

top10_plot <- ggplot(top10) +
  geom_bar(aes(y=Total, 
               x=Organisation), 
           stat="identity", 
           fill = rgb(0.1,0.4,0.5,0.7)) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Number of claims by Organisation") +
  xlab("Organisation") +
  ylab("Number of claims")

top10_plotly <- ggplotly(top10_plot)
top10_plotly





# Topics graph




get_top_topics <- function(organisation) {

  all_keywords <- read_csv("all_keywords.csv")
  claims <- jsonlite::read_json("claims.json")
  all_claim_reviews <- read_csv("all_claim_reviews.csv")
  all_organizations <- read_csv("all_organisations.csv")
  all_news_articles <- read_csv("all_news_articles.csv")
  
org <- organisation
org_id <- all_organizations$`@id`[all_organizations$name==org]
articles_by_org <- subset(all_news_articles, all_news_articles$author==org_id)
claim_by_org <- articles_by_org$claim

keywords <- c()
for (i in 1:length(claims)) {
  
  if(claims[[i]][["@id"]] %in% claim_by_org) {
    keywords <- c(keywords, claims[[i]][["keywords"]])
  }
}

keywords_df <- as.data.frame(unlist(keywords))

org_keywords <- merge(all_keywords, keywords_df, 
                      by.x = "@id",
                      by.y = "unlist(keywords)")

keywords_count_org <- org_keywords %>%
  group_by(tolower(name)) %>%
  count()

top_topics <- head(keywords_count_org[order(-keywords_count_org$n),], n = 10)
colnames(top_topics) <- c("Topic", "Total")

topics_plot <- ggplot(top_topics) +
  geom_bar(aes(y=Total, 
               x=Topic), 
           stat="identity", 
           fill = rgb(0.1,0.4,0.5,0.7)) +
  coord_flip() +
  theme_minimal() +
  ggtitle(paste0("Top Keywords for the organisation ", organisation)) +
  xlab("Keywords") +
  ylab("Frequency")

topics_plotly <- ggplotly(topics_plot)
topics_plotly

}

get_top_topics("Sputnik Arabic")



# Total article

get_total_articles <- function(organisation) {
  
  org <- organisation
  org_id <- all_organizations$`@id`[all_organizations$name==org]
  articles_by_org <- subset(all_news_articles, all_news_articles$author==org_id)
  length(articles_by_org$X1)
}

get_total_articles("Sputnik Arabic")



# Prefered languge


get_languages <- function(organisation) {
  all_languages <- read_csv("all_languages.csv")
  
  org <- organisation
  org_id <- all_organizations$`@id`[all_organizations$name==org]
  articles_by_org <- subset(all_news_articles, all_news_articles$author==org_id)
  
  languages <- merge(articles_by_org, all_languages, 
                    by.x = "inLanguage", by.y = "@id")
  
  languages_count_org <- languages %>%
    group_by(tolower(name.y)) %>%
    count()
  
  colnames(languages_count_org) <- c("Language", "Total")
  
  language_plot <- ggplot(languages_count_org, aes(x="", y=Total, fill=Language)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    ggtitle(paste0("Languages of the articles from ", organisation)) +
    theme_void() # remove background, grid, numeric labels
  
  language_plot
  
}

get_languages("Sputnik Arabic")





# Activity over time


# All articles
