#load libraries
library(shiny)
library(readr)
library(jsonlite)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(SnowballC)

all_keywords <- read_csv("all_keywords.csv")
claims <- jsonlite::read_json("claims.json")
all_claim_reviews <- read_csv("all_claim_reviews.csv")
all_organizations <- read_csv("all_organisations.csv")
all_news_articles <- read_csv("all_news_articles.csv")
continents <- read.csv("continents")


get_top_topics <- function(organisation) {
  
  all_keywords <- read_csv("all_keywords.csv")
  claims <- jsonlite::read_json("claims.json")
  all_claim_reviews <- read_csv("all_claim_reviews.csv")
  all_organizations <- read_csv("all_organisations.csv")
  all_news_articles <- read_csv("all_news_articles.csv")
  all_countries <- read_csv("all_countries.csv")
  all_languages <- read_csv("all_languages.csv")
  
  
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
             fill = "#2062A5") +
    coord_flip() +
    xlab("Keywords") +
    ylab("Frequency") +
    theme_minimal()
  
  topics_plotly <- ggplotly(topics_plot)
  topics_plotly
}



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
  
  p <- plot_ly(languages_count_org, labels = ~Language, values = ~Total, type = 'pie')
  p
  
}


get_total_articles <- function(organisation) {
  
  org <- organisation
  org_id <- all_organizations$`@id`[all_organizations$name==org]
  articles_by_org <- subset(all_news_articles, all_news_articles$author==org_id)
  length(articles_by_org$X1)
}



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

m <- list(
  l = 0,
  r = 0,
  b = 0,
  t = 0,
  pad = 4
)

p <- plot_geo(all_the_countries) %>%
  add_trace(
    z = ~all, color = ~all, colors = 'Reds',
    text = ~name, locations = ~name, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of articles') %>%
  layout(
    geo = g,
    legend = list(orientation = 'h'), 
    margin = m
  )




dates <- c()

for (i in 1:length(claims)) {
  dates[i] <- claims[[i]]$datePublished
  
}

dates <- gsub('.{18}$', '', dates)
dates_df <- data.frame(dates, 1)

time_series <- dates_df %>% 
  group_by(dates) %>% 
  summarise(frequency = n())


time_series_plot <- ggplot(time_series, aes(x=dates, y=frequency, group = 1)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("") +
  scale_x_discrete(breaks=dates[seq(1,length(dates),by=2000)]) +
  theme_minimal()
time_series_plotly <- ggplotly(time_series_plot)





# Most active plot
all_news_articles <- read_csv("all_news_articles.csv")
all_organizations <- read_csv("all_organisations.csv")

merged_a_o <- merge(all_news_articles, 
                    all_organizations, 
                    by.x = "author",
                    by.y = "@id")

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
           fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  xlab("Organisation") +
  ylab("Number of claims")

top10_plotly <- ggplotly(top10_plot)







ui <- dashboardPage(
  
  dashboardHeader(title = "Disinfo Explorer"),
  
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Explorer", tabName = "home", icon = icon("th"))
      )
    ),
    
    
    dashboardBody(
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      
      tabItems(
        
      tabItem(tabName = "overview",
              fluidRow(
                h1("Explore the EUvsDisinfo database", style="text-align: center;"), 
                br(),
                p("You will find an overview of the disinformation data. The next tabs allow you to dig into individual organisation and content.", style="text-align: center;"), 
                br(),
                box(plotlyOutput("plot_3"), title = "Number of articles by country target", width = 12, height = 500)
              ),
              fluidRow(
                box(plotlyOutput("plot_4"), title = "Most active organisation", width = 6, height = 500),
                box(plotlyOutput("plot_5"), title = "Articles published over time", width = 6, height = 500)
              )
      ),
      
      
    tabItem(tabName = "home",
    fluidRow(
      box(selectInput(inputId = "var", 
                      label = "Choose a variable to display",
                      choices = unique(all_organizations$name[all_organizations$`@id` %in% all_news_articles$author]),
                      selected = "Sputnik Arabic"), width = 6, height = 100), 
      box(htmlOutput("articles"), width = 6, height = 100)
    ),
      
fluidRow(
  box(plotlyOutput("plot"), title = "Top Keywords", width = 6, height = 500),
  box(plotlyOutput("plot_2"), title = "Languages of the articles", width = 6, height = 500)
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$plot <- renderPlotly({ 
    get_top_topics(input$var)
  })
  
  output$plot_2 <- renderPlotly({ 
    get_languages(input$var)
  })
  
  output$articles <- renderUI({ 
    str1 <- paste("<b>", "Articles from organisation retrieved in database", "</b>")
    str2 <- paste0(input$var, " published ", get_total_articles(input$var), " articles.")
    HTML(paste(str1, str2, sep = '<br/><br/>'))
  })
  
  output$plot_3 <- renderPlotly({ 
   p
  })
  
  output$plot_4 <- renderPlotly({ 
    top10_plotly
  })
  
  output$plot_5 <- renderPlotly({ 
    time_series_plotly
  })
}

shinyApp(ui, server)
