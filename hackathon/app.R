#load libraries
library(shiny)
library(readr)
library(jsonlite)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(wordcloud)
library(tm, SnowballC)
library(RColorBrewer)
library(glue)


all_keywords <- read_csv("all_keywords.csv")
claims <- jsonlite::read_json("claims.json")
all_claim_reviews <- read_csv("all_claim_reviews.csv")
all_organizations <- read_csv("all_organisations.csv")
all_news_articles <- read_csv("all_news_articles.csv")
continents <- read.csv("continents")
all_countries <- read_csv("all_countries.csv")
all_languages <- read_csv("all_languages.csv")
claims_w_emb <- read_csv("claims_w_emb.csv")

d <- claims_w_emb %>%
  arrange(yearPublished)

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)


plot_word_embedding <- plot_ly(data=d, x=~tfidf_embedding_0, y=~tfidf_embedding_1) %>%
  layout(title=list(text="<b>Claims over the Years</b><br>2015 to 2020",
                    x=0.1),
         annotations=list(x=c(5.5, 3, -1), 
                          y=c(2.2, -4.4, -4), 
                          text=c("<b>Georgia</b>","<b>Belarus</b>", "<b>Nuclear</b>"), showarrow=F),
         shapes = list(
           list(type="circle",
                x0=3.7, x1=5.7, y0=1.5, y1=3,  fillcolor = 'rgb(30, 100, 120)', line = list(color = 'rgb(30, 100, 120)'),
                opacity = 0.15),
           list(type="circle",
                x0=2.2, x1=3.7, y0=-4.2, y1=-3.2,  fillcolor = 'rgb(30, 100, 120)', line = list(color = 'rgb(30, 100, 120)'),
                opacity = 0.15)
         ),
         xaxis=ax, 
         yaxis=ax) %>%
  add_trace(type="scatter",
            mode="markers",
            marker=list(
              opacity=0.8,
              color=~yearPublished,
              colorscale='YlGnBu',
              reversescale=TRUE,
              size=4,
              colorbar = list(
                title="Year Published"
              )
            ),
            text = ~glue('<b>Claim:</b>
                         <br>{stringr::str_wrap(claimReviewed)}<br>
                         <b>Year:</b>
                         <br>{yearPublished}'),
            hoverinfo="text") 

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

  top_topics <- head(keywords_count_org[order(-keywords_count_org$freq),], n = 10)
  colnames(top_topics)[2] <- c("Total")
  colnames(top_topics)[4] <- c("Topic")
  
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
  colnames(languages_count_org)[1] <- "Language"
  colnames(languages_count_org)[2] <- "Total"
  colnames(languages_count_org)[14] <- "Language name"
  
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

all_the_countries <- merge(continents, countries_2, by.x = "name", by.y = "name", all = T)
all_the_countries[is.na(all_the_countries)] = 0
all_the_countries$all <- all_the_countries$freq + all_the_countries$freq

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
  count()

time_series_plot <- ggplot(time_series, aes(x=dates, y=freq, group = 1)) +
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

articles_organisations <- merged_a_o %>%
  select(name.y) %>%
  group_by(tolower(name.y)) %>%
  count()

top10 <- head(articles_organisations[order(-articles_organisations$freq),], n = 10)
colnames(top10) <- c("Organisation", "lower", "Total")

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





get_wordcloud <- function(country) {
  
  all_countries <- read_csv("all_countries.csv")
  all_claim_reviews <- read_csv("all_claim_reviews.csv")
  
  country_id <- all_countries$`@id`[all_countries$name==country]
  
  reviews <- c()
  for (i in 1:length(claims)) {
    
    if((country_id %in% unlist(claims[[i]]$contentLocations))) {
      reviews <- c(reviews, claims[[i]][["claimReview"]])
    }
  }
  
  text <- all_claim_reviews$claimReviewed[all_claim_reviews$`@id` %in% reviews]
  text <- removeWords(text, stopwords("english"))
  corpus <- VCorpus(VectorSource(text))
  ## make lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  ## remove white space
  corpus <- tm_map(corpus, stripWhitespace)
  ## remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  ## remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(corpus, max.words = 30, width=20, height=20, min.freq=3, colors = pal2)
}





ui <- dashboardPage(
  
  dashboardHeader(title = "Disinfo Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Explore Organisations", tabName = "home", icon = icon("users")),
      menuItem("Explore Claims", tabName = "claims", icon = icon("th"))
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
                h1("Explore the organisations spreading disinformation", style="text-align: center;"), 
                br(),
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
      ),
      
      tabItem(tabName = "claims",
              
              fluidRow(
                h1("Explore the claims made", style="text-align: center;"), 
                br(),
                box(selectInput(inputId = "country", 
                                label = "Choose a country to display",
                                choices = all_countries$name,
                                selected = "Russia"), 
                    plotOutput("plot_6"), title = "Most common terms", width = 12, height = 500)
              ),
              
              fluidRow(
                box(plotlyOutput("plot_7"), width = 12, height = 500)
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
  
  output$plot_6 <- renderPlot({ 
    get_wordcloud(input$country)
  })
  
  output$plot_7 <- renderPlotly({ 
    plot_word_embedding
  })
  
}
shinyApp(ui, server)
