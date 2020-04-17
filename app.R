library(shiny)
library(tidyverse)
library(DT)
library(scales)
library(utils)
library(lubridate)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(maps)
library(viridisLite)

options(scipen = 999)

#Import data from the site.Import with tidyverse pack
data <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
#Create Date column as date class with libridate function
data <- data %>% mutate(Date = dmy(dateRep))

#Prepare data, create total cases, total deaths and mortality rate.
dat <- data %>% group_by(countriesAndTerritories) %>%
  summarise(Total_Cases = sum(cases),
            Total_Deaths = sum(deaths),
            Mortality = round(sum(deaths)/sum(cases)*100,2),
            Population = unique(popData2018),
            Crude_cases = round(Total_Cases/Population*100000,2),
            Crude_Mortality = round(Total_Deaths/Population*100000,2))



#subset data for data table, probably it can be removed.
dat1 <- data %>% group_by(countriesAndTerritories) %>%
  select(Date,countriesAndTerritories,deaths,cases)

#create new column names country and change name of united states and united kingdom to prepare
#for join with dat and create the map.
dat2 <- dat %>%
  mutate(country=if_else(
    countriesAndTerritories=="United_States_of_America","USA",
    if_else(countriesAndTerritories=="United_Kingdom","UK",
            countriesAndTerritories)))

#Data for the map
dat_maps <- world.cities %>%
  filter(capital == 1) %>%
  dplyr::select(country = country.etc,lat,lng=long) %>%
  left_join(dat2,.,by=c("country"="country"))

#create palette for coloring the map circles
domain <- range(dat_maps$Mortality)
pal <- colorNumeric(palette = inferno(100),domain = domain)

##########################UI##################################

ui <- navbarPage("COVID-19",
                 theme = shinytheme("yeti"),
                 tabPanel("Map",
                          mainPanel(leafletOutput("mymap",width = 1400,height = 600))),
                 tabPanel("Table Summary",
                          mainPanel(tabPanel("DataTable",DT::dataTableOutput("DataTable")))),
                 tabPanel("Chart",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "Country",
                                          label = "Select Country",
                                          choices = c("Select from the list",dat$countriesAndTerritories),
                                          selected = FALSE,
                                          multiple = FALSE
                              ),
                              
                            ),
                            mainPanel(plotOutput(outputId = "countryplot"),
                                      DT::dataTableOutput("mytable")))))


server <- function(input,output){
  
  output$mymap <- renderLeaflet({
    leaflet(dat_maps) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~dat_maps$Crude_Mortality/1,
        stroke = TRUE,
        color = ~pal(Mortality),
        fillOpacity = 0.5,
        popup = paste("<br>Country</b>:",dat_maps$country,
                      "<br>Total Cases</b>:",dat_maps$Total_Cases,
                      "<br>Total Deaths</b>:",dat_maps$Total_Deaths,
                      "<br>Mortality Rate</b>:",dat_maps$Mortality,
                      "<br>Crude Mortality</b>:",dat_maps$Crude_Mortality,
                      "<br>Crude Cases</b>:",dat_maps$Crude_cases)
      )
  })
  
  output$DataTable <- DT::renderDataTable({
    dat
  })
  
  output$countryplot <- renderPlot({
    data %>% group_by(Date) %>%
      filter(countriesAndTerritories == input$Country) %>%
      ggplot(aes(x=Date,y=cases)) +
      geom_line() +
      geom_smooth(method = "auto") + scale_x_date(labels=date_format("%d-%m-%y")) +
      xlab("Date") +
      ylab("Cases") +
      ggtitle("Time Series for daily Cases with loess smooth") +
      theme_classic()
  })
  
  output$mytable <- DT::renderDataTable({
    dat %>% filter(countriesAndTerritories == input$Country)
  })
  
}

shinyApp(ui,server)
