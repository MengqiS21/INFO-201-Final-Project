#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(stringr)
library(pivottabler)
library(plotly)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(wordcloud2)
library(RColorBrewer)

seattle_pet_licenses <- read_delim("seattle_pet_licenses.csv")

#Trend of licensed cats and dogs between 2005 - 2016
dogs_year <- seattle_pet_licenses %>%  
  filter (species == "Dog") %>% 
  group_by(year = year(license_issue_date)) %>% 
  summarize(Dogs = n())

cats_year <- seattle_pet_licenses %>%  
  filter (species == "Cat") %>% 
  group_by(year = year(license_issue_date)) %>% 
  summarize(Cats = n())

species_year <- merge(dogs_year,cats_year, by="year")

draft <- species_year %>%
  pivot_longer(!year, names_to = "Species",
               values_to = "Licensed_pet"
  )

#Trend of total pet, dog, cat licenses by Month
cat_by_month <- seattle_pet_licenses %>% 
  filter(species == "Cat") %>% 
  group_by(month = month(license_issue_date)) %>% 
  summarize(Cats = n())

dog_by_month <- seattle_pet_licenses %>%
  filter (species == "Dog") %>% 
  group_by(month = month(license_issue_date)) %>% 
  summarize(Dogs = n())

total_by_month <- seattle_pet_licenses %>% 
  group_by(month = month(license_issue_date)) %>% 
  summarize(Total_Pets = n())

species_month <- merge(cat_by_month,dog_by_month, by="month")

trend_data <- left_join(species_month, total_by_month, by = "month")

pivot_data <- pivot_longer(trend_data, 2:4, names_to = "Types", values_to = "number_of_pets")

hello <- seattle_pet_licenses %>%
  ggplot(data = pivot_data, mapping = aes(x= month, y = number_of_pets, color = Types)) +
  geom_line() +
  labs(title = 'Trend of total pet, dog, cat licenses by Month', 
       x='Month', 
       y='Number of pets') 


#Licensed count of Dogs and Cat by Zipcode
cat_zip <- seattle_pet_licenses %>% 
  filter(species == "Cat") %>% 
  filter(zip_code > 0 ) %>% 
  group_by(zip_code) %>% 
  summarize(Cats = n())

dog_zip <- seattle_pet_licenses %>%  
  filter (species == "Dog") %>% 
  filter(zip_code > 0 ) %>% 
  group_by(zip_code) %>% 
  summarize(Dogs = n())

species_zip <- merge(cat_zip, dog_zip, by="zip_code", na.rm=T)

zip_complete <- species_zip[!(is.na(species_zip$zip_code) & species_zip$zip_code==" "), ]

zip_data <- pivot_longer(zip_complete, 2:3, names_to = "Types", values_to = "number_of_pets")

zip_data$substring_zip_code = str_sub(zip_data$zip_code,1,5)

seattle_pet_licenses_year <- seattle_pet_licenses %>% 
  group_by(year = year(license_issue_date))

count_range <- range(seattle_pet_licenses_year$year)

# Define UI for application that draws a histogram

library("shiny")
library(tidyverse)
library(bslib)
library("rbokeh")
library(lubridate)


ui <- fluidPage(
  
  titlePanel("Seattle Pet Licenses"),
  navbarPage("Seattle Pet Licenses",
             tabPanel(
               "Introduction",
               
               fluidPage(
                         h1("Welcome to Seattle Pet Licenses app!"),
                         mainPanel(
                           img(src = "https://www.heart.org/-/media/Healthy-Living-Images/Healthy-Lifestyle/Pets/puppy-kitten-heart.jpg", 
                               height = 300, width = 400, align = "center"),
                                   textOutput("introduction")
                         )
               )
             ),
             tabPanel("Trends", 
                      # Side bar layout
                      sidebarLayout(
                        sidebarPanel(
                          # Allow user to input range
                          selectInput(
                            inputId = "user_category",
                            label = "Select Species",
                            choices = draft$Species,
                            selected = "Dogs",
                            multiple = TRUE),
                          sliderInput("year", "Choose year range:",
                                      min = count_range[1],
                                      max = count_range[2],
                                      value = count_range,
                                      step = 1)
                        ),
                        # Main Panel
                        mainPanel(
                          tabPanel("Trends",
                          textOutput("Text_trends"),
                          # display Bokeh output
                          rbokehOutput("Trends", width = "100%", height = "auto"))
                        ) 
                      )),
             tabPanel("Month Trends",
                      # Side bar layout
                      sidebarLayout( 
                        sidebarPanel(
                          # Allow user to input range
                          selectInput(
                            inputId = "user_types", 
                            label = "Select Pet Types",
                            choices = pivot_data$Types,
                            selected = "Dogs",
                            multiple = TRUE),
                          sliderInput("month", label = "Month slider",
                                      min = min(pivot_data$month),
                                      max = max(pivot_data$month),
                                      value = c(3, 9),
                                      sep = "",
                                      step = 1)
                          
                        ),
                        # Main Panel3
                        mainPanel(
                          tabPanel(
                            "Month Trends",
                            textOutput("Tab2_trends"),
                            # display Bokeh output3
                            plotlyOutput(outputId = "pet_comparisonPlot"))
                          
                        ) 
                      )),
             
             tabPanel("Zip Code Trends",
                      #Side Bar layout
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("substring_zip_code", label = "Zip Code Slider",
                                      min = 98100,
                                      max = max(as.numeric(zip_data$substring_zip_code)),
                                      value = c(9810, 98225),
                                      sep = "",
                                      step = 10)
                          
                        ),
                        mainPanel(
                          tabPanel(
                            "Zip code Tredns",
                          textOutput("Tab3_trends"),
                          # plotly output for chart
                          plotlyOutput(outputId = "zip_comparisonPlot"))
                          
                        ))
             ),
             
             tabPanel("Name wordcloud",
                      # Side bar layout
                      sidebarLayout( 
                        sidebarPanel(
                          # Allow user to input range
                          selectInput(
                            "animal",
                            label = "",
                            choices = c("Dog", "Cat"),
                            selected = "Dog"),
                        ),
                        # Main Panel3
                        mainPanel(
                          tabPanel(
                            "Name wordcloud",
                            # display Bokeh output3
                            wordcloud2Output("wordcloud"))
                          
                        ) 
                      )),
             
             tabPanel(
               "Conclusion",
               p("Welcome to The End!"),
               textOutput("conclusion"),
               textOutput("Value") 
             ))
)

server <- function(input, output) {
  
  output$Trends <- renderRbokeh({
    
    # Allow user to filter by multiple year
    
    draft <- draft %>% filter(Species %in% input$user_category) %>% 
      filter(year <= input$year[2],
             year >= input$year[1])
    
    figure(title = "Trend of licensed cats and dogs between 2005 - 2016") %>%
      ly_lines(year, Licensed_pet, data = draft, color = Species) %>%
      x_axis(label = "Year") %>%
      y_axis(label = "Licensed pet")
    
  })
  
  output$pet_comparisonPlot <- renderPlotly({
    pivot_data <- pivot_data %>% 
      filter(Types %in% input$user_types) %>% 
      filter(month <= input$month[2],
             month >= input$month[1])
    
  hello <- ggplot(data = pivot_data, mapping= aes(x=month, y = number_of_pets, color = Types)) +
      geom_line() +
      labs(title = 'Trend of total pet, dog, cat licenses by Month', x='Month', y='Number of pets') 
    
  ggplotly(hello)
    
  })
  
  output$zip_comparisonPlot <- renderPlotly({
    zip_data <- zip_data %>% 
      filter(substring_zip_code <= as.numeric(input$substring_zip_code[2]),
             substring_zip_code >= as.numeric(input$substring_zip_code[1]))
    
    compare <- ggplot(data = zip_data) +
      geom_col(mapping = aes(x=substring_zip_code, y = number_of_pets, fill = Types), position = "dodge") +
      theme(axis.text.x = element_text(angle = 45))+
      labs(title = 'Licensed count of Dogs and Cat by Zip code', x='Zip Code', y='Number of pets') 
    ggplotly(compare)  })

  
  output$wordcloud <- renderWordcloud2({
    dog<-seattle_pet_licenses[seattle_pet_licenses$species=='Dog',]
    cat<-seattle_pet_licenses[seattle_pet_licenses$species=='Cat',]
    if (input$animal == "Dog") {
      animal.names <- dog$animal_s_name
    } else {
      animal.names <- cat$animal_s_name
    }
    animal.names <- animal.names[complete.cases(animal.names)]
    animal.names <- as.data.frame(table(animal.names))
    wordcloud2(data = animal.names, size = 0.8, color = "random-light", 
               backgroundColor = "white")
  })


  output$introduction<- renderText({"For our group, we decided to work with the pet licensing data, to inform Seattleites on pet ownership trends and encouraged the people of Seattle to adopt unlicensed pets at the animal shelters."})
  
  output$Text_trends <- renderText({"This chart shows us how drastically the licenses of cats and dogs increased throughout 2005-2016 and the difference between cats and dogs."})
  
  output$Tab2_trends <- renderText({"This chart shows the changes in pet licensing amounts over the course of the year, broken down by months."})
  
  output$Tab3_trends <- renderText({"Looking at this data visualization, a couple of trends begin to appear."})
  
  output$conclusion <- renderText({"One of the main take away from the three graphs is seeing how the number of people licensing their pets has drastically increased throughout the years. "})
}

# Run the application 
shinyApp(ui = ui, server = server)
