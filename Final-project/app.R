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
                 img(src = "https://www.heart.org/-/media/Healthy-Living-Images/Healthy-Lifestyle/Pets/puppy-kitten-heart.jpg", 
                               height = 300, width = 400, align = "center"),
                         p("Welcome to Seattle Pet Licenses app!"),
                         p(h4(strong("Creators:"))),
                         p(tags$li("Bruce Xu)",
                           tags$li("Mengqi Shi"),
                           tags$li("Katherine Guo"),
                           tags$li("Jerry Xu"))),
                         br(),
                         p(h4(strong("Overview:"))),
                         p("The data set we analyzed is the pet licenses in Seattle from", strong("2005"),"to" ,  strong("2016.")),
                         p("Our report provides different aspects of what the pet licenses shows us. We display the pet’s distribution with zip code, month, and year range."),
                         br(),
                         p(h4(strong("Dataset:"))),
                         p("The dataset we working on with our Seattle Pet Licenses report is made by "),
                         br(),
                         p(h4(strong("Audiences:"))),
                         p("Our target audience is anyone who is interested in pets and considering having a pet.  
                         We believe that those pet lovers can take pleasure in discovering information about popular", strong("cat/dog"), "breeds in Seattle 
                         and how", strong("pet-friendly"), "each area is", em("(depending on the number of pet in each zip code)"), 
                         " Moreover, it could provide some suggestions for people who are interested in having a pet in the future,", "for example", strong("the name, species, and breed", style = "color: orange",
                         "they should choose. If someone finds a lost pet on the street, they could easily find the pet’s license on it.")),
                         br(),
                         p(h4(strong("Main Focuses:"))),
                         p("The main quesitions wer focused on :",
                           tags$li("What is the distrubution pattern of pets in Seattle?"),
                           tags$li("What area is the most pet friendly?"),
                           tags$li("What are the popular pet's name for dog/cat?"),
                           tags$li("Which area is most regulated for pets?")),
                         
                         mainPanel(
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
                 tags$img(src = "https://www.aahneenah.com/wp-content/uploads/2012/01/5-pets-cutout.jpg", height = 400, width = 400),
               p(h4(strong("Welcome to The End!"))),
               p(strong("Based on our analyzation of the data we have noticed that:")),
               p(tags$li("The number of pet licenses has drastically increased over the years, and dog's owner are much higher than cat's owner. "),
                 br(),
                 tags$li("By the year trends, It shows a clearly increases of the number of pet registration. This can be seen that
                         more and more people are aware of the importance of pet registration, it is beneficial to 
                         every person, whether have or have no pet. It provides a better chance of being returned home if lost, and it is 
                         a prof that the pet has properly vaccinated which reassure others. "),
                 br(),
                 tags$li("By the month trends, it illustrates an overall view on how many people register for pet license each month. Most people have their
                 pet license during", strong("October - December"), "There are least people in", strong("September"), 
                 "It can help pet owners better schedule their appointments to government agencies to register their pet license and avoid the peak time.
                         from the plot, it is better for people go to register their pet license during", strong("June or September."),
                 br(),
                 tags$li("From the pet name worldcloud, we found the patterns of pets name for dog/cat. 
                         'Lucy', 'Charlie', 'Bella', 'Luna', 'Oliver' and 'Max' seem to be shared names for dogs and cats.
                         However, 'Buddy' and 'Sadie' seem to only work for dogs.")),
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
      filter(month >= input$month[1], month <= input$month[2])
    
    pivot_data$month <- round(pivot_data$month)
    
  hello <- ggplot(data = pivot_data, mapping= aes(x= month, y = number_of_pets, color = Types)) +
      geom_line() +
      scale_x_continuous(limits = c(1, 12), breaks = 1:12) +
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

  output$Text_trends <- renderText({"This chart shows us how drastically the licenses of cats and dogs increased throughout 2005-2016 and the difference between cats and dogs."})
  
  output$Tab2_trends <- renderText({"This chart shows the changes in pet licensing amounts over the course of the year, broken down by months."})
  
  output$Tab3_trends <- renderText({"Looking at this data visualization, a couple of trends begin to appear."})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
