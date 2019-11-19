library(markdown)
library(shiny)

# Define UI for application that displays my graph
ui <- fluidPage(

    # Application title
    titlePanel("Guns in America"),
    
    navbarPage("Navbar",
               tabPanel("Firearm Death Rate",
                        selectInput(inputId = "year",  # Give the input a name "genotype"
                                    label = "Select Year",  # Give the input a label to be displayed in the app
                                    choices = c("2017" = 2017, "2016" = 2016, "2015" = 2015,"2014" = 2014, "2005" = 2005), selected = "a"),
                        plotOutput("firearm_plot")
               ),
                tabPanel("A Different Graphic",
                         selectInput(inputId = "year2",  # Give the input a name "genotype"
                                     label = "Select Year",  # Give the input a label to be displayed in the app
                                     choices = c("2017" = 2017, "2016" = 2016, "2015" = 2015,"2014" = 2014, "2005" = 2005), selected = "a"),
                         plotOutput("suicide_plot")
                ),
               tabPanel("About",
                        includeMarkdown("about.md")
               )
    )
    )



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$firearm_plot <- renderPlot(ggplot(data = final_data_map[final_data_map$year == input$year2,],
                                     mapping = aes(x = long, y = lat, group = group, fill = rate_per_1000)) + 
                                  geom_polygon(color = "gray90", size = 0.1) +
                                  coord_map(projection = "albers", 
                                            lat0 = 39, lat1 = 45) + 
                                      labs(title = "Firearm Mortality by State") + 
                                      theme_map() + 
                                      labs(fill = "Death Rate per 100,000") + 
                                            scale_fill_gradient(low = "#ffcccb", 
                                            high = "#CB454A") +
                                  labs(title = "Firearm Mortality by State",
                                       subtitle = "Firearm Death Rate Per 1000 People",
                                       caption = "Data from CDC") +
                                  theme(legend.position = "right",
                                        plot.title = element_text(hjust = 0.5))
    )
    output$suicide_plot <- renderPlot(ggplot(data = final_data_suicide[final_data_suicide$year == input$year2,], aes(x = suicide_rate, y = deaths_year)) +
                                          geom_point() +
                                          geom_smooth(method = "lm") +
                                          labs(
                                              title = "Deaths per Year by Guns by Suicide Rate of State",
                                              x = "Suicide Rate per 1000",
                                              y = "Deaths per Year"
                                          )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
