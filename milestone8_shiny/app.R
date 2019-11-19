library(markdown)
library(shiny)

# Define UI for application that displays my graph
ui <- fluidPage(

    # Application title
    titlePanel("Guns in America"),
    
    navbarPage("Navbar",
               tabPanel("Firearm Death Rate",
                        selectInput(inputId = "year",  # Give the input a name "genotype"
                                    label = "1. Select genotype",  # Give the input a label to be displayed in the app
                                    choices = c("2017" = 2017, "2016" = 2016, "2015" = 2015,"2014" = 2014, "2005" = 2005), selected = "a"),
                        plotOutput("firearm_plot")
               ),
                tabPanel("A Different Graphic",
                ),
               tabPanel("Map",
                        includeMarkdown("about.md")
               )
    )
    )



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$firearm_plot <- renderPlot(ggplot(data = final_data_map[final_data_map$year == input$year,],
                                     mapping = aes(x = long, y = lat, group = group, fill = rate_per_1000)) + 
                                  geom_polygon(color = "gray90", size = 0.1) +
                                  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + labs(title = "Firearm Mortality by State") + theme_map() + labs(fill = "Death Rate per 100,000") + scale_fill_gradient(low = "#ffcccb", high = "#CB454A") +
                                  labs(title = "2017 Firearm Mortality by State") +
                                  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))
    )    
}

# Run the application 
shinyApp(ui = ui, server = server)
