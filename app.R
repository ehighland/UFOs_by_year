library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)

### Import cleaned data set from 
### https://www.kaggle.com/datasets/NUFORC/ufo-sightings
### As per the data set description, the data cleaning removed
### "entries where the location of the sighting was not found or 
### blank (0.8146%) or have an erroneous or blank time (8.0237%)"
### from the complete data set, also available at the 
### link above.

ufo_scrubbed <- read.csv('scrubbed.csv')

#time_span_year <- max(year(as.Date(ufo_scrubbed$datetime,format=c("%m/%d/%Y %H:%M")))) - min(year(as.Date(ufo_scrubbed$datetime,format=c("%m/%d/%Y %H:%M"))))

ufo_scrubbed$decade <- paste(as.character(year(as.Date(ufo_scrubbed$datetime,format=c("%m/%d/%Y %H:%M"))) - year(as.Date(ufo_scrubbed$datetime,format=c("%m/%d/%Y %H:%M"))) %% 10),"s",sep="") 

# ufo_time <- ggplot(data=ufo_scrubbed,aes(x=decade)) +
#   geom_bar(fill="lightgreen")

#,fill= ufo_scrubbed$decade)) +
#geom_histogram(binwidth = time_span_year,color="black") + facet_wrap(~decade)

#ufo_time

ui <- fluidPage(

  # App title ----
  titlePanel(h1("UFO Data Visualizer")),
  hr(),
  sidebarLayout( 
    sidebarPanel(
      h2("Choose one or more decades below"),
      checkboxGroupInput("checkGroup", label = h5("Decades"), 
                      # choices=unique(ufo_scrubbed$decade) %>% order(),
                     choices = list("1900s" = "1900s", "1910s" = "1910s", 
                                    "1920s" = "1920s","1930s" = "1930s",
                                    "1940s" = "1940s", "1950s" = "1950s",
                                    "1960s" = "1960s", "1970s" = "1970s", 
                                    "1980s" = "1980s","1990s"="1990s",
                                    "2000s" = "2000s", "2010s" = "2010s"),
                                    #"1990s" = 10, "2000s" = 11, "2010s" = 12),
                      selected = c("1990s","2000s","2010s")) #unique(ufo_scrubbed$decade))
    ),
  mainPanel(
    h2("UFO Sightings by Decade"),
    plotOutput("ufoPlot"))
  )
)



server <- function(input, output) {
  
  dataplot <- eventReactive(input$checkGroup, {
    df <- ufo_scrubbed %>% filter(as.factor(decade) %in% c(input$checkGroup))
  })
  
  output$ufoPlot <- renderPlot({
  ggplot(data=dataplot(),aes(x=decade)) +
      geom_bar(fill="lightgreen",color="black") +
      xlab("Decade of UFO Sighting") +
      ylab("Count of Sightings")
  })
}

shinyApp(ui, server)
  
    #renderPlot({
    
    #x    <- faithful$waiting
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
  
  # You can access the values of the widget (as a vector)
  # with input$checkGroup, e.g.
  # output$value <- renderPrint({ input$checkGroup })
  
# 
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
# 
#     # Sidebar panel for inputs ----
#     sidebarPanel(
# 
#       # Input: Slider for the number of bins ----
#       sliderInput(inputId = "bins",
#                   label = "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
# 
#     ),
# 
#     # Main panel for displaying outputs ----
#     mainPanel(
# 
#       # Output: Histogram ----
#       plotOutput(outputId = "distPlot")
# 
#     )
#   )
#)