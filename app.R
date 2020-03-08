library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)

# load data 
data <- read.csv2('cleaned-cdc-mortality-1999-2010-2.csv', sep=',', header=TRUE, stringsAsFactors = FALSE) %>% as_tibble()
data$Crude.Rate <- as.numeric(data$Crude.Rate)


ui <-  navbarPage(theme=shinytheme("slate"),
             h1("Death and Disease in the U.S.", align="center"),
             h4(tags$a(href="https://wonder.cdc.gov/wonder/help/ucd.html", "Data provided by CDC")), #Centers for Disease Control and Prevention"),
             
          
  tabPanel(title="Mortality by State in 2010",
          selectInput("disease", 
                      "Select an ICD Disease Code", 
                      unique(data$ICD.Chapter), 
                      selected="Neoplasms"
                      ),
          plotlyOutput("plot") 
          ),


  tabPanel(title="Mortaility Time Slider",
           h5("Rates are relative to the national average(0).  Negative Rates indicate states with mortality rates less than the national average"),
          selectInput("disease2", 
                      "Select an ICD Disease Code",
                      unique(data$ICD.Chapter),
                      selected="Neoplasms"
                      ),
                plotlyOutput("plot2") 
                )

  )



server <- function(input, output){

  # load in data as a reative object 
    dat <- reactive({read.csv2('cleaned-cdc-mortality-1999-2010-2.csv', 
                             sep=',', header=TRUE, stringsAsFactors = FALSE)})

  
    national_averages <- data %>%
      group_by(ICD.Chapter, Year) %>%
      summarize("tot_deaths"=sum(Deaths),"tot_pop"=sum(Population)) %>%
      mutate("national_rate"=(tot_deaths/tot_pop)*100000)


  ####  OUTPUT PLOT Q1
  output$plot <- renderPlotly({
    req(input$disease)
    
    dat <- dat()
    dat$Crude.Rate <- as.numeric(dat$Crude.Rate)
    dat$State <- factor(dat$State)
    
    dat <- dat %>% filter(ICD.Chapter==input$disease, Year==2010) %>% 
      ggplot(aes(x=reorder(State, Crude.Rate),y=Crude.Rate)) +
      geom_segment(aes(xend=State,yend=0), color="grey50") +
      geom_point(size=2,color="blue")+
      coord_flip() +
      labs(y='Mortality Rate [Deaths/100,000 Population]',
         x='State',
         title="Mortality Rate in 2010 ") +
      coord_flip() +
      theme_minimal()
    ggplotly(dat, height=700)
})



output$plot2 <- renderPlotly({
    req(input$disease2)
    dat2 <- dat()
    dat2$Crude.Rate <- as.numeric(dat2$Crude.Rate)
    dat2 %>%
      filter(ICD.Chapter==input$disease2) %>%
      left_join(national_averages, by=c("ICD.Chapter"="ICD.Chapter","Year"="Year")) %>%
      mutate("compare"=Crude.Rate-national_rate) %>%
      plot_ly(x=~State,
              y=~compare,
              type='bar',
              size=~Population,
              #color=~Deaths)
              frame=~Year, height=600) %>%
      layout(yaxis=list(title="Mortality Rate Relative to National Avergage")) %>%
      animation_opts(1000, easing="elastic", redraw=FALSE) %>%
      animation_button(x=1,xanchor="right",y=0,yanchor="bottom") %>%
      animation_slider(currentvalue = list(prefix = "Year ", font = list(color="red")))
     })
}

shinyApp(ui = ui, server=server)





