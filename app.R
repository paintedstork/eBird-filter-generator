library(shiny)
library(DT)

source("helper.R")
source("global.R")

shinyServer <- function(input, output) {
  
output$minutes <- renderText( { 
  paste ("List duration <= ",
          getMinutes (input$duration, 'None', input$filterRegion), 
          " minutes &  percentile of counts at ", input$countPercentile, 
         sep='')
})

output$downloadData <- downloadHandler(
  filename = function() { paste('Filter_',
                                Sys.Date(),'_',
                                '.csv', sep='') },
  content = function(file) {
    currentfilter<-generateFilter (state = input$state,
                                   filterRegion =  input$filterRegion,
                                   fortnightly = (input$fortnight=='Fortnight'), 
                                   duration = getMinutes (input$duration, input$state, input$filterRegion),
                                   filterPercentile = input$countPercentile,
                                   makeXAs1 = input$Xas1,
                                   dataView = input$alldata)  
    write.csv(currentfilter, file)
  }  
)

output$filter <- renderDT ({currentfilter<-generateFilter (state = input$state,
                                                    filterRegion =  input$filterRegion,
                                                    fortnightly = (input$fortnight=='Fortnight'), 
                                                    duration = getMinutes (input$duration, input$state, input$filterRegion),
                                                    filterPercentile = input$countPercentile,
                                                    makeXAs1 = input$Xas1,
                                                    dataView = input$alldata)
                  },options = list(
                    lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
                    pageLength = 100))

  
}

shinyUI <- fluidPage(
  titlePanel('Filter Generator'),
  fluidRow(
    column(12,
           p("Uses eBird data to generate a fortnightly/monthly eBird filter automatically"),
           p("Created and maintained by Praveen J, Bird Count India",
             a("(@Praveen J)", href = "Email:paintedstork@gmail.com")),
           p("Last Date of Update. Data: 10 May 2019. Code: 10 October 2019. Filter Configuration: 10 October 2019"))
  ), 
  
  
  sidebarPanel(
    width = 3,  
    
    selectInput('filterRegion', 'Filter Region', choices = c("None",g_all_filters), selected = 'India--West Bengal--North'),
    selectInput('state', 'State', choices = c("None",g_states$STATE), selected = 'West Bengal'),
                            
    selectInput('fortnight', 'Period', c('Month', 'Fortnight')),

    selectInput('alldata', 'Display Data', c("Counts Only"=1, "Lists Only"=2, "Both"=3)),

    sliderInput('duration', "List Duration Percentile", min=1, max=100,
                value=90, step=1, round=0),
    
    sliderInput('countPercentile', 'Count Percentile', min=1, max=100,
                value=90, step=1, round=0),
    
    checkboxInput('Xas1', 'Consider X as 1'),
    
    helpText('These filter suggestions are created at monthly/fortnightly scale using the aggregated eBird data from a selected region.
              All lists are sorted on duration and lists below a certain duration are only considered,
              as typical lists. Long lists may have bigger counts but are atypical.
              All counts for each species is sorted and filter will be set at a percentile to catch only
              counts above that value. This is a preference of the filter editor, as lower value would mean
              longer review queues. In data poor areas, we should consider counts with X as 1, else we will
              get lot of zeros, when the species is actually present. No of complete lists where it was reported
              can be optionally shown in brackets based on user selection')
    ),

  mainPanel(
    headerPanel(textOutput ("minutes")),
    tabsetPanel(
        tabPanel("Filters", dataTableOutput('filter')),
        tabPanel("About", 
                 br(), h1("About Filter Generator"), 
                 br(), p("eBird Central implemented filters that support custom boundaries."), 
                 br(), p("This program is useful in creating/customising filters using existing eBird data which typically filter editors use past experience."), 
                 br(), p("For more information on Filters, check out these articles:"), 
                 br(), a("Understanding the eBird review and data quality process", href = "http://help.ebird.org/customer/portal/articles/1055676-understanding-the-ebird-review-and-data-quality-process/"), 
                 br(), br(), a("Understanding eBird Filters", href = "https://teamebirdmichigan.wordpress.com/2014/04/04/understanding-the-ebird-filters//"),
                 br(), br(), a("eBird Data Quality and Review Process", href = "http://www.birdcount.in/ebird-data-quality-review/") 
        )
      ),
    downloadButton('downloadData', 'Download')
  )
)

    

shinyApp(ui = shinyUI, server = shinyServer)
