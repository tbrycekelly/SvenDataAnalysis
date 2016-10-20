#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel(""),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            titlePanel("Data File"),
            fileInput("file", "Data File", multiple=F, width='400px'),
            titlePanel("Analysis Parameters"),
            textInput("start", "Start Time (seconds)", value=0),
            radioButtons("step", "Increment Time (min):", c(1, 5, 10, 15, 25), selected=25),
            sliderInput("buff", "Buffer between steps (min):", 0, 5, 5),
            downloadButton("go",label="Process & Download")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Graphical",
                         titlePanel("Input"),
                         plotOutput("pre"),
                         titlePanel("Results Preview"),
                         plotOutput("pre2"),
                         radioButtons("cSelect", "Select Preview Channel:",
                                      c("Ch1", "Ch2", "Ch3", "Ch4"), inline=T)
                ),
                tabPanel("Raw Data",
                         titlePanel("Input"),
                         dataTableOutput("preview"),
                         titlePanel("Results Preview"),
                         dataTableOutput("preview2")
                ),
                tabPanel("About",
                         titlePanel("About this App"),
                         p("This app is designed to fit Type 1 linear regressions to  O2 production
            verses time curves. Once uploaded, the window for this fit is provided by the
            Increment Time box with the time between sequential windows (spacer) given by the
            Buffer slider."),
                         br(),
                         p("Here is an example plot showing the data (black) and the segments that make
            up the sequential linear fits."),
                         img(src="ExampleFit.png")
                )
            )
        )
    ),
    br(),
    hr(),
    br(),
    p("Brought to you by the FSU Plankton Ecology & Biogeochemistry Lab",
      align="center", style="color:grey")
))
