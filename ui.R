## Copyright (C) 2015 Phil Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

zoneinfo <- c("Europe/Moscow") # OlsonNames()

ui <- function(req) {
fluidPage(
  includeCSS("www/bootstrap-slate.css"),
  includeCSS("www/my.css"),
  div(style = "display: none;",
      textInput("remote_addr", "remote_addr",
                if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
                  req[["HTTP_X_FORWARDED_FOR"]]
                else
                  req[["REMOTE_ADDR"]]
      )
  ),
  titlePanel("obAnalytics | Microstructure visualisation"),
  sidebarLayout(
    sidebarPanel(width=3,
      wellPanel(
        fluidRow(column(6,selectInput("tz", "", choices=zoneinfo, selected="Europe/Moscow")), 
                 column(6, dateInput("date",
                                     label="",
                                     value=Sys.Date(),
                                     min="2019-04-01",
                                     max=Sys.Date()))),
        fluidRow(
                column(6, selectInput("exchange", "", choices="")),
                column(6, selectInput("pair", "", choices=""))
                 )
      ),
      hr(),
      plotOutput("overview.plot",
                 height="250px"),
      hr(),
      wellPanel(
	h4("Time of day"),
        sliderInput("time.point.h",
                    "Hour",
                    min=0,
                    max=23,
                    value=as.POSIXlt(Sys.time() - 3600)$hour,
                    step=1,
                    width="100%",
                    animate=animationOptions(interval=3000, loop=F)),
        sliderInput("time.point.m",
                    "Minute",
                    min=0,
                    max=59,
                    value=as.POSIXlt(Sys.time() - 3600)$min,
                    step=1,
                    width="100%",
                    animate=animationOptions(interval=3000, loop=F)),
	sliderInput("time.point.s",
                    "Second",
                    min=0,
                    max=59,
                    value=0,
                    step=1,
                    width="100%",
                    animate=animationOptions(interval=3000, loop=F)),
        sliderInput("time.point.ms",
                    "Millisecond",
                    min=0,
                    max=999,
                    value=0,
                    step=1,
                    width="100%",
                    animate=animationOptions(interval=3000, loop=F)),
	verbatimTextOutput("time.point.out")),
      hr(),
      wellPanel(
        selectInput("res",
                    "Time range", 
                    list("15 seconds"=15,
                         "30 seconds"=30,
                         "1 minute"=60,
                         "5 minutes"=300,
                         "15 minutes"=900,
                         "30 minutes"=1800,
                         "1 hour"=3600,
                         "3 hours"=10800,
                         "6 hours"=21600,
                         "12 hours"=43200,
                         "1 day"=86399,
                         "custom"=0),
                    selected=3600)
      ),
      conditionalPanel(
        condition="input.res == 0",
        wellPanel(
          sliderInput("zoom.width",
                      "Zoom width",
                      min=1,
                      max=86399,
                      value=60,
                      step=1,
                      width="100%"),
        verbatimTextOutput("zoom.width.out"))),
      hr(),
      conditionalPanel(
        condition="input.histcheck",
        wellPanel(
          plotOutput("price.histogram.plot",
                     height="250px"),
          plotOutput("volume.histogram..plot",
                     height="250px"))
      ),
      wellPanel(
        fluidRow(
          column(6, selectInput("pvrange",
                                "Price & Volume range",
                                list("Auto"=1,
                                     "Custom"=0))),
          column(5, checkboxInput("histcheck",
                                  label="Show histograms",
                                  value=F)))),
      conditionalPanel(
        condition="input.pvrange == 0",
        wellPanel(
          fluidRow(
            column(6, numericInput("price.from",
                                   label="Price from",
                                   value=0.01,
                                   min=0.01,
                                   max=999.99)),
            column(6, numericInput("price.to",
                                   label="Price to",
                                   value=1000.00,
                                   min=0.02,
                                   max=1000.00))),
          hr(),
          fluidRow(
            column(6, numericInput("volume.from",
                                   label="Volume from",
                                   value=0.00000001,
                                   min=0.00000001,
                                   max=99999.99999999)),
            column(6, numericInput("volume.to",
                                   label="Volume to",
                                   value=100000,
                                   min=0.00000002,
                                   max=100000))),
          actionButton("reset.range", label="Reset range")))),
    mainPanel(width=9,
      tabsetPanel(type="tabs", selected="Price level volume",
        tabPanel("Order book",
                 wellPanel(
                   plotOutput("ob.depth.plot")),
                 hr(),
                 fluidRow(
                   column(6, align="right",
                          wellPanel(
                            tags$h3("Bids"),
                            hr(),
                            tableOutput("ob_bids_out"))),
                   column(6, align="left",
                          wellPanel(
                            tags$h3("Asks"),
                            hr(),
                            tableOutput("ob.asks.out"))))),
        tabPanel("Price level volume", 
                 wellPanel(
                   fluidRow( column(2, checkboxInput("showtrades",label="Show trades", value=F)),
                             column(2, checkboxInput("showspread",label="Show spread", value=F)),
                             column(2, checkboxInput("showmidprice", label="Show midprice", value=T)),
                             column(2, checkboxInput("showalldepth",label="Show resting orders", value=T)),
                             column(2, selectInput("depthbias","Colour bias",list("Log10"=2,"Custom"=0), selected=0)),
                             column(2, conditionalPanel(condition="input.depthbias == 0",numericInput("depthbias.value", label="bias", value=0.1)))
                            )
                   ,
                   plotOutput("depth.map.plot", height="800px"))
#                 ,
#                conditionalPanel(
#                  condition="input.showpercentiles == true",
#                  wellPanel(
#                    plotOutput("depth.percentile.plot", height="400px"))),
#          wellPanel(
#            fluidRow(column(4,  checkboxInput("showpercentiles",label="Show liquidity percentiles", value=F))
#              ))
        ),
        tabPanel("Order events",
                 wellPanel(
                   plotOutput("quote.map.plot", height="800px"))
        ),
        tabPanel("Cancellations",
                 wellPanel(
                     plotOutput("cancellation.volume.map.plot",
                                height="800px"),
                     checkboxInput("logvol",
                                   label="Logarithmic scale",
                                   value=T))),
        tabPanel("Trades",
                 wellPanel(
                   dataTableOutput(outputId="trades.out"))),
        tabPanel("Events",
                 wellPanel(
                   dataTableOutput(outputId="events.out"))),
        tabPanel("About",
                 wellPanel(
                   h1("About"),
                   p("This experimental tool/demo has been developed using the",
                     a("Shiny", href="http://shiny.rstudio.com/"),
                     "web application framework for R."),
                   p("It is based on", tags$b(a("obAnalytics",
                       href="https://github.com/phil8192/ob-analytics")),
                     "- an R package created to explore and visualise
                      limit order book data."),
                   br(),
                   p("Copyright", HTML("&copy;"), "2015, Phil Stubbings."),
                   p(a("phil@parasec.net", href="mailto:phil@parasec.net")),
                   p(a("http://parasec.net", href="http://parasec.net"))))))))
}

