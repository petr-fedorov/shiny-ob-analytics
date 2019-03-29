
## Copyright (C) 2015 Phil Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

library(obAnalytics)
library(obAnalyticsDb)
library(RPostgres)
library(config)

config <- config::get()
Sys.chmod(config$sslkey, mode="0600")
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user=config$user,
                      dbname=config$dbname,
                      host=config$host,
                      port=config$port,
                      sslmode="require",
                      sslrootcert=config$sslrootcert,
                      sslcert=config$sslcert,
                      sslkey=config$sslkey,
                      bigint="numeric")



# display milliseconds
options(digits.secs=3)

# auxiliary function.. flip a matrix.
reverseMatrix <- function(m) m[rev(1:nrow(m)), ]


get_time_format <- function(from.time, to.time ) {
  if(difftime(  to.time, from.time ) >= as.difftime(1, units="days") ) {
    fmt <- "%Y-%m-%d %H:%M"
  }
  else if (difftime(  to.time, from.time ) >= as.difftime(1, units="hours") )  {
    fmt <- "%H:%M:%S"
  }
  else if (difftime(  to.time, from.time ) >= as.difftime(1, units="mins") )  {
    fmt <- "%M:%S"
  } else  {
    fmt <- "%M:%OS"
  }
  fmt
}

# shiny server ep.
shinyServer(function(input, output, session) {

  output$exchanges <- renderUI({
    exchanges <- RPostgres::dbGetQuery(con, "select exchange from obanalytics.exchanges")
    selectInput("exchange","", choices=exchanges$exchange, selected="bitstamp")
  })
  
  output$pairs <- renderUI({
    pairs <- RPostgres::dbGetQuery(con, "select pair from obanalytics.pairs")
    selectInput("pair","", choices=pairs$pair, selected="BTCUSD")
  })
  
  
  depth <- reactive( {
    req(input$exchange, input$pair, input$date, input$tz)    
    from.time <- paste0(input$date, " 00:00:00 ", input$tz)
    to.time <- paste0(input$date, " 23:59:59.999 ", input$tz)
    depth <- withProgress(message="loading depth ...", {
      obAnalyticsDb::depth(con, from.time, to.time, input$exchange, input$pair)  
      }) 
  })
  
  spread <- reactive( {
    req(input$exchange, input$pair, input$date, input$tz)    
    from.time <- paste0(input$date, " 00:00:00 ", input$tz)
    to.time <- paste0(input$date, " 23:59:59.999 ", input$tz)
    spread <- withProgress(message="loading spread...", {
      obAnalyticsDb::spread(con, from.time, to.time, input$exchange, input$pair)  
    }) 
    
  })
  
  trades <- reactive( {
    req(input$exchange, input$pair, input$date, input$tz)
    from.time <- paste0(input$date, " 00:00:00 ", input$tz)
    to.time <- paste0(input$date, " 23:59:59.999 ", input$tz)
    withProgress(message="loading trades ...", {
      obAnalyticsDb::trades(con, from.time, to.time, input$exchange, input$pair)  
    }) 
  })
  
  
  events <- reactive( {
    req(input$exchange, input$pair, input$date, input$tz)
    from.time <- paste0(input$date, " 00:00:00 ", input$tz)
    to.time <- paste0(input$date, " 23:59:59.999 ", input$tz)
    withProgress(message="loading events ...", {
      obAnalyticsDb::events(con, from.time, to.time, input$exchange, input$pair)  
    }) 
  })

  depth.summary <- reactive( {
    req(input$exchange, input$pair, input$date, input$tz)
    tp <- timePoint() 
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    withProgress(message="loading depth summary ...", {
      obAnalyticsDb::depth_summary(con, from.time, to.time, input$exchange, input$pair)  
    }) 
  })

  

  # time reference 
  timePoint <- reactive({
    req(input$tz)
    second.of.day <- (input$time.point.h*3600) + (input$time.point.m*60) +
                      input$time.point.s + input$time.point.ms/1000
    as.POSIXlt(format(input$date), tz=input$tz) + second.of.day
  })

  # time window
  zoomWidth <- reactive({
    resolution <- as.integer(input$res)
    if(resolution == 0) return(input$zoom.width) # custom
    else return(resolution)
  })

  # set time point in ui
  output$time.point.out <- renderText(as.character(timePoint()))
  output$zoom.width.out <- renderText(paste(zoomWidth(), "seconds"))

  # get order book given time point
  ob <- reactive({
    req(input$exchange, input$pair, input$date, input$tz)
    tp <- timePoint() 
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    order.book.data <- withProgress(message="loading order book ...", {
      obAnalyticsDb::order_book(con, tp, input$exchange, input$pair, bps.range=100 )  
    }) 
    if(!autoPvRange()) {
      bids <- order.book.data$bids 
      bids <- bids[bids$price >= priceVolumeRange()$price.from
                 & bids$price <= priceVolumeRange()$price.to
                 & bids$volume >= priceVolumeRange()$volume.from
                 & bids$volume <= priceVolumeRange()$volume.to, ]
      asks <- order.book.data$asks
      asks <- asks[asks$price >= priceVolumeRange()$price.from
                 & asks$price <= priceVolumeRange()$price.to
                 & asks$volume >= priceVolumeRange()$volume.from
                 & asks$volume <= priceVolumeRange()$volume.to, ]
      order.book.data$bids <- bids
      order.book.data$asks <- asks
    }
    order.book.data
  })

  # auto price+volume range?
  autoPvRange <- reactive(input$pvrange != 0)

  # specified price+volume range
  priceVolumeRange <- reactive({
    list(price.from=as.numeric(input$price.from),
         price.to=as.numeric(input$price.to),
         volume.from=as.numeric(input$volume.from),
         volume.to=as.numeric(input$volume.to))
  })

  # reset specified price+volume range to limits
  observe({
    if(input$reset.range) {
      updateNumericInput(session, "price.from", value=0.01)
      updateNumericInput(session, "price.to", value=1000.00)
      updateNumericInput(session, "volume.from", value=0.00000001)
      updateNumericInput(session, "volume.to", value=100000)
    }
  })

  # overview timeseries plot
  output$overview.plot <- renderPlot({
    tp <- timePoint() 
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    fmt <- get_time_format(from.time, to.time)  
    
    p <- plotTrades(trades())
    p <- p + ggplot2::geom_vline(xintercept=as.numeric(from.time), col="blue")
    p <- p + ggplot2::geom_vline(xintercept=as.numeric(tp), col="red")
    p + ggplot2::geom_vline(xintercept=as.numeric(to.time), col="blue") + ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
  })

  # optional price histogram plot
  output$price.histogram.plot <- renderPlot({
    width.seconds <- zoomWidth()
    tp <- timePoint()
    from.time <- tp-width.seconds/2
    to.time <- tp+width.seconds/2
    fmt <- get_time_format(from.time, to.time)  
    
    events.filtered <- events()
    #events.filtered$volume <- events.filtered$volume*10^-8
    if(!autoPvRange()) {
      events.filtered <-
          events.filtered[events.filtered$price >= priceVolumeRange()$price.from
                        & events.filtered$price <= priceVolumeRange()$price.to
                        & events.filtered$volume >= priceVolumeRange()$volume.from
                        & events.filtered$volume <= priceVolumeRange()$volume.to, ]
    }
    plotEventsHistogram(events.filtered, from.time, to.time, val="price", bw=0.25)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
  })

  # optional histogram plot
  output$volume.histogram..plot <- renderPlot({
    width.seconds <- zoomWidth()
    tp <- timePoint()
    from.time <- tp-width.seconds/2
    to.time <- tp+width.seconds/2
    fmt <- get_time_format(from.time, to.time)  
    
    events.filtered <- events()  
    #events.filtered$volume <- events.filtered$volume*10^-8
    if(!autoPvRange()) {
      events.filtered <-
          events.filtered[events.filtered$price >= priceVolumeRange()$price.from
                        & events.filtered$price <= priceVolumeRange()$price.to
                        & events.filtered$volume >= priceVolumeRange()$volume.from
                        & events.filtered$volume <= priceVolumeRange()$volume.to, ] 
    }
    plotEventsHistogram(events.filtered, from.time, to.time, val="volume", bw=5)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
  })

  # order book tab

  # order book depth plot
  output$ob.depth.plot <- renderPlot({
    order.book <- ob()
    if(nrow(order.book$bids) > 0 && nrow(order.book$asks) > 0)
      plotCurrentDepth(order.book)
    else {
      par(bg="#000000")
      plot(0)
    }
  })

  # order book bids
  output$ob_bids_out <- renderTable({
    bids <- ob()$bids
    if(nrow(bids) > 0 && !any(is.na(bids))) {  
      bids$volume <- sprintf("%.8f", bids$volume)
      bids$id <- as.character(bids$id)
      bids$liquidity <- sprintf("%.8f", bids$liquidity)
      bids <- bids[, c("id", "timestamp", "bps", "liquidity", "volume", "price")]
      bids$timestamp <- format(bids$timestamp, "%H:%M:%OS", tz=input$tz, usetz=T)
      bids
    }
  }, rownames=F, colnames=T, align=paste0(rep("l", 6), collapse=""))

  # order book asks
  output$ob.asks.out <- renderTable({
    asks <- ob()$asks
    if(nrow(asks) > 0 && !any(is.na(asks))) {  
      asks <- reverseMatrix(asks)
      asks$volume <- sprintf("%.8f", asks$volume)
      asks$liquidity <- sprintf("%.8f", asks$liquidity)
      asks$id <- as.character(asks$id)
      asks <- asks[, c("price", "volume", "liquidity", "bps", "timestamp", "id")]
      asks$timestamp <- format(asks$timestamp, "%H:%M:%OS", tz=input$tz, usetz=T)
      asks
    }
  }, rownames=F, colnames=T, align=paste0(rep("l", 6), collapse=""))

  # liquidity/depth map plot
  output$depth.map.plot <- renderPlot({
    withProgress(message="generating depth map...", {  
      width.seconds <- zoomWidth()
      tp <- timePoint()
      from.time <- tp-width.seconds/2
      to.time <- tp+width.seconds/2
      depth <- depth()
      show.mp <- input$showmidprice
      trades <- if(input$showtrades) trades() else NULL
      spread <- if(input$showspread || show.mp) spread() else NULL
      show.all.depth <- input$showalldepth
      col.bias <- if(input$depthbias == 0) input$depthbias.value else 0
      
      fmt <- get_time_format(from.time, to.time)  
      
      p <- if(!autoPvRange())
        plotPriceLevels(depth, spread, trades,
                        show.mp=input$showmidprice,
                        show.all.depth=show.all.depth,
                        col.bias=col.bias,
                        start.time=from.time,
                        end.time=to.time,
                        price.from=priceVolumeRange()$price.from,
                        price.to=priceVolumeRange()$price.to,
                        volume.from=priceVolumeRange()$volume.from,
                        volume.to=priceVolumeRange()$volume.to
                        ) + ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
      else 
        plotPriceLevels(depth, spread, trades,
                        show.mp=input$showmidprice,
                        show.all.depth=show.all.depth,
                        col.bias=col.bias,
                        start.time=from.time,
                        end.time=to.time
                        ) + ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
        #p + ggplot2::geom_vline(xintercept=as.numeric(tp), col="red")
        p
    })
  })

  # liquidity percentile plot
  output$depth.percentile.plot <- renderPlot({
    withProgress(message="generating depth percentiles...", {
      width.seconds <- zoomWidth()
      tp <- timePoint()
      from.time <- tp-width.seconds/2
      to.time <- tp+width.seconds/2
      fmt <- get_time_format(from.time, to.time)  
      
      plotVolumePercentiles(depth.summary(), start.time=from.time,
                            end.time=to.time, perc.line=F)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
    })
  })

  # limit order event tab

  # order events plot
  output$quote.map.plot <- renderPlot({
    withProgress(message="generating event map...", {
      width.seconds <- zoomWidth()
      tp <- timePoint()
      from.time <- tp-width.seconds/2
      to.time <- tp+width.seconds/2
      fmt <- get_time_format(from.time, to.time)  
      
      p <- if(!autoPvRange())
        plotEventMap(events(),
                     start.time=from.time,
                     end.time=to.time,
                     price.from=priceVolumeRange()$price.from,
                     price.to=priceVolumeRange()$price.to,
                     volume.from=priceVolumeRange()$volume.from,
                     volume.to=priceVolumeRange()$volume.to)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
      else
        plotEventMap(events(),
                     start.time=from.time,
                     end.time=to.time)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
      p
    })  
  })

  # cancellation map
  output$cancellation.volume.map.plot <- renderPlot({
    withProgress(message="generating cancellation map...", {     
      width.seconds <- zoomWidth()
      tp <- timePoint()
      from.time <- tp-width.seconds/2
      to.time <- tp+width.seconds/2
      fmt <- get_time_format(from.time, to.time)  
      
      p <- if(!autoPvRange())
        plotVolumeMap(events(),
                      action="deleted",
                      start.time=from.time,
                      end.time=to.time,
                      log.scale=input$logvol,
                      price.from=priceVolumeRange()$price.from,
                      price.to=priceVolumeRange()$price.to,
                      volume.from=priceVolumeRange()$volume.from,
                      volume.to=priceVolumeRange()$volume.to)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))            
      else
        plotVolumeMap(events(),
                      action="deleted",
                      start.time=from.time,
                      end.time=to.time,
                      log.scale=input$logvol)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=input$tz))
      p
    })
  })

  # trades tab
  output$trades.out <- renderDataTable({
    trades <- trades()
    tp <- timePoint()
    width.seconds <- zoomWidth()
    from.time <- tp-width.seconds/2
    to.time <- tp+width.seconds/2
    trades <- trades[trades$timestamp >= from.time
                   & trades$timestamp <= to.time, ]
    trades$timestamp <- format(trades$timestamp, "%H:%M:%OS", tz=input$tz, usetz=T)
    trades$volume <- trades$volume
    trades
  }, options=list(pageLength=20, searchHighlight=T, order=list(list(0, "asc")),
                  rowCallback = I('function(row, data) {
                                     $("td", row).css("background",
                                         data[3]=="sell"?"#7C0A02":"#191970");
                                   }')))

  # events tab
  output$events.out <- renderDataTable({
    events <- events()
    tp <- timePoint()
    width.seconds <- zoomWidth()
    from.time <- tp-width.seconds/2
    to.time <- tp+width.seconds/2
    events <- events[events$timestamp >= from.time
                     & events$timestamp <= to.time, ]
    events$timestamp <- format(events$timestamp, "%H:%M:%OS", tz=input$tz, usetz=T)
    events$exchange.timestamp <- format(events$exchange.timestamp, "%H:%M:%OS", tz=input$tz, usetz=T)
    events$volume <- events$volume
    events$fill <- events$fill
    colnames(events) <- c("event.id", "id", "ts", "ex.ts", "price", "vol",
                          "action", "dir", "fill", "match", "type", "agg")
    events$agg <- round(events$agg, 2)
    events$fill <- with(events, ifelse(fill == 0, NA, fill))
    events
  }, options=list(pageLength=20, searchHighlight=T, order=list(list(2, "asc")),
                  rowCallback = I('function(row, data) {
                                     $("td", row).css("background",
                                         data[7]=="ask"?"#7C0A02":"#191970");
                                   }')))
})
