

## Copyright (C) 2015 Phil Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

library(obAnalytics)
library(obadiah)
library(RPostgres)
library(config)
library(dplyr)
library(lubridate)




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
server <- function(input, output, session) {
  
  
  con <- (function() {
    dbObj <- NULL
    function() {
      if(is.null(dbObj) || tryCatch(DBI::dbGetQuery(dbObj, "select false as result")$result, error = function(e) TRUE )) {
        dbObj <<- DBI::dbConnect(RPostgres::Postgres(),
                                 user=config$user,
                                 dbname=config$dbname,
                                 host=config$host,
                                 port=config$port,
                                 sslmode="require",
                                 sslrootcert=config$sslrootcert,
                                 sslcert=config$sslcert,
                                 sslkey=config$sslkey,
                                 bigint="numeric")
      }
      dbObj
    }
  })()


  onSessionEnded(function() { DBI::dbDisconnect(con())})
  
  DBI::dbExecute(con(), paste0("set application_name to ",shQuote(isolate(input$remote_addr)) ))
  
  cache <- new.env(parent=emptyenv())
  
  query <- paste0("select oba_available_exchanges as exchange from obanalytics.oba_available_exchanges() order by 1" )
  exchanges <- RPostgres::dbGetQuery(con(), query)$exchange
  updateSelectInput(session, "exchange",choices=exchanges)
  
  process_dblclick <- function(raw_input) {
    tp <- as.POSIXct(raw_input$x, origin='1970-01-01 00:00.00 UTC')
    tp <- with_tz(tp, tz=values$tz)
    updateDateInput(session, "date", value=date(tp))
    updateSliderInput(session, "time.point.h", value=hour(tp))
    updateSliderInput(session, "time.point.m", value=minute(tp))
    updateSliderInput(session, "time.point.s", value=second(tp))
    
  }
  
  observeEvent(input$price_level_volume_dblclick, {
    process_dblclick(input$price_level_volume_dblclick)
  })
  

  observeEvent(input$overview_dblclick, {
    process_dblclick(input$overview_dblclick)
  })
  
  
  observeEvent(input$quote.map.dblclick, {
    process_dblclick(input$quote.map.dblclick)
  })
  

  observeEvent(input$cancellation.volume.map.dblclick, {
    process_dblclick(input$cancellation.volume.map.dblclick)
  })
  
  
  
  pairs <- reactive({
    
    exchange <- input$exchange
    query <- paste0("select oba_available_pairs as pair from obanalytics.oba_available_pairs(",
                    "obanalytics.oba_exchange_id(", shQuote(exchange), ")", ") order by 1" )
    
    pairs <- RPostgres::dbGetQuery(con(), query)$pair
    pair <- isolate(input$pair)
    
    if(!pair %in% pairs ) {
      updateSelectInput(session, "pair",choices=pairs)
    }
    else {
      updateSelectInput(session, "pair",choices=pairs, selected=pair)
    }
    
    pairs

  })

  pair <- reactive({
    pairs <- pairs()
    pair <- input$pair
    tp <- timePoint()
    
    req(pair %in% pairs )
    
    query <- paste0("select s,e from obanalytics.oba_available_period(",
                     "obanalytics.oba_exchange_id(", shQuote(isolate(input$exchange)), ") , ",
                     "obanalytics.oba_pair_id(", shQuote(pair), ")",             
                     ") order by 1" )
    period <- RPostgres::dbGetQuery(con(), query)

    if(tp < period[1,"s"] | tp > period[1,"e"]) {
          
      if (tp > period[1,"e"])
        tp <- with_tz(period[1,"e"] - isolate(zoomWidth())/2, isolate(input$tz))
      else
        tp <- with_tz(period[1,"s"] + isolate(zoomWidth())/2, isolate(input$tz))
      
      updateDateInput(session, "date", value=date(tp), min=as_date(period[1,"s"]), max=as_date(period[1,"e"]))
      updateSliderInput(session, "time.point.h", value=hour(tp))
      updateSliderInput(session, "time.point.m", value=minute(tp))
      updateSliderInput(session, "time.point.s", value=second(tp))
      req(FALSE)
    }
    updateDateInput(session, "date", min=as_date(period[1,"s"]), max=as_date(period[1,"e"]))    
    pair  
  })
  
  
  values <- reactiveValues()
  
  observeEvent(input$tz, {
    tz.before <- isolate(values$tz)

    if(!is.null(tz.before)) {
      tp <- isolate(timePoint())
      values$tz <- input$tz
      tp <- with_tz(tp, input$tz)
      
      updateDateInput(session, "date", value=date(tp))
      updateSliderInput(session, "time.point.h", value=hour(tp))
      updateSliderInput(session, "time.point.m", value=minute(tp))
      updateSliderInput(session, "time.point.s", value=second(tp))
    }
    else {
      values$tz <- input$tz
    }
  })
  
  

  # time reference 
  timePoint <- reactive(
    {
      req(input$date, input$time.point.h, input$time.point.m, input$time.point.s, input$time.point.ms)
      d <- ymd(input$date)
      make_datetime(year(d), month(d), day(d),input$time.point.h, input$time.point.m, input$time.point.s, isolate(values$tz))
      }
    )  %>% debounce(2000)
  
  


  depth <- reactive( {
    tp <- timePoint() 
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2

    exchange <- isolate(input$exchange)
    pair <- pair()
    
    withProgress(message="loading depth ...", {
        obadiah::depth(con(), from.time, to.time, exchange, pair, cache=cache, tz=tz(tp))  
        }) 
  })
  
  
  
  depth_cache <- reactive( {
    
    exchange <- isolate(input$exchange)
    pair <- pair()
    depth <- depth()
    
    obadiah::getCachedPeriods(cache, exchange, pair, 'depth') %>% 
      arrange(cached.period.start,cached.period.end) %>%
      mutate(cached.period.start=format(with_tz(cached.period.start, tz=values$tz), usetz=TRUE),
             cached.period.end=format(with_tz(cached.period.end, tz=values$tz), usetz=TRUE)
             )
  })
  
  spread <- reactive( {
    tp <- timePoint() 
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    exchange <- isolate(input$exchange)
    pair <- pair()
    

    withProgress(message="loading spread...", {
        obadiah::spread(con(), from.time, to.time, exchange, pair, cache=cache, tz=tz(tp))  
      }) 
  })
  
  trades <- reactive( {

    tp <- timePoint() 
    from.time <- tp-12*60*60
    to.time <- tp+12*60*60

    exchange <- isolate(input$exchange)
    pair <- pair()
    
    withProgress(message="loading trades ...", {
      obadiah::trades(con(), from.time, to.time, exchange, pair, cache=cache, tz=tz(tp))  
    }) 
  })
  
  
  events <- reactive( {
    
    tp <- timePoint() 
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    
    exchange <- isolate(input$exchange)
    pair <- pair()
    
    withProgress(message="loading events ...", {
      obadiah::events(con(), from.time, to.time, exchange, pair, cache=cache, tz=tz(tp))  
    }) 

  })

  depth.summary <- reactive( {
    
    tp <- timePoint() 

    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    

    exchange <- isolate(input$exchange)
    pair <- pair()
    
    withProgress(message="loading liquidity percentiles ...", {
      obadiah::depth_summary(con(), from.time, to.time, exchange, pair, cache=cache, tz=tz(tp))  
    }) 
  })

  


  # time window
  zoomWidth <- reactive({
    resolution <- as.integer(input$res)
    if(resolution == 0) return(input$zoom.width) # custom
    else return(resolution)
  })

  # set time point in ui
  output$time.point.out <- renderText(format(timePoint(),format="%Y-%m-%d %H:%M:%OS3",usetz=TRUE))
  output$zoom.width.out <- renderText(paste(zoomWidth(), "seconds"))

  # get order book given time point
  ob <- reactive({
    
    tp <- timePoint() 
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    
    exchange <- isolate(input$exchange)
    pair <- pair()
    
    
    order.book.data <- withProgress(message="loading order book ...", {
      obadiah::order_book(con(), tp, exchange, pair, bps.range=100, tz=tz(tp) )  
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
  }) %>% debounce(4000)

  # reset specified price+volume range to limits
  observe({
    if(input$reset.range) {
      updateNumericInput(session, "price.from", value=0.01)
      updateNumericInput(session, "price.to", value=10000.00)
      updateNumericInput(session, "volume.from", value=0.00000001)
      updateNumericInput(session, "volume.to", value=100000)
    }
  })

  # overview timeseries plot
  output$overview.plot <- renderPlot({
    tp <- timePoint() 
    tz <- attr(tp, "tzone")
    
    from.time <- tp-zoomWidth()/2
    to.time <- tp+zoomWidth()/2
    
    start.time <- tp - 12*60*60
    end.time <- tp+12*60*60
    
    
    p <- plotTrades(trades(), start.time = start.time, end.time = end.time )
    p <- p + ggplot2::geom_vline(xintercept=as.numeric(from.time), col="blue")
    p <- p + ggplot2::geom_vline(xintercept=as.numeric(tp), col="red")
    p + ggplot2::geom_vline(xintercept=as.numeric(to.time), col="blue") + ggplot2::scale_x_datetime(date_breaks="4 hours", labels=scales::date_format(format="%H:%M:%S", tz=tz), limits=c(start.time, end.time))
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
    plotEventsHistogram(events.filtered, from.time, to.time, val="price", bw=0.25)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz(tp)))
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
  
  output$depth.cache <- renderTable({
    dc <- depth_cache()
    colnames(dc) <- c("start", "end", "# of rows")
    dc
  }, rownames=F, colnames=T, align=c("lll"))

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

  depthbiasvalue <- reactive( {input$depthbias.value}) %>% debounce(2000)
  
  # liquidity/depth map plot
  output$depth.map.plot <- renderPlot({
    withProgress(message="generating Price level volume ...", {  
      width.seconds <- zoomWidth()
      tp <- timePoint()
      tz <- attr(tp, "tzone")
      from.time <- tp-width.seconds/2
      to.time <- tp+width.seconds/2
      depth <- depth()
      
      trades <- trades() %>% filter(direction %in% input$showtrades)
      if("with.ids.only" %in% input$showtrades) trades <- trades %>% filter(!is.na(exchange.trade.id))
        

      spread <- switch(input$showspread,
                       M=spread(),
                       B=spread(),
                       NULL
      )

      show.mp <- if(input$showspread == 'M') TRUE else FALSE
      
      show.all.depth <- "ro" %in% input$showdepth
      
      if("lr" %in% input$showdepth) {
        
        first.depth.timestamp <- (depth %>% filter(timestamp >= from.time) %>% summarize(timestamp=min(timestamp)))$timestamp
        first.spread <- (spread() %>% filter(lead(timestamp) >= first.depth.timestamp))[1, ]
        
        anchor.price <- log10((first.spread$best.bid.price + first.spread$best.ask.price)/2)
        depth <- depth %>% mutate(price = log10(price)- anchor.price) 
        spread <- spread %>% mutate(best.bid.price = log10(best.bid.price) - anchor.price, best.ask.price = log10(best.ask.price) - anchor.price)
        trades <- trades %>% mutate(price = log10(price) - anchor.price)
      } 

      col.bias <- if(input$depthbias == 0) depthbiasvalue() else 0
      
      fmt <- get_time_format(from.time, to.time)  
      
      p <- if(!autoPvRange())
        plotPriceLevels(depth, spread, trades,
                        show.mp=show.mp,
                        show.all.depth=show.all.depth,
                        col.bias=col.bias,
                        start.time=from.time,
                        end.time=to.time,
                        price.from=priceVolumeRange()$price.from,
                        price.to=priceVolumeRange()$price.to,
                        volume.from=priceVolumeRange()$volume.from,
                        volume.to=priceVolumeRange()$volume.to
                        ) + ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz))
      else 
        plotPriceLevels(depth, spread, trades,
                        show.mp=show.mp,
                        show.all.depth=show.all.depth,
                        col.bias=col.bias,
                        start.time=from.time,
                        end.time=to.time
                        ) + ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz))
        #p + ggplot2::geom_vline(xintercept=as.numeric(tp), col="red")
        p
    })
  })

  # liquidity percentile plot
  output$depth.percentile.plot <- renderPlot({
    withProgress(message="generating liquidity percentiles ...", {
      width.seconds <- zoomWidth()
      tp <- timePoint()
      from.time <- tp-width.seconds/2
      to.time <- tp+width.seconds/2
      fmt <- get_time_format(from.time, to.time)  
      
      plotVolumePercentiles(depth.summary()) + ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz(tp))) + ggplot2::coord_cartesian(xlim=c(from.time,to.time))
    })
  })

  # limit order event tab

  # order events plot
  output$quote.map.plot <- renderPlot({
    withProgress(message="generating Order events ...", {
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
                     volume.to=priceVolumeRange()$volume.to)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz(tp)))
      else
        plotEventMap(events(),
                     start.time=from.time,
                     end.time=to.time)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz(tp)))
      p
    })  
  })

  # cancellation map
  output$cancellation.volume.map.plot <- renderPlot({
    withProgress(message="generating Cancellation ...", {     
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
                      volume.to=priceVolumeRange()$volume.to)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz(tp)))            
      else
        plotVolumeMap(events(),
                      action="deleted",
                      start.time=from.time,
                      end.time=to.time,
                      log.scale=input$logvol)+ ggplot2::scale_x_datetime(labels=scales::date_format(format=fmt, tz=tz(tp)))
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
    trades$timestamp <- format(trades$timestamp, "%H:%M:%OS", tz=tz(tp), usetz=T)
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
    events$timestamp <- format(events$timestamp, "%H:%M:%OS", tz=tz(tp), usetz=T)
    events$exchange.timestamp <- format(events$exchange.timestamp, "%H:%M:%OS", tz=tz(tp), usetz=T)
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
}
