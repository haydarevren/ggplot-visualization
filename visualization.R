##several ggplots for sales dataset

##--------------------------------------------------------------------
##----- General
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear unused memory
  cat("\f")       # Clear the console
  
  # Prepare needed libraries
  packages <- c("ggplot2"
                , "lemon"
                , "gridExtra" # For fig1
                , "ggrepel"   # For labels in fig2.b
                , "scales"
                )
  for (i in 1:length(packages)) {
    if (!packages[i] %in% rownames(installed.packages())) {
      install.packages(packages[i], dependencies = TRUE)
    }
    library(packages[i], character.only = TRUE)
  }
  rm(packages)
  
  # Set working directory
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # Load data
  sales <- read.csv("r.sales.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  items <- read.csv("r.items.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  
  # Merge data together
  sales <- merge(sales, items, by= c("item.id"), all.x=TRUE, all.y=FALSE)
  

  
  # Reorder variables
  var.order <- c("date"
                , "category"
                , "subcategory"
                , "item.name"
                , "volume"
                , "price"
                , "sale.bottles"
                , "sale.volume"
                , "sale.dollars"
                )
  sales <- sales[var.order]
  rm(items,var.order)
  # Create a custom set of colors to be used
  mycolors <- c("#999999"
          , "#D55E00"
          , "#E69F00"
          , "#56B4E9"
          , "#009E73"
          , "#F0E442"
          , "#74c476"
          , "#1c92d5"
          , "#CC79A7"
          , "#9e79cc"
  )
##--------------------------------------------------------------------
##Calendar Heatmap
# 6 color scale of daily total sales 1;
# legend at the bottom, as a single row, without title;
# Weekday labels under the month labels;
# No grid lines for non-existing days in a month;
##--------------------------------------------------------------------  

  sales$date <- as.Date(sales$date, format = "%Y-%m-%d")
  
  sales.agg <- aggregate(sale.dollars ~ date
                          , sales
                          , sum
                          )
  sales.agg$sales <-cut(sales.agg$sale.dollars
                        ,breaks= c(0,1000000,1200000,1400000,1600000,2000000)
                        ,labels=c("0-1 mln","1-1.2 mln","1.2-1.4 mln","1.4-1.6 mln","1.6-2 mln")
                        )
  
  sales.agg$sale.dollars <- NULL
  
  orders.dates <- data.frame(date = seq(from = as.Date("2015-01-01")
                                              , to = as.Date("2015-12-31")
                                              , by = "day"
  )
  )
  # Now merge two together:
  sales.daily <- merge(orders.dates, sales.agg
                        , by = "date"
                        , all.x = TRUE
  )
  rm(sales.agg)
  rm(orders.dates)
  sales.daily$day <- as.numeric(format(sales.daily$date, "%d"))
  sales.daily$weekday <- factor(format(sales.daily$date, "%a") # Alternatively, use weekdays() function
                                 , levels = rev(c("Mon" # See below for why
                                                  , "Tue"
                                                  , "Wed"
                                                  , "Thu"
                                                  , "Fri"
                                                  , "Sat"
                                                  , "Sun"
                                 )
                                 )
                                 , ordered = TRUE
                                )
  
  
  sales.daily$week <- as.numeric(format(sales.daily$date, "%W")) + 1 
  # Calculate week of year number for 1st day of every month
  tmp <- as.numeric(format(as.Date(cut(sales.daily$date, "month")), "%W"))
  sales.daily$week <- sales.daily$week - tmp
  rm(tmp)
  sales.daily$month <- factor(format(sales.daily$date, "%b") # Alternatively, use months() function
                               , levels = c("Jan"
                                            , "Feb"
                                            , "Mar"
                                            , "Apr"
                                            , "May"
                                            , "Jun"
                                            , "Jul"
                                            , "Aug"
                                            , "Sep"
                                            , "Oct"
                                            , "Nov"
                                            , "Dec"
                               )
                               , ordered = TRUE
  )
  
  fig1 <- ggplot(sales.daily
                     , aes(x = weekday
                           , y = week
                           , fill = sales
                     )
  ) +
    geom_tile(colour = "white") + # This creates a small rectangular for every date 
    geom_text(aes(label = day)) + # Day numbers inside tiles
    scale_fill_manual(values=(c("red", "orange", "yellow","green","blue"))
                        , na.value = "gray"
    ) + 
    # facet_rep_wrap is from package "lemon" to keep month labels on every row
    facet_rep_wrap( ~ month   # formula defines which variables identify subsets of data for different facets
                    , ncol = 3 # This is needed to define when to wrap facets
                    , strip.position = "top"
                    , repeat.tick.labels = TRUE
    ) + 
    scale_y_reverse() + # Proper order of weeks
    scale_x_discrete(limits = rev(levels(sales.daily$weekday)), position = "top") + # Proper order of weekdays
    labs(x = ""
         , y = ""
         , title = "Daily Total Sales, 2015"
    ) + 
    theme_bw() + 
    theme(strip.background = element_blank() # No background color
          , strip.placement = "outside"
          , axis.text.y = element_blank()
          , legend.position="bottom"
          , legend.title = element_blank()
    ) 
    

  rm(sales.daily)
  
  
  # Export fig1 chart
  png(file = "fig1.png", width = 1920, height = 1920, res = 180)
  fig1
  dev.off()

##--------------------------------------------------------------------
##Price per liter and volume of sales
# 10 color scale for categories;
# horizontal box-plots spaced vertically across categories, colored according to category;
# outliers in box-plots must be colored according to a category;
# legend on the right, no title;
# x-axis has breaks every $2;
##--------------------------------------------------------------------  
  
  # Create a price per volume measure
  sales$price.per.l <- sales$price/sales$sale.volume
  
  # Filter values for boxplot
  ymax <- quantile(sales$price.per.l, 0.95)
  fig2a <- sales[sales$price.per.l < ymax, ]
  # Sample randomly 10% observations
  set.seed(100)
  fig2a <- fig2a[sample(1:nrow(fig2a), 0.1*nrow(fig2a)), ]
  # Box plot, vertical
  fig2a <- ggplot(fig2a, aes(x = category
                         , y = price.per.l
                         , fill = category
  )
  ) +
    geom_boxplot(outlier.shape = 21 # This shape is one of the few that support fill colors
                 , outlier.size = 2
                 , outlier.color = "black"
                 , outlier.fill = NULL # That's needed for fill to be defined by aes() above
    ) +
    scale_fill_manual(values = mycolors) + # Custom fill colors
    scale_x_discrete(limits = rev(levels(sales$category))) + # Same order for legend and axis labels 
    scale_y_continuous(breaks = seq(0, round(1.1*ymax, 0), 2) # Custom price axis breaks
    ) +
    coord_flip() + # Switch x and y axis
    labs(x = "Category"
         , y = "Price per liter, $"
         , title = "Liquor categories, price per liter"
         , subtitle = "Excluding top 5% values"
    ) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
    )
    , plot.subtitle = element_text(hjust = 0.5
                                   , face = "bold"
                                   , size = 12
                                   , color = "#912600"
                                   , margin = margin(b = 20)
    )
    , panel.grid.minor.y = element_blank()
    , axis.text.y  = element_text(size = 10, face = "bold")
    , axis.title.y  = element_text(size = 16
                                   , face = "bold"
                                   , color = "#912600"
                                   , margin = margin(r = 10, l = 10)
    )
    , axis.text.x  = element_text(size = 10, face = "bold")
    , axis.title.x  = element_text(size = 16
                                   , face = "bold"
                                   , color = "#912600"
                                   , margin = margin(t = 10, b = 10)
    )
    , axis.ticks.y = element_blank()
    , legend.position = "right"
    , legend.direction = "vertical"
    , legend.title = element_blank()
    , legend.text = element_text(size = 10, face = "bold")
    )

  
  
  # Scatter plot
  fig2b.agg <- aggregate(cbind(sale.volume)
                       ~ category + subcategory
                       , sales
                       , sum
  )
  colnames(fig2b.agg)[colnames(fig2b.agg) == "sale.volume"] <- "sale.volume.agg"
  fig2b.agg <- merge(sales[, c("category"
                             , "subcategory"
                             , "item.name"
                             , "price.per.l"
                             , "sale.volume"
  )
  ]
  , fig2b.agg
  , by = c("category", "subcategory")
  )
  fig2b.agg$price.per.l.w <- fig2b.agg$price.per.l*fig2b.agg$sale.volume/fig2b.agg$sale.volume.agg
  
  fig2b.agg <- aggregate(cbind(price.per.l.w, sale.volume)
                       ~ category + subcategory
                       , fig2b.agg
                       , sum
  )
  
  # Create labels to be used by ggrepel
  labels <- c("80 Proof Vodkas"
              , "Canadian Whiskies"
              , "Spiced Rum"
              , "Japanese Whisky"
              , "Single Barrel Bourbon Whiskies"
              , "Single Malt Scotch"
              , "Corn Whiskies"
              , "Low Proof Vodkas"
              , "Miscellaneous Brandies"
  )
  # Create empty labels variable
  fig2b.agg$labels <- NA_character_
  # Make lables be non-empty only for selected subcategories
  fig2b.agg$labels[fig2b.agg$subcategory %in% labels] <- fig2b.agg$subcategory[fig2b.agg$subcategory %in% labels]
  # Add price per liter value to non-empty labels
  fig2b.agg$labels.full[!is.na(fig2b.agg$labels)] <- paste(fig2b.agg$labels[!is.na(fig2b.agg$labels)]
                                                       , ", "
                                                       , round(fig2b.agg$price.per.l.w[!is.na(fig2b.agg$labels)], 1)
                                                       , " $/l"
                                                       , sep = ""
  )
  
  
  fig2b <- ggplot(fig2b.agg, aes(x = price.per.l.w
                             , y = sale.volume/1000
                             , label = labels.full # Will be empty for most points
  )
  ) +
    geom_point(aes(color = category)
               , fill = "white", shape = 21, stroke = 2, size = 4
    ) +
    scale_color_manual(values = mycolors) +
    geom_text_repel(force = 10
                    , direction = "both"
                    , nudge_x = 1
                    , nudge_y = 1
                    , point.padding	= 1.5
                    , box.padding = 1.5
                    , segment.size = 0.5
                    , size = 4
    ) +
    scale_x_continuous(breaks = seq(0
                                    , round(max(fig2b.agg$price.per.l.w*1.1), 0)
                                    , 2
    )
    ) +
    scale_y_continuous(breaks = seq(0
                                    , round(max(fig2b.agg$sale.volume*1.1/1000), 0)
                                    , 250
    )
    ) + 
    labs(x = "Average weighted price per liter, $"
         , y = "Liters sold, thousands"
         , title = "Liquor subcategories"
         , subtitle = "price vs quantity"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
                                    , margin = margin(b = 5)
    )
    , plot.subtitle = element_text(hjust = 0.5
                                   , face = "bold"
                                   , size = 12
                                   , color = "#912600"
                                   , margin = margin(b = 20)
    )
    , panel.grid.minor.y = element_blank()
    , axis.text.y  = element_text(size = 10, face = "bold")
    , axis.title.y  = element_text(size = 16
                                   , face = "bold"
                                   , color = "#912600"
                                   , margin = margin(r = 10, l = 10)
    )
    , axis.text.x  = element_text(size = 10, face = "bold")
    , axis.title.x  = element_text(size = 16
                                   , face = "bold"
                                   , color = "#912600"
                                   , margin = margin(t = 10, b = 10)
    )
    , legend.position = c(0.9, 0.8)
    , legend.direction = "vertical"
    , legend.title = element_blank()
    , legend.text = element_text(size = 10, face = "bold")
    , legend.background = element_rect(fill="transparent")
    )

  
  png(file = "fig2a.png", width = 2880, height = 1920, res = 180)
  fig2a
  dev.off()
  
  png(file = "fig2b.png", width = 2880, height = 1920, res = 180)
  fig2b
  dev.off()
  
##--------------------------------------------------------------------
##Sales dynamics
# two line graphs placed next to each other (one row, two columns);
# each chart shows share of sales per month/weekday relative to total sales across all month-
# s/weekdays;
# legend is between charts, no title;
# both charts use the same 10 color scale;
# y-axis is measured in percentages for both charts;
# each line connection has a circle with a black outline and ll color equal to that of the line
##--------------------------------------------------------------------
  
  fig3a.agg <- aggregate(cbind(sale.dollars) ~ category + months(date)
                       , sales
                       , sum
  )
  fig3a.agg$month <- factor(fig3a.agg$`months(date)`
                          , levels = month.name #Built-in constant with English names for months
                          , labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"
                                       , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                          )
                          , ordered = TRUE
  )
  fig3a.agg$`months(date)` <- NULL
  fig3a.agg <- merge(fig3a.agg, aggregate(cbind(sale.dollars) ~ category
                                      , fig3a.agg
                                      , sum
  )
  , by = c("category")
  , all = TRUE
  )
  names(fig3a.agg) <- c("category", "sales.monthly", "month", "sales.annual")
  fig3a.agg$sales.monthly.share <- round(100*fig3a.agg$sales.monthly/fig3a.agg$sales.annual, 2)
  
  fig3a <- ggplot(fig3a.agg, aes(x = month
                             , y = sales.monthly.share
                             , group = category
                             , color = category
                             , fill = category
  )
  ) +
    geom_line(size = 1.25) +
    geom_point(size = 4, shape = 21, color = "black", stroke = 1.5) +
    scale_color_manual(values = mycolors) + 
    scale_fill_manual(values = mycolors) + 
    scale_y_continuous(breaks = breaks_extended(10)) +
    labs(x = "Month"
         , y = "Share of total sales"
         , title = "Share of total sales per month, %"
    ) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
                                    , margin = margin(b = 10)
    )
    , axis.text.y  = element_text(size = 10, face = "bold")
    , axis.title.y  = element_blank()
    , axis.text.x  = element_text(size = 10, face = "bold")
    , axis.title.x  = element_text(size = 16
                                   , face = "bold"
                                   , color = "#912600"
                                   , margin = margin(t = 10, b = 10)
    )
    , legend.position = "right"
    , legend.direction = "vertical"
    , legend.title = element_blank()
    , legend.text = element_text(size = 10, face = "bold")
    )

  
  fig3b.agg <- aggregate(cbind(sale.dollars) ~ category + weekdays(date
                                                                 , abbreviate = TRUE
  )
  , sales
  , sum
  )
  fig3b.agg$weekday <- factor(fig3b.agg[, 2]
                            , levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
                            , ordered = TRUE
  )
  fig3b.agg[, 2] <- NULL
  fig3b.agg <- merge(fig3b.agg, aggregate(cbind(sale.dollars) ~ category
                                      , fig3b.agg, sum
  )
  , by = c("category")
  , all = TRUE
  )
  names(fig3b.agg) <- c("category", "sales.weekday", "weekday", "sales.annual")
  fig3b.agg$sales.weekday.share <- round(100*fig3b.agg$sales.weekday/fig3b.agg$sales.annual, 2)
  
  fig3b <- ggplot(fig3b.agg, aes(x = weekday
                             , y = sales.weekday.share
                             , group = category
                             , color = category
                             , fill = category
  )
  ) +
    geom_line(size = 1.25) +
    geom_point(size = 4, shape = 21, color = "black", stroke = 1.5) +
    scale_color_manual(values = mycolors) + 
    scale_fill_manual(values = mycolors) + 
    labs(x = "Weekday"
         , y = "Share of sales"
         , title = "Share of total sales per weekday, %"
    ) +
    scale_y_continuous(breaks = breaks_extended(10)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
                                    , margin = margin(b = 10)
    )
    , axis.text.y  = element_text(size = 10, face = "bold")
    , axis.title.y  = element_blank()
    , axis.text.x  = element_text(size = 10, face = "bold")
    , axis.title.x  = element_text(size = 16
                                   , face = "bold"
                                   , color = "#912600"
                                   , margin = margin(t = 10, b = 10)
    )
    , legend.position = "none"
    )

  
  fig3c <- grid.arrange(fig3a, fig3b
                      , nrow = 1, ncol = 2
                      , widths = c(2, 1)
  )

  fig3c
  png(file = "fig3a.png", width = 2880, height = 1920, res = 180)
  fig3a
  dev.off()
  
  png(file = "fig3b.png", width = 2880, height = 1920, res = 180)
  fig3b
  dev.off()
  
  png(file = "fig3c.png", width = 2880, height = 1920, res = 180)
  grid.arrange(fig3a, fig3b
               , nrow = 1, ncol = 2
               , widths = c(2, 1)
  )
  dev.off()
  
##--------------------------------------------------------------------
##Category rankings
# no axis or gridlines;
# line segments for ranks are colored based on category, but category labels are not colored;
# three ranking metrics are displayed both at top and bottom of the chart;
# chart has wide empty gaps on left and right, making it appear centered and narrow;
# ranks are shown as colored circles with white numbers inside them;
##--------------------------------------------------------------------

  sales4 <- aggregate(cbind(sale.dollars,sale.volume,sale.bottles) ~ category
                      , sales
                      , sum
                      )

  
  sales4$rank.dollars <- rank(-sales4$sale.dollars)
  sales4$rank.bottles <- rank(-sales4$sale.bottles)
  sales4$rank.volume <- rank(-sales4$sale.volume)
  
  
  
  
  
  fig4 <- ggplot(sales4)+
    geom_segment(aes(x=1
                     , xend=2
                     , y=rank.dollars
                     ,yend=rank.volume
                     ,color=category
                     )
                 ,size=2,show.legend = FALSE
                 )+
    geom_text(aes(label=category
                  ,y =rank.dollars
                  ,x=0.95
                  )
              , hjust =1, size=4, fontface="bold"
              )+
    geom_segment(aes(x=2
                     , xend=3
                     , y=rank.volume
                     ,yend=rank.bottles
                     ,color=category
    )
    ,size=2,show.legend = FALSE
    )+
    geom_point(aes(x=1,y=rank.dollars,color=category)
               ,size =8
               )+
    geom_text(aes(label= rank.dollars
                  ,y=rank.dollars,
                  x=1
                  )
    )+
    geom_point(aes(x=2,y=rank.bottles,color=category)
               ,size =8
    )+
    geom_text(aes(label= rank.bottles
                  ,y=rank.bottles,
                  x=2
    )
    )+
    geom_point(aes(x=3,y=rank.volume,color=category)
               ,size =8
    )+
    geom_text(aes(label= rank.volume
                  ,y=rank.volume,
                  x=3
    )
    )+ scale_color_manual(values=mycolors)+
    scale_x_discrete(limits=c("Sales,$","Sales,liters","Sales,bottles"),position = "top")+
    theme_bw()+
    labs(title = "Liquor Category Rankings"
         , subtitle = ""
         , x = ""
         , y = ""
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
    )
    , plot.subtitle = element_text(hjust = 0.5
                                   , face = "bold"
                                   , size = 14
                                   , color = "#912600"
    )
    , axis.title.x = element_text(face = "bold"
                                  , color = "#912600"
                                  , size = 14
    )
    , axis.text.x  = element_text(face = "bold"
                                  , vjust = 0.5
                                  , size = 12
    )
    , axis.title.y = element_blank()
    , axis.text.y  = element_blank()
    , legend.position = "none"
    ,strip.background = element_blank() # No background color
    , strip.placement = "outside"
    ,panel.border = element_blank()
    )+ 
    scale_y_reverse()

  
  
  # Export fig4 chart
  png(file = "fig4.png", width = 1920, height = 1920, res = 180)
  fig4
  dev.off()
