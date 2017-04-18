################################################################################
# Budget Breakouts Interactive App with R and Shiny
# L.Lipsey and G.Coll 
################################################################################


require(shiny)
require(ggplot2)
require(plyr)
require(dplyr)
require(shinyBS)
require(shinyURL)
require(Cairo)
require(grid)
require(gridExtra)


################################################################################
# 1. Builds the user interface and display areas
################################################################################

# ui function would become ui.R if you were doing a two-file shiny app

ui <- fluidPage(
  #fluidRow(tags$head(
  # tags$style("body {background-color: #F6F8FC; }")
  #), 
  fluidRow(
    
    # left column, with buttons and settings
    column(3, align = 'center',
           br(),
           
           sliderInput('Yr', "Year Range:",
                       min = 2000, max = 2015,
                       value = c(2000,2015),
                       step = 1, width = '100%', sep = ""),
           br(),
           
           selectInput('BreakoutType', "Breakout",
                       c("All Contracts" = "All",
                         "Products by Category" = "Prd",
                         "Services by Category" = "Svc",
                         "R&D by Stage" = "RnD"),
                       selected = "All",
                       selectize = FALSE,
                       width = '100%',
                       size = 5),
           
           br(),
           
           selectInput('C',"Customer",
                       c("Air Force", "Army", "Navy", "MDA",
                         "DLA", "Other DoD"),
                       multiple = TRUE,
                       selectize = FALSE,
                       selected = c("Air Force", "Army", "Navy", "MDA",
                                    "DLA","Other DoD"),
                       width = '100%',
                       size = 7), 
           
           br(),
           
           downloadLink('CSVDownloadBtn', 
                        "Download Displayed Data (csv)"),
           
           br(), 
           br(), 
           
           
           
           helpText(HTML("<strong>Directions:</strong>", 
                         "click on each bar for more detailed information" 
           ))),
    
    # right column, with output areas
    column(9, align = 'center',
           #h2(" ", align = "center"),
           
           # large plot area
           plotOutput("plot", height = '525px',
                      click = clickOpts(id =".plot_click")),
           
           # small plot and text area for on-click info
           fluidRow(
             column(12, align = 'left',
                    
                    # on-click text area
                    textOutput('clickloc'),
                    
                    # style options for text box, in CSS/HTML
                    # I don't really understand how this works and mostly
                    # copied it off something I googled.
                    tags$head(tags$style("#clickloc{color: #554449;
                                         font-size: 18px;
                                         font-style: bold;
                                         }"
                    ))
             )),
           
           br(), 
           
           fluidRow(
             
             column(1), 
             
             # on-click plot area
             column(12, align = 'center', plotOutput("clickplot", height = '270px')
             )
           )) 
    
  )
)

# start of server function - this would be server.R if you were doing a
# two-file shiny app

server <- function(input, output, session){
  
  ################################################################################
  # 2. Reads in and cleans up data initially
  ################################################################################ 
  
  # read in data            
  FullData <- read.csv("Breakouts.csv")
  
  # rename MilitaryHealth to be shorter
  #levels(FullData$Customer)[5] <- "Mil Health"
  
  # Fiscal year is a category, not a numeric variable
  FullData$FY <- as.factor(FullData$FY)
  
  # Rescale total obligations variable (Amount) to units of $Billion
  FullData$Amount <- FullData$Amount / 1000000000
  FullData <- rename(FullData, Billion = Amount)
  
  # Reorder Category levels so R&D will be on top of bars in graph
  FullData$Category <- factor(FullData$Category, levels = 
                                c("Products", "Services", "R&D"))
  
  FullData$Customer[FullData$Customer == "MilitaryHealth"] <- "Other DoD" 
  
  # Reorder Breakouts levels to stack them in the correct order on bar charts
  FullData$Breakout <- factor(FullData$Breakout, levels = 
                                c(
                                  
                                  # R&D breakouts        
                                  "Basic Research (6.1)",
                                  "Applied Research (6.2)",
                                  "Adv. Technology Dev. (6.3)",
                                  "Adv. Component Dev. & Prototypes (6.4)",
                                  "System Dev. & Demonstration (6.5)",
                                  "Operational Systems Dev. (6.7)",
                                  "Operation of Government R&D Facilities",
                                  
                                  # products breakouts
                                  "Aircraft",
                                  "Missiles & Space",
                                  "Ground Vehicles",
                                  "Engines & Power Plants",
                                  "Fuels",
                                  "Ships",
                                  "Launchers & Munitions",
                                  "Electronics & Communications",
                                  "Clothing & Subsistence",
                                  "Other",
                                  
                                  # services breakouts
                                  "ERS",
                                  "FRS&C",
                                  "ICT",
                                  "MED",
                                  "PAMS"
                                ))
  
  # save FY as 2 digits instead of 4, for better visual scale
  FullData$FY <- factor(substring(as.character(FullData$FY), 3, 4))
  
  
  ################################################################################
  # 3. Subsets data based on user inputs
  ################################################################################
  
  dataset <- reactive({
    
    # subset based on year, as requested by user -
    # would work fine filtering by (input > lower limit & input < upper limit)
    # but the findInterval function seems a bit faster
    shown <- filter(FullData,
                    findInterval((as.numeric(as.character(FY)))+2000,
                                 c(input$Yr[1], input$Yr[2]+1)) == 1L)
    
    # subset based on customer field, as requested by user
    shown <- filter(shown, Customer %in% input$C)
    
    # subset to products / services / R&D for the breakout graph types,
    # and rename the "breakout" variable to "category."
    # Do nothing for the all contracts graph type; it already uses "category"
    switch(input$BreakoutType,
           "Prd" = {
             shown <- filter(shown, Category == "Products")
             shown <- select(shown, -Category)
             names(shown)[names(shown)=="Breakout"] <- "Category"
           },
           "Svc" = {
             shown <- filter(shown, Category == "Services")
             shown <- select(shown, -Category)
             names(shown)[names(shown)=="Breakout"] <- "Category"
           },
           "RnD" = {
             shown <- filter(shown, Category == "R&D")
             shown <- filter(shown, Breakout !=
                               "Operation of Government R&D Facilities")
             shown <- select(shown, -Category)
             names(shown)[names(shown)=="Breakout"] <-
               "Category"
           }
    )
    
    #aggregate obligations amount by FY and breakout category
    shown <- ddply(shown, .(FY, Category),
                   summarize, Billion = sum(Billion))
    
    #add midpoint of amounts ("pos") so we can put numbers on the bars in plot
    shown <- ddply(shown, .(FY), transform,
                   pos = cumsum(Billion) - (0.5 * Billion))
    
    # remove negative values to stop them from generating error messages
    shown$Billion[shown$Billion < 0] <- 0
    
    # track highest total billions in a year
    highbill <- numeric()
    shown <- select(shown, -Billion, everything())
    temp <- ddply(shown, .(FY), summarize, Billion = sum(Billion))
    highbill <- max(temp$Billion)
    
    # create subtotals ("ongraph") to add to the plot -
    # these will be the white numbers on the bars
    # modify number of decimal places according to how large totals are
    decimals <- 2 - (max(c(floor(log10(highbill)*1.5), 0)))
    shown$ongraph <- round(shown$Billion, decimals)
    
    
    # drop subtotals if they'll be too small to display well
    # current test: less than 3.5% of largest bar
    shown$ongraph[shown$ongraph <= 0.99*highbill] <- NA
    
    # return updated dataset
    shown
    
  })
  
  
  
  ################################################################################
  # 4. Defines color settings for plot
  ################################################################################
  
  # Color scale for Breakouts and Category
  colorset <- 
    c(
      # overall chart colors
      "Products" = "#CE884E",
      "Services" = "#63c5b8",
      "R&D" = "#628582", 
      
      # products colors
      "Aircraft" = "#554449",
      "Missiles & Space" = "#5F597C",
      "Ground Vehicles" = "#36605a", 
      "Engines & Power Plants" = "#AD4545",
      "Fuels" = "#008e9d", 
      "Ships" = "#599a9e",
      "Launchers & Munitions" = "#CE884E",
      "Electronics & Communications" = "#63c5b8", 
      #"Clothing & Subsistence" = "#C74F4F",
      "Clothing & Subsistence" = "#C76363",
      "Other" = "#628582", 
      
      # services colors
      "ERS" = "#CE884E",
      "FRS&C" = "#008e9d",
      "ICT" = "#63c5b8",
      "MED" = "#C74F4F",
      "PAMS" = "#628582",
      
      # R&D colors
      "Basic Research (6.1)" = "#008e9d",
      "Applied Research (6.2)" = "#599a9e",
      "Adv. Technology Dev. (6.3)" = "#CE884E",
      "Adv. Component Dev. & Prototypes (6.4)" = "#63c5b8",
      "System Dev. & Demonstration (6.5)" = "#C74F4F",
      "Operational Systems Dev. (6.7)" = "#628582"
      
    )
  
  #coll: Titles for the legend. Also added a space to the end of titles for a more spread out legend 
  nameset <- 
    c( 
      # overall chart titles
      "Products" = "Products ",
      "Services" = "Services ",
      "R&D" = "Research & Development ", 
      
      # products titles
      "Aircraft" = "Aircraft ",
      "Missiles & Space" = "Missiles & Space ",
      "Ground Vehicles" = "Ground Vehicles ", 
      "Engines & Power Plants" = "Enginges & Power Plants ",
      "Fuels" = "Fuels ", 
      "Ships" = "Ships ",
      "Launchers & Munitions" = "Launchers & Munitions ",
      "Electronics & Communications" = "Electronics & Communications ", 
      "Clothing & Subsistence" = "Clothing & Subsistence ",
      "Other" = "Other ", 
      
      # services titles 
      "ERS" = "Equipment ",
      "FRS&C" = "Facilities ",
      "ICT" = "Information and Communications Technology ",
      "MED" = "Medical ",
      "PAMS" = "Professional, Administrative, and Management ",
      
      # R&D titles
      "Basic Research (6.1)" = "Basic Research (6.1) ",
      "Applied Research (6.2)" = "Applied Research (6.2) ",
      "Adv. Technology Dev. (6.3)" = "Advanced Technology Development (6.3) ",
      "Adv. Component Dev. & Prototypes (6.4)" = "Advanced Component Development & Prototypes (6.4) ",
      "System Dev. & Demonstration (6.5)" = "System Development & Demonstration (6.5) ",
      "Operational Systems Dev. (6.7)" = "Operational Systems Development (6.7) "
    )
  
  DIIGcolors <- scale_fill_manual(values = colorset, name = NULL, labels = nameset)
  DIIGline <- scale_color_manual(values = colorset, name = NULL, labels= nameset)
  
  
  ################################################################################
  # 5. Dynamic plot title based on options selected
  ################################################################################
  # http://stackoverflow.com/questions/19957536/add-dynamic-subtitle-using-ggplot
  
  getTitle <- function(){
    
    # initialize character variables for title and subtitle
    plot.title <- character()
    plot.subtitle <- character()
    
    # assign title based on type of plot selected
    switch(input$BreakoutType,
           "All" = {
             plot.title <- "Contract Obligations by Major Area"
           },
           "Prd" = {
             plot.title <- "Products Obligations by Product Type"
           },
           "Svc" = {
             plot.title <- "Services Obligations by Service Type"
           },
           "RnD" = {
             plot.title <- "Research & Development Obligations by Phase"
           }
    )
    
    # create subtitle based on customers selected
    if(length(input$C) == 7){
      plot.subtitle <- "Overall DoD Customers"
    } else {
      plot.subtitle <- paste(input$C, collapse = ", ")
    }
    
    # add FY to subtitle
    plot.subtitle <- paste(plot.subtitle, "in Fiscal Years", input$Yr[1],
                           "to", input$Yr[2])
    
    # return ggtitle with dynamic title and subtitle included
    ggtitle(bquote(atop(bold(.(plot.title)),
                        atop(italic(.(plot.subtitle)), ""))))
    
  }
  
  ################################################################################
  # 6. Builds plot based on user input
  ################################################################################
  
  plotsettings <- function(){
    
    plotdata <- dataset()
    
    # calculate yearly totals ("sumBillion")
    plotdata <- ddply(plotdata, .(FY),
                      transform, sumBillion = sum(Billion))
    
    # determine y-axis position ("overpos") to display yearly totals
    plotdata$overpos <- plotdata$sumBillion + (0.03 * max(plotdata$sumBillion))
    
    # round yearly totals
    if(max(plotdata$sumBillion) >= 150){
      plotdata$sumBillion <- round(plotdata$sumBillion)
    } else if(max(plotdata$sumBillion >= 5)){
      plotdata$sumBillion <- round(signif(plotdata$sumBillion, 3), 1)
    } else {
      plotdata$sumBillion <- round(signif(plotdata$sumBillion, 3), 2)
    }
    
    
    # build the plot with a long string of ggplot commands
    p <- ggplot(data = plotdata,
                aes(x=FY, y=Billion, fill = Category)) +
      geom_bar(stat = 'identity', width = 0.7,
               size = 0.9) +
      
      # DIIGcolors defined in section 4
      DIIGcolors +
      
      
      # Custom background color / layout       
      theme(panel.border = element_blank(), 
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white", color="white"),
            #plot.background = element_rect(fill="#F9FBFF"), second choice 
            #plot.background = element_rect(fill="#EFF1F5"),
            #plot.background = element_rect(fill="#ECF2F5"),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_line(size=.1, color="lightgray"), 
            panel.grid.minor.y = element_line(size=.1, color="lightgray")) + 
      
      # Added title 
      getTitle() + 
      
      # ongraph and pos are for displaying the sub-category totals 
      # (white numbers) and are defined in section 3
      geom_text(aes(label = ongraph, y = pos), size = 4,
                color = 'white', fontface = 'bold', family = 'Arial') +
      
      # sumBillion and overpos are for displaying the yearly totals
      # (grey30 numbers) and are defined earlier in this section
      geom_text(aes(label = sumBillion, y = overpos), size = 5,
                color = '#554449', fontface = 'bold', family = "Arial") +
      theme(plot.title = element_text(
        family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(20,0,20,0))) +
      theme(axis.text.x = element_text(
        size = 15, family = "Arial", vjust=7, margin=margin(-15,0,0,0))) +
      theme(axis.text.y = element_text(
        size = 15, family = "Arial", color ="#554449", margin=margin(0,5,0,0))) +
      theme(axis.title.x = element_text(
        size = 16, face = "bold", color = "#554449", family = "Arial",
        margin=margin(15,0,0,60))) +
      theme(axis.title.y = element_text(
        size = 16, face = "bold", color = "#554449", family = "Arial",
        margin=margin(0,15,0,0))) +
      theme(axis.ticks.x = element_blank()) + 
      theme(axis.ticks.y = element_blank()) + 
      theme(legend.text = element_text(size = 15, family = "Arial", color ="#554449")) +
      theme(legend.position = 'bottom') +
      theme(legend.background = element_rect(fill = "white")) + 
      guides(fill=guide_legend(keywidth = 1.5, keyheight = 1.5)) +
      xlab("Fiscal Year") +
      ylab("Constant 2015 $ Billion") 
    
    
    
    # settings for the highlight box that appears when clicking the plot
    if(HL$on){
      return(p + annotate("rect", xmin = HL$xmin,
                          xmax = HL$xmax,
                          ymin = HL$ymin,
                          ymax = HL$ymax,
                          color = "#E2E264",
                          size = .7,
                          fill = "#E2E264",
                          alpha = 0.4)
      )    
    }
    

      
    # return the built plot    
    p      
  }
  

  
  ################################################################################
  # 7. Figures out what year/category the user clicked and highlights it
  ################################################################################
  
  # Initialize tracking variables for plot highlighting,
  # to store whether to draw a highlight and if so, where to draw it.
  
  # These are necessary because we want to draw a highlight on user click,
  # but drawing the highlight involves redrawing the plot, 
  # which resets input$.plot_click to NULL and discards info from it.  So these
  # values ("HL") are used to save that info before it gets discarded upon redraw.
  HL <- reactiveValues(
    on = FALSE, cat = NULL, year = NULL,
    xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL
  )
  
  observeEvent(input$.plot_click,{
    currentdata <- dataset()
    year <- clickyear()
    cat <- clickcat(year, currentdata)
    if(is.null(cat)){
      HL$on <- FALSE 
    } else {
      hlpos <- currentdata$pos[currentdata$Category == cat &
                                 currentdata$FY == year]
      hlbil <- currentdata$Billion[currentdata$Category == cat &
                                     currentdata$FY == year]
      # turn highlighting on
      HL$on <- TRUE
      
      # save values for the category and year where the user clicked
      HL$cat <- cat
      HL$year <- year
      
      # save values for the four sides of the highlight rectangle
      HL$xmin <- round(input$.plot_click$x) - 0.4
      HL$xmax <- round(input$.plot_click$x) + 0.4
      HL$ymin <- hlpos - (0.5 * hlbil)
      HL$ymax <- hlpos + (0.5 * hlbil)
    }
  })
  
  # turns the highlight off when user changes any plot setting
  observeEvent(input$C, {
    HL$on <- FALSE
  })
  observeEvent(input$BreakoutType, {
    HL$on <- FALSE
  })        
  observeEvent(input$Yr, {
    HL$on <- FALSE
  })
  
  # Returns year in which user clicked, based on the x-location of the click.
  # Called by the click observer function above.
  clickyear <- function (){
    year <- input$Yr[1] -1 + round(as.numeric(input$.plot_click$x))
    substring(as.character(year), 3, 4)
  }
  
  # Given year and currently loaded data, returns category in which user clicked.
  # Called by the click observer function above.
  
  # Works by treating the y-location of the user's click ("total") as an amount
  # (in billions of $), and subtracting each category from that amount until it
  # goes below zero, then returning the last category subtracted.
  # A while loop probably wasn't the fastest or most elegant way to do this.
  clickcat <- function(year, loaded){
    yearmoney <- loaded$Billion[loaded$FY == year]
    i <- 1
    total <- input$.plot_click$y
    if(total > sum(yearmoney)){
      return(NULL)
    }
    while(total - yearmoney[i] > 0 & i <= length(yearmoney)){
      total <- total - yearmoney[i]
      i <- i+1
    }
    loaded$Category[i]
  }
  
  ################################################################################
  # 8. Creates on-click detailed output (text and small graph)
  ################################################################################
  
  output$clickloc <-  renderText({
    clicktextsettings()
  }) 
  
  output$clickplot <- renderPlot({
    clickplotsettings()
  })
  
  # clickplotsettings called to produce the small plot upon user click 
  clickplotsettings <- function(){
    if(!HL$on){
      return(NULL)
    }
    clickplotdata <- dataset()
    
    # subset to show only the selected category
    clickplotdata <- filter(clickplotdata, Category == HL$cat)
    
    # set the y axis max for the small line graph
    ymaxdisp <- max(clickplotdata$Billion)
    ymaxdisp <- ymaxdisp * 1.05
    
    # ggplot command for the small plot
    p <- ggplot(data = clickplotdata, aes(x = FY, y = Billion)) +
      geom_line(aes(group = Category, color = Category), size = 2) +
      
      # DIIGline colors the line depending on category, defined in sec. 4
      DIIGline +
      theme(legend.position = 'none') +
      #theme(legend.text = element_text(size = 15, family = "Arial", color ="#554449")) + 
      theme(legend.text = element_blank()) + 
      theme(legend.key = element_rect(fill = "white")) + 
      theme(plot.title = element_text(
        family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(20,0,20,0))) +
      theme(axis.text.x = element_text(
        size = 15, family = "Arial", vjust=7, margin=margin(5,0,0,0))) +
      theme(axis.text.y = element_text(
        size = 15, family = "Arial", color ="#554449", margin=margin(0,5,0,0))) +
      theme(axis.title.x = element_text(
        size = 16, face = "bold", color = "#554449", family = "Arial",
        margin=margin(35,0,0,0))) +
      theme(plot.background = element_rect(fill="white"))+ 
      theme(axis.title.y = element_text(
        size = 16, face = "bold", color = "#554449", family = "Arial",
        margin=margin(0,15,0,0))) +
      xlab("Fiscal Year") + 
      
      # coll: got rid of ticks on the axis       
      theme(axis.ticks.x = element_blank()) + 
      theme(axis.ticks.y = element_blank()) + 
      
      theme(axis.title.x = element_text()) +
      theme(axis.title.y = element_text(
        color = "#554449", family = "Arial")) +
      
      # ymaxdisp defined above, the maximum of the y axis
      coord_cartesian(ylim = c(0, ymaxdisp)) +
      ylab("Constant 2015 $ B") +
      
      # set background theme
      theme(panel.border = element_blank(), 
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_line(size=.1, color="lightgray"), 
            panel.grid.minor.y = element_line(size=.1, color="lightgray")) 
    
    # return plot for output
    p
  }
  
  
  # clicktextsettings called to produce text information upon user click
  clicktextsettings <- function(){
    
    if(!HL$on){
      return(" ")
    }
    
    
    clicktextdata <- dataset()
    clickamount <- clicktextdata$Billion[clicktextdata$Category == HL$cat &
                                           clicktextdata$FY == HL$year]
    
    # build string for selected customer(s)
    cust <- character()
    if(length(input$C) == 7){
      cust <- "All DoD Customers"
    } else {
      cust <- paste(input$C, collapse = ", ")
    }
    
    # build string for change from previous year
    yearchange <- character()
    if(HL$year == '00'){
      yearchange <- "with no data from the previous year."
    } else {
      prevyear <- as.character(as.numeric(as.character(HL$year))- 1)
      if(nchar(prevyear) == 1){prevyear <- paste('0', prevyear, sep="")}
      prevamount <- clicktextdata$Billion[clicktextdata$Category == HL$cat &
                                            clicktextdata$FY == prevyear]
      if(clickamount > prevamount){
        yearchange <- paste("a ", round(100*(clickamount - prevamount) /
                                          prevamount, 2), "% increase from FY", prevyear,
                            ".", sep = "")
      } else {
        yearchange <- paste("a ", round(-100*(clickamount - prevamount) /
                                          prevamount, 2), "% decrease from FY", prevyear,
                            ".", sep = "")
      }
    }
    
    # return a combination of all strings, for output in the text box
    paste(cust, " obligations for ", HL$cat, " in FY", HL$year, ": ",
          "$", round(clickamount, 2), "B,\n", yearchange, sep = "")
    
  }
  
  
  ################################################################################
  # 10. Shows updated plot whenever the user changes a setting
  ################################################################################
  
  output$plot <- renderPlot({
    plotsettings()
  }) 
  
  output$CSVDownloadBtn <- downloadHandler(
    filename = paste('CSIS.Contract Obligations.', Sys.Date(),'.csv', sep=''),
    content = function(file) {
      writedata <- dataset()
      writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
      writedata <- select(writedata, FY, Category, Billion)
      write.csv(writedata, file)
    }
  )
  
  
  # this bracket ends the "server" function that started way back in section 1
}

################################################################################
# 11. Starts the app
################################################################################
shinyApp(ui= ui, server = server)