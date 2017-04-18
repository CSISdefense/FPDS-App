################################################################################
# 5. Platform
################################################################################

require(shiny)
require(ggplot2)
require(dplyr)
require(scales)
require(gridExtra)
require(grid)
require(Cairo)
library(forcats)

################################################################################
# Visual settings for user interface
################################################################################

category <- c("Products", 
              "Services", 
              "R&D")

vendorsize <- c("Big Five", 
                "Large", 
                "Medium", 
                "Small")

customer <- c("Army",
              "Navy", 
              "Air Force", 
              "MDA", 
              "DLA",
              "Other DoD")

portfolio <- c("Aircraft and Drones", 
               "Electronics and Communications",
               "Facilities and Construction", 
               "Land Vehicles",
               "Missile and Space Systems",
               "Ships & Submarines", 
               "Weapons and Ammunition", 
               "Other Products",
               "Other R&D and Knowledge Based", 
               "Other Services")

contract.type <- c("Combination", 
                   "Cost Reimbursement", 
                   "Fixed Price",
                   "Time and Materials", 
                   "Other",
                   "Unlabeled")

classification <- c("Competition with single offer", 
                    "Effective Competition", 
                    "No competition", 
                    "Unlabeled")

# here's the ui section - visual settings for the plot + widgets
ui <- 
  
  
  fluidPage(
    
    ####CSS Import of Google Font "Open Sans" for body  
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Open+Sans');
                    
                    body {
                    font-family: 'Open Sans',  sans-serif;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #554449;
                    }
                    
                    ")),
    tags$head(
      tags$style(HTML("body{background-color: #fcfcfc;}"))),
    tags$div(HTML("<div class='fusion-secondary-header'>
                <div class='fusion-row'>
                <div class='fusion-alignleft'><div class='fusion-contact-info'><center style=' padding:20px;'><a href='http://csis.org/program/international-security-program' target='_blank'><img class='logo' src='https://defense360.csis.org/wp-content/uploads/2015/08/ISP_new.png' width='40%'></a></center><a href='mailto:'></a></div></div>
                </div>
                </div>")),
    tags$style(HTML(".fusion-secondary-header {border-bottom: 3px solid #6F828F}")),
    br(), 
    ####Copy below to change slider color     
    tags$style(HTML(".irs-bar {background: #63c5b8}")),
    tags$style(HTML(".irs-bar {border-top: 1px #63c5b8}")),
    tags$style(HTML(".irs-bar {border-bottom: 1px #63c5b8}")),
    tags$style(HTML(".irs-single, .irs-to, .irs-from {background: #628582}")),
    #tags$style(HTML(".irs-slider {background: black}")),
    #  tags$style(HTML(".irs-grid-pol {display: absolute;}")),
    tags$style(HTML(".irs-max {color: #554449}")),
    tags$style(HTML(".irs-min {color: #554449}")),
    tags$style(HTML(".irs-bar-edge {border: 1px #63c5b8}")),
    tags$style(HTML(".irs-bar-edge {border-color: 1px #63c5b8}")),
    tags$style(HTML(".irs-bar-edge {border-color: 1px #63c5b8}")),
    ####         
    
    fluidRow(
      
      # left column - column sizes should add up to 12, this one is 3 so
      # the other one will be 9
      column(2, align = 'center',
             
             br(), 
             
             # year slider
             sliderInput('Yr', "Year Range:",
                         min = 2000, max = 2016,
                         value = c(2000,2016),
                         ticks = FALSE,
                         step = 1, width = '100%', sep = ""),
             
             selectInput("Cat", "Category",
                         category,
                         multiple = TRUE,
                         selectize = FALSE,
                         selected = category,
                         width = '100%'),
             
             selectInput("VS","Vendor Size",
                         vendorsize,
                         multiple = TRUE,
                         selectize = FALSE,
                         selected = vendorsize,
                         width = '100%'),
             
             selectInput("Customer", "Customer",
                         customer,
                         multiple = TRUE,
                         selectize = FALSE,
                         selected = customer,
                         width = '100%'),
             
             # selectInput("Portfolio","Platform Portfolio",
             #             portfolio,
             #             multiple = TRUE,
             #             selectize = FALSE,
             #             selected = portfolio,
             #             width = '100%'),
             
             selectInput("Contract","Contract Type",
                         contract.type,
                         multiple = TRUE,
                         selectize = FALSE,
                         selected = contract.type,
                         width = '100%'),
             
             selectInput("Classification","Competition",
                         classification,
                         multiple = TRUE,
                         selectize = FALSE,
                         selected = classification,
                         width = '100%'), 
             
             radioButtons("Chart", "Chart",
                          c("Line", "Bar"),
                          inline = TRUE,
                          selected = "Line"),
             
             downloadLink('CSVDownloadBtn', 
                              "Download Displayed Data (csv)")
             
      ),
      
      
      # left column - column sizes should add up to 12, this one is 9 so
      # the other one will be 3 
      column(10, align = "center",
             div(
                style = "position:relative",
                plotOutput("plot", 
                  hover = hoverOpts(id = "plot_hover", delay = 30)),
              uiOutput("hover_info")
              )
      )
    )
    
    # end of ui section
  )


# server function starts

server <- function(input, output, session){

################################################################################
# Read in and clean up data
################################################################################      

# read in data            
FullData <- read.csv("FPDS_Platform_data.csv")

# rename MilitaryHealth to have a space
#levels(FullData$Customer)[5] <- "Military Health"
FullData$Customer[FullData$Customer == "MilitaryHealth"] <- "Other DoD"  

# make Big Five the first category (so it displays at the top of the legend)
FullData$VendorSize <- relevel(FullData$VendorSize, "Big Five")

# Rescale total obligations variable (Amount) to units of $Billion
# Coll: look back at breakouts interactive to see similar code 
FullData$Amount <- FullData$Amount / 1000000000

# drop observations with NULL customer
FullData <- filter(FullData, Portfolio != "NULL" & !is.null(Portfolio))
FullData$Portfolio <- fct_drop(FullData$Portfolio)


# save FY as 2 digits instead of 4, for better visual scale
# FullData$FY <- factor(substring(as.character(FullData$FY), 3, 4))

################################################################################
# Subset data based on user input
################################################################################

dataset <- reactive({
    
    # subset by year, based on year slider ##
    
    # input$Yr[1] is the user-selected minimum year
    # input$Yr[2] is the user-selected maximum year
    # as.numeric(levels(FY))[FY] is just FY, converted from a factor to
    # a numeric variable
    shown <- filter(FullData, FY >= input$Yr[1] & FY <= input$Yr[2])
      
      ## subset data based on which categories the user selected ##
      
      # the selectInput widget holds the selected choices as a vector of
      # strings. This code checks whether the each observation is in the
      # selected categories, and discards it if isn't in all three.  The %in%
      # operator is a nice way to avoid typing lots of conditional tests all
      # strung together 
      shown <- filter(shown, 
                      # Portfolio %in% input$Portfolio & 
                        Category %in% input$Cat &
                        VendorSize %in% input$VS &
                        Customer %in% input$Customer &
                        Contract.Type %in% input$Contract & 
                        Classification %in% input$Classification)
      
      # aggregate amount by (Fiscal Year x Portfolio)
    shown <- shown %>%
      group_by(FY, Portfolio) %>%
      summarise(Amount = sum(Amount))
    
    shown <- shown %>%
      group_by(FY) %>%
      mutate(Percent = Amount / sum(Amount)) 
    
    # reorder by most recent year
     shown$Portfolio <- fct_reorder(
      shown$Portfolio,
      (shown$Percent * (shown$FY == input$Yr[2])) ,
      mean,
      na.rm = TRUE,
      .desc = TRUE)
      
      # return the subsetted dataframe to whatever called dataset()
      return(shown)

# end of dataset() function      
})

################################################################################
# Set colors  
################################################################################
colorset <- 
  # c(
    # #Set Category Colors 
    # "Aircraft and Drones" = "#554449", 
    # "Electronics and Communications" = "#7C3772", 
    # "Facilities and Construction" = "#36605a", 
    # "Missile and Space Systems" = "#AD4545", 
    # "Other Products" = "#C75F5F", 
    # "Weapons and Ammunition" = "#599a9e", 
    # "Other R&D and Knowledge Based" = "#CE884E", 
    # "Ships & Submarines" = "#63c5b8",
    # "Land Vehicles" = "#C74F4F", 
    # "Other Services" = "#629582")
    c(
      #Set Category Colors 
      "Aircraft and Drones" = "#554449", 
      "Electronics and Communications" = "#7C3772", 
      "Facilities and Construction" = "#36605a", 
      "Missile and Space Systems" = "#AD4545", 
      "Other Products" = "#008e9d", 
      "Weapons and Ammunition" = "#599a9e", 
      "Other R&D and Knowledge Based" = "#CE884E", 
      "Ships & Submarines" = "#63c5b8",
      "Land Vehicles" = "#C74F4F", 
      "Other Services" = "#628582")
    
nameset <- 
  c( 
    #Set Category Colors 
    "Aircraft and Drones" = "Aircraft and Drones", 
    "Electronics and Communications" = "Electronics and Communications", 
    "Facilities and Construction" = "Facilities and Construction", 
    "Missile and Space Systems" = "Missile and Space Systems", 
    "Other Products" = "Other Products", 
    "Weapons and Ammunition" = "Weapons and Ammunition", 
    "Other R&D and Knowledge Based" = "Other R&D and Knowledge Based", 
    "Ships & Submarines" = "Ships and Submarines",
    "Land Vehicles" = "Land Vehicles", 
    "Other Services" = "Other Services")

DIIGcolors <- scale_color_manual(values = colorset, name = NULL, labels = nameset)
DIIGfill <- scale_fill_manual(values = colorset, name = NULL, labels = nameset)


################################################################################
# Build the plot for output
################################################################################

plotsettings <- reactive({
  
  # # calculate breaks for x axis
  # xbreaks <- rev(seq(
  #   from = input$Yr[2],
  #    to = input$Yr[1],
  #    by = -1 * ceiling((input$Yr[2] - input$Yr[1]) / 7)))
  # 
  # xlabels <- as.character(xbreaks)
  
  
  # ggplot call
  # p <- ggplot(data = dataset(),
  #             aes(x=FY,
  #                 group=Portfolio, fill = Portfolio)) +
  #   geom_bar(aes(weight=Amount), width=.7) +
    
    p <- ggplot(data = dataset(),
                aes(x=FY, y=Percent, 
                    color=Portfolio, 
                    group=Portfolio, fill=Portfolio)) +
      geom_line(size = 1.5) +
# 
    facet_wrap(~ Portfolio, nrow = 2, scales="free_x", drop = FALSE) +
    
    # theme(strip.text.x = element_text(family = "Arial", size = 12, color = "#554449")) +
    # theme(strip.background = element_rect(color = "gray95", fill=c("white"
    # ))) +

    theme(panel.spacing.y = unit(1, "lines")) +

    #coll: Added title 
    ggtitle("Contract Obligations by Platform Portfolio") + 
    # theme(plot.title = element_text(
    #   family = "Arial", color = "#554449", size = 26, face="bold",
    #   margin=margin(20,0,30,0), hjust = 0.5)) +

    #coll: Custom background color/layout 
    # theme(panel.border=element_blank(),
    #       panel.background = element_blank(),
    #       panel.grid.major.x = element_blank(),
    #       panel.grid.minor.x = element_blank(),
    #       panel.grid.major.y = element_line(size=.1, color="grey80"),
    #       panel.grid.minor.y = element_line(size=.1, color="grey80")) +
    
    #scale_x_continuous() +
    # scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 2),
    #                    labels = function(x) {substring(as.character(x), 3, 4)}) +
    
    DIIGcolors +  
    # diigtheme1:::diiggraph()+ 
    
    # theme(legend.position="none") +
    # theme(legend.text = element_text(size = 18, color="#554449")) +
    # theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
    # theme(legend.key = element_rect(fill="white")) +
    # theme(legend.key.width = unit(3,"line")) +
    # theme(axis.text.x = element_text(size = 10, color="#554449", margin=margin(-5,0,0,0))) +
    # theme(axis.ticks.length = unit(.00, "cm")) +
    # theme(axis.text.y = element_text(size = 14, color="#554449", margin=margin(0,5,0,0))) +
    # theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
    # theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
    # 
    # xlab("Fiscal Year") +
    # ylab("Share") +
    # labs( caption = "Source: FPDS; CSIS analysis", family="Open Sans" )
    ##############################################################################facet below 
    theme(plot.title = element_text(
      family = "Arial", color = "#554449", size = 26, face="bold",
      margin=margin(20,0,30,0), hjust = 0.5)) +
      
      theme(panel.border = element_blank(),
            panel.background = element_rect(fill = "#FCFCFC"),
            plot.background = element_rect(fill = "#FCFCFC", color="#FCFCFC"),
            #plot.background = element_rect(fill="#F9FBFF"), second choice
            #plot.background = element_rect(fill="#EFF1F5"),
            #plot.background = element_rect(fill="#ECF2F5"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(size=.1, color="lightgray"),
            panel.grid.minor.y = element_line(size=.1, color="lightgray")) +
      
      scale_y_continuous(labels=percent) +
      scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 2),
                         labels = function(x) {substring(as.character(x), 3, 4)}) +
      
      
      # theme(strip.text.x = element_text(family = "Arial", size = 12, color = "#554449")) +
      # theme(strip.background = element_rect(color = "gray95", fill=c("white"
      # ))) +
      
      theme(legend.position="none") +
      theme(strip.background = element_rect(color = "gray95", fill=c("#fcfcfc"))) +
      theme(strip.text.x = element_text(family = "Open Sans",
                                        size = rel(1.2),
                                        color = "#554449")) +
      theme(legend.text = element_text(size = 18, color="#554449")) +
      theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
      theme(legend.key = element_rect(fill="#fcfcfc")) +
      theme(legend.key.width = unit(3,"line")) +
      theme(axis.text.x = element_text(size = 11, color="#554449", margin=margin(-5,0,0,0))) +
      theme(axis.ticks.length = unit(.00, "cm")) +
      theme(axis.text.y = element_text(size = 11, color="#554449", margin=margin(0,5,0,0))) +
      theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
      theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
      
      xlab("Fiscal Year") +
      ylab("Share of Contract Obligations") +
      theme(plot.caption = element_text(
        size = 12, face = "bold", color = "#554449", family = "Open Sans"
      )) +
      labs(caption = "Source: FPDS; CSIS analysis", size = 30, family= "Open Sans") 
    ##############################################################################facet above
    
  p     
})


plotsettings2 <- reactive({
  
  # # calculate breaks for x axis
  # xbreaks <- rev(seq(
  #   from = input$Yr[2],
  #    to = input$Yr[1],
  #    by = -1 * ceiling((input$Yr[2] - input$Yr[1]) / 7)))
  # 
  # xlabels <- as.character(xbreaks)
  
  
  # ggplot call
  p <- ggplot(data = dataset(),
              aes(x=FY,
                  group=Portfolio, fill = Portfolio)) +
    geom_bar(aes(weight=Amount), width=.7) +
      # 
    facet_wrap(~ Portfolio, nrow = 2, scales="free_x", drop = FALSE) +
    
    # theme(strip.text.x = element_text(family = "Arial", size = 12, color = "#554449")) +
    # theme(strip.background = element_rect(color = "gray95", fill=c("white"
    # ))) +
    # 
    # theme(panel.spacing.y = unit(1, "lines")) +
    
    #coll: Added title 
    ggtitle("Contract Obligations by Platform Portfolio") + 
    # theme(plot.title = element_text(
    #   family = "Arial", color = "#554449", size = 26, face="bold",
    #   margin=margin(20,0,30,0), hjust = 0.5)) +
    # 
    # #coll: Custom background color/layout 
    # theme(panel.border=element_blank(),
    #       panel.background = element_blank(),
    #       panel.grid.major.x = element_blank(),
    #       panel.grid.minor.x = element_blank(),
    #       panel.grid.major.y = element_line(size=.1, color="grey80"),
    #       panel.grid.minor.y = element_line(size=.1, color="grey80")) +
    # 
    # #scale_x_continuous() +
    # scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 2),
    #                    labels = function(x) {substring(as.character(x), 3, 4)}) +
    
    DIIGfill +
    
    # diigtheme1:::diiggraph()+ 
    
    # theme(legend.position="none") +
    # theme(legend.text = element_text(size = 18, color="#554449")) +
    # theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
    # theme(legend.key = element_rect(fill="white")) +
    # theme(legend.key.width = unit(3,"line")) +
    # theme(axis.text.x = element_text(size = 10, color="#554449", margin=margin(-5,0,0,0))) +
    # theme(axis.ticks.length = unit(.00, "cm")) +
    # theme(axis.text.y = element_text(size = 14, color="#554449", margin=margin(0,5,0,0))) +
    # theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
    # theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
    # 
    # xlab("Fiscal Year") +
    # ylab("Constant 2015 $ Billion") +
    # labs( caption = "Source: FPDS; CSIS analysis", family="Open Sans" )
  ##############################################################################facet below 
  theme(plot.title = element_text(
    family = "Arial", color = "#554449", size = 26, face="bold",
    margin=margin(20,0,30,0), hjust = 0.5)) +
    
    theme(panel.border = element_blank(),
          panel.background = element_rect(fill = "#FCFCFC"),
          plot.background = element_rect(fill = "#FCFCFC", color="#FCFCFC"),
          #plot.background = element_rect(fill="#F9FBFF"), second choice
          #plot.background = element_rect(fill="#EFF1F5"),
          #plot.background = element_rect(fill="#ECF2F5"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(size=.1, color="lightgray"),
          panel.grid.minor.y = element_line(size=.1, color="lightgray")) +
    
    #scale_x_continuous() +
    scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 2),
                       labels = function(x) {substring(as.character(x), 3, 4)}) +
    
    
    # theme(strip.text.x = element_text(family = "Arial", size = 12, color = "#554449")) +
    # theme(strip.background = element_rect(color = "gray95", fill=c("white"
    # ))) +
    
    theme(legend.position="none") +
    theme(strip.background = element_rect(color = "gray95", fill=c("#fcfcfc"))) +
    theme(strip.text.x = element_text(family = "Open Sans",
                                      size = rel(1.2),
                                      color = "#554449")) +
    theme(legend.text = element_text(size = 18, color="#554449")) +
    theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
    theme(legend.key = element_rect(fill="#fcfcfc")) +
    theme(legend.key.width = unit(3,"line")) +
    theme(axis.text.x = element_text(size = 11, color="#554449", margin=margin(-5,0,0,0))) +
    theme(axis.ticks.length = unit(.00, "cm")) +
    theme(axis.text.y = element_text(size = 11, color="#554449", margin=margin(0,5,0,0))) +
    theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
    theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
    
    xlab("Fiscal Year") +
    ylab("Constant 2016 $ Billion") +
    theme(plot.caption = element_text(
      size = 12, face = "bold", color = "#554449", family = "Open Sans"
    )) +
    labs(caption = "Source: FPDS; CSIS analysis", size = 30, family= "Open Sans") 
  ##############################################################################facet above
  
  p     
})

################################################################################
# Run download buttons
################################################################################

# run csv download button
output$CSVDownloadBtn <- downloadHandler(
  filename = paste('CSIS.Contract Obligations by Portfolio.', Sys.Date(),'.csv', sep=''),
  content = function(file) {
    writedata <- dataset()
    writedata$Percent <- writedata$Percent * 100
    write.csv(writedata, file)
  }
)

# run full data download button
# output$FullDownloadBtn <- downloadHandler(
#   filename = paste('CSIS.Contract Obligations by Porfolio.', Sys.Date(),'.csv', sep=''),
#   content = function(file) {
#     writedata <- FullData
#     writedata <- select(writedata, FY, VendorSize, Customer, Category,
#                         Portfolio, Amount)
#     write.csv(writedata, file)
#   }
# )


##############################################################################
# Give details when user hovers the plot
# See https://gitlab.com/snippets/16220
##############################################################################

output$hover_info <- renderUI({
  hover <- input$plot_hover
  if(is.null(hover)) return(NULL)


  point <- nearPoints(dataset(), hover, xvar = "FY", yvar = "Amount",
  threshold = 200,
    maxpoints = 1, addDist = TRUE)
  year <- round(hover$x)
  if(nrow(point) == 0) return(NULL)
  if(year < input$Yr[1] | year > input$Yr[2]) return(NULL)
  if(hover$y < 0) return(NULL)
  
  hov_amount <- dataset() %>%
    filter(FY == year & Portfolio == point$Portfolio) %>%
    .$Amount %>%
    unlist
  
  hov_percent <- dataset() %>%
    filter(FY == year & Portfolio == point$Portfolio) %>%
    .$Percent %>%
    unlist
  
  if(hover$y > hov_amount) return(NULL)
  
  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / 
    (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / 
    (hover$domain$top - hover$domain$bottom)
  
  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * 
    (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * 
    (hover$range$bottom - hover$range$top)
  
  # Use HTML/CSS to change style of tooltip panel here
  style <- paste0(
    "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
   wellPanel(
    style = style,
    p(HTML(paste0("<b> Fiscal Year: </b>", round(hover$x) , "<br/>",
                  "<b> Portfolio: </b>", point$Portfolio, "<br/>",
                  "<b> Share: </b>", round(hov_percent*100,1), "%<br/>",
                  "<b> Amount: </b> $",
                    round(hov_amount,2),  " Billion")))
   )

})



################################################################################
# Output the built plot and start the app
################################################################################


output$plot <- renderPlot({
  (switch(input$Chart, 
          `Line` = plotsettings(), 
          `Bar` = plotsettings2()))
}, height = 700) 



}

# starts the app
shinyApp(ui= ui, server = server)
