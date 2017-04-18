################################################################################
# Vendor Size Charts Interactive App
# L.Lipsey for DIIG May 2016
################################################################################

require(shiny)
require(ggplot2)
require(dplyr)
require(scales)
require(grid)
require(Cairo)

################################################################################
# Visual settings for user interface
################################################################################

vendorsize <- c("Big Five", "Large", "Medium", "Small")

portfolios <- c("Aircraft and Drones", "Electronics and Communications",
                "Facilities and Construction", "Land Vehicles",
                "Missile and Space Systems", "Other Products",
                "Other R&D and Knowledge Based", "Other Services",
                "Ships & Submarines", "Weapons and Ammunition")


ui <- fluidPage(
  
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
            column(3, align = 'center',
                  br(),
                  
                  # year slider
                  sliderInput('Yr', "Year Range:",
                              min = 2000, max = 2015,
                              value = c(2000, 2015),
                              step = 1, width = '100%', sep = ""),
                  
                  # Settings for Category input
                  selectInput("Cat","Category",
                                     c("Products", "Services", "R&D"),
                              multiple = TRUE,
                              selectize = FALSE,
                              selected = c("Products", "Services", "R&D"),
                              width = '100%',
                              size = 3),
                  
                  # Settings for Vendor Size checkbox group
                  selectInput("VS","Vendor Size", vendorsize,
                              multiple = TRUE,
                              selectize = FALSE,
                              selected = vendorsize,
                              width = '100%',
                              size = 5),
                  
                  # Settings for Portfolio checkbox group
                  selectInput("Portfolio","Platform Portfolio",
                              portfolios,
                              multiple = TRUE,
                              selectize = FALSE,
                              selected = portfolios,
                              width = '100%',
                              size = 10),
                
                  downloadLink('CSVDownloadBtn', 
                                 "Download Displayed Data (csv)" 
                  )),
                  #br(), 
                  #downloadButton('FullDownloadBtn', 
                   #              "Download Full Data (csv)")
            #),
            
            #coll: I took out the title 
            # Settings for title + plot frame
            column(9, align = "center",
              div(
                style = "position:relative",
                plotOutput("plot", 
                  hover = hoverOpts(id = "plot_hover", delay = 30)),
              uiOutput("hover_info")
              )
            )  
      )
)


################################################################################
# Server function with underlying code to read and subset data,
# adjust the plot settings, and generate the plot
################################################################################


server <- function(input, output){

################################################################################
# Read in and clean up data
################################################################################      

# read in data            
FullData <- read.csv("CleanedVendorSize.csv")

# rename MilitaryHealth to have a space
#levels(FullData$Customer)[5] <- "Military Health"
FullData$Customer[FullData$Customer == "MilitaryHealth"] <- "Other DoD"  

# drop observations with NULL customer
FullData <- filter(FullData, Customer != "NULL")

# save FY as 2 digits instead of 4, for better visual scale
FullData$FY <- factor(substring(as.character(FullData$FY), 3, 4))

################################################################################
# Subset data based on user input
################################################################################

dataset <- reactive({
      
    # subset by year, based on year slider ##
    
    # input$Yr[1] is the user-selected minimum year
    # input$Yr[2] is the user-selected maximum year
    # as.numeric(levels(FY))[FY] is just FY, converted from a factor to
    # a numeric variable
    shown <- filter(FullData, as.numeric(levels(FY))[FY] + 2000 >= input$Yr[1] &
                              as.numeric(levels(FY))[FY] + 2000 <= input$Yr[2])
      
      # subset by categories selected
      shown <- filter(shown, Portfolio %in% input$Portfolio & 
                             Category %in% input$Cat &
                             VendorSize %in% input$VS)
      
    ## calculate percent of obligations for each Customer category
    
    # aggregate amount by (Fiscal Year x Customer)
    shown <- shown %>%
      group_by(FY, Customer) %>%
      summarise(Amount = sum(Amount))
        
    # create 'Percent' variable as the percent of each FY's obligations that
    # went to that Customer
    shown <- shown %>%
      group_by(FY) %>%
      mutate(Percent = Amount / sum(Amount)) 
      
    # discard any row without Customer data
    shown <- shown %>%
      filter(!is.na(Customer))
      
      
  shown
})

################################################################################
# Set colors  
################################################################################
colorset <- 
  c(
    #Set Customer Colors 
    "Air Force" = "#63c5b8", 
    "Army" = "#CE884E", 
    "Navy" = "#554449", 
    "MDA" = "#008e9d", 
    "DLA" = "#36605a", 
    #"Military Health" = "#AD4545",
    "Other DoD" = "#AD4545")

DIIGcolors <- scale_color_manual(values = colorset, name = NULL)

################################################################################
# Build the plot for output
################################################################################

plotsettings <- reactive({
      p <- ggplot(data = dataset(),
                  aes(x=FY, y=Percent, 
                      color=Customer, group=Customer, fill =Customer)) +
            geom_line(size = 1.5) +
        
            #coll: Added title 
            ggtitle("Share of Contract Obligations by Customer") + 
            theme(plot.title = element_text(
              family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(0,0,30,0))) + 
        
        #coll: Custom background color/layout 
        theme(panel.border=element_blank(), 
              panel.background = element_blank(),
              panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank(), 
              panel.grid.major.y = element_line(size=.1, color="grey80"), 
              panel.grid.minor.y = element_line(size=.1, color="grey80")) + 
        
        scale_y_continuous(labels=percent) +
        coord_cartesian(ylim = c(0, 1.05*max(dataset()$Percent))) +
      
        DIIGcolors+
                        
            theme(legend.text = element_text(size = 18, color="#554449")) +
            theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
            theme(legend.key = element_rect(fill="white")) +
            theme(legend.key.width = unit(3,"line")) +
            theme(axis.text.x = element_text(size = 14, color="#554449", margin=margin(0,0,0,0))) +
            theme(axis.ticks.length = unit(.00, "cm")) +
            theme(axis.text.y = element_text(size = 14, color="#554449", margin=margin(0,5,0,0))) +
            theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
            theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
            xlab("Fiscal Year") +
            ylab("Share of Contract Obligations") +
            labs( caption = "Source: FPDS; CSIS analysis" )
      
     p     
})


################################################################################
# Run download buttons
################################################################################

# run csv download button
output$CSVDownloadBtn <- downloadHandler(
    filename = paste('CSIS.Contract Obligations by Customer.', Sys.Date(),'.csv', sep=''),
    content = function(file) {
        writedata <- dataset()
        writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
        writedata$Percent <- writedata$Percent * 100
        writedata <- select(writedata, FY, Customer, Amount, Percent)
        write.csv(writedata, file)
    }
)


##############################################################################
# Give details when user hovers the plot
# See https://gitlab.com/snippets/16220
##############################################################################

output$hover_info <- renderUI({
  hover <- input$plot_hover
  point <- nearPoints(dataset(), hover, xvar = "FY", yvar = "Percent",
    threshold = 20,
    maxpoints = 1, addDist = TRUE)
  if(nrow(point) == 0) return(NULL)
  
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
    p(HTML(paste0("<b> Fiscal Year: </b>",
                  as.numeric(as.character(point$FY)) + 2000, "<br/>",
                    "<b> Customer: </b>", point$Customer, "<br/>",
                    "<b> Share: </b>", round(point$Percent*100,1), "%<br/>",
                    "<b> Amount: </b> $", 
                      round(point$Amount/1000000000,2),  " Billion")))
  )
})



################################################################################
# Output the built plot and start the app
################################################################################


output$plot <- renderPlot({
      plotsettings()
}, height = 600) 

output$CSVDownloadBtn <- downloadHandler(
  filename = paste('CSIS.Contract Obligations by Customer.', Sys.Date(),'.csv', sep=''),
  content = function(file) {
    writedata <- dataset()
    writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
    writedata$Percent <- writedata$Percent * 100
    writedata <- select(writedata, FY, Customer, Amount, Percent)
    write.csv(writedata, file)
  }
)

# run full data download button
output$FullDownloadBtn <- downloadHandler(
  filename = paste('Customer Full ', Sys.Date(),'.csv', sep=''),
  content = function(file) {
    writedata <- FullData
    writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
    writedata <- select(writedata, FY, VendorSize, Customer, Category,
                        Portfolio, Amount)
    write.csv(writedata, file)
  }
)

}

# starts the app
shinyApp(ui= ui, server = server)