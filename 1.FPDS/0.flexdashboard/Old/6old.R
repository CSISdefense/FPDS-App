################################################################################
# Vendor Size Charts Interactive App
# L.Lipsey for DIIG May 2016
################################################################################

require(shiny)
require(ggplot2)
require(plyr)
require(dplyr)
require(scales)
require(gridExtra)
require(grid)
require(Cairo)

################################################################################
# Visual settings for user interface
################################################################################

# defines a list of customers for use in the customer selectInput.
# defining it seperately isn't necessary and only saves typing it twice in the
# selectInput command in the ui section
customers <- c("Air Force", "Army", "Navy", "MDA", "DLA",
               "Military Health", "Other DoD")

# same thing for list of platform portfolios
vendorsize <- c("Big Five", "Large", "Medium", "Small")

# same thing for list of platform portfolios
portfolios <- c("Aircraft and Drones", "Electronics and Communications",
                "Facilities and Construction", "Land Vehicles",
                "Missile and Space Systems", "Other Products",
                "Other R&D and Knowledge Based", "Other Services",
                "Ships & Submarines", "Weapons and Ammunition")

# here's the ui section - visual settings for the plot + widgets
ui <- 
  
  
  fluidPage(
    fluidRow(
      
      # left column - column sizes should add up to 12, this one is 3 so
      # the other one will be 9
      column(3, align = 'center',
             
             br(), 
             
             # year slider
             sliderInput('Yr', "Year Range:",
                         min = 2000, max = 2015,
                         value = c(2000,2015),
                         step = 1, width = '100%', sep = ""),
             
             # Settings for Category select
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
             
             selectInput("Portfolio","Platform Portfolio",
                         portfolios,
                         multiple = TRUE,
                         selectize = FALSE,
                         selected = portfolios,
                         width = '100%',
                         size = 10),
             
             # br(), adds a blank line
             br(),
             
             #downloadLink('CSVDownloadBtn', 
                #          "Download Displayed Data (csv)", class = NULL)
             
             downloadLink('FullDownloadBtn',
                            "Download Full Data (csv)")
      ),
      
      
      # left column - column sizes should add up to 12, this one is 9 so
      # the other one will be 3 
      column(9, align = "center",
             plotOutput("plot")
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
  FullData <- read.csv("CleanedVendorSize2.csv")
  
  # rename MilitaryHealth to have a space
  levels(FullData$Customer)[5] <- "Military Health"
  
  # make Big Five the first category (so it displays at the top of the legend)
  FullData$VendorSize <- relevel(FullData$VendorSize, "Big Five")
  
  # Rescale total obligations variable (Amount) to units of $Billion
  # Coll: look back at breakouts interactive to see similar code 
  FullData$Amount <- FullData$Amount / 1000000000
  
  # drop observations with NULL customer
  FullData <- filter(FullData, Customer != "NULL")
  FullData <- filter(FullData, Customer != "Other DoD")
  FullData <- filter(FullData, Customer != "DLA")
  FullData <- filter(FullData, Customer != "MDA")
  FullData <- filter(FullData, Customer != "Military Health")
  
  
  # save FY as 2 digits instead of 4, for better visual scale
  FullData$FY <- factor(substring(as.character(FullData$FY), 3, 4))
  
  
  ################################################################################
  # Subset data based on user input
  ################################################################################
  
  dataset <- reactive({
    
    ## subset by year, based on year slider ##
    # findInterval is a confusing (but supposedly faster-running) way to do this
    # that I found on google.  Probably a normal conditional test would be fine.
    shown <- filter(FullData,
                    findInterval((as.numeric(as.character(FY)))+2000,
                                 c(input$Yr[1], input$Yr[2]+1)) == 1L)
    
    ## subset data based on which categories the user selected ##
    
    # the selectInput widget holds the selected choices as a vector of
    # strings. This code checks whether the each observation is in the
    # selected categories, and discards it if isn't in all three.  The %in%
    # operator is a nice way to avoid typing lots of conditional tests all
    # strung together 
    shown <- filter(shown, VendorSize %in% input$VS &
                      Category %in% input$Cat &
                      Portfolio %in% input$Portfolio)
    
    # aggregate rows by summing Amount.  The only breakouts left will be the
    # ones in the .(  ) call - FY and VendorSize in this case
    shown <- ddply(shown, .(FY, Customer), summarize, Amount = sum(Amount))
    
    # return the subsetted dataframe to whatever called dataset()
    shown
    
    # end of dataset() function      
  })
  
  ################################################################################
  # Set colors  
  ################################################################################
  colorset <- 
    c(
      #Set Customer Colors 
      "Air Force" = "#63c5b8", 
      "Army" = "#36605a", 
      "Navy" = "#AD4545", 
      "MDA" = "#008e9d", 
      "DLA" = "#36605a", 
      "Military Health" = "#AD4545",
      "Other DoD" = "#7C3772")
  
  DIIGcolors <- scale_color_manual(values = colorset, name = NULL)
  
  ################################################################################
  # Build the plot for output
  ################################################################################
  
  plotsettings <- reactive({
    p <- ggplot(dataset(),
                aes(x=FY,
                    group=Customer, fill = Customer)) +
      geom_bar(aes(weight=Amount), width=.7) +
      facet_wrap(~ Customer, nrow = 1) + 
      
      theme(strip.text.x = element_text(family = "Arial", size = 12, color = "#554449")) + 
      theme(strip.background = element_rect(color = "gray95", fill=c("white"
      ))) + 
      
      #coll: Added title 
      ggtitle("Contract Obligations by Service") + 
      theme(plot.title = element_text(
        family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(20,0,30,0))) + 
      
      #coll: Custom background color/layout 
      theme(panel.border=element_blank(), 
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_line(size=.1, color="grey80"), 
            panel.grid.minor.y = element_line(size=.1, color="grey80")) + 
      
      scale_fill_manual(values=c("#63c5b8",
                                 "#7FA67A",
                                 "#536082")) + 
      
      theme(legend.position="none") + 
      theme(legend.text = element_text(size = 18, color="#554449")) +
      theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
      theme(legend.key = element_rect(fill="white")) +
      theme(legend.key.width = unit(3,"line")) +
      theme(axis.text.x = element_text(size = 08, color="#554449", margin=margin(-15,0,0,0))) +
      theme(axis.ticks.length = unit(.00, "cm")) +
      theme(axis.text.y = element_text(size = 14, color="#554449", margin=margin(0,5,0,0))) +
      theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
      theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
      xlab("Fiscal Year") +
      ylab("Constant 2015 $ Billion") +

      
     scale_x_discrete(labels = c("00", " ", "02" ," ", "04", " ", "06", " ", "08", " ", "10", 
                                  " ", "12", " ", "14", " ")) + 
      grid.newpage() 
    footnote <- "Source: FPDS; CSIS analysis"
    g <- arrangeGrob(p, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12, col = "#554449")))
    grid.draw(g) +
    
    p     
  })
  
  
  ################################################################################
  # Run download buttons
  ################################################################################
  
  # run csv download button
  #output$CSVDownloadBtn <- downloadHandler(
   # filename = paste('DoD contract shares ', Sys.Date(),'.csv', sep=''),
    #content = function(file) {
     # writedata <- dataset()
    #  writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
     # writedata$Percent <- writedata$Percent * 100
     # writedata <- select(writedata, FY, Customer, Amount, Percent)
    #  write.csv(writedata, file)
  #  }
  #)
  
  output$CSVDownloadBtn <- downloadHandler(
    #filename = paste('Vendor Size ', Sys.Date(),'.csv', sep=''),
    filename = paste('CSIS.Contract Obligations by Customer.', Sys.Date(),'.csv', sep=''),
    content = function(file) {
      writedata <- dataset()
      writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
      writedata$Percent <- writedata$Percent * 100
      writedata <- select(writedata, FY, Customer, Amount)
      write.csv(writedata, file)
    }
  )
  
  # run full data download button
  output$FullDownloadBtn <- downloadHandler(
    filename = paste('CSIS.Contract Obligations by Customer.', Sys.Date(),'.csv', sep=''),
    content = function(file) {
      writedata <- FullData
      writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
      writedata <- select(writedata, FY, VendorSize, Customer, Category,
                          Portfolio, Amount)
      write.csv(writedata, file)
    }
  )
  
  ################################################################################
  # Output the built plot and start the app
  ################################################################################
  
  
  output$plot <- renderPlot({
    plotsettings()
  }, height = 600) 
  

}

# starts the app
shinyApp(ui= ui, server = server)
