################################################################################
# Vendor Size Charts Interactive App
# L.Lipsey for DIIG May 2016
################################################################################

require(shiny)
require(ggplot2)
require(plyr)
require(dplyr)
require(scales)

################################################################################
# Visual settings for user interface
################################################################################

# defines a list of customers for use in the customer selectInput.
# defining it seperately isn't necessary and only saves typing it twice in the
# selectInput command in the ui section
customers <- c("Air Force", "Army", "Navy", "MDA", "DLA",
               "Other DoD")

# same thing for list of platform portfolios
vendorsize <- c("Big Five", "Large", "Medium", "Small")

# same thing for list of platform portfolios
portfolios <- c("Aircraft and Drones", "Electronics and Communications",
                "Facilities and Construction", "Land Vehicles",
                "Missile and Space Systems", "Other Products",
                "Other R&D and Knowledge Based", "Other Services",
                "Ships & Submarines", "Weapons and Ammunition")

# here's the ui section - visual settings for the plot + widgets
ui <- fluidPage(
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
                 
                 # Settings for Vendor Size checkbox group
                 selectInput("VS","Vendor Size", vendorsize,
                             multiple = TRUE,
                             selectize = FALSE,
                             selected = vendorsize,
                             width = '100%',
                             size = 5),
                  
                  # br(), adds a blank line
                  br(),
                  
                  # Settings for Customer select
                  selectInput("Customer","Customer", customers,
                              multiple = TRUE,
                              selectize = FALSE,
                              selected = customers,
                              width = '100%',
                              size = 6),
                  br(),
                  
                  # Settings for Portfolio select
                  selectInput("Portfolio","Platform Portfolio",
                              portfolios,
                              multiple = TRUE,
                              selectize = FALSE,
                              selected = portfolios,
                              width = '100%',
                              size = 10),
                 
                 br(), 
                 downloadLink('CSVDownloadBtn', 
                              "Download Displayed Data (csv)") 
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
FullData <- read.csv("CleanedVendorSize.csv")

# rename MilitaryHealth to have a space
#levels(FullData$Customer)[5] <- "Military Health"
FullData$Customer[FullData$Customer == "MilitaryHealth"] <- "Other DoD" 

# make Big Five the first category (so it displays at the top of the legend)
FullData$VendorSize <- relevel(FullData$VendorSize, "Big Five")

# drop observations with NULL customer
FullData <- filter(FullData, Category != "NULL")


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
                             Portfolio %in% input$Portfolio &
                             Customer %in% input$Customer)
      
      # aggregate rows by summing Amount.  The only breakouts left will be the
      # ones in the .(  ) call - FY and VendorSize in this case
      shown <- ddply(shown, .(FY, Category), summarize, Amount = sum(Amount))
      
      
      # calculate percent of obligations for each VendorSize category
      shown <- ddply(shown, .(FY),
                     function(x){
                        x$Percent <- x$Amount / sum(x$Amount, na.rm = TRUE)
                        x
                        })
      
      # return the subsetted dataframe to whatever called dataset()
      shown

# end of dataset() function      
})

################################################################################
# Set colors  
################################################################################
colorset <- 
  c(
    #Set Category Colors 
    "Products" = "#CE884E", 
    "Services" = "#63c5b8", 
    "R&D" = "#628582")

#Set Category Colors 
#"Products" = "#554449", 
#"Services" = "#CE884E", 
#"R&D" = "#63c5b8")

DIIGcolors <- scale_color_manual(values = colorset, name = NULL)

################################################################################
# Build the plot for output
################################################################################

plotsettings <- reactive({
  p <- ggplot(data = dataset(),
              aes(x=FY, y=Percent, 
                  color=Category, group=Category, fill =Category)) +
    geom_line(size = 1.5) +
    expand_limits(y=0) + 
    
    #coll: Added title 
    ggtitle("Share of Contract Obligations by Area") + 
    theme(plot.title = element_text(
      family = "Arial", color = "#554449", size = 26, face="bold", margin=margin(20,0,30,0))) + 
        
    #coll: Custom background color/layout 
    theme(panel.border=element_blank(), 
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_line(size=.1, color="grey80"), 
          panel.grid.minor.y = element_line(size=.1, color="grey80")) + 
    
    scale_y_continuous(labels=percent) +
    
    DIIGcolors+
    
    theme(legend.text = element_text(size = 18, color="#554449")) +
    theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
    theme(legend.key = element_rect(fill="white")) +
    theme(legend.key.width = unit(3,"line")) +
    theme(axis.text.x = element_text(size = 14, color="#554449", margin=margin(-10,0,0,0))) +
    theme(axis.ticks.length = unit(.00, "cm")) +
    theme(axis.text.y = element_text(size = 14, color="#554449", margin=margin(0,5,0,0))) +
    theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
    theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
    xlab("Fiscal Year") +
    ylab("Share of Contract Obligations") + 
    
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
output$CSVDownloadBtn <- downloadHandler(
  filename = paste('CSIS.Contract Obligations by Area.', Sys.Date(),'.csv', sep=''),
  content = function(file) {
    writedata <- dataset()
    writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
    writedata$Percent <- writedata$Percent * 100
    writedata <- select(writedata, FY, Category, Amount, Percent)
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
