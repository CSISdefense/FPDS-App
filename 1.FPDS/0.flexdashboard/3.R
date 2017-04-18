################################################################################
# Vendor Size Charts Interactive App
# L.Lipsey for DIIG May 2016
################################################################################

require(shiny)
require(ggplot2)
require(plyr)
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
                  br(),
                  
                  # Settings for Vendor Size checkbox group
                  selectInput("VS","Vendor Size", vendorsize,
                              multiple = TRUE,
                              selectize = FALSE,
                              selected = vendorsize,
                              width = '100%',
                              size = 5),
                  br(),
                  
                  # Settings for Portfolio checkbox group
                  selectInput("Portfolio","Platform Portfolio",
                              portfolios,
                              multiple = TRUE,
                              selectize = FALSE,
                              selected = portfolios,
                              width = '100%',
                              size = 10),
                
                  br(), 
                  downloadLink('CSVDownloadBtn', 
                                 "Download Displayed Data (csv)" 
                  )),
                  #br(), 
                  #downloadButton('FullDownloadBtn', 
                   #              "Download Full Data (csv)")
            #),
            
            #coll: I took out the title 
            # Settings for title + plot frame
            column(9,
                  h2("",
                             align = "center"),
                  plotOutput("plot")
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
      
      # subset by year, based on year slider
      shown <- filter(FullData,
                    findInterval((as.numeric(as.character(FY)))+2000,
                                 c(input$Yr[1], input$Yr[2]+1)) == 1L)
      
      # subset by categories selected
      shown <- filter(shown, Portfolio %in% input$Portfolio & 
                             Category %in% input$Cat &
                             VendorSize %in% input$VS)
      
      # sum the amounts for each combination of (FY x Customer)
      shown <- ddply(shown, .(FY, Customer), summarize, Amount = sum(Amount))
      
      # coll: calculate percent of obligations for each Customer category
      shown <- ddply(shown, .(FY),
                     function(x){
                        x$Percent <- x$Amount / sum(x$Amount, na.rm = TRUE)
                        x
                        })
      
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
    filename = paste('CSIS.Contract Obligations by Customer.', Sys.Date(),'.csv', sep=''),
    content = function(file) {
        writedata <- dataset()
        writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
        writedata$Percent <- writedata$Percent * 100
        writedata <- select(writedata, FY, Customer, Amount, Percent)
        write.csv(writedata, file)
    }
)



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