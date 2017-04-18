################################################################################
#gcoll
################################################################################

require(shiny)
require(ggplot2)
require(dplyr)
require(scales)
require(Cairo)
require(grid)
require(gridExtra)

################################################################################
# Visual settings for user interface
################################################################################

Area <- c("Products", "R&D", "Services")

#Fiscal.Year <- levels(data$Fiscal.Year)
#Fiscal.Year <- c(Fiscal.Year)

Customer <- c("Air Force", "Army", "Navy", "MDA", "DLA",
               "Other DoD")

#Classification <- levels(data$Classification)
#Classification <- c(Classification)

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
          
          # left column - column sizes should add up to 12, this one is 3 so
          # the other one will be 9
          column(3, align = 'center',
                  br(),
                  
                  # year slider
                  sliderInput('Yr', "Year Range",
                              min = 2000, max = 2015,
                              value = c(2000,2015),
                              ticks = FALSE, 
                              step = 1, width = '100%', sep = ""),
                  

                  
                  # Settings for Customer select
                  selectInput("Area","Area", Area,
                             multiple = TRUE,
                             selectize = FALSE,
                             selected = Area,
                             width = '100%'),
                 
                 # Settings for Category select
                 selectInput("Customer", "Customer", 
                             Customer, 
                             multiple = TRUE,
                             selectize = FALSE,
                             selected = Customer,
                             width = '100%',
                             size = 6)),
                 
                 # downloadLink('CSVDownloadBtn',
                 #                "Download Displayed Data (csv)", class = NULL)),

                 #br(),
                 #br(), 
                 #downloadButton('FullDownloadBtn',  
                #                "Download Full Data (csv)")
            #),
            
          # left column - column sizes should add up to 12, this one is 9 so
          # the other one will be 3 
          column(9, align = "center",
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
FullData <- read.csv("data_competition.csv")

# rename MilitaryHealth to have a space
#levels(FullData$Customer)[5] <- "Military Health"
FullData$Customer[FullData$Customer == "MilitaryHealth"] <- "Other DoD"  

FullData <- filter(FullData, Area != "Unlabeled")

#FullData <- filter(FullData, Area != "NA")

################################################################################
# Subset data based on user input
################################################################################

dataset <- reactive({
    
    ## subset by year, based on year slider ##
    # findInterval is a confusing (but supposedly faster-running) way to do this
    # that I found on google.  Probably a normal conditional test would be fine.
      shown <- filter(FullData, Fiscal.Year >= input$Yr[1] & Fiscal.Year <= input$Yr[2])
      
      ## subset data based on which categories the user selected ##
      
      # the selectInput widget holds the selected choices as a vector of
      # strings. This code checks whether the each observation is in the
      # selected categories, and discards it if isn't in all three.  The %in%
      # operator is a nice way to avoid typing lots of conditional tests all
      # strung together 
      shown <- filter(shown, Customer %in% input$Customer & 
                        Area %in% input$Area)
      
      # # aggregate rows by summing Amount.  The only breakouts left will be the
      # # ones in the .(  ) call - FY and VendorSize in this case
      # shown <- ddply(shown, .(Fiscal.Year, Classification), summarize, Amount = sum(Amount))
      # 
      # 
      # # calculate percent of obligations for each VendorSize category
      # shown <- ddply(shown, .(Fiscal.Year),
      #                function(x){
      #                   x$Percent <- x$Amount / sum(x$Amount, na.rm = TRUE)
      #                   x
      #                   })
      
      shown <- shown %>%
        group_by(Fiscal.Year, Classification) %>%
        summarise(Amount = sum(Amount))
      
      shown <- shown %>%
        group_by(Fiscal.Year) %>%
        mutate(Percent = Amount / sum(Amount)) 
      
      shown <- shown %>%
        filter(!is.na(Classification))
      
      # return the subsetted dataframe to whatever called dataset()
      shown

# end of dataset() function      
})


################################################################################
# Build the plot for output
################################################################################

plotsettings <- reactive({
      p <- ggplot(data = dataset(),
                  aes(x=Fiscal.Year, y=Percent, 
                      color=Classification, 
                      group=Classification, fill=Classification)) +
            geom_line(size = 1.5) +
            #facet_wrap(~ Area) + 
        
        theme(strip.text.x = element_text(family = "Arial", size = 10, color = "#554449")) + 
        theme(strip.background = element_rect(color = "gray95", fill=c("white"
        ))) + 
        
            #coll: Added title 
            ggtitle("Share of Defense Contract Obligations by Level of Competition") + 
            theme(plot.title = element_text(
              family = "Arial", color = "#554449", size = 26, face="bold",
              margin=margin(20,0,30,0), hjust = 0.5)) + 
        
            #coll: Custom background color/layout 
            theme(panel.border=element_blank(), 
                  panel.background = element_blank(),
                  panel.grid.major.x = element_blank(), 
                  panel.grid.minor.x = element_blank(), 
                  panel.grid.major.y = element_line(size=.1, color="grey80"), 
                  panel.grid.minor.y = element_line(size=.1, color="grey80")) + 
        
            scale_y_continuous(labels=percent) +
        scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 1),
                           labels = function(x) {substring(as.character(x), 3, 4)}) +
        scale_color_manual(values = c("#628582",
                                      "#84B564",
                                      "#C74F4F",
                                      "#554449"),
                           #scale_color_manual(values = c("#C74F4F",
                           #                              "#5F597C",
                           #                              "#599a9e",
                           #                              "#63c5b8"),
                               labels = c("Competition with single offer",
                                          "Effective Competition",
                                          "No Competition",
                                          "Unlabeled"), 
                               name = "") +
            theme(legend.text = element_text(size = 18, color="#554449")) +
            theme(legend.position = "bottom") + 
            theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
            theme(legend.key = element_rect(fill="white")) +
            theme(legend.key.width = unit(3,"line")) +
            theme(axis.text.x = element_text(size = 14, color="#554449", margin=margin(-10,0,0,0))) +
            theme(axis.ticks.length = unit(.00, "cm")) +
            theme(axis.text.y = element_text(size = 14, color="#554449", margin=margin(0,5,0,0))) +
            theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
            theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
            xlab("Fiscal Year") +
            ylab("Share of Contract Obligations") 

# return the built-up plot object to whatever called plotsettings() 
# currently the renderPlot() function below is the only thing that calls it
      
       
      #   grid.newpage() 
      # footnote <- "Source: FPDS; CSIS analysis"
      # g <- arrangeGrob(p, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 16, col = "#554449")))
      # grid.draw(g) +
      #   
      #   
p          
})


################################################################################
# Run download buttons
################################################################################

# run displayed data download button
#output$CSVDownloadBtn <- downloadHandler(
#    filename = paste('DoD contract shares ', Sys.Date(),'.csv', sep=''),
#    content = function(file) {
#        writedata <- dataset()
#        writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
#        writedata$Percent <- writedata$Percent * 100
#        writedata <- select(writedata, FY, VendorSize, Amount, Percent)
#        write.csv(writedata, file)
#    }
#)

# run full data download button
#output$FullDownloadBtn <- downloadHandler(
#    filename = paste('Budget Breakout Full ', Sys.Date(),'.csv', sep=''),
#    content = function(file) {
#        writedata <- FullData
#        writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
#        writedata <- select(writedata, FY, VendorSize, Customer, Category,
#                            Portfolio, Amount)
#        write.csv(writedata, file)
  #}
#)

# runs plot download button 
#output$PlotDownloadBtn <- downloadHandler(
#    filename = paste('Vendor Size Plot ', Sys.Date(),'.png', sep=''),
#    content = function(file) {
#        ggsave(file, plot = plotsettings(), device = "png", width = 10)
#    }
#)

################################################################################
# Output the built plot and start the app
################################################################################


output$plot <- renderPlot({
      plotsettings()
}, height = 600) 

#output$CSVDownloadBtn <- downloadHandler(
#  #filename = paste('CSIS.Contract Obligations by Vendor Size.', Sys.Date(),'.csv', sep=''),
#  filename = paste('Vendor Size ', Sys.Date(),'.csv', sep=''),
#  content = function(file) {
#    writedata <- dataset()
#    writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
#    writedata$Percent <- writedata$Percent * 100
#    writedata <- select(writedata, FY, VendorSize, Amount, Percent)
#    write.csv(writedata, file)
#  }
#)

# run full data download button
#output$FullDownloadBtn <- downloadHandler(
#    filename = paste('CSIS.Contract Obligations by Vendor Size.', Sys.Date(),'.csv', sep=''),
#    content = function(file) {
#        writedata <- FullData
#        writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
#        writedata <- select(writedata, FY, VendorSize, Customer, Category,
#                            Portfolio, Amount)
#        write.csv(writedata, file)
#}
#)

# run displayed data download button
#output$CSVDownloadBtn <- downloadHandler(
#    filename = paste('DoD contract shares ', Sys.Date(),'.csv', sep=''),
#    content = function(file) {
#        writedata <- dataset()
#        writedata$FY <- as.numeric(as.character(writedata$FY)) + 2000
#        writedata$Percent <- writedata$Percent * 100
#        writedata <- select(writedata, FY, VendorSize, Amount, Percent)
#        write.csv(writedata, file)
#    }
#)

##############################################################################
# Give details when user hovers the plot
# See https://gitlab.com/snippets/16220
##############################################################################

output$hover_info <- renderUI({
  hover <- input$plot_hover


  point <- nearPoints(dataset(), hover, xvar = "Fiscal.Year", yvar = "Percent",
                      threshold = (150 / (input$Yr[2] - input$Yr[1])) + 10,
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
    p(HTML(paste0("<b> Fiscal Year: </b>", point$Fiscal.Year , "<br/>",
                  "<b> Classification: </b>", point$Classification, "<br/>",
                  "<b> Share: </b>", round(point$Percent*100,1), "%<br/>",
                  "<b> Amount: </b> $",
                  round(point$Amount/1000000000,2),  " Billion")))
  )
})
 
# end of the server function
}




# starts the app
shinyApp(ui= ui, server = server)