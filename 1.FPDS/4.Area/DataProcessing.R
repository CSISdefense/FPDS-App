################################################################################
# Data Processing for FPDS 4.Area
################################################################################


#----------------------------- General Data Processing ------------------------#
library(dplyr)
library(forcats)

# sets this file's directory as working directory if you are in Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

FullData <- read_csv("KitchenSink Query Results 2000-2016.csv")

names(FullData) <- c("FY", 
                     "Customer", 
                     "Category", 
                     "PS", 
                     "RnD", 
                     "Portfolio", 
                     "Classification2", 
                     "Classification",
                     "Contract.Type",
                     "VendorSize",
                     "Amount")

FullData$Amount <- suppressWarnings(as.numeric(as.character(FullData$Amount)))

########classify vendor
# vendorClassification <- c("Large" = "Large",
#                           "Large(Small Subsidiary)" = "Large",
#                           "Large: Big 5" = "Big Five",
#                           "Large: Big 5 (Small Subsidiary)" = "Big Five",
#                           "Large: Big 5 JV" = "Big Five",
#                           "Large: Big 5 JV (Small Subsidiary)" = "Big Five",
#                           "Large: Pre-Big 6" = "Large",
#                           "Medium <1B" = "Medium",
#                           "Medium >1B" = "Medium",
#                           "Medium >1B (Small Subsidiary)" = "Medium",
#                           "Small" = "Small",
#                           "Unlabeled" = "Small")

FullData$VendorSize[FullData$VendorSize == "Large"] <- "Large"
FullData$VendorSize[FullData$VendorSize == "Large(Small Subsidiary)"] <- "Large"
FullData$VendorSize[FullData$VendorSize == "Large: Big 5"] <- "Big Five"
FullData$VendorSize[FullData$VendorSize == "Large: Big 5 (Small Subsidiary)"] <- "Big Five"
FullData$VendorSize[FullData$VendorSize == "Large: Big 5 JV"] <- "Big Five"
FullData$VendorSize[FullData$VendorSize == "Large: Big 5 JV (Small Subsidiary)"] <- "Big Five"
FullData$VendorSize[FullData$VendorSize == "Large: Pre-Big 6"] <- "Large"
FullData$VendorSize[FullData$VendorSize == "Medium <1B"] <- "Medium"
FullData$VendorSize[FullData$VendorSize == "Medium >1B"] <- "Medium"
FullData$VendorSize[FullData$VendorSize == "Medium >1B (Small Subsidiary)"] <- "Medium"
FullData$VendorSize[FullData$VendorSize == "Small"] <- "Small"
#FullData$VendorSize[FullData$VendorSize == "Unlabeled"] <- "Small"
FullData$VendorSize[FullData$VendorSize == "Large: Big 6"] <- "Big Five"
FullData$VendorSize[FullData$VendorSize == "Large: Big 6 JV"] <- "Big Five"


# 
# FullData$VendorSize <- as.factor(
#   vendorClassification[as.character(FullData$VendorSize)])

##########classify competition 
FullData$Classification[FullData$Classification == ""] <- "Unlabeled"
FullData$Classification[FullData$Classification == "NULL"] <- "Unlabeled"
FullData$Classification[FullData$Classification == "Unlabeled: Blank Extent Competed"] <- "Unlabeled"
FullData$Classification[FullData$Classification == "Unlabeled: Blank Fair Opportunity"] <- "Unlabeled"
FullData$Classification[FullData$Classification == "Unlabeled: Competition; Zero Offers"] <- "Unlabeled"
FullData$Classification[FullData$Classification == "Unlabeled: No competition; multiple offers"] <- "Unlabeled"
FullData$Classification[FullData$Classification == "Unlabeled: No competition; multiple offers; Overrode blank Fair Opportunity)"] <- "Unlabeled"

FullData$Classification[FullData$Classification == "10-24 Offers"] <- "Effective Competition"
FullData$Classification[FullData$Classification == "100+ Offers"] <- "Effective Competition"
FullData$Classification[FullData$Classification == "25-99 Offers"] <- "Effective Competition"
FullData$Classification[FullData$Classification == "3-4 Offers"] <- "Effective Competition"
FullData$Classification[FullData$Classification == "5-9 Offers"] <- "Effective Competition"
FullData$Classification[FullData$Classification == "Two Offers"] <- "Effective Competition"

FullData$Classification[FullData$Classification == "No competition; Overrode blank Fair Opportunity)"] <- "No competition"

FullData$Classification[FullData$Classification == "One Offer"] <- "Competition with single offer"

###########classify Contract.Type

FullData$Contract.Type[FullData$Contract.Type == "Combination (two or more)"] <- "Combination"
FullData$Contract.Type[FullData$Contract.Type == "Cost No Fee"] <- "Cost Reimbursement"
FullData$Contract.Type[FullData$Contract.Type == "Cost Plus Award Fee"] <- "Cost Reimbursement"
FullData$Contract.Type[FullData$Contract.Type == "Cost Plus Fixed Fee"] <- "Cost Reimbursement"
FullData$Contract.Type[FullData$Contract.Type == "Cost Plus Incentive"] <- "Cost Reimbursement"
FullData$Contract.Type[FullData$Contract.Type == "Cost Sharing"] <- "Cost Reimbursement"
FullData$Contract.Type[FullData$Contract.Type == "Firm Fixed Price"] <- "Fixed Price"
FullData$Contract.Type[FullData$Contract.Type == "Fixed Price Award Fee"] <- "Fixed Price"
FullData$Contract.Type[FullData$Contract.Type == "Fixed Price Incentive"] <- "Fixed Price"
FullData$Contract.Type[FullData$Contract.Type == "Fixed Price Level of Effort"] <- "Fixed Price"
FullData$Contract.Type[FullData$Contract.Type == "Fixed Price Redetermination"] <- "Fixed Price"
FullData$Contract.Type[FullData$Contract.Type == "Fixed Price with Economic Price Adjustment"] <- "Fixed Price"
FullData$Contract.Type[FullData$Contract.Type == "Labor Hours"] <- "Time and Materials"
FullData$Contract.Type[FullData$Contract.Type == "Not Reported"] <- "Unlabeled"
FullData$Contract.Type[FullData$Contract.Type == "Order Dependent (IDV only)"] <- "Other" 
FullData$Contract.Type[FullData$Contract.Type == "Other (none of the above)"] <- "Other"
FullData$Contract.Type[FullData$Contract.Type == "Time and Materials"] <- "Time and Materials"
FullData$Contract.Type[FullData$Contract.Type == "NULL"] <- "Unlabeled"



FullData$Customer[FullData$Customer == "MilitaryHealth"] <- "Other DoD"


deflate <- 
  c("2000" = 0.707312744,
    "2001" = 0.726215832,
    "2002" = 0.73828541,
    "2003" = 0.75914093,
    "2004" = 0.779020234,
    "2005" = 0.805910543,
    "2006" = 0.833777068,
    "2007" = 0.855786297,
    "2008" = 0.885694001,
    "2009" = 0.887468939,
    "2010" = 0.901402201,
    "2011" = 0.922523962,
    "2012" = 0.940983316,
    "2013" = 0.953319134,
    "2014" = 0.967518637,
    "2015" = 0.981185659,
    "2016" = 1
  )

FullData$Amount <- round(FullData$Amount / deflate[as.character(FullData$FY)])





#----------------------------- App-specific Processing ------------------------#


FullData <- FullData %>%
  group_by(
    FY,
    VendorSize,
    Customer,
    Portfolio,
    Category, 
    Contract.Type, 
    Classification) %>%
  summarize(Amount = sum(Amount))


# drop nulls
# FullData <- FullData %>%
#   filter(Breakout != "NULL") %>%
#   filter(Category != "NULL") %>%
#   filter(Customer != "NULL")

write_csv(FullData, "FPDS_Area_data.csv")

