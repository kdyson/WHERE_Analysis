### ------ DATA PROCESSING ---------- ###

## This file imports the raw data .csv and converts it into an analysis-ready
## table for creating percentages and figures for the WHERE paper. The data
## comes from the Data Collection WHERE only Excel workbook. Data was collected
## by two authors, Karen Dyson & Emad Dawwas in 2020.

## The full workflow for this project is as follows: data collection (Data
## Collection WHERE only Excel) --> compile all journal information and export
## as .csv (Combined Data WHERE.csv) --> process raw data into analysis-ready
## table (this file, WHERE_processing.R) --> analyze data and create figures for
## publication (WHERE_analysis.R)


### -------- INGEST DATA ---------------------- ###

WHERE_data <-
  read.csv(
    "Combined Data WHERE.csv",
    stringsAsFactors = F,
    na.strings = ""
  )

# Create column names that make sense ie no weird characters (these will become rownames)
colnames(WHERE_data)[1] <- "QID"
colnames(WHERE_data)[2] <- "Q_text"

### ------- MANIPULATE DATA -------------------- ###

## We need to create proper column and row names, and then transpose the data



#First, remove all of the blank rows & columns.

WHERE_data <- WHERE_data[ !is.na(WHERE_data$QID), !is.na(WHERE_data[ 18, ])]

  # the question is arbitrary, just need a row/question where every value should be filled. 

# create text to concatinate into a readible paper id
paper.IDs <- as.character(unlist(WHERE_data[WHERE_data$QID == 7, 3:ncol(WHERE_data)], use.names = F))
reviewer <- as.character(unlist(WHERE_data[WHERE_data$QID == 8, 3:ncol(WHERE_data)], use.names = F))
reviewer <- ifelse(is.na(reviewer), "KD", 
                   ifelse(reviewer == "EM-KD", "EMAD",
                   ifelse(reviewer == "KD-EM", "EMAD",
                          ifelse(reviewer == "KD & TF", "KD",
                                 reviewer
                                 )
                          )
                   )
                   )

paper.IDs <- paste0(paper.IDs, "-", reviewer)

# this cleans up the reviewer column also for future selection
  WHERE_data[WHERE_data$QID == 8, 3:ncol(WHERE_data)] <- reviewer 


colnames(WHERE_data)[3:ncol(WHERE_data)] <- paper.IDs
  

# Now create rownames that make sense (these will become colnames)

WHERE_data$QID <- paste0("Q", WHERE_data$QID)

rownames(WHERE_data) <- WHERE_data$QID

# transpose the data
WHERE_data <- as.data.frame(t(WHERE_data), stringsAsFactors = F)



### ---------- SUBSET THE DATA ----------- ###

WHERE_data_KD <- WHERE_data[WHERE_data$Q8 == "KD",]


## ----------- REMOVE UNNEEDED OBJECTS ------------- ##

remove(reviewer)
