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

### ---------- Check for data error ------------- ###

any(WHERE_data$Q7 == "ECOAPP-HUS-2008")
# should be False.


### ---------- SUBSET THE DATA ----------- ###

WHERE_data_KD <- WHERE_data[WHERE_data$Q8 == "KD",]

### --- ADD COUNTS ---------------###


TotalPapers <- tibble(
  year = c(1949:2018),
  count = c(
    5,
    7,
    4,
    4,
    0,
    4,
    7,
    9,
    6,
    6,
    8,
    9,
    11,
    9,
    7,
    9,
    18,
    13,
    17,
    70,
    53,
    57,
    45,
    55,
    58,
    46,
    37,
    38,
    58,
    31,
    30,
    31,
    29,
    37,
    49,
    47,
    41,
    93,
    77,
    47,
    37,
    43,
    48,
    55,
    66,
    72,
    73,
    62,
    95,
    138,
    141,
    124,
    111,
    139,
    104,
    123,
    180,
    167,
    159,
    176,
    200,
    175,
    221,
    218,
    212,
    255,
    261,
    260,
    241,
    227
  ),
  type = rep("Total", length(year))
)


FittingPapers <- tibble(
  year = c(1949:2018),
  count = c(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    0,
    2,
    1,
    2,
    0,
    1,
    1,
    5,
    5,
    0,
    17,
    17,
    8,
    2,
    4,
    4,
    4,
    5,
    5,
    4,
    6,
    19,
    29,
    25,
    16,
    15,
    21,
    15,
    32,
    36,
    27,
    27,
    31,
    30,
    37,
    33,
    39,
    53,
    81,
    88,
    93,
    64,
    56
  ),
  type = rep("Fitting", length(year))
)


## ----------- REMOVE UNNEEDED OBJECTS ------------- ##

remove(reviewer)
