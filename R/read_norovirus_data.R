# Function to read data from an Excel file and process it
#' Read Norovirus Data from Excel File
#'
#' @param main_sheet_index The index of the sheet to read (default is 2)
#' @param dataset The dataset to be read in (default is 2)
#' @param ytit The title of the y axis based on the sheet/dataset read in (default is "ytit")
#' @return A data frame with norovirus data
#' @export
#' @importFrom readxl excel_sheets read_excel
#' @importFrom epitools as.week
read_norovirus_data <- function(dataset = 1, main_sheet_index = 2) {
  # Get the full path to the data file

  if (dataset == 1) {
     file_path <- system.file("data", "2017-2018-norovirus.xlsx", package = "fono")
  } else if (dataset == 2) {
     file_path <- system.file("data", "2015-2024-norovirus.xlsx", package = "fono")
  } else {
     print("No dataset was set.")
  }

  # Check if the file exists
  if (file_path == "") {
    stop("Data file not found in package")
  }

  # Read the Excel file using readxl
  sheets <- readxl::excel_sheets(file_path)
  data_list <- lapply(sheets, function(sheet) readxl::read_excel(file_path, sheet = sheet))


  # Set ytit based on main_sheet_index and dataset

  if (dataset == 1) {
     ytit <- switch(main_sheet_index,
                 "1" = "Positive Labs",
                 "2" = "ICD Code-A08.1 or A08.4",
                 "3" = "ICD Code-A08.1",
                 "Unknown" # Default case
     )
  }

  if (dataset == 2) {
     ytit <- switch(main_sheet_index,
                 "1" = "ICD Code-A08.1 or A08.4",
                 "2" = "Diarrhea and Vomiting",
                 "3" = "Positive Labs",
                 "Unknown" # Default case
     )
  }

  global_ytit <<- ytit # define a global ytit variable for plotting purposes

  my_data <- data_list[[main_sheet_index]]

  # Assign column names manually since the first row with data does not include headers
  names(my_data) <- c("epi_year_week", "count")

  # in the second dataset, the epi-weeks are not padded if <10, so fix that
  # here:

# Assuming 'epi_year_week' is in the format "YYYY-W" or "YYYY-WW"
my_data$epi_year_week_padded <- sprintf("%s-%02d",
                             substr(my_data$epi_year_week, 1, 4),  # Extract the year part
                             as.numeric(substr(my_data$epi_year_week, 6, 7)))  # Extract the week part correctly

# browser()  # Execution will stop here

  # Convert epi week format 'yyyy-ww' to Date class in R
  # This is the old way - didn't work for 2020 because of leap year
  # my_data$epi_week <- as.Date(paste0(my_data$epi_year_week_padded, "-1"), format = "%Y-%W-%u")

# browser()

   epiYear = as.integer(substr(my_data$epi_year_week_padded, 1, 4))
   epiWeek = as.integer(substr(my_data$epi_year_week_padded, 6, 7))

   my_data$epi_week <- MMWRweek::MMWRweek2Date(MMWRyear = epiYear, MMWRweek = epiWeek, MMWRday = 1)

  # michal's approach
  # my_data$epi_week <- as.numeric(epitools::as.date(my_data$epi_year_week_padded))

  # Sort data by date in increasing order
  my_data <- my_data[order(my_data$epi_year_week), ]

  return(my_data)
}

