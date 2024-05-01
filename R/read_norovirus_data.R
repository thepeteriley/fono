# Function to read data from an Excel file and process it
#' Read Norovirus Data from Excel File
#'
#' @param main_sheet_index The index of the sheet to read (default is 2)
#' @return A data frame with norovirus data
#' @export
#' @importFrom readxl excel_sheets read_excel
read_norovirus_data <- function(main_sheet_index = 2) {
  # Get the full path to the data file
  file_path <- system.file("data", "2017-2018-norovirus.xlsx", package = "fono")

  # Check if the file exists
  if (file_path == "") {
    stop("Data file not found in package")
  }

  # Read the Excel file using readxl
  sheets <- readxl::excel_sheets(file_path)
  data_list <- lapply(sheets, function(sheet) readxl::read_excel(file_path, sheet = sheet))

  # Set ytit based on main_sheet_index
  if (main_sheet_index == 1) {
    ytit <- "Positive Labs"
  } else if (main_sheet_index == 2) {
    ytit <- "ICD Code-A08.1 or A08.4"
  } else if (main_sheet_index == 3) {
    ytit <- "ICD Code-A08.1"
  } else {
    ytit <- "Unknown"  # Default case if main_sheet_index is not 1, 2, or 3
  }

  data <- data_list[[main_sheet_index]]

  # Assign column names manually since the first row with data does not include headers
  names(data) <- c("epi_week", "count")

  # Convert epi week format 'yyyy-ww' to Date class in R
  data$epi_week <- as.Date(paste0(data$epi_week, "-1"), format = "%Y-%W-%u")

  # Sort data by date in increasing order
  data <- data[order(data$epi_week), ]

  return(data)
}

