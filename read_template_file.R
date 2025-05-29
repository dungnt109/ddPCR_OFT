read_template_file <- function(file_path){

    if (!file.exists(file_path)) {
      stop("Error: File not found at path: ", file_path)
    }

    # 2. Read the raw data using read.delim
    #    - header = FALSE: Treat the first line (1, 2, 3...) as data initially.
    #    - sep = "\t": Specify tab as the delimiter.
    #    - fill = TRUE: Important if some rows might have trailing empty cells represented by fewer tabs.
    #    - stringsAsFactors = FALSE: Keep cell contents as character strings.
    #    - quote = "": Prevents potential issues if cell values contain quotes.
    raw_data <- tryCatch({
      read.delim(file_path, sep = "\t", header = FALSE, stringsAsFactors = FALSE, fill = TRUE, quote = "")
    }, error = function(e) {
      stop("Error reading file. Check format and path. Original error: ", e$message)
    })

    # 3. Data Validation (Basic Checks)
    if (nrow(raw_data) < 2 || ncol(raw_data) < 2) {
      stop("Error: File does not seem to contain enough rows or columns after reading.")
    }

    # 4. Extract Column Headers
    #    The actual column headers are in the first row of the read data.
    #    Skip the very first element (V1) of the first row, which corresponds to the empty top-left corner or the row header column title.
    col_headers <- as.character(raw_data[1, -1])

    # Remove any potential leading/trailing whitespace from headers
    col_headers <- trimws(col_headers)
    # Filter out any empty headers, although based on the example, all 1-12 should be present
    valid_col_indices <- which(col_headers != "NA" & !is.na(col_headers))
    col_headers <- col_headers[valid_col_indices]

    # Adjust valid column indices to match the columns in the data part (add 1 because we skipped the first column)
    data_col_indices <- valid_col_indices + 1


    # 5. Extract Row Headers
    #    The row headers (A, B, C...) are in the first column (V1), starting from the second row.
    row_headers <- raw_data[-1, 1]
    # Remove any potential leading/trailing whitespace
    row_headers <- trimws(row_headers)
    valid_row_indices <- which(row_headers != "" & !is.na(row_headers))
    row_headers <- row_headers[valid_row_indices]

    # Adjust valid row indices to match the rows in the data part (add 1 because we skipped the header row)
    data_row_indices <- valid_row_indices + 1



    # 6. Extract the actual plate data
    #    Select the valid rows (excluding the header row) and valid columns (excluding the row header column).
    plate_data <- raw_data[data_row_indices, data_col_indices, drop = FALSE] # Use drop=FALSE to keep it as a data frame even if only one row/col

    # 7. Assign Row and Column Names to the data frame
    #    This makes accessing cells by name (e.g., plate_data["A", "1"]) possible and clear.
    rownames(plate_data) <- row_headers
    colnames(plate_data) <- col_headers

    # Iterate over the actual row names extracted
    for (row_name in rownames(plate_data)) {
      # Iterate over the actual column names extracted
      for (col_name in colnames(plate_data)) {

        var_name <- paste0("cell_", row_name, "_", col_name)

        cell_value <- trimws(plate_data[row_name, col_name])

        #plate_data[row_name, col_name] = paste0(row_name, col_name)

    	 if (grepl("EMPTY", plate_data[row_name, col_name], fixed=TRUE)){
    
    	 	plate_data[row_name, col_name] = ""

    	 } else if(grepl("Dx_OFT", plate_data[row_name, col_name], fixed=TRUE)){
    	 	plate_data[row_name, col_name] = "\\textcolor{blue}{Dx}"
    	 } else if(grepl("NTC_OFT", plate_data[row_name, col_name], fixed=TRUE)){
            plate_data[row_name, col_name] = "\\textcolor{blue}{NTC}"
         } else if(grepl("FU_OFT", plate_data[row_name, col_name], fixed=TRUE)){
            plate_data[row_name, col_name] = "\\textcolor{blue}{FU}"
         } else if(grepl("HL60_OFT", plate_data[row_name, col_name], fixed=TRUE)){
            plate_data[row_name, col_name] = "\\textcolor{blue}{HL60}"
         } else if(grepl("Dx_GUSB", plate_data[row_name, col_name], fixed=TRUE)){
            plate_data[row_name, col_name] = "\\textcolor{darkbrown}{Dx}"
         } else if(grepl("NTC_GUSB", plate_data[row_name, col_name], fixed=TRUE)){
            plate_data[row_name, col_name] = "\\textcolor{darkbrown}{NTC}"
         } else if(grepl("FU_GUSB", plate_data[row_name, col_name], fixed=TRUE)){
            plate_data[row_name, col_name] = "\\textcolor{darkbrown}{FU}"
         } else if(grepl("HL60_GUSB", plate_data[row_name, col_name], fixed=TRUE)){
            plate_data[row_name, col_name] = "\\textcolor{darkbrown}{HL60}"
         }



      }
    }

    return (plate_data) 

}


