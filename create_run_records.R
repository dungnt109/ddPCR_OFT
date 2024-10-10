# Load openxlsx package
library(openxlsx)

col_letter_to_index <- function(letter) {
  # Match the column letter(s) to their numeric index
  return(sum((match(rev(strsplit(toupper(letter), "")[[1]]), LETTERS) * 26^(0:(nchar(letter)-1)))))
}

writeDataAtCell <- function(value, col, row){
	writeData(wb, "ddPCR OFT Run Records", value, startCol = col_letter_to_index(col), startRow = row)
}

writeValueRow7 <- function(value, col){
	writeDataAtCell(value, col, 7)
}

existing_file_path <- "run_records_template.xlsx"

wb <- loadWorkbook(existing_file_path)

data <- data.frame(
  Col1 = c(1, 2, 3),
  Col2 = c(4, 5, 6),
  Col3 = c(7, 8, 9),
  Col4 = c(10, 11, 12)
)

# Write data
writeDataAtCell("A1", "A", 1)
writeDataAtCell("A2", "A", 2)

row <- 7 

writeValueRow7("A7", "A")
writeValueRow7("B7", "B")
writeValueRow7("C7", "C")
writeValueRow7("D7", "D")
writeValueRow7("E7", "E")
writeValueRow7("F7", "F")
writeValueRow7("G7", "G")
writeValueRow7("H7", "H")
writeValueRow7("I7", "I")
writeValueRow7("J7", "J")
writeValueRow7("K7", "K")
writeValueRow7("L7", "L")
writeValueRow7("M7", "M")
writeValueRow7("N7", "N")
writeValueRow7("O7", "O")
writeValueRow7("P7", "P")
writeValueRow7("Q7", "Q")
writeValueRow7("R7", "R")
writeValueRow7("S7", "S")
writeValueRow7("T7", "T")
writeValueRow7("U7", "U")
writeValueRow7("V7", "V")
writeValueRow7("W7", "W")
writeValueRow7("X7", "X")
writeValueRow7("Y7", "Y")
writeValueRow7("Z7", "Z")

writeValueRow7("AA7", "AA")
writeValueRow7("AB7", "AB")
writeValueRow7("AC7", "AC")
writeValueRow7("AD7", "AD")
writeValueRow7("AE7", "AE")
writeValueRow7("AF7", "AF")
writeValueRow7("AG7", "AG")
writeValueRow7("AH7", "AH")
writeValueRow7("AI7", "AI")
writeValueRow7("AJ7", "AJ")
writeValueRow7("AK7", "AK")
writeValueRow7("AL7", "AL")
writeValueRow7("AM7", "AM")
writeValueRow7("AN7", "AN")
writeValueRow7("AO7", "AO")
writeValueRow7("AP7", "AP")
writeValueRow7("AQ7", "AQ")

print(sample_sheet_file)

writeDataAtCell(sample_sheet_file, "A", 3)

# Merge header cells


# Save workbook
saveWorkbook(wb, paste(folder, "ddPCR OFT Run Records.xlsx", sep=separator), overwrite = TRUE)
