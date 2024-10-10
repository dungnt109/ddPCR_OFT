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

extract_value <- function(string){
	result <- gsub("[a-z]", "", string)
	result <- gsub("\\\\", "", result)
	result <- gsub("\\{", "", result)
	result <- gsub("\\}", "", result)
	result <- gsub(" ", "", result)
	return(result)
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
writeDataAtCell(params$sid, "A", 1)
writeDataAtCell(params$mid, "A", 2)

row <- 7 

writeValueRow7(ifelse(is.na(dx.baseline), "in-plate", "preset"), "A")
writeValueRow7(round(dx.baseline, digits=3), "B")
writeValueRow7(ifelse(runType == "absolute", absolute.ptv.formatted, oft.ptv.formatted), "C")
writeValueRow7(ifelse(runType == "absolute", absolute.ntv.formatted, oft.ntv.formatted), "D")
writeValueRow7(oft.call, "E")
writeValueRow7(ifelse(runType == "absolute", absolute.oft.formatted, relative.oft.formatted ), "F")
writeValueRow7(callForReporting, "G")
writeValueRow7("", "H")
writeValueRow7(ifelse(runmode == "silence", "Algorithm", ifelse(is_manual_threshold, "Manual", "Algorithm")), "I")

writeValueRow7("J7", "J")
writeValueRow7("K7", "K")
writeValueRow7("L7", "L")
writeValueRow7("M7", "M")
writeValueRow7("N7", "N")
writeValueRow7(extract_value(min.dx.marker.con.perwell.status), "O")
writeValueRow7("P7", "P")
writeValueRow7("Q7", "Q")
writeValueRow7("R7", "R")
writeValueRow7("S7", "S")
writeValueRow7(extract_value(max.h2o.marker.con.perwell.status), "T")
writeValueRow7("U7", "U")
writeValueRow7("V7", "V")
writeValueRow7("W7", "W")
writeValueRow7("X7", "X")
writeValueRow7("", "Y")
writeValueRow7("Z7", "Z")

writeValueRow7("AA7", "AA")
writeValueRow7("AB7", "AB")

writeValueRow7(extract_value(min.hl60.gus.con.perwell.status), "AC")
writeValueRow7("AD7", "AD")
writeValueRow7("AE7", "AE")
writeValueRow7(extract_value(max.h2o.gus.con.perwell.status), "AF")
writeValueRow7("AG7", "AG")
writeValueRow7("AH7", "AH")
writeValueRow7("AI7", "AI")
writeValueRow7("AJ7", "AJ")
writeValueRow7("AK7", "AK")
writeValueRow7(extract_value(min.fu.gus.conc.perwell.status), "AL")
writeValueRow7("", "AM")
writeValueRow7(extract_value(final.qc.call), "AN")
writeValueRow7("date", "AO")
writeValueRow7( pipeline_version, "AP")
writeValueRow7(runmode, "AQ")



# Merge header cells


# Save workbook
saveWorkbook(wb, paste(folder, "ddPCR OFT Run Records.xlsx", sep=separator), overwrite = TRUE)
