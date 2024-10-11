# Load openxlsx package
library(openxlsx)

sheet_name = paste(params$sid, "_", params$mid)

col_letter_to_index <- function(letter) {
  # Match the column letter(s) to their numeric index
  return(sum((match(rev(strsplit(toupper(letter), "")[[1]]), LETTERS) * 26^(0:(nchar(letter)-1)))))
}

writeDataAtCell <- function(value, col, row){
	writeData(wb, sheet_name, value, startCol = col_letter_to_index(col), startRow = row)
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

capitalize_first <- function(s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

existing_file_path <- "run_records_template.xlsx"

wb <- loadWorkbook(existing_file_path)



renameWorksheet(wb, sheet = 1, newName = sheet_name)

# Write data
writeDataAtCell(paste("Sample ", params$sid), "A", 1)
writeDataAtCell(paste("OFT: ", params$mid), "A", 2)

row <- 7 

writeValueRow7(ifelse(is.na(params$dx.baseline), "In-plate", "Preset"), "A")
writeValueRow7(round(dx.baseline, digits=3), "B")
writeValueRow7(ifelse(runType == "absolute", absolute.ptv.formatted, oft.ptv.formatted), "C")
writeValueRow7(ifelse(runType == "absolute", absolute.ntv.formatted, oft.ntv.formatted), "D")
writeValueRow7(oft.call, "E")
writeValueRow7(ifelse(runType == "absolute", absolute.oft.formatted, relative.oft.formatted ), "F")
writeValueRow7(callForReporting, "G")
writeValueRow7("", "H")
writeValueRow7(ifelse(runmode == "silence", "Algorithm", ifelse(is_manual_threshold, "Manual", "Algorithm")), "I")

writeValueRow7(marker.info[1, "threshold"], "J")

if (length(rownames(marker.merged.info)) == 4){ 
	writeValueRow7(strsplit(rownames(marker.merged.info)[1], "_")[[1]][3], "K")
	writeValueRow7(strsplit(rownames(marker.merged.info)[1], "_")[[1]][5], "L")
	writeValueRow7(marker.merged.info[1, "concentration"], "M")
	writeValueRow7(marker.merged.info[1, "positive"], "N")
	writeValueRow7(extract_value(min.dx.marker.con.perwell.status), "O")
	writeValueRow7(marker.merged.info[2, "concentration"], "P")
	writeValueRow7(marker.merged.info[2, "positive"], "Q")
	writeValueRow7(marker.merged.info[3, "concentration"], "R")
	writeValueRow7(marker.merged.info[3, "positive"], "S")
	writeValueRow7(extract_value(max.h2o.marker.con.perwell.status), "T")
	writeValueRow7(strsplit(rownames(marker.merged.info)[4], "_")[[1]][5], "U")
	writeValueRow7(marker.merged.info[4, "concentration"], "V")
	writeValueRow7(marker.merged.info[4, "positive"], "W")
	writeValueRow7(marker.merged.info[4, "replicates"], "X")
}

writeValueRow7("", "Y")

if (length(rownames(alb.merged.info)) == 4){ 

	writeValueRow7(strsplit(rownames(alb.merged.info)[2], "_")[[1]][4], "Z")

	writeValueRow7(alb.merged.info[2, "concentration"], "AA")
	writeValueRow7(alb.merged.info[2, "positive"], "AB")
	writeValueRow7(extract_value(min.hl60.gus.con.perwell.status), "AC")

	writeValueRow7(alb.merged.info[3, "concentration"], "AD")
	writeValueRow7(alb.merged.info[3, "positive"], "AE")
	writeValueRow7(extract_value(max.h2o.gus.con.perwell.status), "AF")

	writeValueRow7(alb.merged.info[1, "concentration"], "AG")
	writeValueRow7(alb.merged.info[1, "positive"], "AH")

	writeValueRow7(alb.merged.info[4, "concentration"], "AI")
	writeValueRow7(alb.merged.info[4, "positive"], "AJ")
	writeValueRow7(alb.merged.info[4, "replicates"], "AK")

} else if (length(rownames(alb.merged.info)) == 3){

	writeValueRow7(strsplit(rownames(alb.merged.info)[1], "_")[[1]][4], "Z")

	writeValueRow7(alb.merged.info[1, "concentration"], "AA")
	writeValueRow7(alb.merged.info[1, "positive"], "AB")
	writeValueRow7(extract_value(min.hl60.gus.con.perwell.status), "AC")

	writeValueRow7(alb.merged.info[2, "concentration"], "AD")
	writeValueRow7(alb.merged.info[2, "positive"], "AE")
	writeValueRow7(extract_value(max.h2o.gus.con.perwell.status), "AF")

	writeValueRow7("N.A.", "AG")
	writeValueRow7("N.A.", "AH")

	writeValueRow7(alb.merged.info[3, "concentration"], "AI")
	writeValueRow7(alb.merged.info[3, "positive"], "AJ")
	writeValueRow7(alb.merged.info[3, "replicates"], "AK")

}



writeValueRow7(extract_value(min.fu.gus.conc.perwell.status), "AL")
writeValueRow7("", "AM")
writeValueRow7(extract_value(final.qc.call), "AN")
writeValueRow7(paste("",generated_date), "AO")
writeValueRow7( pipeline_version, "AP")
writeValueRow7(capitalize_first(runmode), "AQ")



# Merge header cells
center_style <- createStyle(halign = "center")

addStyle(wb, sheet_name, center_style, rows = 7, cols = 1:44, gridExpand = TRUE)

# Save workbook
saveWorkbook(wb, paste(folder, separator, params$sid, "_", params$mid, "_", runmode, "_Run_Records.xlsx", sep=""), overwrite = TRUE)
