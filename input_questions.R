run_type_question <- function(){

	cat("\nDo you want to perform absolute or relative OFT?
	1. Absolute
	2. Relative\n")

	answer <- readLines("stdin",n=1)

	runType <- switch(
		answer, 
		"1" = "absolute",
		"2" = "relative", 
		"relative")

	return(runType) 

}

verifier_question <- function(){

	cat("\nVerified by?
	1. Prof Allen Yeoh
	2. Others
	3. Blank\n")

	answer <- readLines("stdin",n=1)

	if (answer == "2"){
		cat("Please specify:")
		verifier <- readLines("stdin",n=1)
	} else {
		verifier <- switch(
		answer, 
		"1" = "Prof Allen Yeoh",
		"3" = "", 
		answer)
	}

	return(verifier)
}

run_by_question <- function(){


	cat("\nRun by?
	1. Amanda Lee
	2. Dr Lu Yi
	3. Florence Lim
	4. Huan Pei Tee
	5. Maliha
	6. Nurhilya
	7. Others
	8. Blank\n")
	answer <- readLines("stdin",n=1)

	if (answer == "7"){
		cat("Please specify:")
		run_by <- readLines("stdin",n=1)
	} else {
		run_by <- switch(
		answer, 
		"1" = "Amanda Lee", 
		"2" = "Dr Lu Yi", 
		"3" = "Florence Lim", 
		"4" = "Huan Pei Tee", 
		"5" = "Maliha", 
		"6" = "Nurhilya",
		"8" = "", 
		answer 
		)
	}

	return(run_by)
}

run_date_question <- function(){

	cat("\nDate?
	1.",format(Sys.Date(), "%d/%m/%Y") ,"
	2. Blank\n")
	answer <- readLines("stdin",n=1)

	run_date <- switch(
		answer, 
		"1" = format(Sys.Date(), "%d/%m/%Y"), 
		"2" = "", 
		answer 
		)
	return(run_date)

}

reported_by_question <- function(){


	cat("\nReported by?
	1. Shirley Kham
	2. Dr Lu Yi
	3. Huan Pei Tee
	4. Nurhilya
	5. Others
	6. Blank\n")
	answer <- readLines("stdin",n=1)

	if (answer == "5"){
		cat("Please specify:")
		reported_by <- readLines("stdin",n=1)
	} else {
		reported_by <- switch(
			answer, 
			"1" = "Shirley Kham", 
			"2" = "Dr Lu Yi",
			"3" = "Huan Pei Tee", 
			"4" = "Nurhilya", 
			"6" = "", 
			answer 
			)
	}

	return(reported_by)
}
