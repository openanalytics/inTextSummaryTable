context("Combine variables")

test_that("An error is generated if the variable is not correctly specified", {
			
	expect_error(
		combineVariables(
			data = data,
			paramsList = list(var = "AFL"),
			newVar = "variable"
		),
		".*paramsList.* should be a nested list.*"
	)
			
})

test_that("An error is generated if no variable or expression is specified", {
			
	expect_error(
		combineVariables(
			data = data,
			paramsList = list(list(label = "test")),
			newVar = "variable"
		),
		".*variable.* should be specified.*"
	)
			
})

test_that("The data is correctly extracted based on a specified variable", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c(NA_character_, "N", "", "N")
	)	
	
	sumTable <- combineVariables(
		data = data,
		paramsList = list(list(var = "AFL")),
		newVar = "variable"
	)
	expect_equal(
		sumTable,		
		cbind.data.frame(
			subset(data, !is.na(AFL) & AFL != ""),
			variable = factor("AFL", levels = "AFL")
		)
	)
	
})

test_that("The data is correctly extracted based on a value of a specified variable", {
	
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	sumTable <- combineVariables(
		data = data,
		paramsList = list(list(var = "AFL", value = "Y")),
		newVar = "variable"
	)
	expect_equal(
		sumTable,		
		cbind.data.frame(
			subset(data, AFL == "Y"),
			variable = factor("AFL", levels = "AFL")
		)
	)
			
})

test_that("The data is extracted by default based on the equality if a value of a variable is specified", {
		
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)
	expect_identical(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 1, fctTest = "==")),
			newVar = "variable"
		),
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 1)),
			newVar = "variable"
		)
	)
	
})

test_that("The data is extracted correctly based on a comparison function specified as a character", {
	
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)
	dataLongFct <- combineVariables(data = data,
		paramsList = list(list(var = "AVAL", value = 2, fctTest = ">")),
		newVar = "variable"
	)
	expect_identical(
		dataLongFct,
		cbind.data.frame(
			subset(data, AVAL > 2),
			variable = "AVAL",
			stringsAsFactors = TRUE
		)
	)

})

test_that("The data is extracted correctly based on a comparison function specified by variable", {

	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)
	expect_identical(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2, fctTest = function(x, value) x > value)),
			newVar = "variable"
		),
		cbind.data.frame(
			subset(data, AVAL > 2),
			variable = "AVAL",
			stringsAsFactors = TRUE
		)
	)
	
})

test_that("The data is extracted correctly based on a comparison function specified for all variables", {
	
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)
	expect_identical(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2)),
			newVar = "variable", fctTest = ">"
		),
		cbind.data.frame(
			subset(data, AVAL > 2),
			variable = "AVAL",
			stringsAsFactors = TRUE
		)
	)
	
})

test_that("An error is generated if the comparison function is not a function", {
	
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)
	expect_error(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2, fctTest = "blabla")),
			newVar = "variable"
		)
	)
	
})

test_that("An error is generated if the comparison function doesn't contain parameters to compare", {
			
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)	
	expect_error(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2, fctTest = is.character)),
			newVar = "variable"
		)
	)
			
})

test_that("The data is correctly extracted based on a condition expression of multiple variables", {
			
	data <- data.frame(
		ID =  c(1, 2, 3, 4),
		AVAL = c(4, 3, 2, 1)
	)
	sumTable <- combineVariables(data = data,
		paramsList = list(list(var = "AVAL", exprs = "AVAL <= 3 & ID >= 2")),
		newVar = "variable"
	)
	expect_identical(sumTable[, c("ID", "AVAL")], subset(data, AVAL <= 3 & ID >= 2))
	
})	

test_that("An error is generated if the specification of the condition is incorrect", {
			
	data <- data.frame(
		ID =  c(1, 2, 3, 4),
		AVAL = c(4, 3, 2, 1)
	)
	expect_error(	
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", exprs = "AVAL is of specific type")),
			newVar = "variable"
		)
	)
			
})

test_that("The data is correctly extracted based on multiple conditions of variables", {
		
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4),
		CHG = c(-1, 0, 1, 2)
	)
	
	sumTable <- combineVariables(
		data = data,
		paramsList = list(
			list(var = "CHG", value = 0, label = "Change from baseline positive"),
			list(var = "AVAL", value = 2, label = "Actual value >= 2")
		),
		fctTest = ">=",
		newVar = "variable"
	)
	
	# build reference object
	sumTableCond1 <- cbind.data.frame(
		subset(data, CHG >= 0),
		variable = factor("Change from baseline positive")
	)
	sumTableCond2 <- cbind.data.frame(
		subset(data, AVAL >= 2),
		variable = factor("Actual value >= 2")
	)
	sumTableRef <- rbind(sumTableCond1, sumTableCond2)
	# levels are in the order specified in 'paramsList':
	sumTableRef$variable <- factor(sumTableRef$variable,
		levels = c("Change from baseline positive", "Actual value >= 2"))
	
	expect_equal(object = sumTable, expected = sumTableRef)
			
})

test_that("The variable label is correctly set to the variable name by default", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	sumTable <- combineVariables(
		data = data,
		paramsList = list(list(var = "AFL", value = "Y")),
		newVar = "variable"
	)
	expect_equal(unique(as.character(sumTable$variable)), "AFL")
	
})

test_that("An error is generated if the variable and label are not specified", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	expect_error(
		combineVariables(
			data = data,
			paramsList = list(list()),
			newVar = "variable"
		),
		".*Label should be specified"
	)
	
})

test_that("The label of the variable is correctly set when specified", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	sumTableLabel <- combineVariables(
		data = data,
		paramsList = list(list(var = "AFL", value = "Y", label = "test")),
		newVar = "variable"
	)
	expect_identical(unique(as.character(sumTableLabel$variable)), "test")
	
})

test_that("The label of the variable is correctly extracted from the labels of all variables", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	sumTableLabel <- combineVariables(
		data = data,
		paramsList = list(list(var = "AFL", value = "Y")),
		newVar = "variable",
		labelVars = c(AFL = "test")
	)
	expect_identical(unique(as.character(sumTableLabel$variable)), "test")
	
})

test_that("An error is generated if the variables have duplicated labels", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	expect_error(
		combineVariables(
			data = data,
			paramsList = list(
				list(var = "AFL", value = "Y", label = "test 1"),
				list(var = "AFL", value = "N", label = "test 1")
			),
			newVar = "variable"
		),
		"Duplicated labels in the specified parameter list."
	)
			
})

test_that("The extra label of the variable is correctly set when specified", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	sumTableLabelExtra <- combineVariables(
		data = data,
		paramsList = list(list(var = "AFL", value = "Y", 
			label = "test", labelExtra = "blabla")),
		newVar = "variable"
	)
	expect_identical(unique(as.character(sumTableLabelExtra$variable)), "test blabla")
			
})

test_that("All records are correctly included when requested", {
				
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)		
	sumTableIncludeAll <- combineVariables(
		data = data,
		paramsList = list(
			list(var = "AFL", value = "Y")
		),
		includeAll = TRUE, newVar = "variable"
	)
	
	sumTableRef <- rbind(
		# entire dataset is returned with 'variable' set to: 'Any'
		cbind.data.frame(data, variable = factor("Any")),
		# the rest of the dataset is based on specified condition in 'paramsList':
		cbind.data.frame(
			subset(data, AFL == "Y"),
			variable = factor("AFL")
		)	
	)
	
	expect_equal(
		object = sumTableIncludeAll, 
		expected = sumTableRef,
		check.attributes = FALSE
	)
			
})

test_that("A label is correctly set to all records when specified", {

	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)		
	sumTableLabelAll <- combineVariables(
		data = data,
		paramsList = list(
			list(var = "AFL", value = "Y")
		),
		newVar = "variable",
		includeAll = TRUE, labelAll = "Entire dataset"
	)
	expect_equal(
		object = subset(sumTableLabelAll, variable == "Entire dataset", select = -variable), 
		expected = data,
		check.attributes = FALSE
	)
			
})
			