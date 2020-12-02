context("Combine variables")

test_that("wrong spec of paramsList", {
			
	expect_error(
		combineVariables(
			data = data,
			paramsList = list(var = "AFL"),
			newVar = "variable"
		),
		".*paramsList.* should be a nested list.*"
	)
			
})

test_that("no variable or expression is specified", {
			
	expect_error(
		combineVariables(
			data = data,
			paramsList = list(list(label = "test")),
			newVar = "variable"
		),
		".*variable.* should be specified.*"
	)
			
})

test_that("data is extracted based on var only", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c(NA_character_, "N", "", "N")
	)	
	
	expect_silent(
		sumTable <- combineVariables(
			data = data,
			paramsList = list(list(var = "AFL")),
			newVar = "variable"
		)
	)
	expect_s3_class(sumTable, "data.frame")
	# default is to consider elements not missing and not empty
	expect_equal(sumTable$ID, subset(data, !is.na(AFL) & AFL != "")$ID)
	expect_true("variable" %in% names(sumTable))
	expect_s3_class(sumTable$variable, "factor")
	expect_equal(levels(sumTable$variable), "AFL")
	expect_equal(unique(as.character(sumTable$variable)), "AFL")
	expect_equal(unique(as.character(sumTable$AFL)), "N")
			
})

test_that("data is extracted based on var and value", {
	
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
	
	expect_silent(
		sumTable <- combineVariables(
			data = data,
			paramsList = list(list(var = "AFL", value = "Y")),
			newVar = "variable"
		)
	)
	expect_s3_class(sumTable, "data.frame")
	expect_equal(sumTable$ID, subset(data, AFL == "Y")$ID)
	expect_true("variable" %in% names(sumTable))
	expect_equal(unique(as.character(sumTable$variable)), "AFL")
	expect_equal(unique(as.character(sumTable$AFL)), "Y")
			
})

test_that("data is extracted based on var, value and custom function", {
		
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)
	
	## default is to test equality
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
	
	## custom fct
	
	# as a character
	dataLongFct <- combineVariables(data = data,
		paramsList = list(list(var = "AVAL", value = 2, fctTest = ">")),
		newVar = "variable"
	)
	expect_identical(dataLongFct, {
		dataLongAll <- combineVariables(data = data, paramsList = list(list(var = "AVAL")), newVar = "variable")
		subset(dataLongAll, AVAL > 2)
	})

	# as a function
	expect_identical(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2, fctTest = function(x, value) x > value)),
			newVar = "variable"
		),
		dataLongFct
	)
	
	# general fct
	expect_identical(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2)),
			newVar = "variable", fctTest = ">"
		),
		dataLongFct
	)
	
	## wrong spec
	
	# not a function
	expect_error(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2, fctTest = "blabla")),
			newVar = "variable"
		)
	)
			
	# function with only one parameter
	expect_error(
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", value = 2, fctTest = is.character)),
			newVar = "variable"
		)
	)
			
})

test_that("data is extracted based on expression", {
			
	data <- data.frame(
		ID =  c(1, 2, 3, 4),
		AVAL = c(4, 3, 2, 1)
	)
	
	# correct spec
	sumTable <- combineVariables(data = data,
		paramsList = list(list(var = "AVAL", exprs = "AVAL <= 3 & ID >= 2")),
		newVar = "variable"
	)
	expect_identical(sumTable[, c("ID", "AVAL")], subset(data, AVAL <= 3 & ID >= 2))
	
	# wrong exprs
	expect_error(	
		combineVariables(data = data,
			paramsList = list(list(var = "AVAL", exprs = "AVAL is of specific type")),
			newVar = "variable"
		)
	)
			
})

test_that("data is extracted based on multiple filters", {
		
	data <- data.frame(
		ID = seq.int(4),
		AVAL = c(1, 2, 3, 4)
	)
	data$CHG <- data$AVAL-2
	
	sumTable <- combineVariables(
		data = data,
		paramsList = list(
			list(var = "CHG", value = 0, label = "Change from baseline positive"),
			list(var = "AVAL", value = 2, label = "Actual value >= 2")
		),
		fctTest = ">=",
		newVar = "variable"
	)
	
	expect_s3_class(sumTable, "data.frame")
	expect_equal(
		subset(sumTable, variable == "Actual value >= 2", select = -variable), 
		subset(data, AVAL >= 2),
		check.attributes = FALSE # rownames differ
	)
	expect_equal(
		subset(sumTable, variable == "Change from baseline positive", select = -variable), 
		subset(data, CHG >= 0),
		check.attributes = FALSE # rownames differ
	)
	expect_s3_class(sumTable$variable, "factor")
	# levels are in the order specified in 'paramsList':
	expect_equal(
		levels(sumTable$variable),
		c("Change from baseline positive", "Actual value >= 2")
	)
			
})

test_that("label is specified", {
			
	data <- data.frame(
		ID = seq.int(4),
		AFL = c("Y", "N", "Y", "N")
	)
		
	# by default, label is set as the variable name
	expect_equal({
		sumTable <- combineVariables(
			data = data,
			paramsList = list(list(var = "AFL", value = "Y")),
			newVar = "variable"
		)
		unique(as.character(sumTable$variable))
		}, "AFL"
	)
	
	# if variable is not specified, label should be specified
	expect_error(
		combineVariables(
			data = data,
			paramsList = list(list()),
			newVar = "variable"
		),
		".*Label should be specified"
	)
	
	# label is specified
	sumTableLabel <- combineVariables(
		data = data,
		paramsList = list(list(var = "AFL", value = "Y", label = "test")),
		newVar = "variable"
	)
	expect_identical(unique(as.character(sumTableLabel$variable)), "test")
	
	# label extracted from labelVars
	expect_identical(
		combineVariables(
			data = data,
			paramsList = list(list(var = "AFL", value = "Y")),
			newVar = "variable",
			labelVars = c(AFL = "test")
		),
		sumTableLabel
	)
	
	# duplicated labels
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

test_that("label extra is specified", {
			
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

test_that("all records are included", {
				
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
	
	# entire dataset is returned with 'variable' set to: 'Any'
	expect_equal(
		object = subset(sumTableIncludeAll, variable == "Any", select = -variable), 
		expected = data,
		check.attributes = FALSE
	)
	
	# the rest of the dataset is based on specified condition in 'paramsList':
	expect_equal(
		object = subset(sumTableIncludeAll, variable != "Any", select = -variable), 
		expected = subset(data, AFL == "Y"),
		check.attributes = FALSE
	)
			
})

test_that("all records are included with custom label", {

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
			