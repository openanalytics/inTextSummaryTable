library(covr)
pc <- package_coverage(
    path = "~/git/GLPGInTextSummaryTable/package/inTextSummaryTable",
    type = "tests", quiet = FALSE, clean = FALSE
)
report(x = pc,
    file = paste0("testCoverage-tests-inTextSummaryTable",
        packageVersion("inTextSummaryTable"), ".html")
)
