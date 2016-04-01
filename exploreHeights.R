library(RUnit)
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_readData();
   test_calculateRelationships()

} # runTests
#----------------------------------------------------------------------------------------------------
readData <- function(filename="heights.tsv", quiet=TRUE)
{
   stopifnot(file.exists(filename))
   if(!quiet)
      printf("we are about to read '%s'", filename)

   tbl <- read.table(filename, sep="\t", header=TRUE)

   return(tbl)

} # readData
#----------------------------------------------------------------------------------------------------
calculateRelationships <- function(tbl, independentVariableName, dependentVariableName)
{
   stopifnot(independentVariableName %in% colnames (tbl))
   stopifnot(dependentVariableName %in% colnames (tbl))

   vec1 <- tbl[, independentVariableName]
   vec2 <- tbl[, dependentVariableName]
   #browser()
   return(list(correlation=cor(vec1, vec2),
               covariance=cov(vec1, vec2)))

} # calculateRelationships
#----------------------------------------------------------------------------------------------------
test_calculateRelationships <- function()
{
   print("--- test_calculateRelationships")

   tbl <- readData()

   result <- calculateRelationships(tbl, "twoYears", "adult")
   checkTrue(result$correlation > 0.9)
   checkTrue(result$covariance > 9.0)

} # test_calculateRelationships
#----------------------------------------------------------------------------------------------------
test_readData <- function()
{
   print("--- test_readData")

      # make sure it handles a bad filename
   checkException(tbl <- readData("bogusFileName.tsv"), silent=TRUE)

   tbl <- readData()
   checkTrue("data.frame" %in% is(tbl))
   checkEquals(colnames(tbl), c("twoYears", "adult"))
   checkTrue(nrow(tbl) > 6)   # need at least a few observations

} # test_readData
#----------------------------------------------------------------------------------------------------
