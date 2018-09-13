#######################
# Lab 2 exit ticket solutions
#######################

# Load in the unemployment dataset
unemployment <- read.table("https://ionides.github.io/401f18/01/unemployment.csv", sep = ",", header = T)

# Check that the dataset loaded in correctly
head(unemployment)

# Find the average unemployment rate for each month
apply(unemployment[, -1], 2, mean) # the -1 tells R to ignore the first column which is the year variable

# Find the function in R for standard deviation
??standarddeviation

# Find the standard deviation of the unemployment rate for each month
apply(unemployment[, -1], 2, sd)
