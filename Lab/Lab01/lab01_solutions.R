# Reading in the GPA dataset
gpa = read.table("C:/Users/ngierty/Downloads/CH01PR19.txt", header = T)

# check that the dataset has been read correctly
head(gpa)

# get summary statistics
summary(gpa)

# gpa and act of 2nd student
gpa[2,]
gpa[106,]

# vector of the ACT scores
gpa[,2]

# working with a matrix
A = matrix(c(3, -2, -1, 4, 1, 2), nrow = 2); A

B = matrix(c(3, -2, -1, 4, 1, 2, 4, 5, 6, 7, 8, 9), nrow = 3)
B

# Lab Ticket
PLASTIC <- read.table("C:/Users/ngierty/Downloads/plastic_hardness.txt", header = T)

# length with dataset
length(plastic) # number of columns in the dataset
length(plastic$hardness) # the number of rows/observations

# challenge question
plastic_24 <- plastic[5:8,]
