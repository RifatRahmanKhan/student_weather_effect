df <- read.delim("Student_Weather_AllColumns_Concise.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
head(df)
cat("Variables:", ncol(df), "\nObservations:", nrow(df), "\n")
names(df)
