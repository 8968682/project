
# reading the csv file into r

training = read.csv(file = "pml-training.csv",header = TRUE,sep = ",")
colnames(training)
str(training)

# removing unnecessary data
i = 1
n = length(colnames(training))
no_of_NA = 0
while(i <= n)
{
  no_of_NA[i] = length(which(is.na(training[,i]) == TRUE))
  i=i+1
}
NA_column_id = which(no_of_NA == 19216)
removed_na = training[,-NA_column_id]

#str(removed_na)

# All columns which will not be used are removed

skew_col = which(grepl("skewness",colnames(removed_na)) == TRUE)
removed_na = removed_na[,-skew_col]
kurt_col = which(grepl("kurtosis",colnames(removed_na)) == TRUE)
removed_na = removed_na[,-kurt_col]
max_col = which(grepl("max",colnames(removed_na)) == TRUE)
removed_na = removed_na[,-max_col]
min_col = which(grepl("min",colnames(removed_na)) == TRUE)
removed_na = removed_na[,-min_col]
amplitude_col = which(grepl("amplitude",colnames(removed_na)) == TRUE)
removed_na = removed_na[,-amplitude_col]

#table(removed_na[,2])
#table(removed_na[,69])

final_data = removed_na

#carlitos = removed_na[which(removed_na[,2] == "carlitos"),]



# model fitting
library("caret")

modfit <- train(classe ~ .,method = "rpart",data = final_data)
            
testing = read.csv(file = "pml-testing.csv",header = TRUE,sep = ",")

used_columns = which(grepl("min",colnames(removed_na)) == TRUE)
                