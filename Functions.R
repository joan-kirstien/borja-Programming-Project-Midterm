setwd("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm")

install.packages(c("readxl", "readr", "dplyr"))
install.packages("tidyverse")
library(tidyverse)

library(readxl)
library(readr)
library(dplyr)

course_offerings <- read_excel("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/subj_offerings.xlsx")
students <- read_csv("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/students.csv")
student_load <- read_excel("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/stud_load.xlsx")

dissolved_rate <- function(offerings, dept) {
  filtered_offerings <- filter(offerings, dept_code == dept)
  
  dissolved_count <- sum(filtered_offerings$status == "DSLVD", na.rm = TRUE)
  total_count <- nrow(filtered_offerings)
  if (total_count > 0) {
    dissolved_percentage <- (dissolved_count / total_count) * 100
  } else {
    dissolved_percentage <- NA
  }
  
  if (!is.na(dissolved_percentage)) {
    cat(paste("The dissolved rate for department", dept, "is:", dissolved_percentage, "%\n"))
  } else {
    cat(paste("No data available for department", dept, "\n"))
  }
  
  return(dissolved_percentage)
}


dissolved_rate(course_offerings, "BUSEN")

dept_course_listing <- function(dept) {
  filtered_courses <- course_offerings %>%
    filter(dept_code == dept)
  
  unique_offer_codes <- unique(filtered_courses$offer_no)
  
  cat("Courses offered in department", dept, ":\n")
  print(unique_offer_codes)
  
  return(unique_offer_codes)
}

dept_course_listing("BUSEN")

dept_density <- function(offerings, student_load, dept_code) {
  joined_data <- left_join(offerings, student_load, by = "offer_no", relationship = "many-to-many")
  
  filtered_data <- filter(joined_data, dept_code == dept_code)
  
  mean_class_size <- mean(filtered_data$wdw, na.rm = TRUE)
  
  cat(paste("The mean class size for department", dept_code, "is:", round(mean_class_size), "\n"))
  
  return(mean_class_size)
}


dept_density(course_offerings, student_load, "BUSEN")

special_class <- function(min_class_size) {
  special_classes <- student_load %>%
    filter(wdw < min_class_size)
  
  cat("Special Classes:\n")
  print(special_classes)
  
  return(special_classes)
}


special_class(20)

students_df <- read_csv("D:/3rdSUMER/ML/Borja_Programming Project - Midterm/students.csv")

grade_mean_by_gender <- function(students_df, gender) {
  filtered_students <- filter(students_df, gender == gender)
  
  mean_grade <- mean(filtered_students$avg_grade, na.rm = TRUE)
  
  cat(paste("Mean grade for", gender, "is:", mean_grade, "\n"))
  
  return(mean_grade)
}

grade_mean_by_gender(students, "Male")

print_column_names <- function(file_path) {
  df <- read_excel(file_path)
  colnames(df)
}



