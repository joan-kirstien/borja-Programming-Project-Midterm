setwd("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm")

install.packages(c("readxl", "readr", "dplyr"))
install.packages("tidyverse")
library(tidyverse)

library(readxl)
library(readr)
library(dplyr)


# Read Course Offerings
course_offerings <- read_excel("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/subj_offerings.xlsx")

# Read Students
students <- read_csv("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/students.csv")

# Read Student Load
student_load <- read_excel("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/stud_load.xlsx")

dissolved_rate <- function(offerings, dept) {
  # Filter offerings for the given department
  filtered_offerings <- filter(offerings, dept_code == dept)
  
  # Calculate the percentage of dissolved classes
  dissolved_count <- sum(filtered_offerings$status == "DSLVD", na.rm = TRUE)
  total_count <- nrow(filtered_offerings)
  if (total_count > 0) {
    dissolved_percentage <- (dissolved_count / total_count) * 100
  } else {
    dissolved_percentage <- NA
  }
  
  # Print the result
  if (!is.na(dissolved_percentage)) {
    cat(paste("The dissolved rate for department", dept, "is:", dissolved_percentage, "%\n"))
  } else {
    cat(paste("No data available for department", dept, "\n"))
  }
  
  return(dissolved_percentage)
}


# Example usage
dissolved_rate(course_offerings, "BUSEN")

dept_course_listing <- function(dept) {
  # Filter the course offerings for the given department
  filtered_courses <- course_offerings %>%
    filter(dept_code == dept)
  
  # Extract and return the unique offer codes
  unique_offer_codes <- unique(filtered_courses$offer_no)
  
  # Print the result
  cat("Courses offered in department", dept, ":\n")
  print(unique_offer_codes)
  
  return(unique_offer_codes)
}

dept_course_listing("BUSEN")

dept_density <- function(offerings, student_load, dept_code) {
  # Join offerings and student_load on offer_no with many-to-many relationship
  joined_data <- left_join(offerings, student_load, by = "offer_no", relationship = "many-to-many")
  
  # Filter for the given department
  filtered_data <- filter(joined_data, dept_code == dept_code)
  
  # Calculate the mean class size (density)
  mean_class_size <- mean(filtered_data$wdw, na.rm = TRUE)
  
  # Print the result
  cat(paste("The mean class size for department", dept_code, "is:", round(mean_class_size), "\n"))
  
  return(mean_class_size)
}


dept_density(course_offerings, student_load, "BUSEN")

special_class <- function(min_class_size) {
  # Filter student_load for classes below min_class_size
  special_classes <- student_load %>%
    filter(wdw < min_class_size)
  
  # Print the result
  cat("Special Classes:\n")
  print(special_classes)
  
  return(special_classes)
}


special_class(20)

colnames(student_load)
colnames(students)

#print_column_names <- function(file_path) {
 # df <- read_excel(file_path)
  #colnames(df)
#}

#class_list <- function(offer_code) {
  # Read the subj_offerings.xlsx file
 # subj_offerings <- read_excel("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/subj_offerings.xlsx")
  
  # Find the course name using the offer code
  #course_name <- subj_offerings$subj_name[subj_offerings$offer_no == offer_code]
  
  # Read the stud_load.xlsx file
  #stud_load <- read_excel("D:/3rdSUMMER/ML/Borja_Programming Project - Midterm/stud_load.xlsx")
  
  # Filter students enrolled in the found course
#  enrolled_students <- stud_load %>% filter(stud_id %in% subj_offerings$stud_id[subj_offerings$offer_no == offer_code])
  
  # Extract and print student names
#  student_names <- enrolled_students$stud_no %in% students$stud_no
#  cat(paste("Students enrolled in", course_name, ": \n", paste(enrolled_students$stud_no, collapse = ", "), "\n"))
#}

#class_list("your_offer_code_here")

students_df <- read_csv("D:/3rdSUMER/ML/Borja_Programming Project - Midterm/students.csv")

grade_mean_by_gender <- function(students_df, gender) {
  # Filter students by gender
  filtered_students <- filter(students_df, gender == gender)
  
  # Calculate the mean grade
  mean_grade <- mean(filtered_students$avg_grade, na.rm = TRUE)
  
  # Print the result
  cat(paste("Mean grade for", gender, "is:", mean_grade, "\n"))
  
  # Return the mean grade
  return(mean_grade)
}


# Example call to the function
grade_mean_by_gender(students, "Male")



print_column_names <- function(file_path) {
  df <- read_excel(file_path)
  colnames(df)
}

# Merge student_load and students on stud_id
merged_students <- cross_join(student_load, students, by = NULL)

merge(student_load, students, by = "stud_no", all.x=TRUE)

