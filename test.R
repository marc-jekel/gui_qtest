# Function to reformat inequalities and equalities
processRelation <- function(relation_string) {
  # Define the possible operators
  operators <- c("=", "<", ">")
  
  # Find which operator is in the string
  operator <- operators[sapply(operators, function(op) grepl(op, relation_string))]
  
  # Split the string into the left and right parts
  parts <- strsplit(relation_string, split = operator)[[1]]
  
  # Trim whitespace
  parts <- trimws(parts)
  
  # Split each part into separate terms
  left_terms <- unlist(strsplit(parts[1], split = "[\\+\\-]"))
  right_terms <- unlist(strsplit(parts[2], split = "[\\+\\-]"))
  
  # Trim whitespace
  left_terms <- trimws(left_terms)
  right_terms <- trimws(right_terms)
  
  # Find which terms on the left are numbers
  left_numbers <- grepl("^\\d*\\.?\\d+$", left_terms)
  
  # Find which terms on the right are numbers
  right_numbers <- grepl("^\\d*\\.?\\d+$", right_terms)
  
  # Find which terms on the left are p's or c*p's
  left_p <- grepl("^(\\d*\\.?\\d*\\*?)?p\\d+$", left_terms)
  
  # Find which terms on the right are p's or c*p's
  right_p <- grepl("^(\\d*\\.?\\d*\\*?)?p\\d+$", right_terms)
  
  # Swap the numbers from left to right and the p's from right to left
  new_left_terms <- c(left_terms[left_p], right_terms[!right_numbers])
  new_right_terms <- c(left_terms[!left_p], right_terms[right_numbers])
  
  # Construct new relation string
  new_relation_string <- paste(paste(new_left_terms, collapse = "-"), operator, paste(new_right_terms, collapse = "+"))
  
  # Remove all blanks
  new_relation_string <- gsub(" ", "", new_relation_string)
  
  # Return new relation string
  return(new_relation_string)
}

# Test the function
print(processRelation("p1 + 2*p3 + 0.3 > p2 + .05"))
print(processRelation("p1 + p3 + 0.3 = 2*p2 + .05"))
print(processRelation("2*p1 + p3 + 0.3 < p2 + .05"))
