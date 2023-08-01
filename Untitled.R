library(stringr)

reformat_equation <- function(equation) {
  # Check if the equation has a parenthesis
  if (str_detect(equation, "\\(.*\\)")) {
    # Extract the content inside the parenthesis
    in_parentheses <- str_extract(equation, "\\(.*\\)")
    # Remove the parentheses
    in_parentheses <- str_replace_all(in_parentheses, "\\(|\\)", "")
    # Split the string based on the "+" operator
    vars <- str_split(in_parentheses, "\\+", simplify = TRUE)
    # Split the equation to get the divisor
    eq_parts <- str_split(equation, "/", simplify = TRUE)
    divisor <- as.numeric(eq_parts[2])
    # Calculate the coefficient for each variable
    new_vars <- paste0(1/divisor, "*", vars)
    # Build the new equation
    equation <- str_replace(equation, "\\(.*\\)", paste(new_vars, collapse = "+"))
  } else {
    # If the equation doesn't have a parenthesis
    # Split the string based on the "+" operator
    vars <- str_split(equation, "\\+", simplify = TRUE)
    # Check if the variable has a division operation
    vars <- sapply(vars, function(x) {
      if (str_detect(x, "/")) {
        var_parts <- str_split(x, "/", simplify = TRUE)
        var_name <- var_parts[1]
        divisor <- as.numeric(var_parts[2])
        return(paste0(1/divisor, "*", var_name))
      } else {
        return(paste0("1*", x))
      }
    })
    # Build the new equation
    equation <- paste(vars, collapse = "+")
  }
  # Return the new equation
  return(equation)
}

# Test the function
print(reformat_equation("(p1+p2+p3)/4 < 1"))
print(reformat_equation("p1+p2+p3/4 < 1"))
