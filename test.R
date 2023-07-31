convert_inequality <- function(inequality) {
  # Replace "> ." with "> 0."
  inequality <- gsub("> \\."," > 0.", inequality)
  # Replace "< ." with "< 0."
  inequality <- gsub("< \\."," < 0.", inequality)
  # Replace ">" with "<" and vice versa
  inequality <- gsub(">", "<temp>", inequality)
  inequality <- gsub("<", ">", inequality)
  inequality <- gsub("<temp>", "<", inequality)
  
  # Swap sides
  inequality <- gsub("([a-z]+) > ([0-9.]+)", "\\2 < \\1", inequality)
  inequality <- gsub("([a-z]+) >= ([0-9.]+)", "\\2 <= \\1", inequality)
  inequality <- gsub("([a-z]+) < ([0-9.]+)", "\\2 > \\1", inequality)
  inequality <- gsub("([a-z]+) <= ([0-9.]+)", "\\2 >= \\1", inequality)
  
  return(inequality)
}

# Example usage:
original_inequality <- "p1 - p2 > .05"
converted_inequality <- convert_inequality(original_inequality)
cat("Converted inequality: ", converted_inequality, "\n")
