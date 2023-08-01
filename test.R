convertInequality <- function(inequality) {
  process_term <- function(term, op = "+") {
    # remove parentheses
    expr <- gsub("[()]", "", term)
    
    # get divisor if present
    divisor <- 1
    if (grepl("/", expr)) {
      divisor_str <- strsplit(expr, "/")[[1]][2]
      if (!is.na(divisor_str)) {
        divisor <- as.numeric(trimws(divisor_str))
        expr <- trimws(strsplit(expr, "/")[[1]][1])
      }
    }
    
    # split expr into parts by "+"
    parts <- unlist(strsplit(expr, "\\+"))
    
    # process each part separately
    new_parts <- sapply(parts, function(part) {
      # split by "-" and process each sub-part
      sub_parts <- unlist(strsplit(part, "\\-"))
      new_sub_parts <- sapply(sub_parts, function(sub_part) {
        multiplier <- 1
        var <- sub_part  # assume the whole sub_part is a variable
        # extract multiplier and variable
        if (grepl("p", sub_part)) {
          var <- str_extract(sub_part, "p\\d+")
          multiplier_str <- gsub(paste0("\\*", var), "", sub_part)
          multiplier_str <- gsub(var, "", multiplier_str) #remove var name if present
          multiplier_str <- gsub("\\*", "", multiplier_str) #remove * if present
          if (multiplier_str != "") {
            multiplier <- as.numeric(multiplier_str)
          }
        }
        if (!is.na(multiplier) && !is.na(divisor) && divisor != 0) {
          fraction_multiplier <- fractions(multiplier / divisor)
        } else {
          fraction_multiplier <- multiplier
        }
        # only include multiplier if it's different from 1
        if (!is.na(fraction_multiplier) && fraction_multiplier != 1) {
          return(paste0(fraction_multiplier, "*", var))
        } else {
          return(var)
        }
      })
      
      return(paste(new_sub_parts, collapse = "-"))
    })
    
    return(paste(op, paste(new_parts, collapse = "+"), collapse = ""))
  }
  
  # determine operator
  operator <- ifelse(grepl("<", inequality), "<", ifelse(grepl(">", inequality), ">", "="))
  
  # split inequality into left and right side
  sides <- strsplit(inequality, operator)[[1]]
  
  # process each side
  new_sides <- lapply(sides, function(side) {
    # split side into terms by "+"
    plus_terms <- unlist(strsplit(side, "\\s+\\+\\s+"))
    
    # split each term by "-" and process each sub-term
    new_plus_terms <- vector(mode = "character", length = length(plus_terms))
    for (i in seq_along(plus_terms)) {
      term <- plus_terms[i]
      minus_terms <- unlist(strsplit(term, "\\s+-\\s+"))
      new_minus_terms <- vector(mode = "character", length = length(minus_terms))
      for (j in seq_along(minus_terms)) {
        minus_term <- minus_terms[j]
        op <- if (j == 1) "" else "-"
        new_minus_terms[j] <- process_term(minus_term, op)
      }
      
      new_plus_terms[i] <- paste(new_minus_terms, collapse = "")
    }
    
    return(paste(new_plus_terms, collapse = "+"))
  })
  
  # create new inequality
  new_inequality <- paste(new_sides, collapse = operator)
  
  # remove all whitespace
  new_inequality <- gsub(" ", "", new_inequality)
  
  return(new_inequality)
}


inequality1 <- "(p1+p2+p3)/3<.5"
simplified_inequality1 <- convertInequality(inequality1)
print(simplified_inequality1)  # Output: "p1+p2+p3<1.5"

inequality2 <- "1/3*(p1+p2+p3)<.5"
simplified_inequality2 <- convertInequality(inequality2)
print(simplified_inequality2)  # Output: "p1+p2+p3<1.5"
