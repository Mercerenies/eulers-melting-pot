
# Not going to be good enough. This is the same approach at
# problem170.cpp and was going to be the final answer for 170, but R
# is way too eager to copy arrays around for our solution, which
# depends on keeping intermediate data in mutable vectors/sets. I'll
# save R for an array-heavy challenge that doesn't require frequent
# mutation.

concatenated_product <- function(n, values) {
    paste(sapply(values, function(value) n * value), sep="", collapse="")
}

concatenated_product_except_last <- function(n, values) {
    concatenated_product(n, values[-length(values)])
}

is_pandigital <- function(string) {
    if (nchar(string) != 10) {
        return(FALSE)
    }
    chars <- strsplit(string, NULL)[[1]]
    all(sort(chars) == c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
}

repeats_digits <- function(string) {
    chars <- sort(strsplit(string, NULL)[[1]])
    any(chars[-length(chars)] == chars[0:-1])
}

# We represent used_digits as an array of ten Booleans.
build_result <- function(leading_value, values, used_digits, output_digit_count) {
    if (output_digit_count > sum(used_digits)) {
        return(0)
    }
    if (repeats_digits(concatenated_product_except_last(leading_value, values))) {
        return(0)
    }

    if (all(used_digits)) {
        # All digits used, check if the result is pandigital.
        product <- concatenated_product(leading_value, values)
        if (is_pandigital(product)) {
            return(as.integer(product))
        } else {
            return(0)
        }
    }

    if (sum(used_digits) <= 3) {
        print(sum(used_digits))
    }

    best_result <- 0

    # We could start a new number.
    for (i in 0:9) {
        if (used_digits[i + 1]) {
            next
        }
        new_values <- c(values, i)
        used_digits[i + 1] <- TRUE
        new_output_product <- as.character(leading_value * i)
        best_result <- max(best_result, build_result(leading_value, new_values, used_digits, output_digit_count + nchar(new_output_product)))
        used_digits[i + 1] <- FALSE
    }

    # Or we could continue the current number, if it exists and is not
    # a leading zero.
    if ((length(values) > 0) && (values[length(values)] != 0)) {
        for (i in 0:9) {
            if (used_digits[i + 1]) {
                next
            }
            old_output_product <- as.character(leading_value * values[length(values)])
            values[length(values)] <- values[length(values)] * 10 + i
            used_digits[i + 1] <- TRUE
            best_result <- max(best_result, build_result(leading_value, values, used_digits, output_digit_count - nchar(old_output_product) + nchar(new_output_product)))
            used_digits[i + 1] <- FALSE
            values[length(values)] <- values[length(values)] %/% 10
        }
    }

    best_result
}

run_for_leading_value <- function(leading_value) {
    values <- integer(0)
    used_digits <- logical(10)
    used_digits[(leading_value %% 10) + 1] = TRUE
    if (leading_value >= 10) {
        used_digits[(leading_value %/% 10) + 1] = TRUE
    }
    build_result(leading_value, values, used_digits, 0)
}

run_for_leading_value(9)
