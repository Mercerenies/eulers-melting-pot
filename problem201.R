
sum_cache <- new.env(hash = TRUE)
unique_cache <- new.env(hash = TRUE)

hash_key <- function(target, max_term, term_count) {
    paste(target, max_term, term_count, sep = ",")
}

sum_squares <- function(n) {
    n * (n + 1) * (2 * n + 1) / 6
}

sum_squares_from_to <- function(i, j) {
    sum_squares(j) - sum_squares(i - 1)
}

can_sum_to <- function(target, max_term, term_count) {
    if (term_count == 0) {
        target == 0
    } else if ((term_count < 0) || (max_term < term_count) || (target == 0)) {
        FALSE
    } else if ((target < sum_squares(term_count)) || (target > sum_squares_from_to(max_term - term_count + 1, max_term))) {
        FALSE
    } else {
        key <- hash_key(target, max_term, term_count)
        if (exists(key, envir = sum_cache)) {
            sum_cache[[key]]
        } else {
            result <- (
                can_sum_to(target - max_term * max_term, max_term - 1, term_count - 1) ||
                can_sum_to(target, max_term - 1, term_count)
            )
            sum_cache[[key]] <- result
            result
        }
    }
}

can_uniquely_sum_to <- function(target, max_term, term_count) {
    key <- hash_key(target, max_term, term_count)
    if (exists(key, envir = unique_cache)) {
        unique_cache[[key]]
    } else {
        result <- can_uniquely_sum_to_impl(target, max_term, term_count)
        unique_cache[[key]] <- result
        result
    }
}

can_uniquely_sum_to_impl <- function(target, max_term, term_count) {
    if (term_count == 0) {
        target == 0
    } else if (target < 0) {
        FALSE
    } else {
        found_one <- FALSE
        for (i in max_term:1) {
            if (can_sum_to(target - i * i, i - 1, term_count - 1)) {
                if (!can_uniquely_sum_to(target - i * i, i - 1, term_count - 1)) {
                    # Not unique
                    return(FALSE)
                }
                if (found_one) {
                    # Not unique
                    return(FALSE)
                } else {
                    found_one <- TRUE
                }
            }
        }
        found_one
    }
}

total_sum <- 0
i <- 42925
while (i <= 295425) {
    if (can_uniquely_sum_to(i, 100, 50)) {
        total_sum <- total_sum + i
    }
    i <- i + 1
}
total_sum
