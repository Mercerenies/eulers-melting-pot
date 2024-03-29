
// Simple direct computation, no tricks.

#include <vector>
#include <set>
#include <sstream>
#include <algorithm>
#include <string>
#include <iostream>

std::string concatenated_product(long n, const std::vector<long>& values) {
  std::ostringstream out;
  for (const auto &v : values) {
    out << (n * v);
  }
  return out.str();
}

std::string concatenated_product_except_last(long n, const std::vector<long>& values) {
  if (values.size() == 0) {
    return "";
  }
  std::ostringstream out;
  for (auto it = values.begin(); it != values.end() - 1; it++) {
    out << (n * (*it));
  }
  return out.str();
}

bool is_pandigital(const std::string& s) {
  std::string text { s };
  std::ranges::sort(text);
  return text == "0123456789";
}

bool is_pandigital(long n) {
  return is_pandigital(std::to_string(n));
}

bool repeats_digits(const std::string& p) {
  std::set<char> unique { std::begin(p), std::end(p) };
  return (unique.size() != p.length());
}

bool can_be_big_enough(long best_value_so_far, const std::string& product_so_far) {
  // Consider the largest number we could possibly build from product_so_far.
  long largest_product = std::stol(product_so_far);
  for (int i = product_so_far.size(); i < 10; i++) {
    largest_product = largest_product * 10 + 9;
  }
  return (largest_product > best_value_so_far);
}

long build_result(long& best_value_so_far, long leading_value, std::vector<long>& values, std::set<long>& used_digits, long output_digit_count) {
  if ((unsigned long)output_digit_count > used_digits.size()) {
    // We can short circuit out, because we have too many digits, so
    // we can't possibly get a pandigital number in the end.
    return 0L;
  }

  std::string product_so_far = concatenated_product_except_last(leading_value, values);
  if (repeats_digits(product_so_far)) {
    // We've already repeated a digit, so bail out.
    return 0L;
  }
  if ((product_so_far.size() > 0) && (!can_be_big_enough(best_value_so_far, product_so_far))) {
    // We already know of a larger solution, so short-circuit out.
    return 0L;
  }

  if (used_digits.size() == 10) {
    // All digits used, check if the result is pandigital.
    std::string product = concatenated_product(leading_value, values);
    if (is_pandigital(product)) {
      /*
      std::cout << product << " = " << leading_value << " with";
      for (const auto& value : values) {
        std::cout << " " << value;
      }
      std::cout << std::endl;
      */
      return std::stol(product);
    } else {
      return 0L;
    }
  }

  // We could start a new number.
  for (int i = 0; i < 10; i++) {
    if (used_digits.contains(i)) {
      continue;
    }
    values.push_back(i);
    used_digits.insert(i);
    std::string new_output_product = std::to_string(leading_value * values.back());
    best_value_so_far = std::max(build_result(best_value_so_far, leading_value, values, used_digits, output_digit_count + new_output_product.length()), best_value_so_far);
    used_digits.erase(i);
    values.pop_back();
  }

  // Or we could continue the current number, if it exists and is not
  // a leading zero.
  if ((values.size() > 0) && (values.back() != 0)) {
    for (int i = 0; i < 10; i++) {
      if (used_digits.contains(i)) {
        continue;
      }
      std::string old_output_product = std::to_string(leading_value * values.back());
      values.back() = values.back() * 10 + i;
      used_digits.insert(i);
      std::string new_output_product = std::to_string(leading_value * values.back());
      best_value_so_far = std::max(build_result(best_value_so_far, leading_value, values, used_digits, output_digit_count - old_output_product.length() + new_output_product.length()), best_value_so_far);
      used_digits.erase(i);
      values.back() /= 10;
    }
  }

  return best_value_so_far;
}

long build_result(long leading_value) {
  std::vector<long> values;
  std::set<long> used_digits;
  long best_value_so_far = 0L;
  used_digits.insert(leading_value % 10);
  if (leading_value >= 10) {
    used_digits.insert(leading_value / 10);
  }
  return build_result(best_value_so_far, leading_value, values, used_digits, 0L);
}

int main() {
  long best = 0L;
  // Note: The leading value cannot be zero (since all products would
  // be zero), or one (since all products would be the same length as
  // the original, and we need to gain a digit somewhere).
  //
  // The leading value must be less than 100. If it's greater than or
  // equal to 100, then even if there's only two numbers on the
  // right-hand side, we pick up four digits from multiplying by a
  // 3-digit number, and we can't possibly be pandigital in the end.
  //
  // As it turns out, since our short-circuiting behavior is optimized
  // to cut out when we already know of a bigger solution, it's faster
  // to start at 99 and go down rather than starting at 2 and going
  // up.
  for (int i = 99; i >= 2; i--) {
    if (i % 11 == 0) {
      // Trivially has a repeating digit
      continue;
    }
    std::cout << "leading = " << i << std::endl;
    best = std::max(best, build_result(i));
  }
  std::cout << best << std::endl;
}

// Final answer: 9857164023 = 27 * (36508, 149)
//
// This solution runs in about 20 seconds (requires -O3)
