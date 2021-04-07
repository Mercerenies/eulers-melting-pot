
pub external fn format(a, b) -> Float = "io" "format"
pub external fn reverse(a) -> List(Int) = "lists" "reverse"
pub external fn sum(a) -> Int = "lists" "sum"
pub external fn length(a) -> Int = "erlang" "length"
pub external fn takewhile(a, b) -> List(Int) = "lists" "takewhile"
pub external fn nth(a, b) -> Int = "lists" "nth"
pub external fn append(a, b) -> List(Int) = "lists" "append"
pub external fn usort(a) -> List(Int) = "lists" "usort"

pub fn print(a) {
  format("~B~n", [a])
}

fn digits_impl(n) {
  case n < 10 {
    True -> [n]
    False -> {
      let result = digits_impl(n / 10)
      [n % 10, ..result]
    }
  }
}

pub fn digits(n) {
  reverse(digits_impl(n))
}

pub fn digitsum(n) {
  sum(digits(n))
}

pub fn biggestdigitsum(n) {
  9 * length(digits(n))
}

pub fn pow(a, b) {
  case b == 0 {
    True -> 1
    False -> pow(a, b - 1) * a
  }
}

fn findall_impl(power, base) {
  let curr = pow(base, power)
  case biggestdigitsum(curr) >= base {
    True -> {
      case digitsum(curr) == base {
        True -> [curr, ..findall_impl(power, base + 1)]
        False -> findall_impl(power, base + 1)
      }
    }
    False -> {
      []
    }
  }
}

pub fn findall(power) {
  findall_impl(power, 1)
}

pub fn confidentresults(power, answers) {
  let smallest = pow(2, power)
  takewhile(fn(x) { x < smallest }, answers)
}

pub fn confidentlyget(power, n, answers) {
  let results = confidentresults(power, answers)
  case length(results) < n {
    True -> -1
    False -> nth(n, results)
  }
}

fn run_impl(power, answers) {
  let newanswers = findall(power)
  let answers1 = usort(append(answers, newanswers))
  let result = confidentlyget(power, 31, answers1)
  case result >= 0 {
    True -> result
    False -> run_impl(power + 1, answers1)
  }
}

pub fn run() {
  run_impl(2, [])
}

pub fn main() {
  let answer = run()
  print(answer)
}
