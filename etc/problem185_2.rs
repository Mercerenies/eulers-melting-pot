
// Rust solution using short-circuiting brute-force, just to see how
// efficient it is.
//
// Works instantly for the 5-digit but too slow for the full solution.

#[derive(Debug, Clone)]
struct Guess {
  guess: Vec<i32>,
  correct: i32,
}

impl Guess {
  fn new(guess: &str, correct: i32) -> Self {
    Self {
      guess: guess.chars().map(|c| c.to_digit(10).unwrap() as i32).collect(),
      correct,
    }
  }
}

fn solve(mut guesses: Vec<Guess>) -> Vec<i32> {
  assert!(!guesses.is_empty());
  let mut solution_vec = Vec::with_capacity(guesses[0].guess.len());
  for _ in 0..guesses[0].guess.len() {
    solution_vec.push(0);
  }
  solve_rec(&mut guesses, &mut solution_vec, 0);
  solution_vec
}

fn solve_rec(guesses: &mut [Guess], solution: &mut [i32], index: usize) -> bool {
  if index >= solution.len() {
    return guesses.iter().all(|guess| guess.correct == 0);
  }
  for value in 0..=9 {
    solution[index] = value;
    let mut bad_solution = false;
    for guess in guesses.iter_mut() {
      if guess.guess[index] == value {
        guess.correct -= 1;
      }
      if guess.correct < 0 || guess.correct as usize >= solution.len() - index {
        bad_solution = true;
      }
    }
    if !bad_solution {
      let found_solution = solve_rec(guesses, solution, index + 1);
      if found_solution {
        return true;
      }
    }
    for guess in guesses.iter_mut() {
      if guess.guess[index] == value {
        guess.correct += 1;
      }
    }
  }
  false
}

fn run_simple() -> String {
  let guesses = vec![
    Guess::new("90342", 2),
    Guess::new("70794", 0),
    Guess::new("39458", 2),
    Guess::new("34109", 1),
    Guess::new("51545", 2),
    Guess::new("12531", 1),
  ];
  let solution = solve(guesses);
  solution.into_iter().map(|i| i.to_string()).collect()
}

fn run_full() -> String {
  let guesses = vec![
    Guess::new("5616185650518293", 2),
    Guess::new("3847439647293047", 1),
    Guess::new("5855462940810587", 3),
    Guess::new("9742855507068353", 3),
    Guess::new("4296849643607543", 3),
    Guess::new("3174248439465858", 1),
    Guess::new("4513559094146117", 2),
    Guess::new("7890971548908067", 3),
    Guess::new("8157356344118483", 1),
    Guess::new("2615250744386899", 2),
    Guess::new("8690095851526254", 3),
    Guess::new("6375711915077050", 1),
    Guess::new("6913859173121360", 1),
    Guess::new("6442889055042768", 2),
    Guess::new("2321386104303845", 0),
    Guess::new("2326509471271448", 2),
    Guess::new("5251583379644322", 2),
    Guess::new("1748270476758276", 3),
    Guess::new("4895722652190306", 1),
    Guess::new("3041631117224635", 3),
    Guess::new("1841236454324589", 3),
    Guess::new("2659862637316867", 2),
  ];
  let solution = solve(guesses);
  solution.into_iter().map(|i| i.to_string()).collect()
}

fn main() {
  //let solution = run_simple();
  let solution = run_full();
  println!("{}", solution);
}
