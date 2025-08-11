
// Simple brute-force, nothing fancy

const PETER_OUTCOMES: u64 = 262144;
const COLIN_OUTCOMES: u64 = 46656;

fn decode_peter_outcome(outcome: u64) -> [u64; 9] {
  [outcome % 4 + 1, (outcome / 4) % 4 + 1, (outcome / 16) % 4 + 1, (outcome / 64) % 4 + 1,
   (outcome / 256) % 4 + 1, (outcome / 1024) % 4 + 1, (outcome / 4096) % 4 + 1,
   (outcome / 16384) % 4 + 1, (outcome / 65536) % 4 + 1]
}

fn decode_colin_outcome(outcome: u64) -> [u64; 6] {
  [outcome % 6 + 1, (outcome / 6) % 6 + 1, (outcome / 36) % 6 + 1, (outcome / 216) % 6 + 1, (outcome / 1296) % 6 + 1, (outcome / 7776) % 6 + 1]
}

fn main() {
  // Colin's dice can sum to a maximum of 36. This array shall be
  // indexed by Colin's score and gives the number of ways Peter can
  // beat him.
  let mut possible_peter_wins = [0; 37];
  for peter_outcome in 0..PETER_OUTCOMES {
    let peter_outcome: u64 = decode_peter_outcome(peter_outcome).iter().sum();
    for i in 0..=peter_outcome - 1 {
      possible_peter_wins[i as usize] += 1;
    }
  }

  let mut peter_wins: u64 = 0;
  for colin_outcome in 0..COLIN_OUTCOMES {
    let colin_outcome: u64 = decode_colin_outcome(colin_outcome).iter().sum();
    peter_wins += possible_peter_wins[colin_outcome as usize];
  }

  const TOTAL_OUTCOMES: u64 = PETER_OUTCOMES * COLIN_OUTCOMES;
  println!("{:.7}", (peter_wins as f64) / (TOTAL_OUTCOMES as f64));
}
