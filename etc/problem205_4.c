
// Pretend doubles don't exist :)

#include <stdio.h>
#include <math.h>

#define TOTAL6 ((float)2038431744.0)

int main() {
  int possible_peter_wins[37] = {0};
  for (int peter_a = 1; peter_a <= 4; peter_a++) {
    for (int peter_b = 1; peter_b <= 4; peter_b++) {
      for (int peter_c = 1; peter_c <= 4; peter_c++) {
        for (int peter_d = 1; peter_d <= 4; peter_d++) {
          for (int peter_e = 1; peter_e <= 4; peter_e++) {
            for (int peter_f = 1; peter_f <= 4; peter_f++) {
              for (int peter_g = 1; peter_g <= 4; peter_g++) {
                for (int peter_h = 1; peter_h <= 4; peter_h++) {
                  for (int peter_i = 1; peter_i <= 4; peter_i++) {
                    int peter = peter_a + peter_b + peter_c + peter_d + peter_e + peter_f + peter_g + peter_h + peter_i;
                    for (int i = 0; i <= peter - 1; i++) {
                      possible_peter_wins[i] += 1;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  for (int i = 0; i < 37; i++) {
    printf("%d\n", possible_peter_wins[i]);
  }

  printf("%f\n", (float)2038431744.0);

  float peter_wins = 0.0;
  float peter_wins_low_bits = 0.0;
  for (int colin_a = 1; colin_a <= 6; colin_a++) {
    for (int colin_b = 1; colin_b <= 6; colin_b++) {
      for (int colin_c = 1; colin_c <= 6; colin_c++) {
        for (int colin_d = 1; colin_d <= 6; colin_d++) {
          for (int colin_e = 1; colin_e <= 6; colin_e++) {
            for (int colin_f = 1; colin_f <= 6; colin_f++) {
              int colin = colin_a + colin_b + colin_c + colin_d + colin_e + colin_f;
              peter_wins += possible_peter_wins[colin];
              peter_wins_low_bits += fmod(possible_peter_wins[colin] / TOTAL6 / 6.0, 0.0001);
              peter_wins_low_bits = fmod(peter_wins_low_bits, 0.0001);
            }
          }
        }
      }
    }
  }
  float answer_first_half = (int)((peter_wins / TOTAL6 / 6.0) / 0.0001) * 0.0001;
  float answer_second_half = peter_wins_low_bits + 0.00000005;
  printf("%.4f", answer_first_half);
  // Print the remaining three decimals that we need from answer_second_half.
  printf("%d%d%d\n", (int)(answer_second_half * 100000) % 10, (int)(answer_second_half * 1000000) % 10, (int)(answer_second_half * 10000000) % 10);
}
