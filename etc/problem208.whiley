
import std::io

public export method main():
    int[] memo = [-1; 3796875]

    int dir = 0
    while dir <= 4:
        memo[dir] = 1
        dir = dir + 1

    int a = 0
    while a <= 14:
        int b = 0
        while b <= 14:
            int c = 0
            while c <= 14:
                int d = 0
                while d <= 14:
                    int e = 0
                    while e <= 14:
                        dir = 0
                        while dir <= 4:
                            if a == 0 && b == 0 && c == 0 && d == 0 && e == 0:
                                dir = dir + 1
                                continue
                            int memo_key = dir + (e * 5) + (d * 75) + (c * 1125) + (b * 16875) + (a * 253125)
                            int sum = 0
                            switch dir:
                                case 0:
                                    if a > 0:
                                        sum = sum + memo[(memo_key - 253125) + 1]
                                    if e > 0:
                                        sum = sum + memo[(memo_key - 5) + 4]
                                case 1:
                                    if b > 0:
                                        sum = sum + memo[(memo_key - 16875) + 1]
                                    if a > 0:
                                        sum = sum + memo[(memo_key - 253125) - 1]
                                case 2:
                                    if c > 0:
                                        sum = sum + memo[(memo_key - 1125) + 1]
                                    if b > 0:
                                        sum = sum + memo[(memo_key - 16875) - 1]
                                case 3:
                                    if d > 0:
                                        sum = sum + memo[(memo_key - 75) + 1]
                                    if c > 0:
                                        sum = sum + memo[(memo_key - 1125) - 1]
                                case 4:
                                    if e > 0:
                                        sum = sum + memo[(memo_key - 5) - 4]
                                    if d > 0:
                                        sum = sum + memo[(memo_key - 75) - 1]
                            memo[memo_key] = sum
                            dir = dir + 1
                        e = e + 1
                    d = d + 1
                c = c + 1
            b = b + 1
        a = a + 1
    io::println(memo[3796870])
