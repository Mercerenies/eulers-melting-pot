Module: problem103

define function verify
    (arr :: <vector>) => (result :: <boolean>)
  block(return)
    // Condition 1
    let sums = make(<table>);
    sums[0] := 0;
    for (u in arr)
      let sums1 = make(<stretchy-vector>);
      for (s in sums)
        if (element(sums, s + u, default: #f))
          return(#f);
        end if;
        add!(sums1, s + u);
      end for;
      for (s in sums1)
        // Whee! Fun hack so we can iterate over values or keys and
        // get the same result.
        sums[s] := s;
      end for;
    end for;
    // Condition 2
    if (arr[0] + arr[1] < arr[6])
      return(#f);
    end if;
    if (arr[0] + arr[1] + arr[2] < arr[5] + arr[6])
      return(#f);
    end if;
    if (arr[0] + arr[1] + arr[2] + arr[3] < arr[4] + arr[5] + arr[6])
      return(#f);
    end if;
    // Clear
    #t
  end block
end function verify;

define function main
    (name :: <string>, arguments :: <vector>)

  let best = 255;
  let best_value = #f;

  for (a from 1 to 35)
    let bmax = ceiling/(241 - a, 6);
    for (b from a + 1 to bmax)
      let cmax = min(a + b, ceiling/(246 - a - b, 5));
      for (c from b + 1 to cmax)
        let dmax = min(a + b, ceiling/(250 - a - b - c, 4));
        for (d from c + 1 to dmax)
          let emax = min(a + b, ceiling/(251 - a - b - c - d, 3));
          for (e from d + 1 to emax)
            let fmax = min(a + b, ceiling/(255 - a - b - c - d - e, 2));
            for (f from e + 1 to fmax)
              let gmax = min(a + b, 256 - a - b - c - d - e - f);
              for (g from f + 1 to gmax)
                let arr = vector(a, b, c, d, e, f, g);
                if (reduce(gcd, 0, arr) == 1)
                  let sum = reduce(\+, 0, arr);
                  if (sum <= best & sum >= 128)
                    if (verify(arr))
                      best_value := arr;
                      best := sum;
                    end if;
                  end if;
                end if;
              end for;
            end for;
          end for;
        end for;
      end for;
    end for;
  end for;

  for (result in best_value)
    format-out("%d", result);
  end for;
  format-out("\n");

  exit-application(0);
end function main;

main(application-name(), application-arguments());
