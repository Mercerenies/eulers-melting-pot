
$memo = @(-1) * 3796875

# Base cases
for ($dir = 0; $dir -le 4; $dir++) {
    $memo[$dir] = 1
}

# Recursion
for ($a = 0; $a -le 14; $a++) {
    for ($b = 0; $b -le 14; $b++) {
        for ($c = 0; $c -le 14; $c++) {
            for ($d = 0; $d -le 14; $d++) {
                for ($e = 0; $e -le 14; $e++) {
                    for ($dir = 0; $dir -le 4; $dir++) {
                        if (($a -eq 0) -and ($b -eq 0) -and ($c -eq 0) -and ($d -eq 0) -and ($e -eq 0)) {
                            continue;
                        }
                        $memokey = $dir + $e * 5 + $d * 75 + $c * 1125 + $b * 16875 + $a * 253125
                        $sum = 0L
                        switch ($dir) {
                            0 {
                                if ($a -gt 0) {
                                    $sum += $memo[$memokey - 253125 + 1]
                                }
                                if ($e -gt 0) {
                                    $sum += $memo[$memokey - 5 + 4]
                                }
                            }
                            1 {
                                if ($b -gt 0) {
                                    $sum += $memo[$memokey - 16875 + 1]
                                }
                                if ($a -gt 0) {
                                    $sum += $memo[$memokey - 253125 - 1]
                                }
                            }
                            2 {
                                if ($c -gt 0) {
                                    $sum += $memo[$memokey - 1125 + 1]
                                }
                                if ($b -gt 0) {
                                    $sum += $memo[$memokey - 16875 - 1]
                                }
                            }
                            3 {
                                if ($d -gt 0) {
                                    $sum += $memo[$memokey - 75 + 1]
                                }
                                if ($c -gt 0) {
                                    $sum += $memo[$memokey - 1125 - 1]
                                }
                            }
                            4 {
                                if ($e -gt 0) {
                                    $sum += $memo[$memokey - 5 - 4]
                                }
                                if ($d -gt 0) {
                                    $sum += $memo[$memokey - 75 - 1]
                                }
                            }
                        }
                        $memo[$memokey] = $sum
                    }
                }
            }
        }
    }
}

$memokey = 3796870
Write-Output ("{0:f0}" -f $memo[$memokey])
