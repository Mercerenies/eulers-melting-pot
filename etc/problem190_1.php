<?php
function p($m) {
    $prod = 1;
    for ($i = 1; $i <= $m; $i++) {
        $prod *= (2 * $i / ($m + 1)) ** $i;
    }
    return $prod;
}

$sum = 0;
for ($i = 2; $i <= 15; $i++) {
    $sum += floor(p($i));
}
echo $sum;
?>
