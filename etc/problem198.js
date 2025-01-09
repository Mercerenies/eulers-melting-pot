
function gcd(a, b) {
    while (b != 0) {
        [a, b] = [b, a % b];
    }
    return a;
}

export function countFractionsJs(ratio, isAmbiguous) {
    let count = 0;
    for (let q = 1; q < 100000000; q++) {
        if (q % 1000 == 0) {
            console.log(q);
        }
        //for (let p = 1; p < q; p++) {
            let p = 1;
            //if (gcd(p, q) != 1) {
            //    continue;
            //}
            if (isAmbiguous(ratio(p)(q))) {
                count += 1;
            }
        //}
    }
    return count;
}
