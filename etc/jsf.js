
cached = {};

function is_prime(x) {
    if ((x+[]) in cached) {
        return cached[x+[]];
    }
    if (x <= 1)
        return false;
    var value = true;
    for (var i = 2; i < x; i++) {
        if (x % i == 0) {
            value = false;
            break;
        }
    }
    cached[x+[]] = value;
    return value;
}

function is_left_truncatable(x) {
    if (x == "")
        return true;
    else
        return is_prime(+x) && is_left_truncatable(x.substring(0, x.length - 1));
}

function is_right_truncatable(x) {
    if (x == "")
        return true;
    else
        return is_prime(+x) && is_right_truncatable(x.substring(1, x.length));
}

function is_truncatable(x) {
    return is_left_truncatable(x) && is_right_truncatable(x);
}

var sum = 0;
var count = 0;
var working = ["2", "3", "5", "7"];
while (count < 11) {
    var new_working = [];
    for (var i = 0; i < 10; i++) {
        for (var j = 0; j < working.length; j++) {
            if (is_truncatable(i + working[j])) {
                ++count;
                sum += +(i + working[j]);
            }
            if (is_right_truncatable(i + working[j])) {
                new_working.push(i + working[j]);
            }
        }
    }
    working = new_working;
}
alert(sum);
