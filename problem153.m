
upper_limit = 100000000;

sums_of_squares = 1:upper_limit;

for a = 1:sqrt(upper_limit)
    b_values = 1:sqrt(upper_limit-a*a);
    for b = b_values(gcd(a, b_values) == 1)
        primitive = a * a + b * b;
        index = primitive:primitive:upper_limit;
        updates = (1:length(index)) * 2 * a;
        sums_of_squares(index) += updates;
    end
end

% Calculate all sums of squares.

format long;

% Now sum everything in one big go. We can't just use sum() for all of
% this, since double-precision floats are too small. But we can't
% iterate over the whole array because Octave is too slow to do that.
% So we iterate in batches, using sum() on intermediate batches.
weights = floor(upper_limit ./ (1:upper_limit));
arr = sums_of_squares .* weights;
total_sum = int64(0);
for i = 1:1000:upper_limit
    total_sum += int64(sum(arr(i:i+999)));
end
disp(total_sum);
