
val values = [1, 2, 5, 10, 20, 50, 100, 200]

fun count_to(left, start_at) =
  if left < 0 then
      0
  else if left = 0 then
      1
  else
      foldl op+ 0 (List.map (fn x => count_to(left - x, x)) (List.filter (fn x => x <= start_at) values))

val _ = print(Int.toString(count_to(200, 200)))
