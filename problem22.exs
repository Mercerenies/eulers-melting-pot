
import Enum

case File.read "./files/p022_names.txt" do
  {:ok, var} ->
    list = var |> String.replace("\"", "") |> String.split(",") |> sort
    result = with_index(list, 1) |> map(fn {word, index} ->
      alphabetical_value = to_charlist(word) |> map(fn cp -> cp - 64 end) |> reduce(&+/2)
      index * alphabetical_value
    end)
    IO.puts reduce(result, &+/2)
  _ -> :no
end
