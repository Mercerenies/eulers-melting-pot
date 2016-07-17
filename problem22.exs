import Enum

case File.read "./files/p022_names.txt" do
  {:ok, var} ->
    list = sort(String.split(String.replace(var, "\"", ""), ","))
    result = map(with_index(list, 1),
                  fn {val, ind} ->
                    ind * reduce(map(to_char_list(val), fn cp ->
                                                          cp - 64
                                    end), &+/2)
                  end)
    IO.puts reduce(result, &+/2)
  _ -> :no
end
