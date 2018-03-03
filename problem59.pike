
array(int) acceptable =
  map((array(string))"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz().,'!; ",
      lambda(string ch) { return (int)ch; });

array(int) read_file() {

  Stdio.File file = Stdio.File("./files/p059_cipher.txt");
  string text = file.read();
  file.close();

  return map(text / ",", lambda(string s) { return (int)s; });
}

array(int) decrypt(array(int) key, array(int) text) {
  array(int) result = ({});
  for (int index = 0; index < sizeof(text); index++) {
    int curr = key[index % 3] ^ text[index];
    if (Array.search_array(acceptable, lambda(int j) { return curr == j; }) < 0)
      return 0;
    result = Array.push(result, curr);
  }
  return result;
}

int main() {
  array(int) text = read_file();
  for (int i = 0; i <= 255; i++) {
    for (int j = 0; j <= 255; j++) {
      for (int k = 0; k <= 255; k++) {
        array(int) text1 = decrypt(({i, j, k}), text);
        if (text1 != 0) {
          write("%d\n", Array.sum(text1));
          return 0;
        }
      }
    }
  }
  return 0;
}
