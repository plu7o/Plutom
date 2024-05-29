let globalOne;
let globalTwo;

fn main() {
  for (let a = 1; a <= 2; a = a + 1) {
    fn closure() {
      print(a);
    }
    if globalOne == none: {
      globalOne = closure;
    } else {
      globalTwo = closure;
    }
  }
}

main();
globalOne();
globalTwo();
