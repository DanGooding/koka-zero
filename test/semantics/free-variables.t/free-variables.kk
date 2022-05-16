
fun f1(x1, y1) {
  fun f2(x2, y2) {
    fun f3(x3, y3) {
      fun g() {
        fun f4(x4, y4) {
          fun f5(x5, y5) {
            x1 + y1 + x2 + y2 + x3 + y3 + x4 + y4 + x5 + y5;
          };
          f5;
        };
        f4;
      };
      g;
    };
    f3;
  };
  f2;
}

fun main() {
  // 1 + 2 + ... + 10 = 55
  f1(1, 2)(3, 4)(5, 6)()(7, 8)(9, 10).println-int();
}
