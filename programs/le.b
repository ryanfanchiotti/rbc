main() {
    extrn printf;
    auto x;
    x = 5 < 2 + 6;
    printf("x = %d\n", x);
    x = x + 1;
    printf("x = %d\n", x);
    x = x + 1;
    printf("x = %d\n", x);
}
