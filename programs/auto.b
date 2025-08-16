main() {
    extrn printf;
    auto a, b;
    a = 1;
    b = a + 5;
    printf("a = %d, b = %d\n", a, b);
    return b - a;
}