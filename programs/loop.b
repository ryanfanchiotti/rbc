main() {
    extrn printf;
    auto i;
    i = 0;
    while (i < 15) {
        printf("i = %d\n", i);
        i = i + 1;
    }
}