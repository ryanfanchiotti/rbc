printvar(i) {
    extrn printf;
    printf("i = %d\n", i);
    return i;
}

main() {
    auto i;

    i = 1;
    while (i < 2 << 10) {
        printvar(i);
        i <<= 1;
    }
    return 255;
}
