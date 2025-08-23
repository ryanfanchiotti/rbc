main(argc) {
    auto x;
    extrn printf;

    x = argc > 1 ? 0 : 4;
    printf("x = %d\n", x);
}
