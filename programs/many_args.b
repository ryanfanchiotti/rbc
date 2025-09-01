arg_taker(a, b, c, d, e, f, g, h, i) {
    extrn printf;
    printf("a = %d, b = %d, c = %d, d = %d, e = %d, f = %d, g = %d, h = %d, i = %d\n", a, b, c, d, e, f, g, h, i);
    return 22;
}

main(argc, argv) {
    auto a, b, c, d, e, f, g, h, i;
    a = 0;
    b = 1;
    c = 2;
    d = 3;
    e = 4;
    f = 5;
    g = 6;
    h = 7;
    i = 8;
    return arg_taker(a, b, c, d, e, f, g, h, i);
}
