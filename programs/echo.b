main(argc, argv) {
    extrn printf;
    auto i;
    i = 1;
    while (i < argc) {
        printf("%s ", *(argv + (8 * i)));
        i = i + 1;
    }
    printf("\n");
}
