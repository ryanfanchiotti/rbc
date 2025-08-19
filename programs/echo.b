main(argc, argv) {
    extrn printf;
    auto i;
    i = 1;
    while (i < argc) {
        printf("%s ", argv[i]);
        ++i;
    }
    printf("\n");
}
