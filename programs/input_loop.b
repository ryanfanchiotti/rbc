main(argc, argv) {
    extrn atoi, puts, printf;
    auto count, i;
    if (argc != 2) {
        puts("expected one argument");
        return 1;
    }
    count = atoi(argv[1]);
    i = 0;
    while (i++ < count) {
        printf("loop iteration %d\n", i);
    }
    return 0;
}
