main(argc, argv) {
    extrn printf;
    auto i;
    i = 1;
    while (i < argc) {
        printf("i = %d\n", i);
        printf("argv = %x\n", argv);
        // printf("%s ", *(argv + (8 * i)));
        printf("argv = %x\n", argv);
        i = i + 1;
    }
    printf("\n");
}