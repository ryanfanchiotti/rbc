main(argc, argv) {
    extrn puts;
    extrn printf;
    if (argc < 3) {
        puts("Small arg count!");
    }
    printf("Arg count = %d\n", argc);
    return 42;
}
