main(argc, argv) {
    extrn printf;
    auto myprintfptr;
    myprintfptr = &printf;
    printf("Arg count = %d\n", argc);
    (*myprintfptr)("Arg count = %d\n", argc);
    return 42;
}
