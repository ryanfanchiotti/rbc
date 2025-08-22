main(argc, argv) {
    extrn puts, printf;
    auto i;
    i = 0;
    switch (argc) {
        case 1:
            puts("a");
            i = 1;
            auto j;
            j = 4 * 5;
            goto end;
        case 2:
            puts("b");
            goto end;
        case 3:
            puts("c");
        if (1 == 2) {
            case 4:
            puts("d");
        }
    }
    end:
    puts("ending");
    auto k;
    k = 5;
    printf("k = %d\n", k);
    return 42;
}
