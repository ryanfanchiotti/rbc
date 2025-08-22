main(argc, argv) {
    extrn puts;
    switch (argc) {
        case 1:
            puts("a");
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
    puts("ending");
    end:
}
