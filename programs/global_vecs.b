myglob[] 22, 23, 24;

printarr() {
    auto i;
    extrn printf;
    i = 0;
    printf("myglob = %d\n", myglob);
    while (i < 3) {
        printf("myglob[%d] = %d\n", i, myglob[i]);
        i++;
    }
}

main(argc, argv) {
    printarr();
    myglob[2] = 0;
    printarr();
}
