
printarr(myauto) {
    auto i;
    extrn printf;
    i = 0;
    printf("myauto = %p\n", myauto);
    while (i < 3) {
        printf("myauto[%d] = %d\n", i, myauto[i]);
        i++;
    }
}

main(argc, argv) {
    auto myauto[3];
    myauto[0] = 22;
    myauto[1] = 23;
    myauto[2] = 24;
    printarr(myauto);
    myauto[2] = 0;
    printarr(myauto);
}
