myglob 22;

main(argc, argv) {
    extrn printf;
    printf("myglob = %d\n", myglob);
    myglob = 4;
    printf("myglob = %d\n", myglob);
}
