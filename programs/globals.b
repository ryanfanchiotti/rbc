myglob 22;

main(argc, argv) {
    extrn myglob, printf;
    printf("myglob = %d\n", myglob);
    myglob = 4;
    printf("myglob = %d\n", myglob);
}
