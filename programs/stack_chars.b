main() {
    extrn printf;
    auto z;
    z = 'abcde\0';
    printf("z = %s\n", &z);
}
