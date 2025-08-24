main() {
    extrn printf;
    auto z[2];
    z[0] = 'Hello wo';
    z[1] = 'rld!\0';
    printf("z = %s\n", z);
}
