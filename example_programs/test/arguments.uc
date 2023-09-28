/*
    Program that prints arguments.
*/

main (argc, args) {
    printf("arg count: %d\n", argc);
    i = 0;
    while (i < argc) {
        printf("args[%d]: %s\n", i, *(args + i * 8));
        i = i + 1;
    }
}