/*
    Program that prints out fibonacci numbers.
*/

main () {
    a = 1;
    b = 0;
    i = 0;

    while (i < 20) {
        printf("%d:%d\n", i, a); // yes we can call libc functions !
        oldb = b;
        b = a;
        a = a + oldb;

        i = i + 1;
    }
}