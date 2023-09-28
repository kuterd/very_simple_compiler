test (a) {
    a("This is my message");
}

custom_log(message) {
    printf("MSG: %s\n", message);
}

main () {
    test(puts);
    test(custom_log);
}