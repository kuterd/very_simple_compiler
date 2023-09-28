fib(n) {
  if (n <= 1)
    return 1;
  
  return fib(n - 1) + fib(n - 2);
}

main (argc, argv) {
  i = 0;
  while (i < 20) {
    printf("fib %d: %d\n", i, fib(i)); 
    i = i + 1;
  }
}
