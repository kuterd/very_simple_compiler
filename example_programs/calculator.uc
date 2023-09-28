/*
 * A simple calculator written in uC.
 */

char (ptr) {
  (*ptr & 255);
}

skipWhitespace(reader) {
  while(char(*reader) == *" " & char(*reader) != 0) {
    *reader = *reader + 1;
  }
}

parseInt(reader) {
    result = 0;
    sign = 1;
    if (**reader == *"-") {
        sign = -1;
        *reader = *reader + 1;
    }
    while (char(*reader) >= *"0" & char(*reader) <= *"9") {
        c = char(*reader);
        // I will overflow and feel no shame about it.
        result = result * 10;
        result = result + c - *"0";
        *reader = *reader + 1;
    }
    
    return result * sign;
}

evalMul(reader) {
  left = parseInt(reader);
  while (1) {
    skipWhitespace(reader);
    size = 0; 
    skipWhitespace(reader);
    op = char(*reader);

    if (op == *"*") {
        *reader = *reader + 1;
        skipWhitespace(reader);
        left = left * parseInt(reader);
    } else if(op == *"/") {
        *reader = *reader + 1;
        skipWhitespace(reader);
        left = left / parseInt(reader);
    } else {
        break;
    }
  }

  return left;
}

evalAdd(reader) {
  left = evalMul(reader);
  while (1) {
    skipWhitespace(reader);
    size = 0; 
    skipWhitespace(reader);
    op = char(*reader);

    if (op == *"+") {
      *reader = *reader + 1;
      skipWhitespace(reader);
      left = left + evalMul(reader);
    } else if(op == *"-") {
       *reader = *reader + 1;
       skipWhitespace(reader);
      left = left - evalMul(reader);
    } else {
      break;
    }
  }

  return left;
}


main(argc, argv) {
  if(argc != 2) {
    puts("Usage calculator.sc '10 * 23'");
    exit(1);
  }
  reader = *(argv + 8); 
  printf("Result: %d\n", evalAdd(&reader));
}
