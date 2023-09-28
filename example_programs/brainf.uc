/*
 * A simple BrainF interpreter written in uC.
 */

byte (ptr) {
  (*ptr & 255);
}

readFile(fileName) {
    file = fopen(fileName, "r");
    if (file == 0) {
      puts("Couldn't open file !");
      exit(1);
    }

    fseek(file, 0, 2);
    fileSize = ftell(file);
    fseek(file, 0, 0);
    buffer = calloc(fileSize + 16, 1);
    fread(buffer, 1, fileSize, file);

    (buffer);
}

main (argc, argv) {
  if (argc != 2) {
    puts("./Usage brainf.sc <filename>");
    exit(1);
  }

  memory = calloc(3000000 * 8, 1);
  contents = readFile(*(argv + 8));
  fileLength = strlen(contents);
  index = 3000000 / 2 * 8;
  codeIndex = 0;

  while (byte(contents + codeIndex) != 0) {
    c = byte(contents + codeIndex);
    //printf("Processing %d: %c, memory: %d\n ", codeIndex, c, index);
    if (c == *"+") {
      *(memory + index) = (*(memory + index) + 1) & 255;
    } else if (c == *"-") {
      *(memory + index) = (*(memory + index) - 1) & 255;
    } else if (c == *">") {
      index = index + 8;
    } else if (c == *"<") {
      index = index - 8;
      if (index < 0) {
        puts("Memory Index Negative !");
        exit(1);
      }
    } else if (c == *".") {
      printf("%c", *(memory + index));
    } else if (c == *",") {
      scanf("%c", memory + index);
    } else if (c == *"[" & *(memory + index) == 0 ) {
      balance = 0;
      continue = 1;
      while (continue & codeIndex != fileLength) {
        nc = byte(contents + codeIndex);
        if (nc == *"]") {
          balance = balance - 1;
          if (balance == 0)
            continue = 0;
        } else if (nc == *"[") {
          balance = balance + 1;
        }
        
        codeIndex = codeIndex + 1;
      }
    } else if (c == *"]" & *(memory + index) != 0) {
      balance = 0;
      continue = 1;
      while (continue & codeIndex != -1) {
        nc = byte(contents + codeIndex);
        if (nc == *"[") {
          balance = balance - 1;
          if (balance == 0)
            continue = 0;
        } else if (nc == *"]") {
          balance = balance + 1;
        }
        
        codeIndex = codeIndex - 1;
      }
    }

    codeIndex = codeIndex + 1;
  }

}
