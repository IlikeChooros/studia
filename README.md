# studia
Ada my beloved

### Ada

#### Instalation

gnat & gprbuild

```bash

sudo apt-get update
sudo apt-get install gnat
sudo apt-get install gprbuild

```

#### Build and run

Build and run Ada programs with the following commands:

```bash

gnatmake [file].adb && ./[file]
./[file]

```

### Java

```bash
javac [file].java
java [file]
```

#### JavaFX

[Install JavaFX SDK](https://gluonhq.com/products/javafx/)

```bash
javac --module-path /opt/javafx-sdk-17.0.15/lib --add-modules javafx.controls [file].java
java --module-path /opt/javafx-sdk-17.0.15/lib --add-modules javafx.controls [file]
```


### C

#### Instalation

gcc

```bash

sudo apt-get update
sudo apt-get install gcc

```

Build and run C programs with the following commands:

```bash

gcc -o [file] [file].c -lm && ./[file]
./[file]

```

### Misc

Links:
```
https://cs.pwr.edu.pl/gebala/dyd/wip2024.html
https://learn.adacore.com/courses/intro-to-ada/chapters/imperative_language.html
https://adaic.org/resources/add_content/standards/05aarm/html/AA-5-5.html
```