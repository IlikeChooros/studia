## How to run this shithole

Use lastest java and maven (https://docs.oracle.com/en/java/javase/25/install/installation-jdk-linux-platforms.html#GUID-CF001E7F-7E0D-49D4-A158-9CF3ED4C247C)


```sh
mvn exec:java -Dexec.mainClass=app.App
```

Generate UML class diagram (requires graphviz installed):

```sh
mvn javadoc:javadoc
```

[!NOTE]
> Download graphviz from https://graphviz.org/download/
> 
> Or just use:
> `apt-get install graphviz`

Then open [target/site/apidocs/index.html](target/site/apidocs/index.html) and click on "UML Class Diagram" link. 

Whole diagram at [target/site/apidocs/app/package.svg](target/site/apidocs/app/package.svg)