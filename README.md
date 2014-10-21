ethz-information-retrieval
==========================

Run SearchEngine.scala and use the following JVM arguments to make sure that you don't run out of memory:
  -  -Xss800m -Xms8G -Xmx8G


There is also a Java-wrapper Project called TiniIRExport to create a runnable jar file using eclipse. In order to run the jar file you need to specify the JVM parameter on the command line:
  - java -Xss800m -Xms8G -Xmx8G -jar wrapper.jar
