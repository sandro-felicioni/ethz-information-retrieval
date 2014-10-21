ethz-information-retrieval
==========================

Run SearchEngine.scala and use the following JVM arguments to make sure that you don't run out of memory:
  -  -Xss800m -Xms8G -Xmx8G


There is also a Java-wrapper Project called TiniIRExport to create a runnable jar file using eclipse. In order to run the jar file you need to specify the JVM parameter on the command line:
  - java -Xss800m -Xms8G -Xmx8G -jar wrapper.jar

There are 4 parameter arguments to specify (otherwise some default values are taken):
  - path to dataset containing all zips
  - path to query file
  - path to ground truth file
  - whether you want to use the term model (TM) or language model (LM)

For example:

java -Xss800m -Xms8G -Xmx8G -jar wrapper.jar ./TiniIR/tipster-dataset/zips/  ./TiniIR/tipster-dataset/topics ./TiniIR/tipster-dataset/qrels TM

