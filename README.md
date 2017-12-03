# ProjectEuler

These are my Scala solutions and tests for all the Project Euler problems I've solved so far. 

You should be able to, e.g., start a new IntelliJ project by importing build.sbt file. 

Virtually all of the logic is organized in re-usable libraries in MathUtils package. 

The actual problem solutions in EulerProblems package basically just call the logic in various MathUtils.

Some of the Project Euler problems come with external resources (data input files) which are stored in src/main/resources directory.

There are extensive tests under /src/test both for all the actual Project Euler problems as well as all MathUtils libraries.
