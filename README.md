# ProjectEuler

These are my Scala solutions and tests for all the Project Euler problems I've solved so far. 

You should be able to, e.g., start a new IntelliJ project by import build.sbt file. 

Virtually all of the logic is organized in re-usable libraries in MathUtils package. 

The actual problems in EulerProblems package basically just call the logic in MathUtils.

There are extensive tests under /src/test both for all the actual Project Euler problems as well as all MathUtil libraries.
