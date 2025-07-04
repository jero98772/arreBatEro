#python main.py
import jpype
import jpype.imports
from jpype.types import *
jpype.startJVM(classpath=["target/uberjar/clojure_hackasembler_web-0.1.0-SNAPSHOT-standalone.jar"])
asembler = jpype.JClass("clojure_hackasembler_web.core")
clojure = jpype.JPackage("clojure")
assemble = clojure.java.api.Clojure.var("clojure_hackasembler_web.core", "assemble")
test_code="""// Simple program to add two numbers
@2
D=A
@3
D=D+A
@0
M=D
// Loop example
@i
M=1
(LOOP)
@i
D=M
@100
D=D-A
@END
D;JGT
@i
M=M+1
@LOOP
0;JMP
(END)
@END
0;JMP"""
result = assemble.invoke(test_code)
print(f"The result is: {result}")
"""
"""
jpype.shutdownJVM()
