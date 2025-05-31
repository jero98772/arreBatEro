import jpype
import jpype.imports
# Start the JVM with the classpath to your JAR
jpype.startJVM(classpath=["hello.jar"])
# Load the Scala class
Hello = jpype.JClass("Hello")
hello = Hello()
# Call methods
print(hello.greet("Alice"))
print(hello.sum(5, 7))
# Shutdown the JVM
jpype.shutdownJVM()


#scalac Hello.scala
#jar cf hello.jar Hello.class 
#python main.py