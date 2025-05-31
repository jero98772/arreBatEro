import jpype
import jpype.imports
from jpype.types import *

# Start the JVM
jpype.startJVM(classpath=["target/uberjar/myapp-0.1.0-SNAPSHOT-standalone.jar"])

# Import your Clojure namespace class
MyAppCore = jpype.JClass("pythonclj.core")

# Since `add` is a Clojure var, we must require the namespace first
clojure = jpype.JPackage("clojure")
clojure.java.api.Clojure.var("clojure.core", "require").invoke(clojure.java.api.Clojure.read("myapp.core"))

# Get reference to the Clojure function
add_fn = clojure.java.api.Clojure.var("myapp.core", "add")

# Call the function
result = add_fn.invoke(10, 320)
print(f"Result from Clojure: {result}")

jpype.shutdownJVM()
