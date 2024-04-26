import jpype

def run_java_code():
    # Check if the JVM is running
    if not jpype.isJVMStarted():
        # Start the Java Virtual Machine (JVM)
        jpype.startJVM(jpype.getDefaultJVMPath())

    # Load the Java class
    java_class = jpype.JClass("Hello")

    # Call the Java method
    result = java_class.sayHello()

    # Return the result

    return result

if __name__ == "__main__":
    print(run_java_code())

    # Check if the JVM is running
    if jpype.isJVMStarted():
        # Shut down the JVM
        jpype.shutdownJVM()
