import subprocess
import sys
import jpype


def build_classpath():
    result = subprocess.run(
        ["clojure", "-Spath"],
        capture_output=True,
        text=True,
        check=True,
    )
    return result.stdout.strip()


def clojure_to_python(obj):
    if obj is None:
        return None

    if isinstance(obj, (str, int, float, bool)):
        return obj

    cls_name = obj.getClass().getName()

    if cls_name == "clojure.lang.Keyword":
        return str(obj.getName())

    JMap = jpype.JClass("java.util.Map")
    JList = jpype.JClass("java.util.List")
    JIterable = jpype.JClass("java.lang.Iterable")

    if isinstance(obj, JMap):
        return {
            clojure_to_python(entry.getKey()): clojure_to_python(entry.getValue())
            for entry in obj.entrySet()
        }

    if isinstance(obj, JList) or isinstance(obj, JIterable):
        return [clojure_to_python(item) for item in obj]

    return str(obj)


def main():
    classpath = build_classpath()

    if not jpype.isJVMStarted():
        jpype.startJVM(classpath=[classpath])

    Clojure = jpype.JClass("clojure.java.api.Clojure")

    require_fn = Clojure.var("clojure.core", "require")
    require_fn.invoke(Clojure.read("ollama-structured"))

    get_structured_response = Clojure.var(
        "ollama-structured", "get-structured-response"
    )

    prompt = "Give me a famous sci-fi book."
    print(f"Calling Clojure with prompt: {prompt!r}")

    result = get_structured_response.invoke(prompt)

    if result is None:
        print("No structured content returned.")
        return

    py_result = clojure_to_python(result)
    print(py_result)


if __name__ == "__main__":
    main()
