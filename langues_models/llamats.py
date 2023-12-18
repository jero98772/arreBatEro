from llama_cpp import Llama
model="/tmp/ggml-vicuna-13b-4bit-rev1.bin"
llm=Llama(model_path=model)
ans=llm("test it works?i wanna buy a fish")
print(ans)
print(ans["choices"][0]["text"])
#https://huggingface.co/eachadea/legacy-ggml-vicuna-13b-4bit/tree/main
#solve problem
#pip install --force-reinstall --ignore-installed --no-cache-dir llama-cpp-python
#pip install --force-reinstall --ignore-installed --no-cache-dir llama-cpp-python==0.1.48
#vicunia
