from langchain_core.messages import AIMessage, HumanMessage
from langchain_openai import ChatOpenAI
from langchain_core.output_parsers import StrOutputParser
from langchain_core.prompts import ChatPromptTemplate

def get_response(user_query, chat_history):
    template = """
    You are a helpful assistant. Answer the following questions considering the history of the conversation:

    Chat history: {chat_history}

    User question: {user_question}
    """

    prompt = ChatPromptTemplate.from_template(template)

    llm = ChatOpenAI(
        base_url="http://localhost:1234/v1",
        api_key="not-needed",
        model="llama-3.2-1b-instruct"
    )

    chain = prompt | llm | StrOutputParser()
    
    return chain.stream({
        "chat_history": chat_history,
        "user_question": user_query,
    })


# Initialize chat history
chat_history = [
    AIMessage(content="Hello, I am a bot. How can I help you?")
]

print(chat_history[0].content)  # Print the initial bot message

while True:
    user_query = input("You: ")
    
    if user_query.lower() in ["exit", "quit"]:
        print("Goodbye!")
        break

    chat_history.append(HumanMessage(content=user_query))

    # Get response from LLM
    response_stream = get_response(user_query, chat_history)

    # Process streamed response
    response_text = ""
    for chunk in response_stream:
        print(chunk, end="", flush=True)  # Stream response in real-time
        response_text += chunk
    
    print("\n")  # Newline after the response

    # Append AI response to history
    chat_history.append(AIMessage(content=response_text))


