from langchain_core.messages import AIMessage, HumanMessage
from langchain_openai import ChatOpenAI
from langchain_core.output_parsers import StrOutputParser
from langchain_core.prompts import ChatPromptTemplate

def get_response(user_query, chat_history):
    template = """
    You are a helpful assistant. Answer the following questions considering the history of the conversation:

    Chat history: {chat_history}

    User question: {user_question}
    Example:

    
    # Initialize chat history
    chat_history = [
        AIMessage(content="Hello, I am a bot. How can I help you?")
    ]
    response_stream = get_response("hello", chat_history)


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



