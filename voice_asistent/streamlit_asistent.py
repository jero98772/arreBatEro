####
#### Streamlit Streaming using LM Studio as OpenAI Standin with Voice Features
#### run with `streamlit run app.py`

# Install required packages:
# pip install openai-whisper sounddevice numpy scipy pyttsx3 pypdf langchain langchain_openai streamlit-audiorecorder audioop-lts

import streamlit as st
from langchain_core.messages import AIMessage, HumanMessage
from langchain_openai import ChatOpenAI
from langchain_core.output_parsers import StrOutputParser
from langchain_core.prompts import ChatPromptTemplate
import whisper
from audiorecorder import audiorecorder

import pyttsx3
import tempfile
import sounddevice as sd
from scipy.io.wavfile import write

# Initialize TTS engine once
@st.cache_resource
def get_tts_engine():
    return pyttsx3.init()

# App config
st.set_page_config(page_title="Voice-enabled LM Studio Chatbot", page_icon="🤖")
st.title("Voice-enabled LM Studio Chatbot 🎙️")

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
        model="llama-3.2-1b-instruct"  # Match your local model name
    )

    chain = prompt | llm | StrOutputParser()
    
    return chain.stream({
        "chat_history": chat_history,
        "user_question": user_query,
    })

# Session state
if "chat_history" not in st.session_state:
    st.session_state.chat_history = [
        AIMessage(content="Hello, I am a bot. How can I help you?"),
    ]

# Sidebar for voice settings
with st.sidebar:
    st.header("Voice Settings")
    tts_enabled = st.checkbox("Enable Text-to-Speech", value=True)
    stt_enabled = st.checkbox("Enable Speech-to-Text", value=True)

# Conversation history
for message in st.session_state.chat_history:
    if isinstance(message, AIMessage):
        with st.chat_message("AI"):
            st.write(message.content)
    elif isinstance(message, HumanMessage):
        with st.chat_message("Human"):
            st.write(message.content)

# Voice input section
user_query = None
if stt_enabled:
    st.header("Voice Input")
    audio_bytes = audio_recorder(pause_threshold=2.0)
    
    if audio_bytes:
        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as fp:
            fp.write(audio_bytes)
            audio_path = fp.name
        
        # Transcribe audio using Whisper
        model = whisper.load_model("base")
        result = model.transcribe(audio_path)
        user_query = result["text"]

# Text input fallback
if not user_query:
    user_query = st.chat_input("Type your message or use voice input above...")

# Process query
if user_query:
    st.session_state.chat_history.append(HumanMessage(content=user_query))
    
    with st.chat_message("Human"):
        st.write(user_query)

    with st.chat_message("AI"):
        response_container = st.empty()
        full_response = ""
        tts_engine = get_tts_engine()
        word_buffer = ""
        
        for chunk in get_response(user_query, st.session_state.chat_history):
            full_response += chunk
            response_container.markdown(full_response + "▌")
            
            # TTS processing
            if tts_enabled:
                word_buffer += chunk
                # Speak when we hit a space or punctuation
                if chunk in [" ", ".", ",", "!", "?"]:
                    tts_engine.say(word_buffer)
                    tts_engine.runAndWait()
                    word_buffer = ""
        
        # Speak any remaining words
        if tts_enabled and word_buffer:
            tts_engine.say(word_buffer)
            tts_engine.runAndWait()
        
        response_container.markdown(full_response)
        st.session_state.chat_history.append(AIMessage(content=full_response))