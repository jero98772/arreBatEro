from TTS.api import TTS
import os

# Function to generate speech in different languages
def generate_multilingual_speech(output_dir="tts_outputs"):
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Get list of available models
    models = TTS.list_models()
    print("Available models:")
    for i, model in enumerate(models):
        print(f"{i}: {model}")
    
    # Dictionary mapping languages to appropriate models
    language_models = {
        "English": "tts_models/en/ljspeech/vits",
        "German": "tts_models/de/thorsten/vits",
        "French": "tts_models/fr/mai/vits",
        "Spanish": "tts_models/es/css10/vits",
        "Chinese": "tts_models/zh-CN/baker/vits",
        "Japanese": "tts_models/ja/kokoro/tacotron2-DDC"
    }
    
    # Example texts for each language
    texts = {
        "English": "This is an example of text-to-speech in English.",
        "German": "Dies ist ein Beispiel für Text-zu-Sprache auf Deutsch.",
        "French": "Ceci est un exemple de synthèse vocale en français.",
        "Spanish": "Este es un ejemplo de texto a voz en español.",
        "Chinese": "这是一个中文文本到语音的例子。",
        "Japanese": "これは日本語のテキスト読み上げの例です。"
    }
    
    # Generate speech for each language
    for language, model_name in language_models.items():
        try:
            print(f"\nProcessing {language}...")
            
            # Initialize TTS with appropriate model
            tts = TTS(model_name=model_name, progress_bar=False)
            
            # Generate speech
            output_file = os.path.join(output_dir, f"{language.lower()}_output.wav")
            tts.tts_to_file(text=texts[language], file_path=output_file)
            
            print(f"Generated: {output_file}")
            
        except Exception as e:
            print(f"Error processing {language}: {str(e)}")

# Run the function to generate speech in multiple languages
if __name__ == "__main__":
    generate_multilingual_speech()
    print("\nAll processing complete!")