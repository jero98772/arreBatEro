
def generate_natural_voice(text, output_path="output.wav", model_name="tts_models/es/mai/tacotron2-DDC", speaker_wav=None):
    import torch
    import torchaudio
    from TTS.api import TTS
    import numpy as np
    
    device = "cuda" if torch.cuda.is_available() else "cpu"
    
    tts = TTS(model_name=model_name, progress_bar=False, gpu=torch.cuda.is_available())
    
    if speaker_wav:
        audio = tts.tts(text=text, speaker_wav=speaker_wav)
    else:
        audio = tts.tts(text=text)
    
    audio_tensor = torch.tensor(audio).unsqueeze(0)
    
    torchaudio.save(output_path, audio_tensor, sample_rate=22050)
    
    return output_path, audio
  

def transcribe_and_translate(audio_file):
    """Transcribe Russian audio to text and translate to English."""
    recognizer = sr.Recognizer()

    # Convert the audio file to .wav format
    audio = AudioSegment.from_file(audio_file)
    converted_file = "converted_audio.wav"
    audio.export(converted_file, format="wav")
    # Load the audio for transcription
    with sr.AudioFile(converted_file) as source:
        audio_data = recognizer.record(source)

    # Transcribe the Russian audio to text
    try:
        russian_text = recognizer.recognize_google(audio_data, language="ru-RU")
        # Use translate_llm function to translate the transcribed Russian text to English
        english_translation = translate_llm(russian_text, 'ru', 'en')
        return russian_text, english_translation
    except sr.UnknownValueError:
        return None, "Sorry, I could not understand the audio."
    except sr.RequestError as e:
        return None, f"Error: {e}"


import edge_tts
import asyncio

async def tts_to_file_edge(text, filename, language):
    print("edge", language)
    languages = {
        'en': 'en-US-JennyNeural',
        'es': 'es-ES-AlvaroNeural',
        'ru': 'ru-RU-SvetlanaNeural',
        'de': 'de-DE-KatjaNeural',
        #...
        'pl': 'pl-PL-AleksanderNeural',
    }
    voice = languages[language]
    communicate = edge_tts.Communicate(text, voice)
    await communicate.save(filename)
    print(f"Speech saved to {filename} with voice {voice}")

asyncio.run(tts_to_file_edge("Hello how are you", "output_en_edge.mp3", "en"))



def tts_to_file_gtts(text, filename, language):
    from gtts import gTTS

    tts = gTTS(text=text, lang=language, slow=False, tld="co.uk")
    tts.save(filename)

def text_to_speach(text, filename, language):
    print("tts",language)
    tts_to_file_edge(text,filename,language)
    #google delayed

  

#generate_natural_voice("Hola, como estas? que hiciste hoy", output_path="outputes.wav", model_name="tts_models/en/ljspeech/tacotron2-DDC",speaker_wav="yo.wav")
