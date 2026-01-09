import asyncio
import os
from telegram import Update
from telegram.ext import Application, CommandHandler, MessageHandler, filters, ContextTypes, ConversationHandler
from openai import OpenAI
from TTS.api import TTS
from pydub import AudioSegment
import soundfile as sf
import tempfile
import json
import numpy as np

# Initialize OpenAI client for LM Studio
client = OpenAI(base_url="http://localhost:1234/v1", api_key="lm-studio")

# TTS Models configuration
TTS_MODELS = {
    "english": {"model": "tts_models/en/vctk/vits", "code": "en"},
    "spanish": {"model": "tts_models/es/css10/vits", "code": "es"},
    "russian": {"model": "tts_models/ru/ruslan/ruslan", "code": "ru"},  # Fixed Russian model
    "german": {"model": "tts_models/de/thorsten/tacotron2-DDC", "code": "de"},
    "french": {"model": "tts_models/fr/css10/vits", "code": "fr"},
    "italian": {"model": "tts_models/it/mai_male/glow-tts", "code": "it"},
    "portuguese": {"model": "tts_models/pt/cv/vits", "code": "pt"},
    "polish": {"model": "tts_models/pl/mai_female/vits", "code": "pl"},
    "turkish": {"model": "tts_models/tr/common-voice/glow-tts", "code": "tr"},
    "dutch": {"model": "tts_models/nl/css10/vits", "code": "nl"},
}

# Cache for loaded TTS models
tts_cache = {}

# Conversation states
TOPIC, LEARNING_LANG, NATIVE_LANG = range(3)

# Conversation states
TOPIC, LEARNING_LANG, NATIVE_LANG = range(3)

def chat_answer(messages):
    """Generate vocabulary using LM Studio"""
    completion = client.chat.completions.create(
        model="TheBloke/dolphin-2.2.1-mistral-7B-GGUF",
        messages=messages,
        temperature=0.7,  # Lower temperature for more consistent output
        max_tokens=800,  # Increased to accommodate 50 words
    )
    return completion.choices[0].message.content

def list_available_tts_models():
    """List all available TTS models - useful for debugging"""
    try:
        from TTS.api import TTS
        models = TTS().list_models()
        print("=" * 60)
        print("AVAILABLE TTS MODELS:")
        print("=" * 60)
        for model in models:
            if '/ru/' in model or 'russian' in model.lower():
                print(f"  Russian: {model}")
        print("=" * 60)
    except Exception as e:
        print(f"Could not list models: {e}")

def generate_vocabulary(topic, learning_lang, native_lang):
    """Generate vocabulary list using improved prompt"""
    prompt = f"""You must generate EXACTLY 50 single words in {learning_lang} about the topic: {topic}

STRICT RULES:
- Each line must contain ONLY ONE SINGLE WORD
- NO sentences, NO phrases, NO descriptions
- NO numbers, NO bullets, NO explanations
- Words must be in {learning_lang} language
- Related to topic: {topic}
- Mix of nouns, verbs, adjectives

Example of CORRECT format:
apple
run
beautiful
house
eat

Now generate 50 single words:"""

    messages = [
        {"role": "system", "content": f"You are a vocabulary generator. You ONLY output single words, one per line. Never output sentences, phrases, or explanations. Only output words in {learning_lang}."},
        {"role": "user", "content": prompt}
    ]
    
    response = chat_answer(messages)
    
    # Parse and clean the response
    lines = response.split('\n')
    words = []
    
    for line in lines:
        # Clean the line
        cleaned = line.strip()
        # Remove numbers, bullets, dashes at start
        cleaned = cleaned.lstrip('0123456789.-•*) ')
        # Remove any trailing punctuation
        cleaned = cleaned.rstrip('.,;:!?')
        
        # Only keep if it's a single word (no spaces) and not empty
        if cleaned and ' ' not in cleaned and len(cleaned) > 1:
            # Skip common non-words
            if cleaned.lower() not in ['the', 'a', 'an', 'and', 'or', 'but']:
                words.append(cleaned)
    
    # Remove duplicates while preserving order
    seen = set()
    unique_words = []
    for word in words:
        word_lower = word.lower()
        if word_lower not in seen:
            seen.add(word_lower)
            unique_words.append(word)
    
    print(f"Generated {len(unique_words)} unique words from response")
    
    # Return up to 50 words
    return unique_words[:50] if len(unique_words) >= 50 else unique_words

def get_tts_model(language):
    """Load and cache TTS model for a language"""
    lang_lower = language.lower()
    
    if lang_lower not in TTS_MODELS:
        # Default to English if language not supported
        print(f"Warning: Language '{language}' not supported, using English")
        lang_lower = "english"
    
    if lang_lower not in tts_cache:
        try:
            print(f"Loading TTS model for {language}...")
            model_name = TTS_MODELS[lang_lower]["model"]
            print(f"Model name: {model_name}")
            tts_cache[lang_lower] = TTS(model_name)
            print(f"✓ Model loaded successfully: {model_name}")
        except Exception as e:
            print(f"✗ Failed to load model for {language}: {e}")
            print(f"Full error: {type(e).__name__}: {str(e)}")
            raise Exception(f"Could not load TTS model for {language}: {str(e)}")
    
    return tts_cache[lang_lower]

def text_to_speech(text, language, output_file):
    """Convert text to speech using Coqui TTS"""
    try:
        tts = get_tts_model(language)
        
        # Check if model has multiple speakers
        speaker = None
        if hasattr(tts, "speakers") and tts.speakers and len(tts.speakers) > 0:
            # Use first speaker for consistency
            speaker = tts.speakers[0]
            print(f"Using speaker: {speaker}")
        
        # Generate speech
        print(f"Generating speech for: '{text}' in {language}")
        wav = tts.tts(text=text, speaker=speaker)
        
        # Save as WAV
        sf.write(output_file, wav, 22050)
        print(f"✓ Audio saved: {output_file}")
        return True
        
    except Exception as e:
        print(f"✗ TTS Error for '{text}' in {language}: {type(e).__name__}: {str(e)}")
        return False
    """Translate a single word using LM Studio"""
    prompt = f"Translate this word from {from_lang} to {to_lang}. Provide ONLY the translation, nothing else:\n\n{word}"
    
    messages = [
        {"role": "system", "content": "You are a translator. Provide only the direct translation of the word, no explanations."},
        {"role": "user", "content": prompt}
    ]
    
    translation = chat_answer(messages)
    return translation.strip().split('\n')[0].strip('.-:*" ')

def translate_word(word, from_lang, to_lang):
    """Translate a single word using LM Studio"""
    prompt = f"Translate this word from {from_lang} to {to_lang}. Provide ONLY the translation, nothing else:\n\n{word}"
    
    messages = [
        {"role": "system", "content": "You are a translator. Provide only the direct translation of the word, no explanations."},
        {"role": "user", "content": prompt}
    ]
    
    translation = chat_answer(messages)
    return translation.strip().split('\n')[0].strip('.-:*" ')

def create_audio_lesson(words, learning_lang, native_lang):
    """Create audio file with pattern: native word, target word (3x) using Coqui TTS"""
    audio_segments = []
    temp_files = []
    
    for i, word in enumerate(words):
        try:
            print(f"Processing word {i+1}/{len(words)}: {word}")
            
            # Translate word to native language
            native_word = translate_word(word, learning_lang, native_lang)
            
            # Create native language audio (WAV)
            native_audio_file = tempfile.NamedTemporaryFile(delete=False, suffix='.wav')
            temp_files.append(native_audio_file.name)
            
            if text_to_speech(native_word, native_lang, native_audio_file.name):
                native_audio = AudioSegment.from_wav(native_audio_file.name)
            else:
                print(f"Skipping word {word} - native TTS failed")
                continue
            
            # Create learning language audio (WAV)
            learning_audio_file = tempfile.NamedTemporaryFile(delete=False, suffix='.wav')
            temp_files.append(learning_audio_file.name)
            
            if text_to_speech(word, learning_lang, learning_audio_file.name):
                learning_audio = AudioSegment.from_wav(learning_audio_file.name)
            else:
                print(f"Skipping word {word} - learning TTS failed")
                continue
            
            # Combine: native word + pause + target word 3 times
            silence = AudioSegment.silent(duration=600)  # 600ms pause
            short_silence = AudioSegment.silent(duration=400)  # 400ms between repetitions
            
            segment = native_audio + silence
            for j in range(3):
                segment += learning_audio + short_silence
            segment += silence
            
            audio_segments.append(segment)
            
        except Exception as e:
            print(f"Error processing word '{word}': {e}")
            continue
    
    # Combine all segments
    if audio_segments:
        print("Combining all audio segments...")
        final_audio = sum(audio_segments)
        output_file = tempfile.NamedTemporaryFile(delete=False, suffix='.mp3')
        final_audio.export(output_file.name, format='mp3', bitrate="192k")
        
        # Clean up temporary files
        for temp_file in temp_files:
            try:
                os.unlink(temp_file)
            except:
                pass
        
        return output_file.name
    
    return None

# Bot handlers
async def start(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Start command handler"""
    supported_langs = ", ".join([l.title() for l in TTS_MODELS.keys()])
    
    await update.message.reply_text(
        "🎓 Welcome to the Vocabulary Learning Bot!\n\n"
        "I'll help you learn vocabulary with high-quality audio lessons using Coqui TTS.\n\n"
        f"🌍 Supported languages:\n{supported_langs}\n\n"
        "Please tell me the topic you want to learn about:"
    )
    return TOPIC

async def get_topic(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Store topic and ask for learning language"""
    context.user_data['topic'] = update.message.text
    await update.message.reply_text(
        f"Great! You want to learn about: {update.message.text}\n\n"
        "What language do you want to learn?\n"
        "(e.g., English, Spanish, French, German, Italian, etc.)"
    )
    return LEARNING_LANG

async def get_learning_lang(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Store learning language and ask for native language"""
    lang = update.message.text
    lang_lower = lang.lower()
    
    # Check if language is supported
    if lang_lower not in TTS_MODELS:
        supported = ", ".join([l.title() for l in TTS_MODELS.keys()])
        await update.message.reply_text(
            f"⚠️ Sorry, '{lang}' is not supported yet.\n\n"
            f"Supported languages:\n{supported}\n\n"
            f"Please choose one of these languages:"
        )
        return LEARNING_LANG
    
    context.user_data['learning_lang'] = lang
    await update.message.reply_text(
        f"Perfect! Learning language: {lang}\n\n"
        "What is your native language?\n"
        "(This will be used for translations)"
    )
    return NATIVE_LANG

async def generate_lesson(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Generate vocabulary and audio lesson"""
    native_lang = update.message.text
    native_lang_lower = native_lang.lower()
    
    # Check if native language is supported
    if native_lang_lower not in TTS_MODELS:
        supported = ", ".join([l.title() for l in TTS_MODELS.keys()])
        await update.message.reply_text(
            f"⚠️ Sorry, '{native_lang}' is not supported yet.\n\n"
            f"Supported languages:\n{supported}\n\n"
            f"Please choose one of these languages:"
        )
        return NATIVE_LANG
    
    context.user_data['native_lang'] = native_lang
    
    topic = context.user_data['topic']
    learning_lang = context.user_data['learning_lang']
    
    await update.message.reply_text(
        f"🔄 Generating vocabulary about '{topic}'...\n"
        f"Learning: {learning_lang}\n"
        f"Native: {native_lang}\n\n"
        f"⏳ This may take several minutes (loading AI models)..."
    )
    
    try:
        # Generate vocabulary
        words = generate_vocabulary(topic, learning_lang, native_lang)
        
        await update.message.reply_text(
            f"✅ Generated {len(words)} words!\n\n"
            f"🎵 Creating high-quality audio lesson with Coqui TTS...\n"
            f"Pattern: Native word → Target word (3x)\n\n"
            f"⏳ Please wait, this takes time for better quality..."
        )
        
        # Create audio
        audio_file = create_audio_lesson(words, learning_lang, native_lang)
        
        if audio_file:
            # Send audio file
            await update.message.reply_text(
                "📤 Uploading audio file..."
            )
            
            with open(audio_file, 'rb') as audio:
                await update.message.reply_audio(
                    audio=audio,
                    title=f"{topic} - {learning_lang} Vocabulary",
                    caption=f"🎧 Your vocabulary lesson is ready!\n\n"
                            f"Topic: {topic}\n"
                            f"Words: {len(words)}\n"
                            f"Language: {learning_lang}\n\n"
                            f"🔊 High-quality natural voice\n"
                            f"Listen and repeat! 📚"
                )
            
            # Send word list as text
            word_list = "\n".join([f"{i+1}. {word}" for i, word in enumerate(words)])
            await update.message.reply_text(f"📝 Word List:\n\n{word_list}")
            
            # Clean up
            os.unlink(audio_file)
            
            await update.message.reply_text(
                "✅ Done! Use /start to create another lesson."
            )
        else:
            await update.message.reply_text("❌ Error creating audio. Please try again.")
        
    except Exception as e:
        await update.message.reply_text(f"❌ Error: {str(e)}\n\nPlease try again with /start")
    
    return ConversationHandler.END

async def cancel(update: Update, context: ContextTypes.DEFAULT_TYPE):
    """Cancel the conversation"""
    await update.message.reply_text(
        "❌ Cancelled. Use /start to begin again."
    )
    return ConversationHandler.END

def main():
    """Start the bot"""
    # Replace with your bot token from @BotFather
    TOKEN = "YOUR_BOT_TOKEN_HERE"
    
    # Create application
    application = Application.builder().token(TOKEN).build()
    
    # Create conversation handler
    conv_handler = ConversationHandler(
        entry_points=[CommandHandler('start', start)],
        states={
            TOPIC: [MessageHandler(filters.TEXT & ~filters.COMMAND, get_topic)],
            LEARNING_LANG: [MessageHandler(filters.TEXT & ~filters.COMMAND, get_learning_lang)],
            NATIVE_LANG: [MessageHandler(filters.TEXT & ~filters.COMMAND, generate_lesson)],
        },
        fallbacks=[CommandHandler('cancel', cancel)],
    )
    
    application.add_handler(conv_handler)
    
    # Start the bot
    print("🤖 Bot is running...")
    application.run_polling(allowed_updates=Update.ALL_TYPES)

if __name__ == '__main__':
    main()
