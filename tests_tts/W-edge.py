# Example usage
import asyncio
import edge_tts

async def tts_to_file(text, voice, output_file):
    communicate = edge_tts.Communicate(text, voice)
    await communicate.save(output_file)
    print(f"Speech saved to {output_file}")

# Run async function with different languages
async def generate_multilingual_samples():
    # English (US female)
    await tts_to_file(
        "This is an English example using Microsoft Edge TTS.",
        "en-US-JennyNeural",
        "edge_english.mp3"
    )
    
    # Spanish (Spain male)
    await tts_to_file(
        "Este es un ejemplo en español usando Microsoft Edge TTS.",
        "es-ES-AlvaroNeural",
        "edge_spanish.mp3"
    )
    
    # French (France female)
    await tts_to_file(
        "C'est un exemple en français utilisant Microsoft Edge TTS.",
        "fr-FR-DeniseNeural",
        "edge_french.mp3"
    )
    
    # German (Germany female)
    await tts_to_file(
        "Dies ist ein Beispiel auf Deutsch mit Microsoft Edge TTS.",
        "de-DE-KatjaNeural",
        "edge_german.mp3"
    )

# Run the async function
asyncio.run(generate_multilingual_samples())