from pydub import AudioSegment
import os

def hide_audio_in_cover(cover_path: str, 
                       secret_path: str, 
                       output_path: str, 
                       volume_reduction_db: float = -16.0):
    """
    Hides Audio B inside Audio A using the exact technique you described.
    """

    print("Loading audio files...")
    cover = AudioSegment.from_file(cover_path)
    secret = AudioSegment.from_file(secret_path)

    # Step 2: Process secret audio B
    print(f"Processing secret audio: -{abs(volume_reduction_db)} dB + Mono")
    secret = secret + volume_reduction_db          # Reduce volume
    secret = secret.set_channels(1)                # Convert to mono

    # Match sample rate and sample width to avoid artifacts
    if secret.frame_rate != cover.frame_rate:
        secret = secret.set_frame_rate(cover.frame_rate)
    if secret.sample_width != cover.sample_width:
        secret = secret.set_sample_width(cover.sample_width)

    # Step 3 & 4: Create two copies and invert one
    print("Duplicating and inverting phase...")
    secret_non_inverted = secret
    secret_inverted = secret.invert_phase()        # Built-in and reliable method

    # Step 5: Apply stereo panning
    print("Applying stereo channel routing...")
    b_left = secret_non_inverted.pan(-1.0)   # 100% Left
    b_right = secret_inverted.pan(1.0)       # 100% Right
    cover_right = cover.pan(1.0)             # Original A to Right

    # Mix everything
    print("Mixing tracks...")
    mixed = b_left.overlay(b_right)
    mixed = mixed.overlay(cover_right)

    # Export
    mixed.export(output_path, format="wav")
    print(f"✅ Success! Hidden file saved as: {output_path}")

    print("\n=== Playback Instructions ===")
    print("• Mono playback   → You should hear mainly the original Audio A")
    print("• Stereo playback → Audio B is clearly audible on the LEFT channel")
    print("   (Audio A is on the right with some cancellation)")


# ====================== USAGE ======================
if __name__ == "__main__":
    # Change these paths to your files
    AUDIO_A = "1.mp3"      # Cover file
    AUDIO_B = "2.mp3"      # Secret file to hide
    OUTPUT  = "output_hidden.wav"

    hide_audio_in_cover(AUDIO_A, AUDIO_B, OUTPUT, volume_reduction_db=-16.0)
