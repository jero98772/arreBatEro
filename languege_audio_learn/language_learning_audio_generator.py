#!/usr/bin/env python3
"""
Language Learning Audio Generator — Chatterbox Edition
=======================================================
Generates a vocabulary audio file for language learning by theme.
"""

import argparse
import json
import re
import sys
import time
from pathlib import Path
import librosa
import ollama
import torch
import torchaudio
import numpy as np
import soundfile as sf


# ─── Defaults ─────────────────────────────────────────────────────────────────

OLLAMA_MODEL        = "gemma3:4b"
WORDS_PER_THEME     = 2
TARGET_REPETITIONS  = 2
SILENCE_SHORT_S     = 1.2   # gap between words inside a pair
SILENCE_LONG_S      = 1.5   # gap between pairs

# Multilingual slow-speech model params
# cfg_weight  ↓ → slower, more deliberate pacing   (range 0.0–1.0, default 0.5)
# exaggeration↓ → calmer, flatter tone              (range 0.25–2.0, default 0.5)

CFG_NORMAL      = 12.0
EXAG_NORMAL     = 0.05

# BCP-47 codes for ChatterboxMultilingualTTS
LANG_CODES: dict[str, str] = {
    "arabic":     "ar",
    "danish":     "da",
    "german":     "de",
    "greek":      "el",
    "english":    "en",
    "spanish":    "es",
    "finnish":    "fi",
    "french":     "fr",
    "hebrew":     "he",
    "hindi":      "hi",
    "italian":    "it",
    "japanese":   "ja",
    "korean":     "ko",
    "malay":      "ms",
    "dutch":      "nl",
    "norwegian":  "no",
    "polish":     "pl",
    "portuguese": "pt",
    "russian":    "ru",
    "swedish":    "sv",
    "swahili":    "sw",
    "turkish":    "tr",
    "chinese":    "zh",
    "mandarin":   "zh",
}


# ─── Step 1 — Vocabulary via Ollama ──────────────────────────────────────────

def generate_vocabulary(theme: str, native: str, target: str, n: int) -> list[dict]:
    prompt = (
        f"You are a vocabulary generator for language learning.\n"
        f"Generate exactly {n} vocabulary words for the theme: \"{theme}\".\n"
        f"Return ONLY a valid JSON array — no markdown, no explanation, nothing else.\n"
        f"Each element MUST have exactly two keys:\n"
        f"  \"native\": the word in {native}\n"
        f"  \"target\": the word in {target}\n"
        f"STRICT RULES:\n"
        f"  - Every value must be a SINGLE word (no spaces, no phrases, no articles)\n"
        f"  - At least 2 characters long\n"
        f"  - A real vocabulary word (noun, verb or adjective — no proper names)\n"
        f"Correct example (animals, Spanish → German):\n"
        f'[{{"native":"gato","target":"Katze"}},{{"native":"perro","target":"Hund"}}]\n'
        f"Generate {n} words for \"{theme}\":"
    )

    print(f"🧠  Querying Ollama ({OLLAMA_MODEL}): theme='{theme}' | {native} → {target} …")
    resp = ollama.chat(
        model=OLLAMA_MODEL,
        messages=[{"role": "user", "content": prompt}],
        options={"temperature": 0.2},
    )
    raw = resp["message"]["content"].strip()
    raw = re.sub(r"```(?:json)?", "", raw).strip().strip("`").strip()

    m = re.search(r"\[.*\]", raw, re.DOTALL)
    if not m:
        raise ValueError(f"No JSON array in Ollama response:\n{raw}")
    return json.loads(m.group())


def validate_pairs(pairs: list[dict]) -> list[dict]:
    valid = []
    for p in pairs:
        n = str(p.get("native", "")).strip()
        t = str(p.get("target", "")).strip()
        if (
            n and t
            and len(n.split()) == 1
            and len(t.split()) == 1
            and len(n) >= 2
            and len(t) >= 2
            and not n.isdigit()
            and not t.isdigit()
        ):
            valid.append({"native": n, "target": t})
        else:
            print(f"  ⚠️  Discarded: native={n!r}  target={t!r}")
    return valid


# ─── Step 2 — Load TTS ───────────────────────────────────────────────────────

def detect_device() -> str:
    if torch.cuda.is_available():
        print(f"🚀  GPU: {torch.cuda.get_device_name(0)}")
        return "cuda"
    print("⚠️  CUDA not found — using CPU (slow)")
    return "cpu"


def load_multilingual(device: str):
    from chatterbox.mtl_tts import ChatterboxMultilingualTTS
    print("🌍  Loading Chatterbox-Multilingual (23+ languages, ~500M) …")
    return ChatterboxMultilingualTTS.from_pretrained(device=device)


# ─── Step 3 — Synthesise ─────────────────────────────────────────────────────

def to_numpy(wav) -> np.ndarray:
    if hasattr(wav, "numpy"):
        wav = wav.squeeze().cpu().numpy()
    return np.array(wav, dtype=np.float32).ravel()


def normalise(audio: np.ndarray, peak: float = 0.85) -> np.ndarray:
    m = np.abs(audio).max()
    return audio / m * peak if m > 0 else audio



def synth_multi(model, word: str, lang_code: str) -> np.ndarray:
    """Synthesise with Multilingual model. Slow mode uses low cfg_weight."""
    wav = model.generate(word, language_id=lang_code, exaggeration=EXAG_NORMAL, cfg_weight=CFG_NORMAL)
    return normalise(to_numpy(wav))


def silence(secs: float, sr: int) -> np.ndarray:
    return np.zeros(int(secs * sr), dtype=np.float32)


# ─── Step 4 — Assemble ───────────────────────────────────────────────────────

def build_audio(
    model,
    pairs:       list[dict],
    native_lang: str,
    target_lang: str,
    repeat:      int,
) -> tuple[np.ndarray, int]:

    sr = model.sr
    native_code = LANG_CODES.get(native_lang.lower(), "es")
    target_code = LANG_CODES.get(target_lang.lower(), "de")
    print(native_code,target_code)

    segs: list[np.ndarray] = [silence(SILENCE_LONG_S, sr)]

    for i, pair in enumerate(pairs, 1):
        nw = pair["native"]
        tw = pair["target"]
        print(nw,tw)
        #print(f"  [{i:>2}/{len(pairs)}]  {nw:>22}  ({native_lang})  "+f"→  {tw}  ({target_lang})  ×{repeat}")

        n_audio = synth_multi(model, nw, native_code)
        segs.append(n_audio)
        segs.append(silence(SILENCE_SHORT_S, sr))
        t_audio = synth_multi(model, tw, target_code)

        for _ in range(repeat):
            segs.append(t_audio)
            segs.append(silence(SILENCE_SHORT_S, sr))

        segs.append(silence(SILENCE_LONG_S, sr))

    return np.concatenate(segs), sr


# ─── Step 5 — Export ─────────────────────────────────────────────────────────

def export_wav(audio: np.ndarray, sr: int, path: Path) -> None:
    sf.write(str(path), audio, sr)
    dur  = len(audio) / sr
    size = path.stat().st_size / 1024 / 1024
    print(f"\n✅  Saved: {path}")
    print(f"   Duration   : {dur:.1f} s  ({dur / 60:.1f} min)")
    print(f"   File size  : {size:.2f} MB")
    print(f"   Sample rate: {sr} Hz")


# ─── CLI ─────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Language Learning Audio Generator — Chatterbox TTS",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
MODELS:
  Default (recommended): ChatterboxMultilingualTTS — 23+ languages
  --turbo               : ChatterboxTurboTTS — English ONLY, faster

SLOW SPEECH (default ON):
  Multilingual → cfg_weight=0.1, exaggeration=0.3  (model-native params)
  Turbo        → time-stretch ×1.4 via librosa (cfg/exag ignored by Turbo)
  --no-slow    → natural speaking pace
  --stretch N  → custom stretch ratio for Turbo (default 1.4)

SUPPORTED LANGUAGES:
  Arabic, Danish, German, Greek, English, Spanish, Finnish, French,
  Hebrew, Hindi, Italian, Japanese, Korean, Malay, Dutch, Norwegian,
  Polish, Portuguese, Russian, Swedish, Swahili, Turkish, Chinese/Mandarin

EXAMPLES:
  python language_audio_gen.py --theme animals   --native Spanish  --target German
  python language_audio_gen.py --theme food      --native English  --target French
  python language_audio_gen.py --theme colors    --native Spanish  --target Japanese --words 15 --repeat 4
  python language_audio_gen.py --theme numbers   --native Portuguese --target Korean --no-slow
  python language_audio_gen.py --theme greetings --native English  --target German  
        """,
    )
    p.add_argument("--theme",        required=True,  help="Vocabulary theme (e.g. animals, food, verbs)")
    p.add_argument("--native",       required=True,  help="Your native language (e.g. Spanish)")
    p.add_argument("--target",       required=True,  help="Language you are learning (e.g. German)")
    p.add_argument("--words",        type=int, default=WORDS_PER_THEME,
                   help=f"Words to generate (default: {WORDS_PER_THEME})")
    p.add_argument("--repeat",       type=int, default=TARGET_REPETITIONS,
                   help=f"Target-word repetitions (default: {TARGET_REPETITIONS})")
    p.add_argument("--output",       default=None,
                   help="Output WAV path (default: <theme>_<native>_<target>.wav)")
    p.add_argument("--ollama-model", default=OLLAMA_MODEL,
                   help=f"Ollama model (default: {OLLAMA_MODEL})")

    return p.parse_args()


def main() -> None:
    args = parse_args()

    global OLLAMA_MODEL
    OLLAMA_MODEL  = args.ollama_model
    output    = Path(args.output or f"{args.theme}_{args.native}_{args.target}.wav")


    t0 = time.time()

    # ── 1. Vocabulary ─────────────────────────────────────────────────────────
    raw   = generate_vocabulary(args.theme, args.native, args.target, args.words)
    print(raw)

    pairs = validate_pairs(raw)
    print(pairs)
    col = max(len(p["native"]) for p in pairs) + 2
    print(f"\n📋  {len(pairs)} valid word pairs:\n")
    for p in pairs:
        print(f"     {p['native']:>{col}}  →  {p['target']}")

    # ── 2. Load TTS ───────────────────────────────────────────────────────────
    print()
    device = detect_device()
    model  = load_multilingual(device)

    # ── 3. Synthesise ─────────────────────────────────────────────────────────
    audio, sr = build_audio(
        model, pairs, args.native, args.target, args.repeat
    )
    

    # ── 4. Export ─────────────────────────────────────────────────────────────
    export_wav(audio, sr, output)

    print(f"   Total time : {time.time() - t0:.1f} s\n")
    print("🎧  Tip: Import into Anki, VLC or any media player for studying!")


if __name__ == "__main__":
    main()