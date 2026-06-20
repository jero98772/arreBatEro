"""
Minimal i18n module for the FastAPI demo.

- Loads JSON translation files from /locales
- Supports {placeholder} interpolation
- Supports basic pluralization, including Russian's 3-form plural rules
"""

import json
from pathlib import Path
from typing import Any

LOCALES_DIR = Path(__file__).parent / "locales"
SUPPORTED_LANGUAGES = ["en", "es", "ru"]
DEFAULT_LANGUAGE = "en"

LANGUAGE_NAMES = {
    "en": "English",
    "es": "Español",
    "ru": "Русский",
}

_translations: dict[str, dict[str, Any]] = {}


def load_translations() -> None:
    """Load all translation JSON files into memory."""
    for lang in SUPPORTED_LANGUAGES:
        path = LOCALES_DIR / f"{lang}.json"
        with open(path, "r", encoding="utf-8") as f:
            _translations[lang] = json.load(f)


def normalize_language(lang: str | None) -> str:
    """Return a supported language code, falling back to default."""
    if lang and lang.lower() in SUPPORTED_LANGUAGES:
        return lang.lower()
    return DEFAULT_LANGUAGE


def parse_accept_language(header_value: str | None) -> str:
    """Pick the best supported language from an Accept-Language header."""
    if not header_value:
        return DEFAULT_LANGUAGE

    # Parse "ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7" style headers
    candidates = []
    for part in header_value.split(","):
        part = part.strip()
        if not part:
            continue
        if ";q=" in part:
            tag, q = part.split(";q=")
            try:
                weight = float(q)
            except ValueError:
                weight = 1.0
        else:
            tag, weight = part, 1.0
        primary = tag.strip().split("-")[0].lower()
        candidates.append((primary, weight))

    candidates.sort(key=lambda x: x[1], reverse=True)
    for primary, _ in candidates:
        if primary in SUPPORTED_LANGUAGES:
            return primary
    return DEFAULT_LANGUAGE


def _plural_form_ru(n: int) -> str:
    """Russian pluralization rule (3 forms: one, few, many)."""
    n_abs = abs(n)
    if n_abs % 10 == 1 and n_abs % 100 != 11:
        return "one"
    if 2 <= n_abs % 10 <= 4 and not (12 <= n_abs % 100 <= 14):
        return "few"
    return "many"


def _plural_form_en_es(n: int) -> str:
    """English/Spanish pluralization rule (2 forms: one, other)."""
    return "one" if abs(n) == 1 else "other"


def get_plural_form(lang: str, n: int) -> str:
    if lang == "ru":
        return _plural_form_ru(n)
    return _plural_form_en_es(n)


def t(lang: str, key: str, count: int | None = None, **kwargs) -> str:
    """
    Translate `key` into `lang`.
    If `count` is given, picks the correct plural form.
    Remaining kwargs are used for {placeholder} interpolation.
    """
    lang = normalize_language(lang)
    table = _translations.get(lang, _translations[DEFAULT_LANGUAGE])
    value = table.get(key)

    if value is None:
        # fall back to English, then to the raw key
        value = _translations[DEFAULT_LANGUAGE].get(key, key)

    if isinstance(value, dict):
        # pluralization dict, e.g. {"one": ..., "few": ..., "many": ..., "other": ...}
        if count is None:
            count = 0
        form = get_plural_form(lang, count)
        value = value.get(form, value.get("other", key))
        kwargs.setdefault("count", count)

    try:
        return value.format(**kwargs)
    except (KeyError, IndexError):
        return value


load_translations()
