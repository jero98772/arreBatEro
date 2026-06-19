# FastAPI i18n Demo (EN / ES / RU)

A minimal FastAPI site demonstrating internationalization with English,
Spanish, and Russian — including correct Russian pluralization (3 plural
forms: one / few / many).

## Run it

```bash
pip install -r requirements.txt
uvicorn main:app --reload
```

Then open http://localhost:8000/ — it auto-redirects based on your
browser's `Accept-Language` header, or visit a language directly:

- http://localhost:8000/en/
- http://localhost:8000/es/
- http://localhost:8000/ru/

## What it shows

- **URL-prefix routing**: `/en/`, `/es/`, `/ru/` — each page, including
  `/about`, is served in the matching language.
- **Accept-Language detection**: visiting `/` redirects to the best
  matching supported language.
- **String interpolation**: `{name}`, `{date}` placeholders filled per request.
- **Pluralization**: `/en|es|ru/?items=N` — try N = 0, 1, 2, 5, 11, 21, 22
  to see English/Spanish 2-form plurals vs. Russian's 3-form rule
  (один товар / два товара / пять товаров).
- **JSON API**: `GET /api/{lang}/greeting?name=...&items=...` returns
  translated strings as JSON — useful if you're building a frontend
  that consumes translations from an API instead of server-rendered HTML.
- **Form handling**: a POST `/​{lang}/contact` endpoint that re-renders
  the page with a translated success message.
- **Fallback**: unsupported language codes (e.g. `/de/`) silently fall
  back to English rather than erroring.

## File structure

```
fastapi-i18n/
├── main.py              # FastAPI app & routes
├── i18n.py              # Translation loading, lookup, pluralization logic
├── locales/
│   ├── en.json
│   ├── es.json
│   └── ru.json
└── templates/
    ├── base.html         # Layout + nav + language switcher
    ├── home.html
    └── about.html
```

## Adding a new language

1. Copy `locales/en.json` to `locales/<code>.json` and translate the values.
2. Add `<code>` to `SUPPORTED_LANGUAGES` and `LANGUAGE_NAMES` in `i18n.py`.
3. If the language has different plural rules than English, add a branch
   in `get_plural_form()` (see `_plural_form_ru` for an example of a
   3-form Slavic plural rule).

## Adding a new translation key

Add the key to all three JSON files, then use `{{ t('your_key') }}` in
templates or `i18n.t(lang, 'your_key')` in Python.
