"""
FastAPI i18n demo
==================
Run with:  uvicorn main:app --reload

Demonstrates:
- Language selection via URL path prefix: /en/, /es/, /ru/
- Automatic redirect based on Accept-Language header at "/"
- A JSON API endpoint that also returns translated content
- Pluralization differences (Russian has 3 plural forms, EN/ES have 2)
- A simple translated contact form (POST)
"""

from datetime import date

from fastapi import FastAPI, Request, Form
from fastapi.responses import RedirectResponse
from fastapi.templating import Jinja2Templates
from fastapi.staticfiles import StaticFiles

import i18n

app = FastAPI(title="FastAPI i18n Demo")
templates = Jinja2Templates(directory="templates")


def render(request: Request, template_name: str, lang: str, **extra):
    """Helper to build the common template context."""
    lang = i18n.normalize_language(lang)

    def t(key: str, **kwargs):
        return i18n.t(lang, key, **kwargs)

    context = {
        "lang": lang,
        "languages": i18n.LANGUAGE_NAMES,
        "t": t,
        **extra,
    }
    return templates.TemplateResponse(request, template_name, context)


@app.get("/")
async def root(request: Request):
    """Detect preferred language from browser header and redirect."""
    accept_lang = request.headers.get("accept-language")
    lang = i18n.parse_accept_language(accept_lang)
    return RedirectResponse(url=f"/{lang}/")


@app.get("/{lang}/")
async def home(request: Request, lang: str, items: int = 0):
    lang = i18n.normalize_language(lang)
    return render(
        request,
        "home.html",
        lang,
        active_page="home",
        current_path="",
        visitor_name="Claude",
        today=date.today().isoformat(),
        item_count=items,
        submitted=False,
    )


@app.get("/{lang}/about")
async def about(request: Request, lang: str):
    lang = i18n.normalize_language(lang)
    return render(
        request,
        "about.html",
        lang,
        active_page="about",
        current_path="about",
    )


@app.post("/{lang}/contact")
async def contact(
    request: Request,
    lang: str,
    name: str = Form(...),
    email: str = Form(...),
    message: str = Form(""),
):
    lang = i18n.normalize_language(lang)
    # In a real app you'd persist/send this; here we just acknowledge it.
    return render(
        request,
        "home.html",
        lang,
        active_page="home",
        current_path="",
        visitor_name=name,
        today=date.today().isoformat(),
        item_count=0,
        submitted=True,
    )


@app.get("/api/{lang}/greeting")
async def api_greeting(lang: str, name: str = "World", items: int = 0):
    """Example JSON API endpoint returning translated strings."""
    lang = i18n.normalize_language(lang)
    return {
        "language": lang,
        "greeting": i18n.t(lang, "greeting", name=name),
        "items_message": i18n.t(lang, "items_count", count=items),
        "welcome_title": i18n.t(lang, "welcome_title"),
    }


@app.get("/api/languages")
async def api_languages():
    """List supported languages."""
    return {"supported": i18n.SUPPORTED_LANGUAGES, "names": i18n.LANGUAGE_NAMES}
