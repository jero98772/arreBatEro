"""
Презентация MCP — Manim Slides
================================
Рендеринг:
    manim render mcp_presentation.py MCPPresentation -pql
Интерактивные слайды:
    manim-slides present MCPPresentation
Экспорт в HTML:
    manim-slides convert MCPPresentation presentation.html
"""

from __future__ import annotations

import os

from manim import *
from manim_slides import Slide

# ── Палитра ────────────────────────────────────────────────────────────────────
DARK = "#1A1916"  # Основной текст
SEC = "#73726C"  # Вторичный текст
TERT = "#9C9A92"  # Третичный текст
CRAIL = "#C15F3C"  # Крейл — единственный тёплый акцент
CLOUDY = "#B1ADA1"  # Облачный — приглушённый средний тон
PAMPAS = "#F4F3EE"  # Пампас — тёплый почти белый фон
BG = "#FFFFFF"  # Белый — фон слайда

# Устаревшие псевдонимы, сохранены для корректного разрешения всех ссылок
ACCENT = CRAIL
ACCENT2 = CLOUDY
ACCENT3 = TERT
ORANGE = CRAIL
MUTED = SEC

# ── Вспомогательные функции ────────────────────────────────────────────────────


def H(text: str, scale: float = 1.0, color: str = DARK, **kw) -> Text:
    return Text(text, color=color, weight=BOLD, **kw).scale(scale)


def B(text: str, scale: float = 0.6, color=DARK, **kw) -> Text:
    return Text(text, color=color, **kw).scale(scale)


def pill(label: str, color: str, w: float = 2.6, h: float = 0.75) -> VGroup:
    box = RoundedRectangle(
        corner_radius=0.15,
        width=w,
        height=h,
        fill_color=color,
        fill_opacity=0.10,
        stroke_color=color,
        stroke_width=1.5,
    )
    txt = Text(label, color=color, weight=BOLD).scale(0.58).move_to(box)
    return VGroup(box, txt)


def divider(
    title: str, color: str = CRAIL, text_color: str = DARK
) -> tuple[Rectangle, Text]:
    bar = Rectangle(
        width=config.frame_width,
        height=0.05,
        fill_color=color,
        fill_opacity=1,
        stroke_width=0,
    ).to_edge(UP, buff=0)
    lbl = B(title, scale=0.52, color=text_color, weight=BOLD).next_to(
        bar, DOWN, buff=0.14
    )
    return bar, lbl


def code_block(src: str, language: str = "python") -> Code:
    return Code(
        code_string=src,
        language=language,
        formatter_style="friendly",
        background="window",
        background_config={"fill_color": "#2b2b2b"},
        add_line_numbers=False,
    ).scale(0.85)


# ══════════════════════════════════════════════════════════════════════════════
class MCPPresentation(Slide):
    """Полная презентация MCP — белый фон."""

    # ── жизненный цикл ──────────────────────────────────────────────────────────
    def construct(self):
        self.camera.background_color = ManimColor(BG)

        self._slide_title()
        self._slide_about_me()
        self._slide_what_is_mcp()
        self._slide_mcp_engine()
        self._slide_why_tools()
        self._slide_strawberry()
        self._slide_langchain_tool_intro()
        self._slide_langchain_tool_flow()
        self._slide_tools_problems()
        self._slide_mcp_vs_cli()
        self._slide_wolfram()
        self._slide_network_protocol()
        self._slide_connect()
        self._slide_conclusions()
        self._slide_thanks()

    # ── утилиты ───────────────────────────────────────────────────────────────
    def _clear(self, t: float = 0.45):
        if self.mobjects:
            self.play(FadeOut(*self.mobjects), run_time=t)

    def _header(self, title: str, color: str = CRAIL, text_color: str = CRAIL):
        bar, lbl = divider(title, color, text_color)
        self.play(FadeIn(bar, run_time=0.3), Write(lbl, run_time=0.6))
        return lbl

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 1 — Заголовок
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_title(self):
        title = H("MCP — Протокол Контекста Модели", scale=1.05, color=CRAIL)
        subtitle = B("Даниэль Аранго Сом", color=DARK).scale(1.1)
        subtitle.next_to(title, DOWN, buff=0.32)
        line = Line(LEFT * 3.5, RIGHT * 3.5, color=CLOUDY, stroke_width=2)
        line.next_to(subtitle, DOWN, buff=0.28)

        group = VGroup(title, subtitle, line).move_to(ORIGIN)

        self.play(DrawBorderThenFill(title), run_time=1.1)
        self.play(FadeIn(subtitle, shift=UP * 0.2))
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 2 — О себе
    # ══════════════════════════════════════════════════════════════════════════

    def _slide_about_me(self):
        anchor = self._header(
            "О себе",
        )
        # ── Имя и должность ───────────────────────────────────────────────────────
        name = H("Даниэль Аранго Сом", scale=0.85)
        name.next_to(anchor, DOWN, buff=0.35)
        name.set_x(0)
        role_lines = [
            ("Студент EAFIT  •  Инженер в EPAM", DARK, 0.52),
            ("Лидер (диктатор) ML EAFIT", CRAIL, 0.50),
        ]
        role_mobs = []
        prev = name
        for text, color, scale in role_lines:
            m = B(text, color=color, scale=scale)
            m.next_to(prev, DOWN, buff=0.12)
            m.set_x(0)
            role_mobs.append(m)
            prev = m
        # ── Флаги ───────────────────────────────────────────────────────────────
        flags = B("(Ко) 50%   (Не) 25%   (Ру/Ук) 25%", color=DARK, scale=0.52)
        flags.next_to(role_mobs[-1], DOWN, buff=0.18)
        flags.set_x(0)
        # ── Значки докладчика ────────────────────────────────────────────────
        speaker_title = B("Докладчик:", color=CRAIL, weight=BOLD, scale=0.50)
        speaker_title.next_to(flags, DOWN, buff=0.22)
        speaker_title.set_x(0)
        talks = [
            "Python Moscow 2024",
            "Python Medellín",
            "PyCon Colombia 2025",
            "Medellín JS",
        ]
        talk_mobs = []
        prev = speaker_title
        for t in talks:
            m = B(f"• {t}", color=SEC, scale=0.48)
            m.next_to(prev, DOWN, buff=0.10)
            m.set_x(0)
            talk_mobs.append(m)
            prev = m
        # ── Награды ───────────────────────────────────────────────────────────
        awards = [
            ("🏆 Лучший проект CS EAFIT  2022-1, 2023-1, 2024-1", SEC),
            ("🥇 Хакатон Claude — 1-е место (сделан за один день)  2025-2", CRAIL),
        ]
        award_prev = talk_mobs[-1]
        award_mobs = []
        for text, color in awards:
            m = B(text, color=color, weight=BOLD, scale=0.50)
            m.next_to(award_prev, DOWN, buff=0.18)
            m.set_x(0)
            award_mobs.append(m)
            award_prev = m
        # ── Анимация ───────────────────────────────────────────────────────────
        self.play(Write(name))
        self.play(
            LaggedStart(
                *[FadeIn(r, shift=RIGHT * 0.15) for r in role_mobs], lag_ratio=0.25
            )
        )
        self.play(FadeIn(flags))
        self.next_slide()
        self.play(FadeIn(speaker_title))
        self.play(
            LaggedStart(
                *[FadeIn(t, shift=RIGHT * 0.1) for t in talk_mobs], lag_ratio=0.2
            )
        )
        self.play(
            LaggedStart(
                *[FadeIn(a, shift=UP * 0.12) for a in award_mobs], lag_ratio=0.3
            )
        )
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 3 — Что такое MCP?
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_what_is_mcp(self):
        anchor = self._header("Что такое MCP?")

        created_by = B("Создан компанией  Anthropic", color=TERT, scale=0.55).next_to(
            anchor, DOWN, buff=0.35
        )

        server = pill("MCP Сервер", CRAIL, w=3.2)
        client = pill("MCP Клиент", CLOUDY, w=3.2)

        server_sub = B(
            "Предоставляет инструменты и ресурсы", color=SEC, scale=0.42
        ).next_to(server, DOWN, buff=0.1)
        client_sub = B(
            "Приложение, вызывающее инструменты через LLM", color=SEC, scale=0.42
        ).next_to(client, DOWN, buff=0.1)

        s_grp = VGroup(server, server_sub).shift(LEFT * 2.9 + DOWN * 0.5)
        c_grp = VGroup(client, client_sub).shift(RIGHT * 2.9 + DOWN * 0.5)

        arrow = DoubleArrow(
            server.get_right(),
            client.get_left(),
            buff=0.12,
            color=TERT,
            stroke_width=2.5,
        )
        proto = B("JSON-RPC  /  stdio  /  HTTP SSE", color=TERT, scale=0.38).next_to(
            arrow, UP, buff=0.38
        )

        self.play(Write(created_by))
        self.play(FadeIn(s_grp), FadeIn(c_grp))
        self.play(GrowArrow(arrow), FadeIn(proto))
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 4 — MCP как движок
    # ══════════════════════════════════════════════════════════════════════════

    def _slide_mcp_engine(self):
        anchor = self._header("MCP как движок")

        row1_data = [
            ("MCP — это движок, позволяющий LLM", DARK),
        ]
        row2_data = [
            ("подключаться", CRAIL),
            ("  к  ", DARK),
            ("внешним", SEC),
            ("  инструментам как  ", DARK),
            ("функциям кода", TERT),
        ]

        row1 = VGroup(
            *[
                B(w, color=c, weight=BOLD if c != DARK else NORMAL, scale=0.62)
                for w, c in row1_data
            ]
        ).arrange(RIGHT, buff=0.12)

        row2 = VGroup(
            *[
                B(w, color=c, weight=BOLD if c != DARK else NORMAL, scale=0.62)
                for w, c in row2_data
            ]
        ).arrange(RIGHT, buff=0.12)

        headline = (
            VGroup(row1, row2).arrange(DOWN, buff=0.15).next_to(anchor, DOWN, buff=0.5)
        )
        headline.set_x(0)

        parts = [*row1, *row2]

        llm = pill("LLM", SEC, w=2.4, h=0.9)
        mcp = pill("MCP", CRAIL, w=2.4, h=0.9)
        tools = pill("Инструменты / API", CLOUDY, w=2.6, h=0.9)
        chain = (
            VGroup(llm, mcp, tools)
            .arrange(RIGHT, buff=1.2)
            .next_to(headline, DOWN, buff=0.65)
        )
        chain.set_x(0)
        a1 = Arrow(
            llm.get_right(), mcp.get_left(), buff=0.1, color=CRAIL, stroke_width=2.5
        )
        a2 = Arrow(
            mcp.get_right(), tools.get_left(), buff=0.1, color=CLOUDY, stroke_width=2.5
        )
        note = B(
            "LLM решает КОГДА и КАК вызывать инструмент — MCP стандартизирует КАК",
            color=SEC,
            scale=0.48,
        ).next_to(chain, DOWN, buff=0.5)
        note.set_x(0)
        self.play(LaggedStart(*[Write(p) for p in parts], lag_ratio=0.06))
        self.play(FadeIn(llm), FadeIn(mcp), FadeIn(tools))
        self.play(GrowArrow(a1), GrowArrow(a2))
        self.play(FadeIn(note))
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 5 — Зачем нужны инструменты? + мем
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_why_tools(self):
        anchor = self._header("Зачем инструменты? — Проблема следующего токена")

        question = B(
            "Если LLM просто предсказывает следующее слово...", color=DARK, scale=0.65
        ).next_to(anchor, DOWN, buff=0.38)
        follow = B(
            "...как она может ДЕЛАТЬ что-то в реальном мире?", color=CRAIL, scale=0.62
        ).next_to(question, DOWN, buff=0.12)

        # Блоки токенов
        token_words = ["Сле", "-дую", "-щий", " то", "-кен..."]
        token_boxes = (
            VGroup(
                *[
                    VGroup(
                        RoundedRectangle(
                            corner_radius=0.1,
                            width=max(0.65, len(w) * 0.19),
                            height=0.55,
                            fill_color=PAMPAS,
                            fill_opacity=1,
                            stroke_color=CLOUDY,
                            stroke_width=1,
                        ),
                        B(w, color=DARK, scale=0.48),
                    ).arrange(ORIGIN)
                    for w in token_words
                ]
            )
            .arrange(RIGHT, buff=0.12)
            .next_to(follow, DOWN, buff=0.45)
        )

        last_q = B("  ???", color=CRAIL, weight=BOLD, scale=0.7).next_to(
            token_boxes, RIGHT, buff=0.1
        )

        solution = B(
            "Решение: движок инструментов, например @tool из langchain —\nнаучить модель вызывать функции!",
            color=TERT,
            weight=BOLD,
            scale=0.55,
        ).next_to(token_boxes, DOWN, buff=0.5)

        self.play(Write(question))
        self.play(FadeIn(follow))
        self.play(
            LaggedStart(*[FadeIn(t) for t in token_boxes], lag_ratio=0.12),
            FadeIn(last_q),
        )
        self.next_slide()
        self.play(Write(solution))

        # meme.jpg если есть
        meme_path = "meme.jpg"
        if os.path.exists(meme_path):
            meme = (
                ImageMobject(meme_path).scale_to_fit_width(3.2).to_corner(DR, buff=0.55)
            )
            self.play(FadeIn(meme))

        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 6 — Проблемы, которые решают инструменты  +  1.png
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_tools_problems(self):
        anchor = self._header("Проблемы, которые решают инструменты")

        bullets = [
            (
                CRAIL,
                "Данные в реальном времени",
                "У LLM есть дата отсечения — инструменты получают актуальную информацию",
            ),
            (
                SEC,
                "Вычисления",
                "LLM не умеет точно считать, вычислять или строго рассуждать",
            ),
            (TERT, "Побочные эффекты", "Запись файлов, отправка писем, вызов API"),
            (CRAIL, "Внешнее состояние", "Базы данных, календари, запуск кода"),
        ]

        img_path = "1.png"
        has_img = os.path.exists(img_path)

        if has_img:
            img = (
                ImageMobject(img_path)
                .scale_to_fit_width(4.5)
                .to_edge(RIGHT, buff=0.4)
                .shift(DOWN * 0.4)
            )
            self.play(FadeIn(img))

        bullet_mobs = []
        for color, title, desc in bullets:
            t = B(f"  {title}", color=color, weight=BOLD, scale=0.57)
            d = B(f"    {desc}", color=DARK, scale=0.46)
            bullet_mobs.append(VGroup(t, d).arrange(DOWN, aligned_edge=LEFT, buff=0.04))

        group = VGroup(*bullet_mobs).arrange(DOWN, aligned_edge=LEFT, buff=0.32)
        group.to_edge(LEFT, buff=0.55).shift(DOWN * 0.3)

        self.play(
            LaggedStart(
                *[FadeIn(b, shift=RIGHT * 0.25) for b in bullet_mobs], lag_ratio=0.3
            )
        )
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 7 — Клубника / токенизация
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_strawberry(self):
        anchor = self._header("LLM не умеет считать — Проблема клубники")

        question = B(
            'Сколько букв  "r"  в слове  "strawberry"?',
            color=DARK,
            weight=BOLD,
            scale=0.68,
        )
        question.next_to(anchor, DOWN, buff=0.4)

        tok_label = B(
            "Токенизатор разбивает слово на части\n(токенизатор BPE от ChatGPT):",
            color=SEC,
            scale=0.52,
        ).next_to(question, DOWN, buff=0.38)

        strt = pill("str", CRAIL, w=2.5)
        plus = B("+", color=TERT, weight=BOLD, scale=0.9)
        aw = pill("aw", CLOUDY, w=2.5)
        berry = pill("berry", CRAIL, w=2.5)
        tokens_row = (
            VGroup(strt, plus, aw, plus, berry)
            .arrange(RIGHT, buff=0.3)
            .next_to(tok_label, DOWN, buff=0.3)
        )

        strt_r = B('1 буква "r"  здесь', color=CRAIL, scale=0.44).next_to(
            strt, DOWN, buff=0.1
        )
        aw_r = B('0 букв "r"  здесь', color=CLOUDY, scale=0.44).next_to(
            aw, DOWN, buff=0.1
        )
        berry_r = B('1 буква "r"  здесь', color=CRAIL, scale=0.44).next_to(
            berry, DOWN, buff=0.1
        )

        warning = B(
            "LLM видит токены, а не символы\n  =>  пропускает лишнюю 'r' в 'straw'!",
            color=CRAIL,
            weight=BOLD,
            scale=0.5,
        ).next_to(strt_r, DOWN, buff=0.38)

        src = 's = "strawberry"\n' 'print(s.count("r"))  # -> 3'
        cblock = code_block(src).next_to(warning, DOWN, buff=0.35)

        self.play(Write(question))
        self.play(FadeIn(tok_label))
        self.play(FadeIn(strt), FadeIn(plus), FadeIn(aw), FadeIn(plus), FadeIn(berry))
        self.play(FadeIn(strt_r), FadeIn(aw_r), FadeIn(berry_r))
        self.play(Write(warning))
        self.next_slide()
        self.play(FadeIn(cblock))
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД — От функции Python к инструменту LangChain
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_langchain_tool_intro(self):
        anchor = self._header("От функции Python к инструменту LangChain")

        plain_src = (
            "def count_letter(text: str, letter: str) -> int:\n"
            '    """Подсчитать, сколько раз буква встречается в строке."""\n'
            "    return text.count(letter)\n\n"
            'print(count_letter("strawberry", "r"))  # -> 3'
        )
        tool_src = (
            "from langchain.tools import tool\n\n"
            "@tool\n"
            "def count_letter(text: str, letter: str) -> int:\n"
            '    """Подсчитать, сколько раз буква встречается в строке."""\n'
            "    return text.count(letter)"
        )

        label_plain = B("Обычная функция", color=SEC, weight=BOLD, scale=0.48)
        label_tool = B("Инструмент LangChain", color=CRAIL, weight=BOLD, scale=0.48)

        cblock_plain = code_block(plain_src).scale(0.70)
        cblock_tool = code_block(tool_src).scale(0.70)

        col_plain = VGroup(label_plain, cblock_plain).arrange(
            DOWN, aligned_edge=LEFT, buff=0.10
        )
        col_tool = VGroup(label_tool, cblock_tool).arrange(
            DOWN, aligned_edge=LEFT, buff=0.10
        )

        cols = (
            VGroup(col_plain, col_tool)
            .arrange(DOWN, aligned_edge=LEFT, buff=0.22)
            .next_to(anchor, DOWN, buff=0.28)
        )
        cols.set_x(0)

        divline = (
            DashedLine(
                LEFT * 5.5,
                RIGHT * 5.5,
                color=CLOUDY,
                stroke_width=1.5,
            )
            .next_to(col_plain, DOWN, buff=0.10)
            .set_x(0)
        )

        schema_label = B(
            "@tool читает имя, строку документации и аннотации типов → строит JSON-схему, понятную LLM",
            color=TERT,
            scale=0.44,
        ).next_to(cols, DOWN, buff=0.24)
        schema_label.set_x(0)

        self.play(FadeIn(col_plain))
        self.next_slide()
        self.play(Create(divline), FadeIn(col_tool))
        self.play(FadeIn(schema_label))
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД — Как агент использует инструмент
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_langchain_tool_flow(self):
        anchor = self._header("Как агент использует инструмент")

        schema_src = (
            "{\n"
            '  "name": "count_letter",\n'
            '  "description": "Подсчитать вхождения буквы.",\n'
            '  "parameters": {\n'
            '    "text":   "string",\n'
            '    "letter": "string"\n'
            "  }\n"
            "}"
        )
        schema_label = B("Схема, которую видит LLM", color=SEC, weight=BOLD, scale=0.48)
        cblock_schema = code_block(schema_src, language="json").scale(0.72)
        col_schema = (
            VGroup(schema_label, cblock_schema)
            .arrange(DOWN, aligned_edge=LEFT, buff=0.12)
            .to_edge(LEFT, buff=0.45)
            .shift(DOWN * 0.2)
        )

        flow_steps = [
            (DARK, 'Пользователь:   Сколько "r" в слове strawberry?'),
            (TERT, "LLM читает схему → выбирает count_letter"),
            (CRAIL, 'Вызов:  count_letter(text="strawberry", letter="r")'),
            (TERT, "Инструмент возвращает:  3"),
            (CRAIL, 'Ответ:  В слове "strawberry" 3 буквы "r".'),
        ]

        step_mobs = [B(text, color=color, scale=0.48) for color, text in flow_steps]
        flow_group = VGroup(*step_mobs).arrange(DOWN, aligned_edge=LEFT, buff=0.32)
        flow_group.to_edge(RIGHT, buff=0.45).shift(DOWN * 0.2)

        arrows = [
            Arrow(
                step_mobs[i].get_bottom(),
                step_mobs[i + 1].get_top(),
                buff=0.06,
                color=CLOUDY,
                stroke_width=1.8,
                max_tip_length_to_length_ratio=0.18,
            )
            for i in range(len(step_mobs) - 1)
        ]

        invoke_src = (
            "result = count_letter.invoke({\n"
            '    "text":   "strawberry",\n'
            '    "letter": "r",\n'
            "})\n"
            "print(result)  # -> 3"
        )
        invoke_label = B("Ручной вызов", color=SEC, weight=BOLD, scale=0.48)
        cblock_invoke = code_block(invoke_src).scale(0.72)
        col_invoke = (
            VGroup(invoke_label, cblock_invoke)
            .arrange(DOWN, aligned_edge=LEFT, buff=0.10)
            .next_to(col_schema, DOWN, buff=0.28)
            .align_to(col_schema, LEFT)
        )

        self.play(FadeIn(col_schema))
        self.next_slide()
        self.play(FadeIn(step_mobs[0]))
        for i, arr in enumerate(arrows):
            self.play(GrowArrow(arr), FadeIn(step_mobs[i + 1]), run_time=0.5)
        self.next_slide()
        self.play(FadeIn(col_invoke))
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД  — MCP против CLI
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_mcp_vs_cli(self):
        anchor = self._header("Проблемы, которые MCP решает, а CLI — нет")

        cli_items = [
            "Требует наличия CLI-интерфейса",
            "Возвращает сырой текст / коды выхода",
            "Нет структурированной схемы",
            "Выполняет команду в терминале",
            "Входные данные — это команды",
        ]
        mcp_items = [
            "Работает через HTTP SSE или stdio",
            "Структурированные ответы JSON-RPC",
            "Самоописывающиеся схемы инструментов",
            "Поддерживает состояние и потоковую передачу",
            "LLM автоматически обнаруживает все инструменты",
        ]

        head_cli = B("CLI инструменты", color=CLOUDY, weight=BOLD, scale=0.65).shift(
            LEFT * 4.2 + UP * 1.5
        )
        head_mcp = B("MCP Серверы", color=CRAIL, weight=BOLD, scale=0.65).shift(
            RIGHT * 2.8 + UP * 1.5
        )

        divline = DashedLine(UP * 1.7, DOWN * 2.7, color=TERT, stroke_width=1.5)

        cli_mobs = (
            VGroup([B(f"x  {p}", color=SEC, scale=0.5) for p in cli_items])
            .arrange(DOWN, aligned_edge=LEFT, buff=0.05)
            .next_to(head_cli, DOWN, buff=0.3)
            .align_to(head_cli, LEFT)
        )

        cli_mobs.set_max_width(3.0)

        mcp_mobs = (
            VGroup(*[B(f"v  {p}", color=CRAIL, scale=0.5) for p in mcp_items])
            .arrange(DOWN, aligned_edge=LEFT, buff=0.25)
            .next_to(head_mcp, DOWN, buff=0.3)
            .align_to(head_mcp, LEFT)
        )

        self.play(FadeIn(head_cli), FadeIn(head_mcp), Create(divline))
        self.play(
            LaggedStart(
                *[FadeIn(m, shift=RIGHT * 0.15) for m in cli_mobs], lag_ratio=0.18
            ),
            LaggedStart(
                *[FadeIn(m, shift=LEFT * 0.15) for m in mcp_mobs], lag_ratio=0.18
            ),
        )
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 9 — Кейс Wolfram Alpha
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_wolfram(self):
        anchor = self._header("Кейс: Wolfram Alpha")

        stmt = B(
            "У Wolfram Alpha НЕТ CLI  —  но есть MCP-сервер.",
            color=DARK,
            scale=0.65,
        )
        stmt.next_to(anchor, DOWN, buff=0.42)
        why = B("Почему?", color=CRAIL, weight=BOLD, scale=0.85).next_to(
            stmt, DOWN, buff=0.22
        )

        reasons = [
            (
                CRAIL,
                "Только HTTP API",
                "Wolfram предоставляет REST-эндпоинты — нечего запускать как процесс",
            ),
            (
                SEC,
                "Структурированный вывод",
                "Возвращает LaTeX и JSON — не текст в терминале",
            ),
            (
                TERT,
                "LLM нужны схемы",
                "MCP описывает входы/выходы, чтобы LLM знал, как его вызвать",
            ),
            (
                CRAIL,
                "Безопасность",
                "Давать LLM доступ к оболочке сервера было бы опасно",
            ),
        ]

        reason_mobs = []
        for color, title, desc in reasons:
            t = B(f"  {title}", color=color, weight=BOLD, scale=0.56)
            d = B(f"    {desc}", color=DARK, scale=0.46)
            reason_mobs.append(VGroup(t, d).arrange(DOWN, aligned_edge=LEFT, buff=0.04))

        grp = VGroup(*reason_mobs).arrange(DOWN, aligned_edge=LEFT, buff=0.28)
        grp.next_to(why, DOWN, buff=0.38).to_edge(LEFT, buff=0.55)

        self.play(Write(stmt))
        self.play(Write(why))
        self.play(
            LaggedStart(
                *[FadeIn(r, shift=RIGHT * 0.2) for r in reason_mobs], lag_ratio=0.28
            )
        )
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 10 — MCP как сетевой протокол  (SVG или запасная диаграмма)
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_network_protocol(self):
        anchor = self._header("MCP как сетевой протокол")

        img_path = "proto.jpg"
        if os.path.exists(img_path):
            img = (
                ImageMobject(img_path)
                .scale_to_fit_width(10)
                .next_to(anchor, DOWN, buff=0.25)
            )
            self.play(FadeIn(img))
        else:
            self._draw_protocol_stack(anchor)

        self.next_slide()
        self._clear()

    def _draw_protocol_stack(self, anchor):
        layers = [
            (
                CRAIL,
                "Прикладной уровень",
                "Claude Desktop / Cursor / VS Code / Пользовательское LLM-приложение",
            ),
            (
                SEC,
                "Уровень протокола MCP",
                "Схемы инструментов (JSON), вызовы и ответы JSON-RPC 2.0",
            ),
            (TERT, "Транспортный уровень", "stdio  |  HTTP с SSE  |  WebSockets"),
            (
                CLOUDY,
                "Уровень MCP-сервера",
                "Инструменты  |  Ресурсы  |  Промпты  |  Сэмплирование",
            ),
        ]
        mobs = []
        for color, title, desc in layers:
            box = RoundedRectangle(
                corner_radius=0.1,
                width=10.5,
                height=0.88,
                fill_color=color,
                fill_opacity=0.08,
                stroke_color=color,
                stroke_width=1.5,
            )
            lbl = B(title, color=color, weight=BOLD, scale=0.58)
            dsc = B(desc, color=DARK, scale=0.44)
            content = VGroup(lbl, dsc).arrange(RIGHT, buff=0.55).move_to(box)
            mobs.append(VGroup(box, content))

        stack = VGroup(*mobs).arrange(DOWN, buff=0.1).next_to(anchor, DOWN, buff=0.4)
        self.play(
            LaggedStart(*[FadeIn(m, shift=DOWN * 0.08) for m in mobs], lag_ratio=0.22)
        )

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 11 — Подключение к MCP-серверу
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_connect(self):
        anchor = self._header("Подключение к MCP-серверу")

        subtitle = B(
            "Пример: Claude Desktop  <->  локальный Python MCP-сервер",
            color=SEC,
            scale=0.52,
        )
        subtitle.next_to(anchor, DOWN, buff=0.28)

        config_src = """{
  "mcpServers": {
    "my-tool": {
      "command": "python",
      "args": ["-m", "my_mcp_server"],
      "transport": "stdio"
    }
  }
}"""
        cblock = code_block(config_src, language="json").next_to(
            subtitle, DOWN, buff=0.3
        )

        steps = [
            "1  Декорируйте функции через  @mcp.tool()",
            "2  Добавьте запись в  claude_desktop_config.json",
            "3  Перезапустите Claude Desktop",
            "4  LLM автоматически обнаружит и вызовет ваши инструменты",
        ]
        step_mobs = (
            VGroup(
                *[
                    B(s, color=CRAIL if i % 2 == 0 else DARK, scale=0.52)
                    for i, s in enumerate(steps)
                ]
            )
            .arrange(DOWN, aligned_edge=LEFT, buff=0.22)
            .next_to(cblock, DOWN, buff=0.32)
            .to_edge(LEFT, buff=0.5)
        )

        self.play(FadeIn(subtitle))
        self.play(FadeIn(cblock))
        self.next_slide()
        self.play(
            LaggedStart(
                *[FadeIn(s, shift=RIGHT * 0.2) for s in step_mobs], lag_ratio=0.25
            )
        )
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 12 — Выводы
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_conclusions(self):
        title = H("Ключевые выводы", scale=1.15).shift(UP * 2.1)
        takeaways = [
            (
                CRAIL,
                "MCP = стандартный протокол для взаимодействия LLM с инструментами",
            ),
            (
                SEC,
                "Решает то, чего не могут CLI и чистые API: структурировано, безопасно, масштабируемо",
            ),
            (TERT, "Принят Claude, Cursor, Windsurf, VS Code и многими другими"),
            (CRAIL, "Начните создавать свой MCP-сервер уже сегодня!"),
        ]
        ta_mobs = (
            VGroup(
                *[B(f"  {t}", color=c, weight=BOLD, scale=0.55) for c, t in takeaways]
            )
            .arrange(DOWN, aligned_edge=LEFT, buff=0.32)
            .next_to(title, DOWN, buff=0.5)
        )
        self.play(DrawBorderThenFill(title))
        self.play(
            LaggedStart(*[FadeIn(t, shift=UP * 0.18) for t in ta_mobs], lag_ratio=0.28)
        )
        self.next_slide()
        self._clear()

    # ══════════════════════════════════════════════════════════════════════════
    # СЛАЙД 13 — Спасибо
    # ══════════════════════════════════════════════════════════════════════════
    def _slide_thanks(self):
        title = H("Спасибо!", scale=1.15, color=CRAIL).shift(UP * 1.2)
        line = Line(LEFT * 5, RIGHT * 5, color=CLOUDY, stroke_width=1).next_to(
            title, DOWN, buff=0.6
        )

        qr_label = B("Вопросы?", color=SEC, weight=BOLD, scale=0.7).next_to(
            line, DOWN, buff=0.55
        )
        self.play(DrawBorderThenFill(title))
        self.play(Create(line))
        self.play(FadeIn(qr_label, shift=UP * 0.2))
        self.next_slide()
