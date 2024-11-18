use std::ops::Range;

use wolfram_library_link::{
    self as wll,
    expr::{
        convert::{
            forms::{List, RGBColor, Rule},
            parse_headed, parse_headed_args, FromExpr, FromExprError,
        },
        st, Expr, Symbol,
    },
};

use once_cell::sync::Lazy;

use syntect::{
    easy::HighlightLines,
    highlighting::{Color, FontStyle, Style, StyleModifier, Theme, ThemeSet},
    parsing::{SyntaxReference, SyntaxSet},
    util::LinesWithEndings,
};

#[wll::export(wstp, hidden)]
fn load_library_functions(args: Vec<Expr>) -> Expr {
    assert!(args.len() == 0);
    return wll::exported_library_functions_association(Some("libnotebook_website_tools".into()));
}

//==========================================================
// Syntax Highlighting
//==========================================================

static SYNTAX_SET: Lazy<SyntaxSet> = Lazy::new(|| {
    let mut builder = SyntaxSet::load_defaults_newlines().into_builder();

    // Fake a Mojo syntax from the Python syntax.

    let mut mojo = builder
        .syntaxes()
        .iter()
        .find(|syntax_def| syntax_def.name == "Python")
        .expect("unable to locate builtin Python syntax")
        .clone();

    mojo.name = String::from("Mojo");
    mojo.file_extensions = vec![String::from("mojo"), String::from("🔥")];

    builder.add(mojo);

    builder.build()
});
static THEME_SET: Lazy<ThemeSet> = Lazy::new(|| ThemeSet::load_defaults());

/// Highlights the input source string and returns an HTML string.
#[wll::export(wstp)]
fn highlight_to_html(args: Vec<Expr>) -> Expr {
    if args.len() != 4 {
        panic!("expected 4 arguments, got {}: {args:?}", args.len())
    }

    let source: &str = args[0]
        .try_as_str()
        .expect("expected 1st arg to be a String");
    let syntax_name: &str = args[1]
        .try_as_str()
        .expect("expected 2nd arg to be a String");
    let theme_name: &str = args[2]
        .try_as_str()
        .expect("expected 3rd arg to be a String");
    let line_numbering = args[3]
        .try_as_bool()
        .expect("expected 4th argument to be a boolean");

    let syntax = match lookup_syntax(syntax_name) {
        Ok(syntax) => syntax,
        Err(error) => return error,
    };

    let theme = match lookup_theme(theme_name) {
        Ok(theme) => theme,
        Err(error) => return error,
    };

    let default_background = theme.settings.background.unwrap_or(Color::WHITE);

    // Open a pre.nb-HighlightSyntax element.
    // Note: This logic based on syntect::html::start_highlighted_html_snippet(theme);
    let mut html = {
        let class = match line_numbering {
            true => "nb-HighlightSyntax line-numbers",
            false => "nb-HighlightSyntax",
        };
        format!(
            "<pre class=\"{class}\" style=\"background-color:#{:02x}{:02x}{:02x};\">\n",
            default_background.r, default_background.g, default_background.b
        )
    };

    let mut highlighter = HighlightLines::new(syntax, theme);

    for line in LinesWithEndings::from(source) {
        let ranges: Vec<(Style, &str)> = highlighter
            .highlight_line(line, &SYNTAX_SET)
            .expect("error highlighting line");

        // Wrap each line in a `span.ln` element, so that the CSS line-numbering
        // logic can use that selector.
        html.push_str("<span class=\"ln\">");

        syntect::html::append_highlighted_html_for_styled_line(
            &ranges,
            syntect::html::IncludeBackground::IfDifferent(default_background),
            &mut html,
        )
        .expect("error converting highlight styles to HTML");

        html.push_str("</span>");
    }

    html.push_str("</pre>");

    Expr::string(html)
}

/// Highlights the input source string and returns Wolfram `Style[..]`
/// expressions.
#[wll::export(wstp)]
fn highlight_to_wolfram(args: Vec<Expr>) -> Expr {
    if args.len() != 4 {
        panic!("expected 4 arguments, got {}: {args:?}", args.len())
    }

    let source: &str = args[0]
        .try_as_str()
        .expect("expected 1st arg to be a String");
    let syntax_name: &str = args[1]
        .try_as_str()
        .expect("expected 2nd arg to be a String");
    let theme_name: &str = args[2]
        .try_as_str()
        .expect("expected 3rd arg to be a String");
    let custom_highlights = if args[3] != st::None {
        List::<CustomHighlight>::from_expr_req(&args[3])
            .expect("Invalid form for custom highlight specification")
            .0
    } else {
        vec![]
    };

    let syntax = match lookup_syntax(syntax_name) {
        Ok(syntax) => syntax,
        Err(error) => return error,
    };

    let theme = match lookup_theme(theme_name) {
        Ok(theme) => theme,
        Err(error) => return error,
    };

    let default_background = theme.settings.background.unwrap_or(Color::WHITE);

    let mut highlighter = HighlightLines::new(syntax, theme);

    let mut segments = Vec::new();

    for (line_number, line) in LinesWithEndings::from(source).enumerate() {
        let mut ranges: Vec<(Style, &str)> = highlighter
            .highlight_line(line, &SYNTAX_SET)
            .expect("error highlighting line");

        for custom in custom_highlights
            .iter()
            .filter(|custom| custom.line == line_number)
        {
            ranges = syntect::util::modify_range(&ranges, custom.range.clone(), custom.style_mod);
        }

        let line = Expr::list(
            ranges
                .into_iter()
                .map(|(style, source_fragment)| {
                    syntect_style_span_to_wolfram(style, source_fragment, default_background)
                })
                .collect(),
        );

        segments.push(line);
    }

    Expr::list(segments)
}

fn syntect_style_span_to_wolfram(
    style: Style,
    source_fragment: &str,
    default_background: Color,
) -> Expr {
    let Style {
        foreground,
        background,
        font_style,
    } = style;

    let mut wl_style_args = vec![
        Expr::string(source_fragment),
        Expr::rule(
            Symbol::new("System`FontColor"),
            syntect_color_to_wolfram(foreground),
        ),
    ];

    // Only include this option if it is different from
    // the (presumed to exist) theme default background
    // color.
    if background != default_background {
        wl_style_args.push(Expr::rule(
            Symbol::new("System`Background"),
            syntect_color_to_wolfram(background),
        ));
    }

    if font_style.intersects(FontStyle::BOLD) {
        wl_style_args.push(Expr::from(Symbol::new("System`Bold")));
    }

    if font_style.intersects(FontStyle::UNDERLINE) {
        wl_style_args.push(Expr::from(Symbol::new("System`Underlined")));
    }

    if font_style.intersects(FontStyle::ITALIC) {
        wl_style_args.push(Expr::from(Symbol::new("System`Italic")));
    }

    Expr::normal(Symbol::new("System`Style"), wl_style_args)
}

fn syntect_color_to_wolfram(color: syntect::highlighting::Color) -> Expr {
    let syntect::highlighting::Color { r, g, b, a } = color;

    let rgb_color = RGBColor {
        r,
        g,
        b,
        a: if a == 255 { None } else { Some(a) },
    };

    rgb_color.to_expr()
}

fn wolfram_to_syntect_color(color: RGBColor) -> syntect::highlighting::Color {
    let RGBColor { r, g, b, a } = color;

    syntect::highlighting::Color {
        r,
        g,
        b,
        a: a.unwrap_or(u8::MAX),
    }
}

#[wll::export(wstp)]
fn theme_default_background(args: Vec<Expr>) -> Expr {
    if args.len() != 1 {
        panic!("expected 1 arguments, got {}: {args:?}", args.len())
    }

    let theme_name: &str = args[0]
        .try_as_str()
        .expect("expected 1st arg to be a String");

    let theme = match lookup_theme(theme_name) {
        Ok(theme) => theme,
        Err(error) => return error,
    };

    match theme.settings.background {
        Some(color) => syntect_color_to_wolfram(color),
        None => Expr::symbol(Symbol::new("System`None")),
    }
}

#[wll::export(wstp)]
fn known_highlight_choices(args: Vec<Expr>) -> Expr {
    if args.len() != 0 {
        panic!("expected 0 arguments, got {}: {args:?}", args.len())
    }

    let syntaxes = SYNTAX_SET
        .syntaxes()
        .iter()
        .map(|syntax| Expr::string(&syntax.name))
        .collect();
    let themes = THEME_SET
        .themes
        .keys()
        .map(|name| Expr::string(name))
        .collect();

    Expr::normal(
        Symbol::new("System`Association"),
        vec![
            Expr::rule(Expr::string("Syntaxes"), Expr::list(syntaxes)),
            Expr::rule(Expr::string("Themes"), Expr::list(themes)),
        ],
    )
}

//======================================
// Helpers
//======================================

fn lookup_syntax(syntax_name: &str) -> Result<&'static SyntaxReference, Expr> {
    if let Some(syntax) = SYNTAX_SET.find_syntax_by_name(syntax_name) {
        return Ok(syntax);
    }

    // Return a Failure[..]
    let known_syntaxes = SYNTAX_SET
        .syntaxes()
        .iter()
        .map(|syntax: &SyntaxReference| Expr::string(syntax.name.clone()))
        .collect::<Vec<_>>();

    Err(Expr::normal(
        Symbol::new("System`Failure"),
        vec![
            Expr::string("UnknownSyntax"),
            Expr::normal(
                Symbol::new("System`Association"),
                vec![
                    Expr::rule(Expr::string("Syntax"), Expr::string(syntax_name)),
                    Expr::rule(Expr::string("KnownSyntaxes"), Expr::list(known_syntaxes)),
                ],
            ),
        ],
    ))
}

fn lookup_theme(theme_name: &str) -> Result<&'static Theme, Expr> {
    if let Some(theme) = THEME_SET.themes.get(theme_name) {
        return Ok(theme);
    }

    // Return a Failure[..]
    let known_themes = THEME_SET
        .themes
        .keys()
        .map(|theme_name: &String| Expr::string(theme_name.clone()))
        .collect::<Vec<_>>();

    Err(Expr::normal(
        Symbol::new("System`Failure"),
        vec![
            Expr::string("UnknownTheme"),
            Expr::normal(
                Symbol::new("System`Association"),
                vec![
                    Expr::rule(Expr::string("Theme"), Expr::string(theme_name)),
                    Expr::rule(Expr::string("KnownThemes"), Expr::list(known_themes)),
                ],
            ),
        ],
    ))
}

struct CustomHighlight {
    line: usize,
    // TODO: Bytes or codepoints?
    range: Range<usize>,
    style_mod: StyleModifier,
}

impl FromExpr<'_> for CustomHighlight {
    fn parse_from_expr(expr: &Expr) -> Result<Self, FromExprError> {
        // lhs -> style_mod:StyleModifier
        let Rule(lhs, New(style_mod)) = Rule::<&Expr, New<StyleModifier>>::parse_from_expr(expr)?;

        // {line:usize, range} -> style_mod:StyleModifier
        // TODO: This should always yield a FromExprError::Malformed.
        let (line, range): (usize, &Expr) = parse_headed_args(lhs, st::List)?;

        // TODO: This should always yield a FromExprError::Malformed.
        let (range_start, range_end): (usize, usize) = parse_headed_args(range, st::Span)?;

        Ok(CustomHighlight {
            line,
            range: Range {
                start: range_start,
                end: range_end,
            },
            style_mod,
        })
    }
}

struct New<T>(T);

impl FromExpr<'_> for New<StyleModifier> {
    fn parse_from_expr(expr: &'_ Expr) -> Result<Self, FromExprError> {
        let mut modifier = StyleModifier::default();

        let directives = match parse_headed(expr, st::Directive) {
            Ok(directives) => directives,
            Err(_) => &[expr.clone()],
        };

        for directive in directives {
            if *directive == st::Bold {
                modifier.font_style = Some(FontStyle::BOLD);
                continue;
            } else if *directive == st::Italic {
                modifier.font_style = Some(FontStyle::ITALIC);
                continue;
            } else if *directive == st::Underlined {
                modifier.font_style = Some(FontStyle::UNDERLINE);
                continue;
            }

            if let Some(Rule(lhs, rhs)) = Rule::<&Expr, _>::from_expr_opt(directive)? {
                if *lhs == st::Background {
                    let rgb_color = RGBColor::from_expr_req(rhs)?;
                    modifier.background = Some(wolfram_to_syntect_color(rgb_color));
                    continue;
                } else if *lhs == st::FontColor {
                    let rgb_color = RGBColor::from_expr_req(rhs)?;
                    modifier.foreground = Some(wolfram_to_syntect_color(rgb_color));
                    continue;
                }
            }

            return Err(FromExprError::unexpected_custom(
                directive,
                "expected style directive of Bold, Italic, Underlined, FontColor -> ..., or Background -> ...",
            ));
        }

        Ok(New(modifier))
    }
}
