use wolfram_library_link::{
    self as wll,
    expr::{Expr, Symbol},
};

use once_cell::sync::Lazy;

use syntect::{
    easy::HighlightLines,
    highlighting::{Color, Style, Theme, ThemeSet},
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

static SYNTAX_SET: Lazy<SyntaxSet> = Lazy::new(|| SyntaxSet::load_defaults_newlines());
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
