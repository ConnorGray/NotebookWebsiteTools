use wolfram_library_link::{
    self as wll,
    expr::{Expr, Symbol},
};

use once_cell::sync::Lazy;

use syntect::{
    highlighting::{Theme, ThemeSet},
    parsing::{SyntaxReference, SyntaxSet},
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
    if args.len() != 3 {
        panic!("expected 3 arguments, got {}: {args:?}", args.len())
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

    let syntax = match lookup_syntax(syntax_name) {
        Ok(syntax) => syntax,
        Err(error) => return error,
    };

    let theme = match lookup_theme(theme_name) {
        Ok(theme) => theme,
        Err(error) => return error,
    };

    match syntect::html::highlighted_html_for_string(source, &SYNTAX_SET, syntax, theme) {
        Ok(html) => Expr::string(html),
        Err(error) => panic!("error generating highlighted source as HTML: {error}"),
    }
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
