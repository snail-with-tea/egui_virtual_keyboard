//! # VirtualKeyboard for egui
//! Adds virtual keyboard to provided ui
//!
//! Rcommended to use in separate window or panel
//!
//! Example use with eframe:
//! ```rust
//! use egui_virtual_keyboard::VirtualKeyboard;
//!
//! pub struct ExampleApp {
//!     label: String,
//!     keyboard: VirtualKeyboard,
//! }
//!
//! impl Default for ExampleApp {
//!     fn default() -> Self {
//!         Self {
//!             label: "Hello World!".to_owned(),
//!             keyboard: Default::default(),
//!         }
//!     }
//! }
//!
//! impl eframe::App for ExampleApp {
//!     fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
//!         egui::CentralPanel::default().show(ctx, |ui| {
//!             ui.horizontal(|ui| {
//!                 ui.label("Write something: ");
//!                 ui.text_edit_singleline(&mut self.label);
//!             });
//!         });
//!
//!         let scr_size = ctx.screen_rect().size();
//!         egui::Window::new("KBD").show(ctx, |ui| {
//!                 self.keyboard.show(ui);
//!             });
//!     }
//!
//!     fn raw_input_hook(&mut self, ctx: &egui::Context, raw_input: &mut egui::RawInput) {
//!         self.keyboard.bump_events(ctx, raw_input);
//!     }
//! }
//!
//!
//! fn main() -> eframe::Result {
//!     let native_options = eframe::NativeOptions::default();
//!
//!     eframe::run_native(
//!         "eframe template",
//!         native_options,
//!         Box::new(|_cc| Ok(Box::new(ExampleApp::default()))),
//!     )
//! }
//! ```
use std::{error::Error, fmt::Display, num::ParseFloatError, ops::RangeInclusive, str::FromStr};

use egui::{vec2, Rect, Response, RichText, Sense, TextStyle, Ui, Vec2, WidgetText};

const DO_NOT_USE_CHARS: &[char] = &['(', ')', '{', '}', '[', ']', '"', ':', ';', '|'];

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
/// If this is not self explanatory, please read this
/// [Wikipedia article](https://en.wikipedia.org/wiki/Modifier_key)
/// to gain general understanding.
pub enum Modifier {
    Alt,
    Shift,
    /// Control key on most machines (except Macs).
    /// Using Command because it souns cooler.
    Command,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseModifierError {
    pub parsing: String,
}

impl Display for ParseModifierError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} is not a Modifier", self.parsing)
    }
}

impl Error for ParseModifierError {}

impl FromStr for Modifier {
    type Err = ParseModifierError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "Alt" => Self::Alt,
            "Shift" => Self::Shift,
            "Command" => Self::Command,
            _ => Err(ParseModifierError {
                parsing: s.to_string(),
            })?,
        })
    }
}

impl Display for Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Alt => "Alt",
                Self::Shift => "Shift",
                Self::Command => "Command",
            }
        )
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
/// # Keyboard key action
/// Determines what button will do
pub enum KeyAction {
    #[default]
    /// Derive action from text
    /// ```
    /// use egui_virtual_keyboard::KeyAction;
    /// use std::str::FromStr;
    ///
    /// KeyAction::from_str("FromText");
    /// ```
    FromText,
    /// Change Layout
    /// ```
    /// use egui_virtual_keyboard::KeyAction;
    /// use std::str::FromStr;
    ///
    /// KeyAction::from_str("Layout(Layout_Name)");
    /// ```
    Layout(String),
    /// [egui::Key],
    /// ```
    /// use egui_virtual_keyboard::KeyAction;
    /// use std::str::FromStr;
    ///
    /// KeyAction::from_str("Key(A)");
    /// ```
    Key(egui::Key),
    /// Modifier key
    /// ```
    /// use egui_virtual_keyboard::KeyAction;
    /// use std::str::FromStr;
    ///
    /// KeyAction::from_str("Modifier(Alt)");
    /// ```
    Modifier(Modifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseKeyActionError {
    pub parsing: String,
    pub kind: ParseKeyActionErrorKind,
    pub source: Option<ParseModifierError>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseKeyActionErrorKind {
    FromText,
    Layout,
    Key,
    Modifier,
    Unknown,
}

impl Display for ParseKeyActionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} while parsing \"{}\"",
            match self.kind {
                ParseKeyActionErrorKind::FromText => "Expected FromText",
                ParseKeyActionErrorKind::Layout => "Expected Layout",
                ParseKeyActionErrorKind::Key => "Expected Key",
                ParseKeyActionErrorKind::Modifier => "Expected Modifier",
                ParseKeyActionErrorKind::Unknown => "Unexpected Token",
            },
            self.parsing,
        )
    }
}

impl Error for ParseKeyActionError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        if let Some(src) = &self.source {
            Some(src)
        } else {
            None
        }
    }
}

impl FromStr for KeyAction {
    type Err = ParseKeyActionError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let first = s.get(0..3).unwrap_or("");
        let mut error = Self::Err {
            parsing: s.to_string(),
            kind: ParseKeyActionErrorKind::Unknown,
            source: None,
        };
        match first {
            "Fro" => {
                if s == "FromText" {
                    Ok(Self::FromText)
                } else {
                    error.kind = ParseKeyActionErrorKind::FromText;
                    Err(error)
                }
            }
            "Lay" => {
                error.kind = ParseKeyActionErrorKind::Layout;
                let laystr = s
                    .strip_prefix("Layout(")
                    .and_then(|s| s.strip_suffix(")"))
                    .and_then(|s| {
                        if s.contains(DO_NOT_USE_CHARS) {
                            None
                        } else {
                            Some(s)
                        }
                    })
                    .ok_or(error)?;
                Ok(Self::Layout(laystr.to_string()))
            }
            "Key" => {
                error.kind = ParseKeyActionErrorKind::Key;
                s.strip_prefix("Key(")
                    .and_then(|s| s.strip_suffix(")"))
                    .and_then(egui::Key::from_name)
                    .map(Self::Key)
                    .ok_or(error)
            }
            "Mod" => {
                error.kind = ParseKeyActionErrorKind::Modifier;
                s.strip_prefix("Modifier(")
                    .and_then(|s| s.strip_suffix(")"))
                    .and_then(|s| {
                        Modifier::from_str(s)
                            .map(Self::Modifier)
                            .map_err(|e| error.source = Some(e))
                            .ok()
                    })
                    .ok_or(error)
            }
            _ => Err(error),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
/// # Single keyboard button
/// Contains text & KeyAction
///
/// Can be created from string.
///
/// For example:
/// ```rust
/// use egui_virtual_keyboard::Button;
/// use std::str::FromStr;
///
/// Button::from_str("{a}");
/// ```
/// Will create button with text "a".
/// ```rust
/// use egui_virtual_keyboard::Button;
/// use std::str::FromStr;
///
/// Button::from_str("{a:Key(A)}");
/// ```
/// After text [KeyAction] can be specified with `:` separator
/// ```rust
/// use egui_virtual_keyboard::Button;
/// use std::str::FromStr;
///
/// Button::from_str("{a;2.5}");
/// ```
/// Button width can be specified after `;` separator
///
/// Note that final width is calculated as
/// `single_width_button * width + item_spacing.x * (width - 1)`
pub struct Button {
    text: String,
    action: KeyAction,
    width_mult: f32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseButtonError {
    pub parsing: String,
    pub source: Option<ParseKeyActionError>,
}

impl Display for ParseButtonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not parse button from \"{}\"", self.parsing)
    }
}

impl Error for ParseButtonError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        if let Some(inner) = &self.source {
            Some(inner)
        } else {
            None
        }
    }
}

impl From<ParseFloatError> for ParseButtonError {
    fn from(value: ParseFloatError) -> Self {
        Self {
            parsing: value.to_string(),
            source: None,
        }
    }
}

impl Button {
    pub fn new(text: String) -> Self {
        Self {
            text,
            action: KeyAction::FromText,
            width_mult: 1.0,
        }
    }

    pub fn with_action(mut self, action: KeyAction) -> Self {
        self.action = action;
        self
    }

    /// Button can be made wider relative to standart size button
    pub fn with_width(mut self, width: f32) -> Self {
        let width = width.max(1.0);
        self.width_mult = width;
        self
    }
}

impl FromStr for Button {
    type Err = ParseButtonError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let inner = s
            .strip_prefix("{")
            .and_then(|s| s.strip_suffix("}"))
            .ok_or(ParseButtonError {
                parsing: s.to_string(),
                source: None,
            })?;

        let mut search_from = 0;
        let mut split_at = inner.len();
        while let Some(colpos) = inner[search_from..].find(":") {
            if colpos > 0 && inner.get(colpos - 1..colpos) == Some("\\") {
                search_from = colpos + 1;
            } else {
                split_at = colpos + search_from;
                break;
            }
        }

        let (text, additional) = inner.split_at(split_at);

        let mut search_from = 0;
        let mut split_at = additional.len();
        while let Some(colpos) = additional[search_from..].find(";") {
            if colpos > 0 && inner.get(colpos - 1..colpos) == Some("\\") {
                search_from = colpos + 1;
            } else {
                split_at = colpos + search_from;
                break;
            }
        }
        let (action, takes_space) = additional.split_at(split_at);

        let text = text.replace("\\:", ":").replace("\\;", ";");
        let action = if action.is_empty() {
            KeyAction::FromText
        } else {
            action[1..].parse().map_err(|e| ParseButtonError {
                parsing: s.to_string(),
                source: Some(e),
            })?
        };

        let takes_space = if takes_space.is_empty() {
            1.0
        } else {
            takes_space[1..].parse::<f32>()?.max(1.0)
        };

        Ok(Self {
            text,
            action,
            width_mult: takes_space,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
/// # Holds a row of buttons
/// Row can be created from string
///
/// For example:
/// ```rust
/// use egui_virtual_keyboard::Row;
/// use std::str::FromStr;
///
/// Row::from_str("[{q}{w}{e}]");
/// ```
/// will create row with 3 buttons
pub struct Row {
    buttons: Vec<Button>,
}

impl Row {
    /// Can be created from vec
    pub fn from_vec(buttons: Vec<Button>) -> Self {
        Self { buttons }
    }

    fn takes_width(&self) -> f32 {
        self.buttons.iter().map(|btn| btn.width_mult).sum()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseRowError {
    pub parsing: String,
    pub inner: Vec<ParseButtonError>,
}

impl Display for ParseRowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not parse row from \"{}\"", self.parsing)
    }
}

impl FromStr for Row {
    type Err = ParseRowError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut error = Self::Err {
            parsing: s.to_string(),
            inner: vec![],
        };

        s.strip_prefix("[")
            .and_then(|s| s.strip_suffix("]"))
            .and_then(|inner| {
                let buttons = inner
                    .split_inclusive("}")
                    .filter_map(|btn| Button::from_str(btn).map_err(|e| error.inner.push(e)).ok())
                    .collect();
                if error.inner.is_empty() {
                    Some(Row { buttons })
                } else {
                    None
                }
            })
            .ok_or(error)
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
/// # Named row sequence
///
/// For example:
/// ```rust
/// use egui_virtual_keyboard::Layout;
/// use std::str::FromStr;
///
/// Layout::from_str("Scream
/// [{a}]
/// [{A}]");
/// ```
/// Will create a layout with name "Scream"
/// and 2 rows containing buttons `a` and `A`
pub struct Layout {
    name: String,
    rows: Vec<Row>,
}

impl Layout {
    /// Create empty layout
    pub fn new(name: String) -> Self {
        Self { name, rows: vec![] }
    }
    /// Add row to layout
    pub fn extend(mut self, row: Row) -> Self {
        self.rows.push(row);
        self
    }
    /// Add modifiers to name
    pub fn with_modifiers(
        mut self,
        mod_alt: ModState,
        mod_shf: ModState,
        mod_cmd: ModState,
    ) -> Self {
        let mid = self.name.find("_MOD").unwrap_or(self.name.len());
        let (no_mod_name, _r) = self.name.split_at(mid);
        self.name = no_mod_name.to_string()
            + &mod_alt.mod_str(Modifier::Alt)
            + &mod_shf.mod_str(Modifier::Shift)
            + &mod_cmd.mod_str(Modifier::Command);
        self
    }
}

#[derive(Debug)]
pub struct ParseLayoutError {
    pub parsing: String,
    pub cause: String,
    pub inner: Vec<ParseRowError>,
}

impl Display for ParseLayoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Could not parse layout from")?;
        writeln!(f, "\"{}\"", self.parsing)?;
        writeln!(f, "{}", self.cause)
    }
}

impl FromStr for Layout {
    type Err = ParseLayoutError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut error = Self::Err {
            parsing: s.to_string(),
            cause: "".to_string(),
            inner: vec![],
        };

        let Some((l, r)) = s.split_once("\n") else {
            error.cause = "Must have at least 2 lines: name and row".to_string();
            return Err(error);
        };

        if l.contains(DO_NOT_USE_CHARS) {
            error.cause = "Layout name cannot contain (){}[]\":;".to_string();
            return Err(error);
        };

        let rows = r
            .split("\n")
            .map(Row::from_str)
            .filter_map(|res| res.map_err(|e| error.inner.push(e)).ok())
            .collect();

        if !error.inner.is_empty() {
            error.cause = "Inner Parsing Error".to_string();
            return Err(error);
        }

        Ok(Self {
            name: l.to_string(),
            rows,
        })
    }
}

#[test]
fn parse_key_action() {
    let action = KeyAction::from_str("Key(Enter)");
    assert_eq!(Ok(KeyAction::Key(egui::Key::Enter)), action);
    let action = KeyAction::from_str("Modifier(Shift)");
    assert_eq!(Ok(KeyAction::Modifier(Modifier::Shift)), action);
    let action = KeyAction::from_str("Layout(QWERTY_Mobile)");
    assert_eq!(Ok(KeyAction::Layout("QWERTY_Mobile".to_string())), action);
    let action = KeyAction::from_str("Layout(QWERTY_Mobile))");
    assert!(action.is_err());
    let action = KeyAction::from_str("FromText");
    assert_eq!(Ok(KeyAction::FromText), action);
}

#[test]
fn parse_button() {
    let button = Button::from_str("{?123}");
    assert_eq!(
        Ok(Button {
            text: "?123".to_string(),
            action: KeyAction::FromText,
            width_mult: 1.0,
        }),
        button
    );
    let button = Button::from_str("{?123:}");
    assert!(button.is_err());
    let button = Button::from_str("{?123:Layout(QWERTY_Mobile)}");
    assert_eq!(
        Ok(Button {
            text: "?123".to_string(),
            action: KeyAction::Layout("QWERTY_Mobile".to_string()),
            width_mult: 1.0,
        }),
        button
    );
    let button = Button::from_str("{\\:}");
    assert_eq!(
        Ok(Button {
            text: ":".to_string(),
            action: KeyAction::FromText,

            width_mult: 1.0,
        }),
        button
    );
    let button = Button::from_str("{\\::Key(A)}");
    assert_eq!(
        Ok(Button {
            text: ":".to_string(),
            action: KeyAction::Key(egui::Key::A),
            width_mult: 1.0,
        }),
        button
    );
    let button = Button::from_str("{\\::Key(A);2.0}");
    assert_eq!(
        Ok(Button {
            text: ":".to_string(),
            action: KeyAction::Key(egui::Key::A),
            width_mult: 2.0,
        }),
        button
    );
}

#[test]
fn parse_row() {
    let row = Row::from_str("[{q}{w}{e}]");
    assert_eq!(
        Ok(Row {
            buttons: vec![
                Button {
                    text: "q".to_string(),
                    action: KeyAction::FromText,
                    width_mult: 1.0,
                },
                Button {
                    text: "w".to_string(),
                    action: KeyAction::FromText,
                    width_mult: 1.0,
                },
                Button {
                    text: "e".to_string(),
                    action: KeyAction::FromText,
                    width_mult: 1.0,
                },
            ]
        }),
        row
    );
    let row = Row::from_str("[{q}{w}{e}");
    assert!(row.is_err());
    let row = Row::from_str("[{q}{w}{e:}]");
    assert!(row.is_err());
}

#[test]
fn parse_layout() {
    let layout_qwerty = Layout::from_str(
        "QWERTY_Mobile
[{q}{w}{e}{r}{t}{y}{u}{i}{o}{p}]
[{a}{s}{d}{f}{g}{h}{j}{k}{l}]
[{⇧:Modifier(Shift);1.5}{z}{x}{c}{v}{b}{n}{m}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_qwerty.is_ok());

    let layout_qwerty_shift_once = Layout::from_str(
        "QWERTY_Mobile_MOD_SHIFT_ONCE
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⬆:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_qwerty_shift_once.is_ok());

    let layout_qwerty_shift_hold = Layout::from_str(
        "QWERTY_Mobile_MOD_SHIFT_HOLD
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⮉:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_qwerty_shift_hold.is_ok());

    let layout_numbers = Layout::from_str(
        "Numeric_Mobile
[{1}{2}{3}{4}{5}{6}{7}{8}{9}{0}]
[{@}{#}{$}{_}{%}{&}{-}{+}{(}{)}]
[{=\\<:Layout(Signs_Mobile);1.5}{*}{\"}{\'}{\\:}{\\;}{!}{?}{<❌:Key(Backspace);1.5}]
[{ABC:Layout(QWERTY_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_numbers.is_ok());

    let layout_symbols = Layout::from_str(
        "Signs_Mobile
[{~}{`}{|}{\\}{=}{^}{<}{>}{[}{]}]
[{?123:Layout(Numeric_Mobile);1.5}{*}{\"}{\'}{\\:}{\\;}{!}{?}{<❌:Key(Backspace);1.5}]
[{ABC:Layout(QWERTY_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_symbols.is_ok());
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
///Modificator key state
pub enum ModState {
    #[default]
    /// Is not pressed
    Off,
    /// Will turn off after any other key is pressed
    Once,
    /// Wil be pressed until clicked again
    Hold,
}

impl ModState {
    fn mod_str(&self, modifier: Modifier) -> String {
        match self {
            Self::Off => "".to_string(),
            Self::Once => format!("_MOD_{}_ONCE", modifier.to_string().to_uppercase()),
            Self::Hold => format!("_MOD_{}_HOLD", modifier.to_string().to_uppercase()),
        }
    }
}

#[test]
fn mod_name() {
    let name = ModState::Once.mod_str(Modifier::Shift);
    assert_eq!("_MOD_SHIFT_ONCE".to_string(), name);
}

/// # A Keyboard, that can be drawn in egui.
/// By default holds mobile version of QWERTY, Numeric & Symbols layouts.
///
/// Needs to hook into raw input with [bump_events](VirtualKeyboard::bump_events).
///
/// Example use:
/// ```rust
/// use egui_virtual_keyboard::VirtualKeyboard;
///
/// pub struct ExampleApp {
///     label: String,
///     keyboard: VirtualKeyboard,
/// }
///
/// impl Default for ExampleApp {
///     fn default() -> Self {
///         Self {
///             label: "Hello World!".to_owned(),
///             keyboard: Default::default(),
///         }
///     }
/// }
///
/// impl eframe::App for ExampleApp {
///     fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
///         egui::CentralPanel::default().show(ctx, |ui| {
///             ui.horizontal(|ui| {
///                 ui.label("Write something: ");
///                 ui.text_edit_singleline(&mut self.label);
///             });
///         });
///
///         let scr_size = ctx.screen_rect().size();
///         egui::Window::new("KBD").show(ctx, |ui| {
///                 self.keyboard.show(ui);
///             });
///     }
///
///     fn raw_input_hook(&mut self, ctx: &egui::Context, raw_input: &mut egui::RawInput) {
///         self.keyboard.bump_events(ctx, raw_input);
///     }
/// }
///
///
/// fn main() -> eframe::Result {
///     let native_options = eframe::NativeOptions::default();
///
///     eframe::run_native(
///         "eframe template",
///         native_options,
///         Box::new(|_cc| Ok(Box::new(ExampleApp::default()))),
///     )
/// }
/// ```
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct VirtualKeyboard {
    #[cfg_attr(feature = "serde", serde(skip))]
    mod_alt: ModState,
    #[cfg_attr(feature = "serde", serde(skip))]
    mod_shf: ModState,
    #[cfg_attr(feature = "serde", serde(skip))]
    mod_cmd: ModState,
    #[cfg_attr(feature = "serde", serde(skip))]
    events: Vec<egui::Event>,
    /// Need to keep layouts in case user added some
    id: egui::Id,
    layouts: Vec<Layout>,
    current: usize,
    /// In case user needs different size for buttons
    button_s: Option<Vec2>,
    button_h: Option<f32>,
    button_w: Option<RangeInclusive<f32>>,
}

impl Default for VirtualKeyboard {
    fn default() -> Self {
        let layout_qwerty = Layout::from_str(
            "QWERTY_Mobile
[{q}{w}{e}{r}{t}{y}{u}{i}{o}{p}]
[{a}{s}{d}{f}{g}{h}{j}{k}{l}]
[{⇧:Modifier(Shift);1.5}{z}{x}{c}{v}{b}{n}{m}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap();

        let layout_qwerty_shift_once = Layout::from_str(
            "QWERTY_Mobile
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⬆:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap()
        .with_modifiers(ModState::Off, ModState::Once, ModState::Off);

        let layout_qwerty_shift_hold = Layout::from_str(
            "QWERTY_Mobile
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⮉:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap()
        .with_modifiers(ModState::Off, ModState::Hold, ModState::Off);

        let layout_numbers = Layout::from_str(
            "Numeric_Mobile
[{1}{2}{3}{4}{5}{6}{7}{8}{9}{0}]
[{@}{#}{$}{_}{%}{&}{-}{+}{(}{)}]
[{=\\<:Layout(Symbols_Mobile);1.5}{*}{\"}{\'}{\\:}{\\;}{!}{?}{<❌:Key(Backspace);1.5}]
[{ABC:Layout(QWERTY_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap();

        let layout_symbols = Layout::from_str(
            "Symbols_Mobile
[{~}{`}{|}{\\}{=}{^}{<}{>}{[}{]}]
[{?123:Layout(Numeric_Mobile);1.5}{*}{\"}{\'}{\\:}{\\;}{!}{?}{<❌:Key(Backspace);1.5}]
[{ABC:Layout(QWERTY_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap();

        Self::empty()
            .extend(layout_qwerty)
            .extend(layout_qwerty_shift_once)
            .extend(layout_qwerty_shift_hold)
            .extend(layout_numbers)
            .extend(layout_symbols)
    }
}

#[derive(Debug, Default, Clone)]
/// Keyboard's state
pub struct State {
    active: bool,
    deactivate: bool,
    focus: Option<egui::Id>,
}

impl VirtualKeyboard {
    /// Creates empty VirtualKeyboard
    pub fn empty() -> Self {
        VirtualKeyboard {
            id: egui::Id::new("VirtualKeyboard"),
            events: vec![],
            layouts: vec![],
            current: 0,
            mod_alt: ModState::Off,
            mod_shf: ModState::Off,
            mod_cmd: ModState::Off,
            button_s: None,
            button_w: None,
            button_h: None,
        }
    }
    #[inline]
    /// Use provided [Id](egui::Id) instead of default "VirtualKeyboard"
    pub fn with_id(mut self, id: egui::Id) -> Self {
        self.id = id;
        self
    }

    #[inline]
    /// Get [Id](egui::Id)
    pub fn id(&self) -> egui::Id {
        self.id
    }

    #[inline]
    /// Add [Layout] to keyboard
    pub fn extend(mut self, layout: Layout) -> Self {
        self.layouts.push(layout);
        self
    }

    #[inline]
    /// Clears layouts in keyboard memory
    pub fn clear_layouts(&mut self) {
        self.layouts.clear();
    }
    /// Switches to [Layout] with provided name (if exists)
    pub fn switch_layout(&mut self, name: &str) {
        let opt = self
            .layouts
            .iter()
            .enumerate()
            .find(|(_, layout)| layout.name == name)
            .map(|(pos, _)| pos);
        if let Some(pos) = opt {
            self.current = pos;
        }
    }

    #[inline]
    /// Use provided spacing instead of context one
    pub fn with_butyon_spacing(mut self, spacing: Vec2) -> Self {
        self.button_s = Some(spacing);
        self
    }

    #[inline]
    /// Use provided button height instead of default `interact_size.x * 0.9`
    pub fn with_button_height(mut self, height: f32) -> Self {
        self.button_h = Some(height);
        self
    }

    #[inline]
    /// Use provided button width range instead of `icon_width..=interact_size.x`
    pub fn with_button_width(mut self, width: impl Into<RangeInclusive<f32>>) -> Self {
        self.button_w = Some(width.into());
        self
    }

    /// Is keyboard currently active
    pub fn is_active(&self, ctx: &egui::Context) -> bool {
        let state = ctx.memory(|m| (m.data.get_temp::<State>(self.id).unwrap_or_default()));
        state.active
    }

    /// Should keyboard become active
    pub fn should_activate(&self, ctx: &egui::Context) -> bool {
        let (focused, state) = ctx.memory(|m| {
            (
                m.focused(),
                m.data.get_temp::<State>(self.id).unwrap_or_default(),
            )
        });
        focused.is_some() && state.focus != focused
    }

    /// Surrender keyboard focus & set keyboard as inactive
    pub fn deactivate(&self, ctx: &egui::Context) {
        let mut state = ctx.memory(|m| (m.data.get_temp::<State>(self.id).unwrap_or_default()));
        state.deactivate = true;
        ctx.memory_mut(|m| m.data.insert_temp(self.id, state));
    }

    /// What ID is keyboard focused on
    pub fn focused(&self, ctx: &egui::Context) -> Option<egui::Id> {
        let state = ctx.memory(|m| (m.data.get_temp::<State>(self.id).unwrap_or_default()));
        state.focus
    }

    /// Adds keyboard events to raw input
    pub fn bump_events(&mut self, _ctx: &egui::Context, raw_input: &mut egui::RawInput) {
        raw_input.events.append(&mut self.events);
    }
    /// Add Keyboard to ui.
    /// Recommended to use in separate window or panel.
    pub fn show(&mut self, ui: &mut Ui) -> Response {
        let (focused, mut state) = ui.memory(|m| {
            (
                m.focused(),
                m.data.get_temp::<State>(self.id).unwrap_or_default(),
            )
        });

        if focused.is_some() && state.focus != focused {
            state.active = true;
            state.focus = focused;
        }

        if state.deactivate {
            if let Some(id) = state.focus {
                ui.memory_mut(|m| m.surrender_focus(id));
            }
            state.active = false;
            state.deactivate = false;
            state.focus = None;
        }

        let resp = self.buttons(ui);

        if let (true, Some(id)) = (state.active, state.focus) {
            ui.memory_mut(|m| m.request_focus(id));
        }

        ui.memory_mut(|m| m.data.insert_temp(self.id, state));

        resp
    }

    fn buttons(&mut self, ui: &mut Ui) -> Response {
        let Some(layout) = self.layouts.get(self.current).cloned() else {
            return ui.response();
        };

        let spacing = ui.spacing().clone();
        let h_space = ui.available_width();

        let btn_mul = layout
            .rows
            .iter()
            .map(|row| row.takes_width())
            .fold(1.0, |acc, val| if val > acc { val } else { acc });
        let row_num = layout.rows.len().max(1) as f32;

        let btn_w_r = self
            .button_w
            .clone()
            .unwrap_or(spacing.icon_width..=spacing.interact_size.x);

        let btn_w = ((h_space - spacing.item_spacing.x * (btn_mul - 1.0)) / btn_mul)
            .clamp(*btn_w_r.start(), *btn_w_r.end());
        let btn_h = self.button_h.unwrap_or(spacing.interact_size.x * 0.9);

        let btn_std = vec2(btn_w, btn_h);

        let itm_spc = self.button_s.unwrap_or(spacing.item_spacing);

        let desired_size = vec2(
            (btn_std + itm_spc).x * btn_mul - itm_spc.x,
            (btn_std + itm_spc).y * row_num - itm_spc.y,
        );

        let (base_id, kbd_rect) = ui.allocate_space(desired_size);
        let base_resp = ui.interact(kbd_rect, base_id, Sense::hover());

        if !ui.is_rect_visible(kbd_rect) {
            return base_resp;
        }

        let vis = ui.style().noninteractive();
        ui.painter()
            .rect(kbd_rect, vis.rounding, vis.weak_bg_fill, vis.bg_stroke);

        let draw_btn = |button: &Button, offset: &mut Vec2, ui: &mut Ui| {
            let kbd_min = kbd_rect.min.to_vec2();
            Self::draw_one(kbd_min, btn_std, itm_spc, button, offset, ui)
        };

        let actions = layout.rows.iter().enumerate().flat_map(|(row_idx, row)| {
            let btn_mul = row.takes_width();
            let mut offset = vec2(
                (kbd_rect.width() - (btn_std + itm_spc).x * btn_mul + itm_spc.x) * 0.5,
                (btn_std + itm_spc).y * row_idx as f32,
            );

            let res: Vec<_> = row
                .buttons
                .iter()
                .map(|button| {
                    let resp = draw_btn(button, &mut offset, ui);
                    (button.action.clone(), button.text.clone(), resp)
                })
                .collect();
            res
        });

        let resp = actions.fold(base_resp, |acc, (action, text, response)| {
            self.process_action(action, text, &response);
            acc | response
        });

        ui.advance_cursor_after_rect(kbd_rect);
        resp
    }

    fn draw_one(
        kbd_min: Vec2,
        btn_std: Vec2,
        itm_spc: Vec2,
        button: &Button,
        offset: &mut Vec2,
        ui: &mut Ui,
    ) -> Response {
        let min = (kbd_min + *offset).to_pos2();
        let size = vec2(
            btn_std.x + (btn_std.x + itm_spc.x) * (button.width_mult - 1.0),
            btn_std.y,
        );
        offset.x += size.x + itm_spc.x;

        let rect = Rect::from_min_size(min, size);
        let resp = ui.allocate_rect(rect, Sense::click());
        let text = WidgetText::RichText(RichText::new(&button.text).monospace()).into_galley(
            ui,
            None,
            size.y,
            TextStyle::Button,
        );
        let visuals = ui.style().interact(&resp);

        ui.painter().rect(
            rect,
            visuals.rounding,
            visuals.weak_bg_fill,
            visuals.bg_stroke,
        );
        ui.painter().galley(
            (rect.center().to_vec2() - text.size() * 0.5).to_pos2(),
            text,
            visuals.text_color(),
        );

        resp
    }

    pub fn layout_name(&self) -> String {
        let Some(layout) = self.layouts.get(self.current).cloned() else {
            return "".to_string();
        };

        let mid = layout.name.find("_MOD").unwrap_or(layout.name.len());
        let (no_mod_name, _r) = layout.name.split_at(mid);
        no_mod_name.to_string()
            + &self.mod_alt.mod_str(Modifier::Alt)
            + &self.mod_shf.mod_str(Modifier::Shift)
            + &self.mod_cmd.mod_str(Modifier::Command)
    }

    fn process_action(&mut self, action: KeyAction, text: String, response: &Response) {
        let modifiers = egui::Modifiers {
            alt: self.mod_alt != ModState::Off,
            shift: self.mod_shf != ModState::Off,
            command: self.mod_cmd != ModState::Off,
            ..Default::default()
        };
        match (response.clicked(), response.double_clicked(), action) {
            (true, _, KeyAction::Layout(name)) => self.switch_layout(&name),
            (true, false, KeyAction::Modifier(modifier)) => {
                match modifier {
                    Modifier::Shift => {
                        if self.mod_shf != ModState::Off {
                            self.mod_shf = ModState::Off;
                        } else {
                            self.mod_shf = ModState::Once;
                        }
                    }
                    Modifier::Alt => {
                        if self.mod_alt != ModState::Off {
                            self.mod_alt = ModState::Off;
                        } else {
                            self.mod_alt = ModState::Once;
                        }
                    }
                    Modifier::Command => {
                        if self.mod_cmd != ModState::Off {
                            self.mod_cmd = ModState::Off;
                        } else {
                            self.mod_cmd = ModState::Once;
                        }
                    }
                };
                self.switch_layout(&self.layout_name());
            }
            (_, true, KeyAction::Modifier(modifier)) => {
                match modifier {
                    Modifier::Shift => {
                        if self.mod_shf != ModState::Hold {
                            self.mod_shf = ModState::Hold;
                        } else {
                            self.mod_shf = ModState::Off;
                        }
                    }
                    Modifier::Alt => {
                        if self.mod_alt != ModState::Hold {
                            self.mod_alt = ModState::Hold;
                        } else {
                            self.mod_alt = ModState::Off;
                        }
                    }
                    Modifier::Command => {
                        if self.mod_cmd != ModState::Hold {
                            self.mod_cmd = ModState::Hold;
                        } else {
                            self.mod_cmd = ModState::Off;
                        }
                    }
                };
                self.switch_layout(&self.layout_name());
            }
            (true, _, KeyAction::FromText) => {
                if let Some(key) = egui::Key::from_name(&text) {
                    self.events.push(egui::Event::Key {
                        key,
                        physical_key: None,
                        pressed: true,
                        repeat: false,
                        modifiers,
                    });
                }
                self.events.push(egui::Event::Text(text));

                let mut changed = false;
                if self.mod_alt == ModState::Once {
                    self.mod_alt = ModState::Off;
                    changed = true;
                }
                if self.mod_shf == ModState::Once {
                    self.mod_shf = ModState::Off;
                    changed = true;
                }
                if self.mod_cmd == ModState::Once {
                    self.mod_cmd = ModState::Off;
                    changed = true;
                }
                if changed {
                    self.switch_layout(&self.layout_name());
                }
            }
            (true, _, KeyAction::Key(key)) => {
                let text = match key {
                    egui::Key::Space => " ",
                    _ => "",
                };

                self.events.push(egui::Event::Key {
                    key,
                    physical_key: None,
                    pressed: true,
                    repeat: false,
                    modifiers,
                });

                if !text.is_empty() {
                    self.events.push(egui::Event::Text(text.to_string()));
                }
            }

            _ => {}
        }
    }
}
