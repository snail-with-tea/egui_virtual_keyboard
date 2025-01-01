use std::{error::Error, fmt::Display, str::FromStr};

use egui::{vec2, Rect, Response, RichText, Sense, TextStyle, Ui, Vec2, WidgetText};

const DO_NOT_USE_CHARS: &[char] = &['(', ')', '{', '}', '[', ']', '"', ':', ';', '|'];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modifier {
    Alt,
    Shift,
    Control,
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
            "Control" => Self::Control,
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
                Self::Control => "Control",
            }
        )
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum KeyAction {
    /// Derive action from text
    #[default]
    FromText,
    /// Change Layout
    Layout(String),
    /// Physycal key,
    Key(egui::Key),
    /// Modifier key
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
pub struct KeyboardButton {
    text: String,
    action: KeyAction,
    takes_space: f32,
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

impl FromStr for KeyboardButton {
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
            takes_space[1..].parse::<f32>().unwrap_or(1.0).max(1.0)
        };

        Ok(Self {
            text,
            action,
            takes_space,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyboardRow {
    pub buttons: Vec<KeyboardButton>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseKeyboardRowError {
    pub parsing: String,
    pub inner: Vec<ParseButtonError>,
}

impl Display for ParseKeyboardRowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not parse row from \"{}\"", self.parsing)
    }
}

impl FromStr for KeyboardRow {
    type Err = ParseKeyboardRowError;
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
                    .filter_map(|btn| {
                        KeyboardButton::from_str(btn)
                            .map_err(|e| error.inner.push(e))
                            .ok()
                    })
                    .collect();
                if error.inner.is_empty() {
                    Some(KeyboardRow { buttons })
                } else {
                    None
                }
            })
            .ok_or(error)
    }
}

impl KeyboardRow {
    fn takes_width(&self) -> f32 {
        self.buttons
            .iter()
            .map(|btn| btn.takes_space.max(1.0))
            .sum()
    }
}

#[derive(Debug, Clone)]
pub struct KeyboardLayout {
    name: String,
    rows: Vec<KeyboardRow>,
}

#[derive(Debug)]
pub struct ParseLayoutError {
    pub parsing: String,
    pub cause: String,
    pub inner: Vec<ParseKeyboardRowError>,
}

impl Display for ParseLayoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Could not parse layout from")?;
        writeln!(f, "\"{}\"", self.parsing)?;
        writeln!(f, "{}", self.cause)
    }
}

impl FromStr for KeyboardLayout {
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
            .map(KeyboardRow::from_str)
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
    let button = KeyboardButton::from_str("{?123}");
    assert_eq!(
        Ok(KeyboardButton {
            text: "?123".to_string(),
            action: KeyAction::FromText,
            takes_space: 1.0,
        }),
        button
    );
    let button = KeyboardButton::from_str("{?123:}");
    assert!(button.is_err());
    let button = KeyboardButton::from_str("{?123:Layout(QWERTY_Mobile)}");
    assert_eq!(
        Ok(KeyboardButton {
            text: "?123".to_string(),
            action: KeyAction::Layout("QWERTY_Mobile".to_string()),
            takes_space: 1.0,
        }),
        button
    );
    let button = KeyboardButton::from_str("{\\:}");
    assert_eq!(
        Ok(KeyboardButton {
            text: ":".to_string(),
            action: KeyAction::FromText,

            takes_space: 1.0,
        }),
        button
    );
    let button = KeyboardButton::from_str("{\\::Key(A)}");
    assert_eq!(
        Ok(KeyboardButton {
            text: ":".to_string(),
            action: KeyAction::Key(egui::Key::A),
            takes_space: 1.0,
        }),
        button
    );
    let button = KeyboardButton::from_str("{\\::Key(A);2.0}");
    assert_eq!(
        Ok(KeyboardButton {
            text: ":".to_string(),
            action: KeyAction::Key(egui::Key::A),
            takes_space: 2.0,
        }),
        button
    );
}

#[test]
fn parse_row() {
    let row = KeyboardRow::from_str("[{q}{w}{e}]");
    assert_eq!(
        Ok(KeyboardRow {
            buttons: vec![
                KeyboardButton {
                    text: "q".to_string(),
                    action: KeyAction::FromText,
                    takes_space: 1.0,
                },
                KeyboardButton {
                    text: "w".to_string(),
                    action: KeyAction::FromText,
                    takes_space: 1.0,
                },
                KeyboardButton {
                    text: "e".to_string(),
                    action: KeyAction::FromText,
                    takes_space: 1.0,
                },
            ]
        }),
        row
    );
    let row = KeyboardRow::from_str("[{q}{w}{e}");
    assert!(row.is_err());
    let row = KeyboardRow::from_str("[{q}{w}{e:}]");
    assert!(row.is_err());
}

#[test]
fn parse_layout() {
    let layout_qwerty = KeyboardLayout::from_str(
        "QWERTY_Mobile
[{q}{w}{e}{r}{t}{y}{u}{i}{o}{p}]
[{a}{s}{d}{f}{g}{h}{j}{k}{l}]
[{⇧:Modifier(Shift);1.5}{z}{x}{c}{v}{b}{n}{m}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_qwerty.is_ok());

    let layout_qwerty_shift_once = KeyboardLayout::from_str(
        "QWERTY_Mobile_MOD_SHIFT_ONCE
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⬆:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_qwerty_shift_once.is_ok());

    let layout_qwerty_shift_hold = KeyboardLayout::from_str(
        "QWERTY_Mobile_MOD_SHIFT_HOLD
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⮉:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_qwerty_shift_hold.is_ok());

    let layout_numbers = KeyboardLayout::from_str(
        "Numeric_Mobile
[{1}{2}{3}{4}{5}{6}{7}{8}{9}{0}]
[{@}{#}{$}{_}{%}{&}{-}{+}{(}{)}]
[{=\\<:Layout(Signs_Mobile);1.5}{*}{\"}{\'}{\\:}{\\;}{!}{?}{<❌:Key(Backspace);1.5}]
[{ABC:Layout(QWERTY_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_numbers.is_ok());

    let layout_symbols = KeyboardLayout::from_str(
        "Signs_Mobile
[{~}{`}{|}{\\}{=}{^}{<}{>}{[}{]}]
[{?123:Layout(Numeric_Mobile);1.5}{*}{\"}{\'}{\\:}{\\;}{!}{?}{<❌:Key(Backspace);1.5}]
[{ABC:Layout(QWERTY_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
    );
    assert!(layout_symbols.is_ok());
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum ModState {
    #[default]
    Off,
    Once,
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

pub struct VirtualKeyboard {
    id: egui::Id,
    focus: Option<egui::Id>,
    events: Vec<egui::Event>,
    layouts: Vec<KeyboardLayout>,
    current: usize,
    mod_alt: ModState,
    mod_shf: ModState,
    mod_cmd: ModState,
}

impl Default for VirtualKeyboard {
    fn default() -> Self {
        let layout_qwerty = KeyboardLayout::from_str(
            "QWERTY_Mobile
[{q}{w}{e}{r}{t}{y}{u}{i}{o}{p}]
[{a}{s}{d}{f}{g}{h}{j}{k}{l}]
[{⇧:Modifier(Shift);1.5}{z}{x}{c}{v}{b}{n}{m}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap();

        let layout_qwerty_shift_once = KeyboardLayout::from_str(
            "QWERTY_Mobile_MOD_SHIFT_ONCE
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⬆:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap();

        let layout_qwerty_shift_hold = KeyboardLayout::from_str(
            "QWERTY_Mobile_MOD_SHIFT_HOLD
[{Q}{W}{E}{R}{T}{Y}{U}{I}{O}{P}]
[{A}{S}{D}{F}{G}{H}{J}{K}{L}]
[{⮉:Modifier(Shift);1.5}{Z}{X}{C}{V}{B}{N}{M}{<❌:Key(Backspace);1.5}]
[{?123:Layout(Numeric_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap();

        let layout_numbers = KeyboardLayout::from_str(
            "Numeric_Mobile
[{1}{2}{3}{4}{5}{6}{7}{8}{9}{0}]
[{@}{#}{$}{_}{%}{&}{-}{+}{(}{)}]
[{=\\<:Layout(Signs_Mobile);1.5}{*}{\"}{\'}{\\:}{\\;}{!}{?}{<❌:Key(Backspace);1.5}]
[{ABC:Layout(QWERTY_Mobile);2.0}{,}{Space:Key(Space);3.0}{/}{.}{⮨:Key(Enter);2.0}]",
        )
        .unwrap();

        let layout_symbols = KeyboardLayout::from_str(
            "Signs_Mobile
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

impl VirtualKeyboard {
    pub fn empty() -> Self {
        VirtualKeyboard {
            id: egui::Id::new("Virtual Keyboard"),
            focus: None,
            events: vec![],
            layouts: vec![],
            current: 0,
            mod_alt: ModState::Off,
            mod_shf: ModState::Off,
            mod_cmd: ModState::Off,
        }
    }

    pub fn with_id(mut self, id: egui::Id) -> Self {
        self.id = id;
        self
    }

    pub fn extend(mut self, layout: KeyboardLayout) -> Self {
        self.layouts.push(layout);
        self
    }

    pub fn clear_layouts(&mut self) {
        self.layouts.clear();
    }

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

    pub fn bump_events(&mut self, _ctx: &egui::Context, raw_input: &mut egui::RawInput) {
        raw_input.events.append(&mut self.events);
    }

    pub fn show(&mut self, ui: &mut Ui) {
        let focus = ui.ctx().memory(|mem| mem.focused());

        if ui.ctx().wants_keyboard_input() && self.focus != focus {
            self.focus = focus;
        }

        self.buttons_2(ui);

        if let Some(focus) = self.focus {
            ui.ctx().memory_mut(|mem| {
                mem.request_focus(focus);
            });
        }
    }

    pub fn buttons_2(&mut self, ui: &mut Ui) {
        let Some(layout) = self.layouts.get(self.current).cloned() else {
            return;
        };

        let spacing = ui.spacing().clone();
        let h_space = ui.available_width();

        let btn_mul = layout
            .rows
            .iter()
            .map(|row| row.takes_width())
            .fold(1.0, |acc, val| if val > acc { val } else { acc });
        let row_num = layout.rows.len().max(1) as f32;

        let btn_w = ((h_space - spacing.item_spacing.x * (btn_mul - 1.0)) / btn_mul)
            .clamp(spacing.icon_width, spacing.interact_size.x);
        let btn_h = spacing.interact_size.x * 0.9;
        let btn_std = vec2(btn_w, btn_h);

        let item_spacing = spacing.item_spacing;
        let desired_size = vec2(
            (btn_std + item_spacing).x * btn_mul - item_spacing.x,
            (btn_std + item_spacing).y * row_num - item_spacing.y,
        );

        let (_id, kbd_rect) = ui.allocate_space(desired_size);
        let vis = ui.style().noninteractive();
        ui.painter()
            .rect(kbd_rect, vis.rounding, vis.weak_bg_fill, vis.bg_stroke);

        let draw_btn = |button: &KeyboardButton, offset: &mut Vec2, ui: &mut Ui| {
            let kbd_min = kbd_rect.min.to_vec2();
            Self::draw_one(kbd_min, btn_std, item_spacing, button, offset, ui)
        };

        let actions = layout.rows.iter().enumerate().flat_map(|(row_idx, row)| {
            let btn_mul = row.takes_width();
            let mut offset = vec2(
                (kbd_rect.width() - (btn_std + item_spacing).x * btn_mul + item_spacing.x) * 0.5,
                (btn_std + item_spacing).y * row_idx as f32,
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

        actions.for_each(|(action, text, response)| {
            self.process_action(action, text, response);
        });
    }

    fn draw_one(
        kbd_min: Vec2,
        btn_std: Vec2,
        itm_spc: Vec2,
        button: &KeyboardButton,
        offset: &mut Vec2,
        ui: &mut Ui,
    ) -> Response {
        let min = (kbd_min + *offset).to_pos2();
        let size = vec2(
            btn_std.x + (btn_std.x + itm_spc.x) * (button.takes_space - 1.0),
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

    fn layout_name_by_mod(&self) -> String {
        let Some(layout) = self.layouts.get(self.current).cloned() else {
            return "".to_string();
        };

        let mid = layout.name.find("_MOD").unwrap_or(layout.name.len());
        let (no_mod_name, _r) = layout.name.split_at(mid);
        no_mod_name.to_string()
            + &self.mod_alt.mod_str(Modifier::Alt)
            + &self.mod_shf.mod_str(Modifier::Shift)
            + &self.mod_cmd.mod_str(Modifier::Control)
    }

    fn process_action(&mut self, action: KeyAction, text: String, response: Response) {
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
                    Modifier::Control => {
                        if self.mod_cmd != ModState::Off {
                            self.mod_cmd = ModState::Off;
                        } else {
                            self.mod_cmd = ModState::Once;
                        }
                    }
                };
                self.switch_layout(&self.layout_name_by_mod());
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
                    Modifier::Control => {
                        if self.mod_cmd != ModState::Hold {
                            self.mod_cmd = ModState::Hold;
                        } else {
                            self.mod_cmd = ModState::Off;
                        }
                    }
                };
                self.switch_layout(&self.layout_name_by_mod());
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
                    self.switch_layout(&self.layout_name_by_mod());
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
