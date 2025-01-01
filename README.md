# egui_virtual_keyboard

Example use with egui & eframe:
```rust
use egui_virtual_keyboard::VirtualKeyboard;

pub struct TemplateApp {
    label: String,
    keyboard: VirtualKeyboard,
}

impl Default for TemplateApp {
    fn default() -> Self {
        Self {
            label: "Hello World!".to_owned(),
            keyboard: Default::default(),
        }
    }
}

impl TemplateApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        Default::default()
    }
}

impl eframe::App for TemplateApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("Write something: ");
                ui.text_edit_singleline(&mut self.label);
            });
        });

        let scr_size = ctx.screen_rect().size();
        egui::Window::new("KBD")
            .show(ctx, |ui| {
                self.keyboard.show(ui);
            });
    }

    fn raw_input_hook(&mut self, ctx: &egui::Context, raw_input: &mut egui::RawInput) {
        self.keyboard.bump_events(ctx, raw_input);
    }
}
```
