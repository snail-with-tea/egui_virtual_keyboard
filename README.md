# VirtualKeyboard for egui

Currently use of SoftKeyboard on native android with egui & eframe is...
["Slightly difficult"](https://github.com/rust-windowing/winit/issues/1823)

And to make TextEdit from egui usable on android, this workaround was created

Example use with egui & eframe:
```rust
use egui_virtual_keyboard::VirtualKeyboard;

pub struct ExampleApp {
    label: String,
    keyboard: VirtualKeyboard,
}

impl Default for ExampleApp {
    fn default() -> Self {
        Self {
            label: "Hello World!".to_owned(),
            keyboard: Default::default(),
        }
    }
}

impl eframe::App for ExampleApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label("Write something: ");
                ui.text_edit_singleline(&mut self.label);
            });
        });

        let scr_size = ctx.screen_rect().size();
        egui::Window::new("KBD").show(ctx, |ui| {
                self.keyboard.show(ui);
            });
    }

    fn raw_input_hook(&mut self, ctx: &egui::Context, raw_input: &mut egui::RawInput) {
        self.keyboard.bump_events(ctx, raw_input);
    }
}


fn main() -> eframe::Result {
    let native_options = eframe::NativeOptions::default();
    
    eframe::run_native(
        "eframe template",
        native_options,
        Box::new(|_cc| Ok(Box::new(ExampleApp::default()))),
    )
}
```
