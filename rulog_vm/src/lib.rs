pub mod environment;
pub mod interpreter;
pub mod resolver;
pub mod types;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_logger() {
        use log::LevelFilter;
        let _ = env_logger::builder()
            .is_test(true)
            .filter_level(LevelFilter::Trace)
            .try_init();
    }

    fn load_specs() -> std::io::Result<Vec<String>> {
        use std::fs;
        use std::path::Path;

        let mut specs = Vec::new();

        let entries = fs::read_dir("spec/")?;

        for entry in entries {
            let entry = entry?;
            if let Some(file_name) = entry.file_name().to_str() {
                if !file_name.ends_with(".ans") {
                    continue;
                }
                let spec_name = file_name.trim_end_matches(".ans");
                if !Path::new(&format!("spec/{}", spec_name)).exists() {
                    log::warn!("skip spec/{}", spec_name);
                    continue;
                }
                specs.push(spec_name.to_string());
            }
        }

        Ok(specs)
    }

    #[test]
    fn test_all_specs() {
        setup_logger();
        let specs = load_specs().unwrap();
        for spec in specs {
            log::info!("test spec/{}", spec);
            let result = add(2, 2);
            assert_eq!(result, 4);
        }
    }
}
