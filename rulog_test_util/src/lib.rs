use std::io::Write;

pub fn setup_logger() {
    use log::LevelFilter;
    let _ = env_logger::builder()
        .is_test(true)
        .format(|buf, record| {
            writeln!(
                buf,
                "[{}] {}:{}\n{}\n",
                record.level(),
                record.file().unwrap_or("unknown"),
                record.line().unwrap_or(0),
                record.args()
            )
        })
        .filter_level(LevelFilter::Trace)
        .try_init();
}
