module.exports = {
    bumpFiles: [
        {
            filename: './rulog-cli/Cargo.toml',
            updater: './scripts/cargo-updater'
        },
        {
            filename: './rulog-core/Cargo.toml',
            updater: './scripts/cargo-updater'
        },
        {
            filename: './Cargo.lock',
            updater: './scripts/cargo-lock-updater.js'
        }
    ],
};
