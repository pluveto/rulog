const fs = require('fs');
const path = require('path');

function createEntries() {
    const entries = fs.readdirSync('./')
        .filter(name => name.startsWith('rulog_') && fs.lstatSync(name).isDirectory())
        .map(name => ({
            filename: `./${name}/Cargo.toml`,
            updater: './scripts/cargo-updater'
        }));

    // Add the entry for package.json
    entries.push({
        filename: './package.json',
        type: 'json'
    });

    return entries;
}

module.exports = {
    bumpFiles: createEntries(),
};
