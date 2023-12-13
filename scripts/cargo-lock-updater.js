const fs = require('fs');
const toml = require('@iarna/toml');

module.exports.readVersion = function (contents) {
    const parsed = toml.parse(contents);
    return parsed.package.find((pkg) => pkg.name.startsWith('rulog-')).version;
}

module.exports.writeVersion = function (contents, newVersion) {
    const parsed = toml.parse(contents);
    parsed.package.forEach((pkg) => {
        if (pkg.name.startsWith('rulog-')) {
            pkg.version = newVersion;
        }
    });

    return toml.stringify(parsed);
}
