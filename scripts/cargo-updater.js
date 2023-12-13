const fs = require('fs');
const toml = require('@iarna/toml');

module.exports.readVersion = function (contents) {
    const parsed = toml.parse(contents);
    return parsed.package.version;
}

module.exports.writeVersion = function (contents, version) {
    const parsed = toml.parse(contents);
    parsed.package.version = version;
    return toml.stringify(parsed);
}
