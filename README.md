# Purescript bindings for strophejs

## Status

Some low level bindings are already implemented and tested - check `Strophe.purs` for details.

Higher level `API` is still in pre alpha state, so don't rely on anything under `Strophe/Xmpp` for now.

## Testing

Tests are implemented against phantomjs library (as strophejs uses `DOM` API extensively), so you have to install it (you can find version number in `package.json`).

To run tests:

  PHANTOM_TEST_PATH=$(pwd) pulp test --runtime phantomjs
