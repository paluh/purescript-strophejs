# Purescript bindings to strophejs

## Status

Some low level bindings are already implemented and tested - check `Strophe.purs` for details.

Higher level `API` is still in pre alpha state, so don't rely on anything under `Strophe/Xmpp` for now.


## Testing

I was not able to run phantomjs without bundling strophejs together with library code (it would require separate runner etc.), so I've decied to incorporate webpack to developement cylce.

To run tests with autorecompilation:

    $ webpack --watch --config test.webpack.config.js

Webpack configuration is focused on rebuild speed, so you should change `devtool` setting (in test.webpack.config.js) if you want to investigate anythin in resulting bundle code.
