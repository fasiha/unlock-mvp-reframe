# mvp-reframe

A [re-frame](https://github.com/Day8/re-frame) application designed to ... well, that part is up to you.

## Development Mode

### Run application:

```
lein clean
lein figwheel dev
```

Figwheel will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:3449](http://localhost:3449).

### Backend-only test

```
lein ring headless-server
```

This starts the backend, so any endpoints defined in
`src/clj/mvp-reframe/handler.clj` will be available on
[http://localhost:3600](http://localhost:3600). This reloads the backend code so
you can experiment with it live, but note! This won't auto-rebuild the
ClojureScript code! If someone knows how to watch both front-end and back-end
code, let me know!

## Production Build

```
lein clean
lein cljsbuild once min
```

# Issues

Correct furigana.
Search JMdict for adjacent sequences of morphemes.
Delete grammar tags.

Allow only a single JMdict tag (?)
Free-form JMdict search.

RULES ENGINE!

Backend sync.

Authentication/authorization.

Document editor:
Enable "show new items".
