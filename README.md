# dual-effects

Yet another experiment on effect systems. Instead of using sum-of-functors to hold the actions, I use product-of-handlers to dispatch it.

# What

An effect system, Reader/Writer/State/Throw/Catch - all that stuff.
Some ideas are taken from [polysemy](http://hackage.haskell.org/package/polysemy-1.3.0.0).

# Why

Because I wanted to have an effect system I have full control of, so I can experiment with it and rewrite it as much as I like.

The defining property is the absolute simplicity of the implementation and the API.
It is kinda slow, thought - the countDown benchmark shows a ~100x slowdown (from 0.16 sec to 13.4 sec).

# Features

The library is designed to be a layer over some `ReaderT Env IO` monad.

Addidng certain effects autoimplements some standart typeclasses:
`Fixpoint` -> `MonadFix`, `Error` -> `MonadThrow`/`MonadCatch`, `NonDet` -> `Alternative`, `Embed IO` -> `LiftIO`.

The `Error` effect operates with `Exceptions`, so it is not `ExceptT`-like.
The `State` and `Env` effect can be combined. The `State` can be wired to work with `IORef`s in `ReaderT` context.

Effects mostly delegate their implementations to the `Final` monad.
