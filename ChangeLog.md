* **0.2.0**: now exports `liftStateT` instead of being an instance of `MonadTrans`, because `MonadTrans` got stricter requirements which `StateT` does not satisfy.
