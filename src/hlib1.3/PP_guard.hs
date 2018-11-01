module PreludeX where
guard :: (MonadZero m) => Bool -> m ()
guard p = if p then return () else zero
