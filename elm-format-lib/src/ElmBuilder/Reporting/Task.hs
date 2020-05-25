{-# LANGUAGE Rank2Types #-}
module ElmBuilder.Reporting.Task
  ( Task
  , run
  , throw
  , mapError
  --
  , io
  , mio
  , eio
  )
  where



-- TASKS


newtype Task m x a =
  Task
  (
    forall result. (a -> m result) -> (x -> m result) -> m result
  )


run :: Monad m => Task m x a -> m (Either x a)
run (Task task) =
  task (return . Right) (return . Left)


throw :: x -> Task m x a
throw x =
  Task $ \_ err -> err x


mapError :: (x -> y) -> Task m x a -> Task m y a
mapError func (Task task) =
  Task $ \ok err ->
    task ok (err . func)



-- IO


{-# INLINE io #-}
io :: Monad m => m a -> Task m x a
io work =
  Task $ \ok _ -> work >>= ok


mio :: Monad m => x -> m (Maybe a) -> Task m x a
mio x work =
  Task $ \ok err ->
    do  result <- work
        case result of
          Just a -> ok a
          Nothing -> err x


eio :: Monad m => (x -> y) -> m (Either x a) -> Task m y a
eio func work =
  Task $ \ok err ->
    do  result <- work
        case result of
          Right a -> ok a
          Left x -> err (func x)



-- INSTANCES


instance Functor (Task m x) where
  {-# INLINE fmap #-}
  fmap func (Task taskA) =
    Task $ \ok err ->
      let
        okA arg = ok (func arg)
      in
      taskA okA err


instance Applicative (Task m x) where
  {-# INLINE pure #-}
  pure a =
    Task $ \ok _ -> ok a

  {-# INLINE (<*>) #-}
  (<*>) (Task taskFunc) (Task taskArg) =
    Task $ \ok err ->
      let
        okFunc func =
          let
            okArg arg = ok (func arg)
          in
          taskArg okArg err
      in
      taskFunc okFunc err


instance Monad (Task m x) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) (Task taskA) callback =
    Task $ \ok err ->
      let
        okA a =
          case callback a of
            Task taskB -> taskB ok err
      in
      taskA okA err
