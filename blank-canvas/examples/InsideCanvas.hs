
data Canvas :: * -> * where
        Method  :: Method                      -> Canvas ()     -- <context>.<method>
        Query   :: Query a                     -> Canvas a
        ...
        Bind    :: Canvas a -> (a -> Canvas b) -> Canvas b
        Return  :: a                           -> Canvas a

instance Monad Canvas where
        return = Return
        (>>=) = Bind

data Method
        = Arc (Double,Double,Double,Double,Double,Bool)
        | ArcTo (Double,Double,Double,Double,Double)
        | BeginPath
        | BezierCurveTo (Double,Double,Double,Double,Double,Double)
        | forall image . Image image => DrawImage (image,[Double])
        ...

data Query :: * -> * where
        IsPointInPath :: (Double,Double)  -> Query Bool
        ...
