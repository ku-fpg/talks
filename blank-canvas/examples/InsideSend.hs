send :: DeviceContext -> Canvas a -> IO a
send cxt commands = send' (deviceCanvasContext cxt) commands id
 where
  send' :: CanvasContext -> Canvas a -> (String -> String) -> IO a
  send' c (Bind (Return a) k)    cmds = send' c (k a) cmds
  send' c (Bind (Bind m k1) k2)  cmds = send' c (Bind m (\ r -> Bind (k1 r) k2)) cmds
  send' c (Bind (Method cmd) k) cmds = send' c (k ()) (cmds . ((showJS c ++ ".") ++) . shows cmd . (";" ++))
  send' c (Bind (Query query) k) cmds = do
      uq <- atomically $ getUniq
      -- The query function returns a function takes the unique port number of the reply.
      sendToCanvas cxt (cmds . ((show query ++ "(" ++ show uq ++ "," ++ showJS c ++ ");") ++))
      v <- KC.getReply (theComet cxt) uq
      case parse (parseQueryResult query) v of
        Error msg -> fail msg
        Success a -> do
                send' c (k a) id
  ...
  send' _ (Return a)           cmds = do
      sendToCanvas cxt cmds
      return a
  send' c cmd                  cmds = send' c (Bind cmd Return) cmds
