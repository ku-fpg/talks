-- Create a web server. Run the given function with
-- a fresh Context each time our webpage is called.
blankCanvas :: Options -> (Context -> IO ()) -> IO ()

-- Sends a set of Canvas commands to the canvas.
-- Attempts to common up as many commands as possible.
-- A canvas command *may* return a result, which
-- will get passed back from the browser to the
-- user, via the IO a.
send :: Context -> Canvas a -> IO a

-- Example of Canvas command
moveTo :: (Double, Double) -> Canvas ()
