loop context n = do
    send context $ do
        clearRect (0,0,width context,height context)
        beginPath()
        save()
        translate (width context / 2,height context / 2)
        rotate (pi * n)
        beginPath()
        moveTo(-100,-100)
        lineTo(-100,100)
        lineTo(100,100)
        lineTo(100,-100)
        closePath()
        lineWidth 10
        strokeStyle "green"
        stroke()
        restore()
    threadDelay (20 * 1000) 
    loop context (n + 0.01)
