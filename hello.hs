import Html

main :: IO()
main = putStrLn (render_ myhtml)

myhtml :: Html
myhtml = html_
  "1st Post"
  (append_
    (h1_ "Hello World")
    (append_
      (p_ "Hello, World, this is my content")
      (append_
        (p_ "I'm learning Haskell a second time.")
        (ul_ [ p_ "item 1", p_ "item 2"]))))
