import Html

main :: IO()
main = putStrLn (render_ myhtml)

myhtml :: Html
myhtml = html_
  "1st Post"
  <> (h1_ "Hello World")
  <> (p_ "Hello, World, this is my content")
  <>  (p_ "I'm learning Haskell a second time.")
  <>  (ul_ [ p_ "item 1", p_ "item 2"]))))

