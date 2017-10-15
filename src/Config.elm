module Config exposing (server)


protocol : String
protocol =
    "http"


server : String
server =
    protocol ++ ":// " ++ "learnmath.pythonanywhere.com"



--"localhost:5000"
