module Config exposing (server)


protocol : String
protocol =
    "http"


serverAddress : String
serverAddress =
    "learnmath-backend-shoham424210.codeanyapp.com"



--"learnmath.pythonanywhere.com"
--"localhost"


serverPort : String
serverPort =
    "8000"


server : String
server =
    protocol ++ ":// " ++ serverAddress ++ ":" ++ serverPort
