module Config exposing (server)


protocol : String
protocol =
    "http"


serverAddress : String
serverAddress =
    -- URLs of server hosts
    --"learnmath-backend-shoham424210.codeanyapp.com"
    --"learnmath.pythonanywhere.com"
    "localhost"


serverPort : String
serverPort =
    "8080"


server : String
server =
    protocol ++ "://" ++ serverAddress ++ ":" ++ serverPort
