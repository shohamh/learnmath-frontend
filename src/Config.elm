module Config exposing (apiUrl, server)


protocol : String
protocol =
    "https"


serverAddress : String
serverAddress =
    -- URLs of server hosts
    --"learnmath-backend-shoham424210.codeanyapp.com"
    --  "213.57.236.76"
    --"learnmath.pythonanywhere.com"
    "localhost"


serverPort : String
serverPort =
    "8080"


server : String
server =
    protocol ++ "://" ++ serverAddress ++ ":" ++ serverPort


apiUrl : String -> String
apiUrl path =
    server ++ path
