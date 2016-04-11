main :: IO ()
main = do
    line <- getLine
    if null line || line == "*"
        then return ()
        else do
            putStrLn (processLine line)
            main

processLine :: String -> String
processLine line =
    let createString xs = (head xs)++": "++(resultIMC (tail xs))
        resultIMC xs
            |imc xs < 18 = "magror"
            |imc xs < 25 = "corpulencia normal"
            |imc xs < 30 = "sobrepes"
            |imc xs <= 40 = "obesitat"
            |otherwise = "obesitat morbida"
        imc ([x,y]) = (read x :: Float)/((read y :: Float)^2)
    in createString (words line)
