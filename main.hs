import System.IO


convert :: (Float, [Char], [Char]) -> Float
--The `div` function is for integral division.
--The (/) function is for fractional types.
--------------------------------------------------------
convert (x,"kg","g") = x * 1000
convert (x,"g","kg") = x / 1000
--------------------------------------------------------
convert (x,"km","m") = x * 1000
convert (x,"m","km") = x / 1000

convert (x,"m","cm") = x * 100
convert (x,"cm","m") = x / 100

convert (x,"km","cm") = x * 100000
convert (x,"cm","km") = x / 100000
--------------------------------------------------------
convert (x,"f","c") = ((x - 32) * 5) / 9
convert (x,"c","f") = ((x * 9) / 5) + 32
--------------------------------------------------------
convertAll (x:xs) = do
    let y = words x --split the first entry by spaces
    let val = (read (y !! 0) :: Float) 
    print y
    --read the first entry, convert it to float, store it in val
    let from = y !! 1 
    let to = y !! 2
    let r = convert (val, from, to)
    let line = x ++ " " ++ show(r)
    --print line
    appendFile "Outputs.txt" (line ++ "\n") --write in file
    if xs == [] then putStr("") else convertAll xs


main = do
    contents <- readFile "Readings.txt" --read the file
    let ls = lines contents --split file contents by lines

    writeFile "Outputs.txt" "" --clear the file

    print ls
    putStrLn " "

    convertAll ls

    putStrLn " "
    putStrLn("Finished")