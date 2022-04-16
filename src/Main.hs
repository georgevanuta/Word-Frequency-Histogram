import Histogram(histogram)

main = do
    putStrLn "Enter file name:"
    fileName <- readLn 
    s <- readFile fileName
    putStrLn $ histogram s