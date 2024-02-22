
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort lst = bubbleSort' (length lst) lst

bubbleSort' :: Ord a => Int -> [a] -> [a]
bubbleSort' 0 lst = lst
bubbleSort' n lst = bubbleSort' (n-1) (bubblePass lst)

bubblePass :: Ord a => [a] -> [a]
bubblePass [x] = [x]
bubblePass (x:y:xs)
    | x > y     = y : bubblePass (x:xs)
    | otherwise = x : bubblePass (y:xs)

main :: IO ()
main = do
    let unsortedList = [4, 2, 7, 1, 9, 5]
    putStrLn "Unsorted list:"
    print unsortedList

    let sortedList = bubbleSort unsortedList
    putStrLn "Sorted list:"
    print sortedList
