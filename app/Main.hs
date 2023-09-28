-- Визначимо тип для представлення відношення як списку пар елементів.
type Relation a = [(a, a)]

-- Функція, яка перевіряє, чи є відношення транзитивним.
isTransitive :: Eq a => Relation a -> Bool
isTransitive [] = True  -- Порожнє відношення є транзитивним
isTransitive rel =
  all (\(a, b) -> all (\(x, y) -> not (b == x) || elem (a, y) rel) rel) rel

-- Приклади відношень:
relation1 :: Relation Int
relation1 = [(1, 2), (2, 3), (1, 3)]

relation2 :: Relation Int
relation2 = [(1, 2), (2, 3), (1, 4)]

relation3 :: Relation Int
relation3 = [(1, 6), (9, 1), (6, 5), (0, 0)]

relation4 :: Relation Int
relation4 = [(1, 2), (2, 4), (6, 5), (1, 4)]

relation5 :: Relation Int
relation5 = [(7, 8), (9, 10), (15, -5)]

main :: IO ()
main = do
  putStrLn $ "relation1 is transitive: " ++ show (isTransitive relation1)
  putStrLn $ "relation2 is transitive: " ++ show (isTransitive relation2)
  putStrLn $ "relation3 is transitive: " ++ show (isTransitive relation3)
  putStrLn $ "relation4 is transitive: " ++ show (isTransitive relation4)
  putStrLn $ "relation5 is transitive: " ++ show (isTransitive relation5)