main :: IO ()
main = return ()

id_ x = x
eval (x, y) = x y
exchange (x, y) = (y, x)
compose x y z = x (y z)
curry_ f a b = f (a, b)
associate ((x, y), z) = (x, (y, z))
