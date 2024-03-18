apply :: (a -> b) -> a -> b
apply f x = f x

twice :: (a -> a) -> a -> a
twice f x = f ( f x )

id :: a -> a
id x = x

flip ::(a -> b -> c) -> b -> a -> c
flip f x y = f y x

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

uflip :: ((a,b) -> c) -> (b,a) -> c
uflip f p = f (swap p)

const :: a -> b -> a
const x y = x

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f ( g x )
