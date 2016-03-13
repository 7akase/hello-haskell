-- Nat(HomCc(X,-), F) === F(X)
--
newtype CoYoneda f x = CoYoneda (forall a, (x -> a) -> f a

liftCoYoneda :: f x -> CoYoneda f x
liftCoYoneda x :: x fmap f x
lowerCoYoneda :: CoYoneda f x -> f x
lowerCoYoneda (CoYoneda y) = y id
