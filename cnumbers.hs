import Prelude hiding (div, abs, exp) -- removes these functions that are also defined for complex numbers. the point is to define them.
import qualified Prelude as P -- this is to able the use of exp of a real number in the definition of exp of a complex number. When doing a qualified import one needs to call functions like P.exp


-- Data definition -------------------------------------------------------------
data Complex a = Mkz {re :: a , im :: a} deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (a,b) = Mkz {re = a, im = b}

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate z = Mkz {re = real z, im = -imaginary z}

abs :: Floating a => Complex a -> a
--abs = sqrt . ( ()**2 . re + ()**2 . im )
abs z = sqrt (real z**2 + imaginary z**2)


real :: Num a => Complex a -> a
real z= re z

imaginary :: Num a => Complex a -> a
imaginary z = im z



exp :: (Floating a) => Complex a -> Complex a
exp z = Mkz {re = P.exp (real z) * cos (imaginary z), im= P.exp (real z) * sin (imaginary z)}





-- binary operators ------------------------------------------------------------

div :: Fractional a => Complex a -> Complex a -> Complex a
div z w = Mkz {re = (real z * real w + imaginary z * imaginary w)/ squaredmod w , im= (imaginary z * real w - real z * imaginary w)/ squaredmod w}
          where squaredmod a = real (mul a (conjugate a))

mul :: Num a => Complex a -> Complex a -> Complex a
mul z w= Mkz {re = real z * real w - imaginary z * imaginary w , im= imaginary z * real w + real z * imaginary w}

add :: Num a => Complex a -> Complex a -> Complex a
add z w = Mkz {re = real z + real w , im= imaginary z + imaginary w}

sub :: Num a => Complex a -> Complex a -> Complex a
sub z w= Mkz {re = real z - real w , im= imaginary z - imaginary w}




main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS



let z = complex(2,3)
let w = complex (4,2)



--print(rlist)

print(add z z)
print(mul z w)
print(div z w)