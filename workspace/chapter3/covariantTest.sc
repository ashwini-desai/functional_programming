import com.exercises.chapter3

sealed trait List[A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]
Cons(1, Nil)

// A <: B
// List[A]<:List[B]
trait Animal
case class Dog(petName: String) extends Animal

val dogs: List[Dog] = Nil
val animals: List[Animal] = bullDogs

sealed trait ListCovariant[+A]
case object NilCovariant extends ListCovariant[Nothing]
case class ConsCovariant[+A](head: A, tail: List[A]) extends ListCovariant[A]

val covariantDogs: ListCovariant[Dog] = NilCovariant
val covariantAnimals: ListCovariant[Animal] = covariantDogs

//+A
//A <: B
//List[A] <: List[B]

Nothing <: every type