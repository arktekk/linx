# Linx - a simple link representation

Linx is a tiny and simple library for building and matching links/paths in a typesafe way.
It models links as consisting of a root, literal parts and variable parts.
A link can be used both as a function and as an extractor for pattern matching

Linx has no dependencies and is currently cross compiled from 2.8.0 to 2.10.0-RC5

	import linx._
	
	val People = Root / "people"
	val Person = Root / "people" / *
	val Pets   = Root / "people" / * / "pets"
	val Pet    = Root / "people" / * / "pets" / *
	
Everything is immutable, so if you prefer a more terse style you can safely build on previous links to create new ones. This is exactly the same as the previous example
	
	val People = Root / "people"
	val Person = People / *
	val Pets   = Person / "pets"
	val Pet    = Pets / *
	
Your links can be used as functions

	Root()                 == "/"
	People()               == "/people"
	Person("personA")      == "/people/personA"
	Pets("personA")        == "/people/personA/pets"
	Pet("personA", "petB") == "/people/personA/pets/petB"
	
And for pattern matching	
	
	"/" match {
		case Root() =>
	}
	
	"/people" match {
		case People() =>
	}
	
	"/people/personA" match {
		case Person(person) => 
			// person == "personA"
	}
	
	"/people/personA/pets" match {
		case Pets(person) => 
			// person == "personA"
	}
	
	"/people/personA/pets/petB" match {
		case Pet(person, pet) => 
			// person == "personA"
			// pet == "petB"
	}	
	
Applying a link with the wrong number of arguments will fail at compile time
	
	Pet("personA")

	error: type mismatch;
	found   : java.lang.String("personA")
	required: (String, String)
	Pet("personA")

Pattern matching with the wrong number of arguments will fail at compile time

	"/people/personA/pets/petB" match {
		case Pet(person) =>
	}
	
	error: wrong number of arguments for value Pet of type Pet.type
	case Pet(person) =>