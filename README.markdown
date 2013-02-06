# Linx - a simple and typesafe link representation

Linx is a tiny and simple library for building and matching links/paths in a typesafe way.
Links are built from the root up, consisting of literal and variable parts.
A link can be used both as a function and as an extractor for pattern matching.

Linx has no dependencies and is currently cross compiled from 2.8.0 to 2.10.0

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

Since Linx works directly on strings it can easily be used with libraries and frameworks that expose paths/links as strings

Here is an example showing how to do use Linx for both matching and creating links in an Unfiltered application

    import linx._
    import unfiltered.filter.Plan
    import unfiltered.request._
    import unfiltered.response.Html5

    object Example extends Plan with App {

      val People = Root / "people"
      val Person = Root / "people" / *
      val Pets   = Root / "people" / * / "pets"
      val Pet    = Root / "people" / * / "pets" / *

      case class Owner(name:String, pets:Map[String, String])

      val people = Map(
        "1" -> Owner("Paris", Map("1" -> "Tinkerbell", "2" -> "Bambi")),
        "2" -> Owner("Snoop Dog", Map("1" -> "Frank Sinatra", "2" -> "Miles Davis")))

      def intent = {
        case Path(Root()) =>
          Html5(<a href={People()}>People</a>)

        case Path(People()) =>
          Html5(people.toSeq.map{
            case (id, person) => <p><a href={Person(id)}>{person.name}</a></p>
          })

        case Path(Person(person)) =>
          val p = people(person)
          Html5(<a href={Pets(person)}>{p.name} Pets</a>)

        case Path(Pets(person)) =>
          val p = people(person)
          Html5(<h1>{p.name}</h1> ++ p.pets.toSeq.map{
            case (id, name) => <p><a href={Pet(person, id)}>{name}</a></p>
          })

        case Path(Pet(person, pet)) =>
          val name = people(person).pets(pet)
          Html5(<h1>{name}</h1>)
      }

      unfiltered.jetty.Http(8080).plan(this).run()
    }
