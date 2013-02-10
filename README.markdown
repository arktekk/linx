# Linx - a simple and typesafe link representation

Linx is a tiny and simple library for building and matching links/paths in a typesafe way.
Links are built from the root up, consisting of literal and variable parts.
A link can be used both as a function and as an extractor for pattern matching.

Linx has no dependencies and cross compiles from 2.8.0 to 2.10.0

	import linx._

	val People = Root / "people"
	val Person = Root / "people" / 'person
	val Pets   = Root / "people" / 'person / "pets"
	val Pet    = Root / "people" / 'person / "pets" / 'pet

Everything is immutable, so if you prefer a more terse style you can safely build on previous links to create new ones.
This does exactly the same as the previous example.
(This style can be very useful when providing alternatives for the same resources)

	val People = Root / "people"
	val Person = People / 'person
	val Pets   = Person / "pets"
	val Pet    = Pets / 'pet

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
	  case Person("personA") => // matches
	}

	"/people/personA/pets" match {
	  case Pets("personA") => // matches
	}

	"/people/personA/pets/petB" match {
	  case Pet("personA", "petB") => // matches
	}	

The wrong number of arguments will fail at compile time

	Pet("personA", "petB", "unknown")

	error: too many arguments for method apply: (a: (String, String))String in trait Linx

Pattern matching with the wrong number of arguments will fail at compile time

	"/people/personA/pets/petB" match {
      case Pet(person, pet, unknown) =>
	}

	error: wrong number of arguments for value Pet of type Pet.type

Since Linx works directly on strings it can easily be used with libraries and frameworks that expose paths/links as strings

Here is an example showing how to do use Linx for both matching and creating links in an Unfiltered application

    import linx._
    import unfiltered.filter.Plan
    import unfiltered.request._
    import unfiltered.response.Html5

    object Example extends Plan with App {

      val People = Root / "people"
      val Person = People / 'person
      val Pets   = Person / "pets"
      val Pet    = Pets / 'pet

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


## Alternatives / Evolving your api
Lets say your application implements this api

	val Persons = Root / "persons"
	val Person  = Persons / 'person
	val Pets    = Person / "pets"
	val Pet     = Pets / 'pet

`"persons"` is poor english and you would like to change it to `"people"`
Simply changing `"persons"` to "people" would break all clients that are hardcoded to your url structure.
A friendlier alternative would be to support both versions, but that would lead to a lot of duplication in link definitions.
Using something like Unfiltered that is based on pattern matching would also bring even more duplication for links with variables
since you can't bind variables in alternative matches.

	val People  = Root / "people"
	val Persons = Root / "persons"

	val Person    = People / 'person
	val PersonOLD = Persons / 'person

	val Pets    = Person / "pets"
	val PetsOLD = Person / "pets"

	val Pet    = Pets / 'pet
	val PetOLD = Pets / 'pet

	// even more trouble
	... match {
	  case People() | Persons() => // alternative matches
	  case Person(person) | PersonOLD(person) => // won't compile
	}

To solve this, Linx can compose link alternatives with the same number of variables
You can add as many alternatives as you like

	val People = Root / "people" | Root / "persons"
	val Person = People / 'person
	val Pets   = Person / "pets"
	val Pet    = Pets / 'pet

And you can leave the rest of your code completely unchanged.
The People link will now match both `"/people"` and `"/persons"`,
and the Person, Pets and Pets links will all match correctly on links starting with both `"/people"` and `"/persons"`

	"/people/personA/pets/petA" match {
	    case Pet(person, pet) => // matches
	}

	"/persons/personA/pets/petA" match {
	    case Pet(person, pet) => // matches
	}
	
When using link alternatives using links as functions, they will always return the leftmost alternative which in this example will be `"/people"` etc.

If you need to retrieve all the available links you can call the `links` method

	Pet.links("personA", "petB") == Stream("/people/personA/pets/petA", "/persons/personA/pets/petA")

## Templates
Not everyone is ready for completely link driven apis yet. Pretty much every api
out there have either exclusively or partly documented link structures for developers to code against (e.g twitter)
As shown in the previous section, we can simplify supporting backwards compatible apis, but how do we document them ?
A common way of doing this is by showing all the links together with some documentation in a URL-template like way.

Twitter uses :variable to represent a variable in its url templates
    
	GET statuses/retweets/:id

In Linx this will look like this
    
	val Retweets = Root / "statuses" / "retweets" / 'id

To get a template for a link we need to provide a functions handling how a variable shoule be rendered
    
	def twitter(v:String) = ":" + v
    Retweets.template(twitter) == "/statuses/retweets/:id"

If we want to render our variables in a different way we simply provide a different function to render the variables
    
	def rfc6570(v:String) = "{" + v + "}"
    Retweets.template(rfc6570) == "/statuses/retweets/{id}"


Linx supports rendering multiple templates for links that have alternatives.
Templates are rendered from left to right
    
	val People = Root / "people" | Root / "persons"
    val Person = People / 'person
    val Pets   = Person / "pets"
    val Pet    = Pets / 'pet

    def twitter(v:String) = ":" + v
    Pet.templates(twitter) == Stream("/people/:person/pets/:pet", "/persons/:person/pets/:pet")

## ToString
the toString method on Linx is overridden to provide a URI-template (rfc6570) compatible template
    
	override def toString = template("{" + _ + "}")

