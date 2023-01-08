object Zad3 {
  import akka.actor._

  object Nadzorca {
    case class Init(liczbaPracownikow: Int)
    case class Zlecenie(tekst: List[String])
    case class Wynik(wordsCount: Int, pracownik: ActorRef)
  }
  class Nadzorca extends Actor {
    import Nadzorca._
    import Pracownik._
    def receive: Receive = {
      case Init(liczbaPracownikow) =>
        val pracownicy = (for { i <- 1 to liczbaPracownikow } yield context.actorOf(Props[Pracownik](), s"Pracownik-${i}")).toList
        context.become(waitingForZlecenie(pracownicy))
    }
    def waitingForZlecenie(pracownicy: List[ActorRef]): Receive = {
      case Zlecenie(tekst) =>
        val tekst1 = tekst.drop(pracownicy.length)
        val tekst2 = tekst.take(pracownicy.length)
        context.become(zlecenieInProgress(pracownicy, tekst.length, 0, 0, tekst1))
        pracownicy.zip(tekst2).foreach((a, t) => a ! Wykonaj(t, self))
    }
    def zlecenieInProgress(pracownicy: List[ActorRef], tekstLength: Int, receivedWyniki: Int, wynikiSum: Int, tekst: List[String]): Receive = {
      case Wynik(wordsCount, pracownik) => 
        val newReceivedWyniki = receivedWyniki + 1
        val newWynikiSum = wynikiSum + wordsCount

        println(s"${self.path.name} otrzymał: ${wordsCount} od ${pracownik.path.name} [${newReceivedWyniki}/${tekstLength}]")

        if tekstLength == newReceivedWyniki then
          println(s"WORDS COUNT: ${newWynikiSum}")
          context.become(waitingForZlecenie(pracownicy))
        else if !tekst.isEmpty then
          context.become(zlecenieInProgress(pracownicy, tekstLength, newReceivedWyniki, newWynikiSum, tekst.tail))
          pracownik ! Wykonaj(tekst.head, self)
        else
          context.become(zlecenieInProgress(pracownicy, tekstLength, newReceivedWyniki, newWynikiSum, List()))
    }
  }

  object Pracownik {
    case class Wykonaj(tekstFragment: String, nadzorca: ActorRef)
  }
  class Pracownik extends Actor {
    import Pracownik._
    import Nadzorca._
    def receive: Receive = {
      case Wykonaj(tekstFragment, nadzorca) => 
        println(s"${self.path.name} otrzymał: ${tekstFragment}")
        val number = tekstFragment.split(" ").length
        println(s"${self.path.name} zwraca: ${number}")
        nadzorca ! Wynik(number, self)
    }
  }

  def main(args: Array[String]): Unit = {
    import Nadzorca._

    def dane(): List[String] = {
      scala.io.Source.fromResource("ogniem_i_mieczem.txt").getLines.toList
    }

    val sys = ActorSystem("sys")

    val nadzorca = sys.actorOf(Props[Nadzorca](), "Nadzorca")

    nadzorca ! Init(5)
    nadzorca ! Zlecenie(dane())
  }
}
