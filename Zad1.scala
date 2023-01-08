import akka.actor._
import scala.concurrent.duration._
import scala.util.Random
/*
  W konfiguracji projektu wykorzystana została wtyczka
  sbt-revolver. W związku z tym uruchamiamy program poleceniem

    reStart

  a zatrzymujemy pisząc (mimo przesuwających się komunikatów)

     reStop

  i naciskając klawisz ENTER. Jeśli czynności powyższe
  już wykonywaliśmy to możemy też przywołać poprzednie
  polecenia używając strzałek góra/dół na klawiaturze.
*/

// Przykład wykorzystania Planisty (Scheduler)
object Zad1 {



  object Zamek {
    case class Init(enemyCastle: ActorRef)
    case class SygnalDoStrzalu()
    case class Strzala()
  }
  class Zamek extends Actor {
    import Zamek._
    import Obronca._
    def receive: Receive = {
      case Init(enemyCastle) =>
        val obroncy = (for { i <- 1 to 100 } yield context.actorOf(Props[Obronca](), s"${self.path.name}-Obronca-${i}")).toList
        obroncy.foreach(context.watch(_))
        context.become(gotowy(obroncy, enemyCastle))
        println(s"${self.path.name} gotowy")
    }
    def gotowy(obroncy: List[ActorRef], enemyCastle: ActorRef): Receive = {
      case Terminated(obronca) =>
        val bezTrupa = obroncy.length - 1
        println(s"${self.path.name}: zostało ${bezTrupa}")
        if bezTrupa == 0 then
          println(s"!!! ${self.path.name} stracił wszystkich obrońców i przegrał !!!")
          context.system.terminate()
        else
          context.become(gotowy(obroncy.filter(_ != obronca), enemyCastle))
      case SygnalDoStrzalu() =>
        println(s"${self.path.name}: otrzymano sygnał do strzału")
        obroncy.foreach(_ ! RozkazDoStrzalu(enemyCastle))
      case Strzala() =>
        val obronca = Random.shuffle(obroncy).head
        obronca ! Postrzal(obroncy.length)
    }
  }

  object Obronca {
    case class RozkazDoStrzalu(enemyCastle: ActorRef)
    case class Postrzal(iloscObroncow: Int)
  }
  class Obronca extends Actor {
    import Obronca._
    import Zamek._
    def receive: Receive = {
      case Postrzal(iloscObroncow) =>
        val szansaNaTrafienie = iloscObroncow.toDouble/(2 * 100)
        val randomNumber = Random.between(0.0, 1.0)
        if Random.between(0.0, 1.0) < szansaNaTrafienie then context.stop(self)
      case RozkazDoStrzalu(enemyCastle) =>
        enemyCastle ! Strzala()
    }
  }

  object SilaWyzsza {
    case class Init(zamek1: ActorRef, zamek2: ActorRef)
    case class Tick()
  }
  class SilaWyzsza() extends Actor {
    import SilaWyzsza._
    import Zamek._
    def receive: Receive = {
      case SilaWyzsza.Init(zamek1, zamek2) =>
        context.become(gotowy(zamek1, zamek2))
    }
    def gotowy(zamek1: ActorRef, zamek2: ActorRef): Receive = {
      case Tick =>
        zamek1 ! SygnalDoStrzalu()
        zamek2 ! SygnalDoStrzalu()
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("system")
    import system.dispatcher

    val zamek1 = system.actorOf(Props[Zamek](), "Zamek1")
    val zamek2 = system.actorOf(Props[Zamek](), "Zamek2")
    zamek1 ! Zamek.Init(zamek2)
    zamek2 ! Zamek.Init(zamek1)

    val silaWyzsza = system.actorOf(Props[SilaWyzsza](), "SilaWyzsza")
    silaWyzsza ! SilaWyzsza.Init(zamek1, zamek2)

    Thread.sleep(5)

    val ticker = system.scheduler.scheduleWithFixedDelay(
      Duration.Zero,
      1000.milliseconds,
      silaWyzsza,
      SilaWyzsza.Tick
    )

    // system.terminate()

  }
}
