package net.degoes.zio

import zio._
import zio.clock.Clock

object motivation {
  final case class Effect[+A](unsafeRun: () => A) { self =>
    final def map[B](f: A => B): Effect[B] =
      Effect(() => f(self.unsafeRun()))

    final def flatMap[B](f: A => Effect[B]): Effect[B] =
      Effect(() => f(self.unsafeRun()).unsafeRun())

    final def zip[B](that: => Effect[B]): Effect[(A, B)] =
      Effect(() => (self.unsafeRun(), that.unsafeRun()))

    final def zipLeft[B](that: => Effect[B]): Effect[A] = (self zip that).map(_._1)

    final def <*[B](that: => Effect[B]): Effect[A] = self zipLeft that

    final def zipRight[B](that: => Effect[B]): Effect[B] = (self zip that).map(_._2)

    final def *>[B](that: => Effect[B]): Effect[B] = self zipRight that
  }

  object Effect {
    def succeed[A](a: => A): Effect[A] = Effect(() => a)
  }

  def putStrLn(line: String): Effect[Unit] = Effect(() => println(line))

  val getStrLn: Effect[String] = Effect(() => scala.io.StdIn.readLine())

  val program: Effect[Unit] =
    Effect { () =>
      putStrLn("What is your name?").unsafeRun()
      val name = getStrLn
      putStrLn(s"Your name is: $name").unsafeRun()
    }

  val program2: Effect[Unit] =
    putStrLn("What is your name?").flatMap(_ => getStrLn.flatMap(name => putStrLn(s"Your name is: $name")))

  program2.unsafeRun()

  val program3: Effect[Unit] = for {
    _    <- putStrLn("What is your name?")
    name <- getStrLn
    _    <- putStrLn(s"Your name is: $name")
  } yield ()

  program3.unsafeRun()

}

object HelloWorld extends App {
  import zio.console._

  def run(args: List[String]) =
    putStrLn("Hello World!").map(_ => 0)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  def run(args: List[String]) =
    (for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Good to meet you, $name!")
    } yield 0) orElse ZIO.succeed(StdInputFailed)
}

object ZIOTypes extends App {
  import zio.console._

  // Future[A]
  // ZIO[R, E, A]
  // A - Success value
  // E - Failure value
  // R - Environment type (the type of environment required to run the effect)

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Good to meet you, $name!")
    } yield 0) orElse UIO(1)
}

object NumberGuesser extends App {
  import zio.console._
  import zio.random._

  def analyseAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly")
    else putStrLn(s"You did not guess correctly. The answer was $random")

  def run(args: List[String]) =
    (for {
      random <- nextInt(10)
      _      <- putStrLn("Please guess a number from 0 to 10: ")
      guess  <- getStrLn
      _      <- analyseAnswer(random, guess)
    } yield ()).fold(_ => 1, _ => 0)
}

object AlarmApp extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDouble(input: String) =
      for {
        double <- ZIO.effect(input.toDouble).refineToOrDie[NumberFormatException]
      } yield (double * 1000).toInt.milliseconds

    def fallback(input: String) =
      putStrLn(s"The input $input in not a valid double") zipRight getAlarmDuration

    for {
      _        <- putStrLn("How many seconds would you like to wait before sounding the alarm?")
      input    <- getStrLn
      duration <- parseDouble(input).orElse(fallback(input))
    } yield duration
  }

  def run(args: List[String]) =
    (for {
      duration <- getAlarmDuration
      _        <- ZIO.sleep(duration)
    } yield ()).foldCauseM(error => putStrLn(s"Error: ${error.prettyPrint}").map(_ => 1), _ => ZIO.succeed(0))
}
