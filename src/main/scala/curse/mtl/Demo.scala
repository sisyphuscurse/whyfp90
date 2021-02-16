package curse.mtl

import cats._
import cats.implicits._
import cats.data._
import cats.effect._
import cats.mtl.ApplicativeAsk
import curse.mtl.Demo.Config

object Demo extends App {
  // These are just String for simplicity
  type Config = String
  type Result = String


  def getConfig: IO[Config] = IO.pure("just config")

  // getConfig: cats.effect.IO[Config]

  def serviceCall(c: Config): IO[Result] = IO.pure(s"${c} returning")

  // serviceCall: (c: Config)cats.effect.IO[Result]

  def readerProgram: ReaderT[IO, Config, Result] = for {
    config <- ReaderT.ask[IO, Config]
    result <- ReaderT.liftF(serviceCall(config))
  } yield result

  // readerProgram: cats.data.ReaderT[cats.effect.IO,Config,Result]
  def main: IO[Result] = getConfig.flatMap(readerProgram.run)

  type Env = String
  type Request = String
  type Response = String

  def initialEnv: Env = ???

  def request(r: Request, env: Env): IO[Response] = ???

  def updateEnv(r: Response, env: Env): Env = ???

  // We also need some fake requests
  def req1: Request = ???

  def req2: Request = ???

  def req3: Request = ???

  def req4: Request = ???

  def requestWithState(r: Request): StateT[IO, Env, Response] = for {
    env <- StateT.get[IO, Env]
    resp <- StateT.liftF(request(r, env))
    _ <- StateT.modify[IO, Env](updateEnv(resp, _))
  } yield resp

  def stateProgram: StateT[IO, Env, Response] = for {
    resp1 <- requestWithState(req1)
    resp2 <- requestWithState(req2)
    resp3 <- requestWithState(req3)
    resp4 <- requestWithState(req4)
  } yield resp4

  def main2: IO[(Env, Response)] = stateProgram.run(initialEnv)

  // main: cats.effect.IO[(Env, Response)]

  object Another {

    import cats.mtl._
    import cats.mtl.instances.all._

    def readerProgram[F[_] : Monad : LiftIO](implicit A: ApplicativeAsk[F, Config]): F[Result] = for {
      config <- A.ask
      result <- serviceCall(config).to[F]
    } yield result

    type RX[A] = ReaderT[IO, Config, A]

    val materializedProgram2 = readerProgram[RX]

    def main3: IO[Result] = getConfig.flatMap(materializedProgram2.run)

    sealed trait AppError

    case object InvalidConfig extends AppError

    type MonadAppError[F[_]] = MonadError[F, AppError]
    type ApplicativeConfig[F[_]] = ApplicativeAsk[F, Config]

    type EitherApp[A] = EitherT[IO, AppError, A]
    type Stack[A] = ReaderT[EitherApp, Config, A]

    def validConfig(c: Config): Boolean = ???


    def program[F[_] : MonadAppError : ApplicativeConfig : LiftIO]: F[Result] = for {
      config <- ApplicativeAsk[F, Config].ask
        .ensure(InvalidConfig)(validConfig)
      result <- serviceCall(config).to[F]
    } yield result

    val materializedProgram: Stack[Result] = program[Stack]

    def main: IO[Either[AppError, Result]] =
      EitherT.liftF(getConfig).flatMap(materializedProgram.run).value
  }

}
