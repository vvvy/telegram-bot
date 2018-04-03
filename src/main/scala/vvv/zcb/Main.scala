package vvv.zcb

import java.io.File

import cats.data.Kleisli
import com.typesafe.config.ConfigFactory
import org.http4s.client._
import org.http4s.{HttpService, _}
import org.http4s.Method._
import org.http4s.circe._
import io.circe.{Encoder, Json, Printer}
import io.circe.syntax._
import io.circe.generic.auto._


trait TelegramProtocolCodec {
  val prntr = Printer.noSpaces.copy(dropNullKeys = true)
  def jsonEOf[E : io.circe.Encoder]: EntityEncoder[E] = jsonEncoderWithPrinterOf[E](prntr)

  case class User(id: Long,
                   is_bot: Boolean,
                   first_name: String,
                   last_name: Option[String],
                   username: Option[String],
                   language_code: Option[String])


  case class BotInfo(ok: Boolean, result: User)
  implicit val BotInfoEntityDecoder : EntityDecoder[BotInfo] = jsonOf[BotInfo]

  sealed trait Keyboard

  case class InlineKeyboardButton(text: String,
                                  url: Option[String],
                                  callback_data: Option[String],
                                  switch_inline_query: Option[String],
                                  switch_inline_query_current_chat: Option[String]
                                 )

  case class InlineKeyboardMarkup(inline_keyboard: Seq[InlineKeyboardButton]) extends Keyboard

  case class KeyboardButton(text:	String, request_contact: Option[Boolean], request_location: Option[Boolean])

  case class ReplyKeyboardMarkup(keyboard: Seq[Seq[KeyboardButton]],
                                 resize_keyboard: Option[Boolean],
                                 one_time_keyboard: Option[Boolean],
                                 selective: Option[Boolean]
                                ) extends Keyboard

  implicit val kenc: Encoder[Keyboard] = new Encoder[Keyboard] {
    def apply(a: Keyboard): Json = a match {
      case ik: InlineKeyboardMarkup => ik.asJson
      case rk: ReplyKeyboardMarkup => rk.asJson
    }
  }

  object ParseModes {
    val Markdown = "Markdown"
    val HTML = "HTML"
  }
  case class SendMessage(
                          chat_id: Long,
                          text: String,
                          parse_mode: Option[String] = None,
                          disable_web_page_preview: Option[Boolean] = None,
                          disable_notification:	Option[Boolean] = None,
                          reply_to_message_id: Option[Long] = None,
                          reply_markup: Option[Keyboard] = None
                        )
  implicit val SendMessageEntityEncoder : EntityEncoder[SendMessage] = jsonEOf[SendMessage]

  case class Chat(id: Long,
                  `type`: String,
                  first_name: String,
                  last_name: Option[String],
                  username: Option[String])

  case class MessageEntity(`type`: String,
                           offset: Int,
                           length: Int,
                           url: Option[String],
                           user: Option[User])

  case class Message(message_id: Long,
                      from: Option[User],
                      date: Int,
                      chat: Chat,
                      text: Option[String],
                      entities: Option[Seq[MessageEntity]])

  case class Update(update_id: Long,
                    message: Option[Message],
                    edited_message: Option[Message])

  case class GetUpdates(ok: Boolean, result: Seq[Update])

  implicit val GetUpdatesEntityDecoder : EntityDecoder[GetUpdates] = jsonOf[GetUpdates]

}

trait TelegramConfig {
  def fopt(s: String) = {
    val f = new File((s))
    if(f.isFile) Some(f) else None
  }

  val baseConf = ConfigFactory.load().getConfig("zclimatebot")

  val conf = fopt(".passwd")
    .map(ConfigFactory.parseFile(_).withFallback(baseConf))
    .getOrElse(baseConf)
}

trait TelegramRestApi {
  self: TelegramConfig =>
  import org.http4s.client.blaze._
  import scala.concurrent.duration._

  val maxLongPollDuration = 90.seconds

  val httpClient = PooledHttp1Client(config = BlazeClientConfig.defaultConfig.copy(
    responseHeaderTimeout = maxLongPollDuration + 30.seconds,
    idleTimeout = maxLongPollDuration + 5.seconds
  ))

  val endpoint = Uri
    .fromString(conf.getString("api_endpoint") + conf.getString("api_token"))
    .getOrElse(sys.error("Invalid URI: either api_endpoint or api_token malformed"))

  val getMe = endpoint / "getMe"
  //println(httpClient.expect[BotInfo](getMe).unsafeRun())

  def getUpdates(offset: Long, limit: Int = 16, timeout: Long = maxLongPollDuration.toSeconds) =
    (endpoint / "getUpdates")
      .withQueryParam("offset", offset)
      .withQueryParam("limit", limit)
      .withQueryParam("timeout", timeout)
  //println(httpClient.expect[GetUpdates](getUpdates(0)).unsafeRun())

  val sendMessage = endpoint / "sendMessage"
  //println(httpClient.expect[String](POST(sendMessage, SendMessage(164157700, "HALLO again!!", None, None, None, None, None))).unsafeRun())
}

trait TelegramAppState {
  self: TelegramProtocolCodec =>

  /**
    * GetUpdates offset host
    * @tparam S
    */
  trait GUOH[S] {
    def get: S => Long
    def set: Long => S => S
    def set(su: Seq[Update])(s: S): S = if (su.isEmpty) s else set(su.map(_.update_id).max + 1)(s)
    def apply(su: Seq[Update])(s: S): (S, Long) = if (su.isEmpty) (s, get(s)) else {
      val off = su.map(_.update_id).max + 1
      (set(off)(s), off)
    }

  }
}

trait TelegramPollClient extends
  TelegramProtocolCodec with
  TelegramConfig with
  TelegramRestApi with
  TelegramAppState {

  import fs2.Task

  implicit val strategy: fs2.Strategy

  def recvLoop[S: GUOH](startState: S)(stateT: (S, Seq[Update]) => Task[S]): Task[S] = {
    def recvTask(offset: Long) = httpClient.expect[GetUpdates](getUpdates(offset))
    val guoh = implicitly[GUOH[S]]

    def recvStep(state: S): Task[S] = recvTask(guoh.get(state))
      .flatMap { case GetUpdates(ok, updates) => stateT(state, updates).map(guoh.set(updates)) }
      .flatMap { state => Task.suspend(recvStep(state)) }
      .attemptFold(_ => state, identity) //ignore errors, loop back

    recvStep(startState)
  }
}

object ConcurrencyUtils {

  /**
    * Thread-safe variable host
    */
  trait VH[T] {
    /**
      * Mutates the variable, providing a result of type A
      * @param f mutator+extractor
      * @tparam A type of the result
      * @return the result
      */
    def apply[A](f: T => (T, A)): A
    def extract[A](f: T => A): A = apply[A](v => (v, f(v)))
    def mutate(f: T => T) = apply[Unit](v => (f(v), ()))
  }

  /**
    * Channel-based impl
    * @param init
    * @tparam T
    */
  class CVH[T](init: T) extends VH[T] {
    private val ch = new scala.concurrent.Channel[T]
    def apply[A](st: T => (T, A)): A = {
      val (s1, a) = st(ch.read)
      ch.write(s1)
      a
    }
  }
}




trait TelegramPollClientAndServer extends
  TelegramProtocolCodec with
  TelegramConfig with
  TelegramRestApi with
  TelegramAppState {
  import fs2.Task

  implicit val strategy: fs2.Strategy
  import ConcurrencyUtils.CVH


  trait BotAppBase[S] {
    val s: CVH[S]
    val guoh: GUOH[S]

    def recvLoop(stateT: Seq[Update] => S => S): Task[Unit] = {
      def recvTask(offset: Long) = httpClient.expect[GetUpdates](getUpdates(offset))

      //todo: log errors
      def recvStep(offset: Long): Task[Unit] = recvTask(offset)
        .map { case GetUpdates(ok, updates) => s(stateT(updates) andThen guoh(updates)) }
        .flatMap { newOffset => Task.suspend(recvStep(newOffset)) }
        .attemptFold(_ => (), identity)  //ignore errors, loop back

      s.extract(s => recvStep(guoh.get(s)))
    }

    def liftH(v: S => (S, HttpService)): HttpService = s(v)
  }

  class BotApp[S: GUOH](
                         init: S,
                         stateT: Seq[Update] => S => S,
                         svcs: (S => (S, HttpService))*) extends BotAppBase[S] {
    val s = new CVH[S](init)
    val guoh = implicitly[GUOH[S]]

    import org.http4s.server.blaze._
    import cats.implicits._

    val serverT = BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(svcs.map(liftH).reduce(_ |+| _), "/")
      .start
    val clientT = recvLoop(stateT)

    def run: S = {
      serverT.race(clientT).unsafeRun()
      s.extract(identity)
    }
  }
}


object Main extends TelegramPollClient {
  import fs2.Task
  implicit val strategy = fs2.Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.Implicits.global)

  implicit val gUOH_Long: GUOH[Long] = new GUOH[Long] {
    override def get = identity
    override def set = v => _ => v
  }
  def sample0() = {
    val loop = recvLoop(0L){ (s, su) =>
      println(s"> $su")
      val pt = Task.parallelTraverse(su) {
        case Update(_, Some(Message(_, Some(u), date, chat, text, _)), _) =>
          httpClient.expect[String](POST(sendMessage, SendMessage(chat.id, s"r: $text"))).map(v => println(s"< $v"))
      }
      pt.map(_ => s)
    }
    loop.unsafeRun()
  }



  def sample1 = {
    /*val sf = new BotApp[(String, String)](
      init = ("", ""),
      stateT = { case (s, su) => if (su.isEmpty) 0 else su.map(_.update_id).max }
    )*/
  }


  def main(args: Array[String]): Unit = {
    sample0()
  }

}
