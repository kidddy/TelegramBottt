package Bot

import org.scalatest._
import java.util.Date

class ParserTest extends FlatSpec with Matchers {

  "CommandParser" should "parse /create_poll #1" in {
    val cmd = CommandParser.parseCommand("/create_poll epic_poll (no) (afterstop)").get
    cmd shouldBe a[Commands.CreatePollCmd]
    cmd match {
      case cmd: Commands.CreatePollCmd => {
        cmd.name shouldBe "epic_poll"
        cmd.anonimity shouldBe Some(false)
        cmd.visibility shouldBe Some("afterstop")
        cmd.startTime shouldBe None
        cmd.endTime shouldBe None
      }
    }
  }

  "CommandParser" should "parse /create_poll #2" in {
    val cmd = CommandParser.parseCommand("/create_poll (one_more_poll) yes continuous 13:22:00 18:03:26 (13:22:00 18:03:27)").get
    cmd match {
      case cmd: Commands.CreatePollCmd => {
        cmd.name shouldBe "one_more_poll"
        cmd.anonimity shouldBe Some(true)
        cmd.visibility shouldBe Some("continuous")
        cmd.startTime shouldBe Some(new Date(18,3,26, 13, 22, 0))
        cmd.endTime shouldBe Some(new Date(18,3,27, 13, 22, 0))
      }
    }
  }

  "CommandParser" should "parse /list" in {
    val cmd = CommandParser.parseCommand("/list").get
    cmd shouldBe a [Commands.ListCmd]
  }

  "CommandParser" should "parse /delete_poll" in {
    val cmd = CommandParser.parseCommand("/delete_poll 42").get
    cmd shouldBe a [Commands.DeletePollCmd]
    cmd match {
      case cmd: Commands.DeletePollCmd => cmd.id shouldBe 42
    }
  }

  "CommandParser" should "parse /start_poll " in {
    val cmd = CommandParser.parseCommand("/start_poll (42)").get
    cmd shouldBe a [Commands.StartPollCmd]
    cmd match {
      case cmd: Commands.StartPollCmd => cmd.id shouldBe 42
    }
  }

  "CommandParser" should "parse /stop_poll " in {
    val cmd = CommandParser.parseCommand("/stop_poll (42)").get
    cmd shouldBe a [Commands.StopPollCmd]
    cmd match {
      case cmd: Commands.StopPollCmd => cmd.id shouldBe 42
    }
  }

  "CommandParser" should "parse /result " in {
    val cmd = CommandParser.parseCommand("/result (42)").get
    cmd shouldBe a [Commands.ResultCmd]
    cmd match {
      case cmd: Commands.ResultCmd => cmd.id shouldBe 42
    }
  }

  "CommandParser" should "parse /begin " in {
    val cmd = CommandParser.parseCommand("/begin (42)").get
    cmd shouldBe a [Commands.BeginCmd]
    cmd match {
      case cmd: Commands.BeginCmd => cmd.id shouldBe 42
    }
  }

  "CommandParser" should "parse /end " in {
    val cmd = CommandParser.parseCommand("/end").get
    cmd shouldBe a [Commands.EndCmd]
  }

  "CommandParser" should "parse /view " in {
    val cmd = CommandParser.parseCommand("/view").get
    cmd shouldBe a [Commands.ViewCmd]
  }

  "CommandParser" should "parse /add_question #1" in {
    val sCmd =
      """/add_question (Why cats?)
        |(cute)""".stripMargin
    val cmd = CommandParser.parseCommand(sCmd).get
    cmd shouldBe a [Commands.AddQuestionCmd]
    cmd match {
      case cmd: Commands.AddQuestionCmd => {
        cmd.variants.size == 1 shouldBe true
        cmd.variants.contains("cute") shouldBe true
        cmd.question shouldBe "Why cats?"
        cmd.questionType shouldBe None
      }
    }
  }

  "CommandParser" should "parse /add_question #2" in {
    val sCmd =
      """/add_question (Are you crazy?) choice
        |(yes)
        |(no)""".stripMargin
    val cmd = CommandParser.parseCommand(sCmd).get
    cmd shouldBe a [Commands.AddQuestionCmd]
    cmd match {
      case cmd: Commands.AddQuestionCmd => {
        cmd.variants.size == 2 shouldBe true
        cmd.variants.contains("yes") shouldBe true
        cmd.variants.contains("no") shouldBe true
        cmd.question shouldBe "Are you crazy?"
        cmd.questionType shouldBe Some("choice")
      }
    }
  }

  "CommandParser" should "parse /add_question #3" in {
    val sCmd =
      """/add_question (Are you crazy?) (multi)
        |(yes)
        |(no)""".stripMargin
    val cmd = CommandParser.parseCommand(sCmd).get
    cmd shouldBe a [Commands.AddQuestionCmd]
    cmd match {
      case cmd: Commands.AddQuestionCmd => {
        cmd.variants.size == 2 shouldBe true
        cmd.variants.contains("yes") shouldBe true
        cmd.variants.contains("no") shouldBe true
        cmd.question shouldBe "Are you crazy?"
        cmd.questionType shouldBe Some("multi")
      }
    }
  }

  "CommandParser" should "parse /delete_question " in {
    val cmd = CommandParser.parseCommand("/delete_question 42").get
    cmd shouldBe a [Commands.DeleteQuestionCmd]
    cmd match {
      case cmd: Commands.DeleteQuestionCmd => cmd.id shouldBe 42
    }
  }

  "CommandParser" should "parse /answer " in {
    val cmd = CommandParser.parseCommand("/answer 42 is the answer").get
    cmd shouldBe a [Commands.AnswerCmd]
    cmd match {
      case cmd: Commands.AnswerCmd => {
        cmd.id shouldBe 42
        cmd.answer.answer shouldBe "is the answer"
      }
    }
  }
}
