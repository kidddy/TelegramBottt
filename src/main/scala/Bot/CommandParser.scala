package Bot

import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Properties
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers


object CommandParser extends RegexParsers{

  override protected val whiteSpace: Regex = """ +""".r


  def createPoll: Parser[Commands.CreatePollCmd] = "/create_poll" ~> (word | "(" ~> word <~ ")") ~ anonimity.? ~
    visibility.? ~ date.? ~ date.? ^^ {case name ~ anonimity ~ visibility ~ startDate ~ endDate =>
      Commands.CreatePollCmd(name, anonimity, visibility, startDate, endDate)}

  def list: Parser[Commands.ListCmd] = "/list" ^^ {_ => new Commands.ListCmd}
  def deletePoll: Parser[Commands.DeletePollCmd] = "/delete_poll" ~> id ^^ {Commands.DeletePollCmd}
  def startPoll: Parser[Commands.StartPollCmd] = "/start_poll" ~> id ^^ {Commands.StartPollCmd}
  def stopPoll: Parser[Commands.StopPollCmd] = "/srop_poll" ~> id ^^ {Commands.StopPollCmd}
  def result: Parser[Commands.ResultCmd] = "/result" ~> id ^^ {Commands.ResultCmd}
  def begin: Parser[Commands.BeginCmd] = "/begin" ~> id ^^ {Commands.BeginCmd}
  def end: Parser[Commands.EndCmd] = "/end" ^^ {_ => new Commands.EndCmd}
  def view: Parser[Commands.ViewCmd] = "/view" ^^ {_ => new Commands.ViewCmd}
  def delete_question: Parser[Commands.DeletePollCmd] = "/delete_question" ~> id ^^ {Commands.DeletePollCmd}
  def answer: Parser[Commands.AnswerCmd] = "/answer" ~> id ~ sentence ^^ { case id ~ answer => Commands.AnswerCmd(id, answer)}

  def addQuestion: Parser[Commands.AddQuestionCmd] = {
    val variant = Parser(Properties.lineSeparator ~> ".*".r)
    val questionType = Parser("open"|"choice"|"multi")
    "/add_question" ~> (sentence | "(" ~> sentence <~ ")") ~
      (questionType | "(" ~> questionType <~ ")").? ~
      rep(variant) ^^ {case question ~ questionType ~ variants=> new Commands.AddQuestionCmd(question, questionType, variants)}
  }


  def word: Parser[String] = """\w+""".r
  def sentence: Parser[String] = """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r
  def anonimity: Parser[Boolean] = ("yes" | "no") ^^ (_ == "yes")
  def visibility: Parser[String] = "afterstop" | "continuous"
  def date: Parser[Date] = """\d\d:\d\d:\d\d:\d\d \d\d:\d\d:\d\d""" ^^
    {new SimpleDateFormat("hh:mm:ss yy:MM:dd").parse(_)}
  def id: Parser[Int] = """\d+""".r ^^ {_.toInt}


}

