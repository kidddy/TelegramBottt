package Bot

import java.util.Date

import scala.util.parsing.combinator.RegexParsers


object CommandParser extends RegexParsers{



  def createPoll: Parser[Commands.CreatePollCmd] = "/create_poll" ~>
    bracketsWrap(word) ~ //name
    bracketsWrap(anonimity).? ~ //anonimity
    bracketsWrap(visibility).? ~ //visibility
    bracketsWrap(date).? ~ // startDate
    bracketsWrap(date).? ^^ {case name ~ anonimity ~ visibility ~ startDate ~ endDate =>
      Commands.CreatePollCmd(name, anonimity, visibility, startDate, endDate)}

  def list: Parser[Commands.ListCmd] = "/list" ^^ {_ => new Commands.ListCmd}
  def deletePoll: Parser[Commands.DeletePollCmd] = "/delete_poll" ~> bracketsWrap(id) ^^ {Commands.DeletePollCmd}
  def startPoll: Parser[Commands.StartPollCmd] = "/start_poll" ~> bracketsWrap(id) ^^ {Commands.StartPollCmd}
  def stopPoll: Parser[Commands.StopPollCmd] = "/stop_poll" ~> bracketsWrap(id) ^^ {Commands.StopPollCmd}
  def result: Parser[Commands.ResultCmd] = "/result" ~> bracketsWrap(id) ^^ {Commands.ResultCmd}
  def begin: Parser[Commands.BeginCmd] = "/begin" ~> bracketsWrap(id) ^^ {Commands.BeginCmd}
  def end: Parser[Commands.EndCmd] = "/end" ^^ {_ => new Commands.EndCmd}
  def view: Parser[Commands.ViewCmd] = "/view" ^^ {_ => new Commands.ViewCmd}
  def delete_question: Parser[Commands.DeleteQuestionCmd] = "/delete_question" ~> bracketsWrap(id) ^^ {Commands.DeleteQuestionCmd}
  def answer: Parser[Commands.AnswerCmd] = "/answer" ~> bracketsWrap(id) ~ bracketsWrap(sentence) ^^ { case id ~ answer => Commands.AnswerCmd(id, SomeAnswer(answer))}

  def addQuestion: Parser[Commands.AddQuestionCmd] = "/add_question" ~>
    ("(" ~> sentence <~ ")") ~
    bracketsWrap(questionType).? ~
    rep("(" ~> sentence <~")") ^^ {case question ~ questionType ~ questions => Commands.AddQuestionCmd(question, questionType, questions)}


  def bracketsWrap[R](parser: Parser[R]): Parser[R] = parser | "(" ~> parser <~ ")"

  def word: Parser[String] = """\w+""".r
  def sentence: Parser[String] = """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r
  def anonimity: Parser[Boolean] = ("yes" | "no") ^^ (_ == "yes")
  def visibility: Parser[String] = "afterstop" | "continuous"
  def questionType: Parser[String] = "open" | "choice" | "multi"
  def date: Parser[Date] = {
    val d: Parser[Int] = """\d+""".r ^^ (num => Integer.parseInt(num))
    val ddd: Parser[(Int, Int, Int)] = (d <~ ":") ~ d ~ (":" ~> d) ^^ {case d1 ~ d2 ~ d3 => (d1, d2, d3)}
    ddd ~ ddd ^^ {case time ~ date => new Date(date._1, date._2, date._3, time._1, time._2, time._3)}
  }
  def id: Parser[Int] = """\d+""".r ^^ {_.toInt}

  def main_pattern: Parser[Command] = createPoll | list | deletePoll | startPoll | stopPoll | result | begin | end |
    view | delete_question | addQuestion | answer

  def parseCommand(line: String): ParseResult[Command] = parse(main_pattern, line)
}

