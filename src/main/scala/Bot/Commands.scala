package Bot

import java.util.Date


sealed trait Command

object Commands {
  case class CreatePollCmd(name: String,
                           anonimity: Option[Boolean],
                           visibility: Option[String],
                           startTime: Option[Date],
                           endTime: Option[Date]) extends Command
  case class ListCmd() extends Command
  case class DeletePollCmd(id: Int) extends Command
  case class StartPollCmd(id: Int) extends Command
  case class StopPollCmd(id: Int) extends Command
  case class ResultCmd(id: Int) extends Command
  case class BeginCmd(id: Int) extends Command
  case class EndCmd() extends Command
  case class ViewCmd() extends Command
  case class AddQuestionCmd(question: String, questionType: Option[String], variants: List[String]) extends Command
  case class DeleteQuestionCmd(id: Int) extends Command
  case class AnswerCmd(id: Int, answer: SomeAnswer) extends Command
}

