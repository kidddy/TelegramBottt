package Bot

import scala.util.{Failure, Success, Try}

trait Question{
  def question: String
  def answers: Map[String, Answer]
}

case class OpenQuestion(question: String,
                        answers: Map[String, OpenAnswer]) extends Question
case class ChoiceQuestion(question: String,
                          answersVariants: Map[Int, String],
                          answers: Map[String, ChoiceAnswer]) extends Question
case class MultiQuestion(question: String,
                         answersVariants: Map[Int, String],
                         answers: Map[String, MultiAnswer]) extends Question


trait Answer

case class OpenAnswer(answer: String) extends Answer
case class ChoiceAnswer(answer: Int) extends Answer
case class MultiAnswer(answer: List[Int]) extends Answer

case class SomeAnswer(answer: String){
  def asOpenAnswer(): Option[OpenAnswer] ={
    Option(OpenAnswer(answer))
  }

  def asChoiceAnswer(): Option[ChoiceAnswer] = {
    Try(ChoiceAnswer(Integer.parseInt(answer))) match {
      case Success(resultAnswer) => Option(resultAnswer)
      case Failure(_) => None
    }
  }

  def asMultiAnswer(): Option[MultiAnswer] = {
    Try(MultiAnswer(answer.split(" ").map(variant => Integer.parseInt(variant)).toList)) match {
      case Success(resultAnswer) => Option(resultAnswer)
      case Failure(_) => None
    }
  }
}