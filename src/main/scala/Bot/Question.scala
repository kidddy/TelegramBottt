package Bot

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
