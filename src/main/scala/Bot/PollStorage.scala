package Bot

import java.util.Date

import Bot.Commands._

import scala.collection.immutable

case class Poll(name: String,
                anonimity: Boolean,
                visibility: Int,
                startTime: Option[Date],
                endTime: Option[Date],
                questions: Map[Int, Question],
                owner: String,
                started: Boolean) {
  val maxQuestionID: Iterator[Int] = Stream.from(questions.size + 1).iterator

  def getNextQuestionId: Int = maxQuestionID.next()
}

object PollHandler {
  def addQuestion(question: Question, poll: Poll): (Int, Poll) = {
    val id = poll.getNextQuestionId
    val newPoll = poll.copy(questions = poll.questions + (id -> question))
    id -> newPoll
  }

  def deleteQuestion(questionId: Int, poll: Poll): Poll = {
    poll.copy(questions = poll.questions - questionId)
  }

  def setStarted(poll: Poll, flag: Boolean): Poll = {
    poll.copy(started = flag)
  }

  def getPollResults(pollId: Int): String = {
    val poll = PollStorage.polls(pollId)
    poll.questions.keys.map(questionId => {
      val question = poll.questions(questionId)
      question match {
        case question: OpenQuestion => question.answers.keys.map(
          sender => {
            val s_sender = if (poll.anonimity) "" else s"$sender:"
            s"- $s_sender ${question.answers(sender).answer}"
          }
        ).foldLeft(s"$questionId) ${question.question}")((res, answer) => s"$res\n\t$answer")

        case question: ChoiceQuestion => question.answersVariants.keys.map(
          variantId => {
            val senders = question.answers.keys.filter(sender => question.answers(sender).answer == variantId).toList
            val percentage = math.round((senders.size.toFloat / question.answers.size.toFloat) * 100)
            val s_senders = if (poll.anonimity) "" else s" (${senders.mkString(", ")})"
            s"$variantId) ${question.answersVariants(variantId)} - $percentage%$s_senders"
          }
        ).foldLeft(s"$questionId) ${question.question}")((res, answer) => s"$res\n\t$answer")

        case question: MultiQuestion => question.answersVariants.keys.map(
          variantId => {
            val senders = question.answers.keys.filter(
              sender => {
                println(s"AAAAAAA $variantId $sender:: ${question.answers(sender)}")
                question.answers(sender).answer.contains(variantId)
              }
            ).toList
            val percentage = math.round((senders.size.toFloat / question.answers.size.toFloat) * 100)
            val s_senders = if (poll.anonimity) "" else s" (${senders.mkString(", ")})"
            s"$variantId) ${question.answersVariants(variantId)} - $percentage%$s_senders"
          }
        ).foldLeft(s"$questionId) ${question.question}")((res, answer) => s"$res\n\t$answer")
      }
    }).foldLeft(s"Poll #$pollId results:")((res, question) => s"$res\n$question")
  }
}

object Constants {
  val VISIBILITY_AFTERSTOP = 0
  val VISIBILITY_CONTINUOUS = 1
}


object PollStorage {
  var polls: Map[Int, Poll] = immutable.Map[Int, Poll]()

  var clientsSelectedIdMap: Map[String, Int] = immutable.Map[String, Int]()

  val maxId: Iterator[Int] = Stream.from(1).iterator

  def getNewId: Int = maxId.next()

  def addPoll(id: Int, poll: Poll): Unit = {
    polls = polls + (id -> poll)
  }

  def deletePoll(id: Int): Unit = {
    polls = polls - id
  }

  def selectPoll(sender: String, id: Int): Unit = {
    clientsSelectedIdMap = clientsSelectedIdMap + (sender -> id)
  }

  def unselectPoll(sender: String): Unit = {
    clientsSelectedIdMap = clientsSelectedIdMap - sender
  }

  def releasePoll(id: Int): Unit = {
    val users = clientsSelectedIdMap.keys.filter(userName => clientsSelectedIdMap(userName) == id).toList
    users.foreach(userName => unselectPoll(userName))
  }

  def getSelectedPoll(sender: String): Option[Poll] = {
    getSelectedPollId(sender) match {
      case 0 => None
      case id => polls.get(id)
    }
  }

  def getSelectedPollId(sender: String): Int = {
    clientsSelectedIdMap.getOrElse(sender, 0)
  }

  def setStated(id: Int, value: Boolean): Unit = {
    val newPoll = PollHandler.setStarted(polls(id), value)
    addPoll(id, newPoll)
  }
}

object CommandHandler {
  def createPoll(sender: String, cmd: CreatePollCmd): String = {
    val poll = Poll(name = cmd.name,
      anonimity = cmd.anonimity.getOrElse("yes") == "yes",
      visibility = cmd.visibility.getOrElse("afterstop") match {
        case "afterstop" => Constants.VISIBILITY_AFTERSTOP
        case "continuous" => Constants.VISIBILITY_CONTINUOUS
      },
      startTime = cmd.startTime,
      endTime = cmd.endTime,
      questions = immutable.Map[Int, Question](),
      owner = sender,
      started = false)
    val id = PollStorage.getNewId
    PollStorage.addPoll(id, poll)

    id.toString
  }

  def addQuestion(sender: String, cmd: AddQuestionCmd): String = {
    handleSelectedPoll(sender: String, poll => {
      doUnlessStarted(poll, poll => {
        checkOwner(poll, sender, (poll: Poll) => {
          val question = cmd.questionType.getOrElse("open") match {
            case "open" => OpenQuestion(question = cmd.question, immutable.Map[String, OpenAnswer]())
            case "choice" => {
              val answersMap = cmd.variants.zip(Stream.from(1)).map(data => data._2 -> data._1).toMap
              ChoiceQuestion(cmd.question, answersMap, immutable.Map[String, ChoiceAnswer]())
            }
            case "multi" => {
              val answersMap = cmd.variants.zip(Stream.from(1)).map(data => data._2 -> data._1).toMap
              MultiQuestion(cmd.question, answersMap, immutable.Map[String, MultiAnswer]())
            }
          }
          val res = PollHandler.addQuestion(question, poll)
          PollStorage.addPoll(PollStorage.getSelectedPollId(sender), res._2)
          res._1.toString
        })
      })
    })
  }

  def deleteQuestion(sender: String, cmd: DeleteQuestionCmd): String = {
    handleSelectedPoll(sender, poll => {
      doUnlessStarted(poll, poll => {
        checkOwner(poll, sender, (poll: Poll) => {
          handleQuestion(poll, cmd.id, (_: Question) => {
            val newPoll = PollHandler.deleteQuestion(cmd.id, poll)
            PollStorage.addPoll(PollStorage.getSelectedPollId(sender), newPoll)
            s"Question ${cmd.id} deleted"
          })
        })
      })
    })
  }

  def handlePoll(id: Int, func: Poll => String): String = {
    PollStorage.polls.get(id).map(func).getOrElse(s"Error: poll $id not found")
  }

  def handleQuestion(poll: Poll, questionId: Int, func: Question => String): String = {
    poll.questions.get(questionId).map(func).getOrElse(s"Question $questionId not found")
  }

  def handleSelectedPoll(sender: String, func: Poll => String): String = {
    PollStorage.getSelectedPoll(sender).map(func).getOrElse("Choose poll first")
  }

  def checkOwner(poll: Poll, userName: String, func: Poll => String): String = {
    if (poll.owner.equals(userName)) func(poll) else "You're not owner"
  }

  def doIfStarted(poll: Poll, func: Poll => String): String = {
    if (poll.started) func(poll) else "Poll is not started"
  }

  def doUnlessStarted(poll: Poll, func: Poll => String): String = {
    if (!poll.started) func(poll) else "Error: cannot modify poll (It's running)"
  }

  def ifAnswerNotExists(poll: Poll, question: Question, sender: String, func: Poll => String): String = {
    if (!question.answers.contains(sender)) func(poll) else "You have answered already"
  }

  def doCommand(sender: String, cmd: Command): String = {
    cmd match {
      case cmd: CreatePollCmd => createPoll(sender, cmd)

      case cmd: ListCmd => {
        PollStorage.polls.keys.foldLeft("Current polls:") {
          (res, pollId) => s"$res\n$pollId) ${PollStorage.polls(pollId).name}"
        }
      }

      case cmd: DeletePollCmd => {
        handlePoll(cmd.id, (poll: Poll) => {
          checkOwner(poll, sender, (_: Poll) => {
            PollStorage.deletePoll(cmd.id)
            PollStorage.releasePoll(cmd.id)
            s"Poll ${cmd.id} deleted"
          })
        })
      }

      case cmd: StartPollCmd => {
        handlePoll(cmd.id, poll => {
          checkOwner(poll, sender, poll => {
            doUnlessStarted(poll, _ => {
              PollStorage.setStated(cmd.id, value = true)
              s"Poll #${cmd.id} started"
            })
          })
        })
      }

      case cmd: StopPollCmd => {
        handlePoll(cmd.id, poll => {
          checkOwner(poll, sender, poll => {
            doIfStarted(poll, _ => {
              PollStorage.setStated(cmd.id, value = false)
              s"Poll #${cmd.id} stopped"
            })
          })
        })
      }

      case cmd: ResultCmd => {
        handlePoll(cmd.id, poll => {
          poll.visibility match {
            case Constants.VISIBILITY_CONTINUOUS => PollHandler.getPollResults(cmd.id)
            case Constants.VISIBILITY_AFTERSTOP => doUnlessStarted(poll, poll => PollHandler.getPollResults(cmd.id))
          }
        })
      }

      case cmd: BeginCmd => {
        handlePoll(cmd.id, (_: Poll) => {
          PollStorage.selectPoll(sender, cmd.id)
          s"Poll #${cmd.id} selected"
        })
      }
      case cmd: EndCmd => {
        PollStorage.unselectPoll(sender)
        "Poll unselected"
      }

      case cmd: ViewCmd => {
        handleSelectedPoll(sender, poll => {
          poll.questions.keys.map(questionId => {
            val question = poll.questions(questionId)
            val answers = question match {
              case _: OpenQuestion => ""
              case question: ChoiceQuestion => {
                question.answersVariants.keys.map(answerId => s"\n\t$answerId) ${question.answersVariants(answerId)}").mkString("")
              }
              case question: MultiQuestion => {
                question.answersVariants.keys.map(answerId => s"\n\t$answerId) ${question.answersVariants(answerId)}").mkString("")
              }
            }
            s"Question #$questionId: ${question.question}$answers"
          }).foldLeft(s"Poll #${PollStorage.getSelectedPollId(sender)}:")((res, question) => {
            val resQuestion = question.split("\n").map(line => s"\t$line").mkString("\n")
            s"$res\n$resQuestion"
          })
        })
      }

      case cmd: AddQuestionCmd => addQuestion(sender, cmd)

      case cmd: DeleteQuestionCmd => deleteQuestion(sender, cmd)

      case cmd: AnswerCmd => {
        handleSelectedPoll(sender, poll => {
          doIfStarted(poll, poll => {
            handleQuestion(poll, cmd.id, question => {
              ifAnswerNotExists(poll, question, sender, _ => {
                try {
                  val newQuestion = question match {
                    case question: OpenQuestion => {
                      val answer = OpenAnswer(cmd.answer)
                      val newAnswers = question.answers + (sender -> answer)
                      question.copy(answers = newAnswers)
                    }
                    case question: ChoiceQuestion => {
                      val idAnswer = Integer.parseInt(cmd.answer)
                      if (!question.answersVariants.contains(idAnswer)) return "Bad answer"
                      val newAnswers = question.answers + (sender -> ChoiceAnswer(idAnswer))
                      question.copy(answers = newAnswers)
                    }
                    case question: MultiQuestion => {
                      val idAnswers = cmd.answer.split(" ").map(s_arg => Integer.parseInt(s_arg)).toList
                      for (id <- idAnswers){
                        if (!question.answersVariants.contains(id)) return "Bad answer"
                      }
                      val answer = MultiAnswer(idAnswers)
                      val newAnswers = question.answers + (sender -> answer)
                      question.copy(answers = newAnswers)
                    }
                  }
                  val newQuestions = poll.questions + (cmd.id -> newQuestion)
                  val newPoll = poll.copy(questions = newQuestions)
                  PollStorage.addPoll(PollStorage.getSelectedPollId(sender), newPoll)
                  "Answer added"
                } catch {
                  case _: NumberFormatException => "Bad answer"
                }
              })
            })
          })
        })
      }
    }
  }
}

object BotCore {

  def doCommand(sender: String, s_cmd: String): String = {
    val my_sCmd = s_cmd.split("\n").map(line => line.trim).mkString(" ")
    val parsed = CommandParser.parseCommand(my_sCmd)
    parsed match {
      case CommandParser.Success(cmd, _) => CommandHandler.doCommand(sender, cmd)
      case CommandParser.Failure(error, _) => s"Error: $error"
    }
  }
}