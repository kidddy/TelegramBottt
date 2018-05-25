package Bot

import org.scalatest._

class PollStorageTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  override protected def afterEach(): Unit = {
    BotCore.clearPolls()
  }
  val sender1 = "Kex"

  "Create poll cmd" should "create poll" in {
    val cmd = "/create_poll (testPoll)"

    val answer1 = BotCore.doCommand(sender1, cmd)
    val answer2 = BotCore.doCommand(sender1, cmd)
    answer1.equals(answer2) shouldBe false
  }

  "Create poll cmd with wrong date" should "return error msg" in {
    val cmd = "/create_poll (testPoll) 13:22:00 18:03:26 13:22:00 18:03:24"

    val answer1 = BotCore.doCommand(sender1, cmd)
    answer1 shouldBe "Wrong dates"
  }

  "List cmd" should "return string with list of polls" in {
    val cmd = "/list"
    val answer = BotCore.doCommand(sender1, cmd)
    answer shouldBe "Current polls:"
  }

  "Wrong cmd" should "should return msg" in {
    val cmd = "/dfghjk"
    val answer = BotCore.doCommand(sender1, cmd)
    answer shouldBe "Error: `/answer' expected but `/' found"
  }

  "Create poll continuous" should "contain continuous" in {
    val cmd = "/create_poll (testPoll) continuous"

    val answer1 = BotCore.doCommand(sender1, cmd)
    PollStorage.polls(Integer.parseInt(answer1)).visibility shouldBe Constants.VISIBILITY_CONTINUOUS
  }

  "Delete poll" should "try to del" in {
    val cmd = "/create_poll (testPoll) continuous"
    val cmd2 = "/delete_poll (1)"
    BotCore.doCommand(sender1, cmd)
    val ans = BotCore.doCommand(sender1, cmd2)
    ans shouldBe "Error: poll 1 not found"
  }

  "Delete poll 3" should "try to del 3" in {
    val cmd = "/create_poll (testPoll)"
        val cmd2 = "/delete_poll 5"
    BotCore.doCommand(sender1, cmd)
    val answer = BotCore.doCommand(sender1, cmd2)
    answer shouldBe "Poll 5 deleted"
  }

  "Start Poll" should "test start poll" in {
    val cmd = "/create_poll (testPoll)"
    val cmd2 = "/start_poll 6 "
    BotCore.doCommand(sender1, cmd)
    val answer = BotCore.doCommand(sender1, cmd2)
    answer shouldBe "Poll #6 started"
  }

  "Stop Poll" should "test stop poll" in {
    val cmd = "/create_poll (testPoll)"
    val cmd2 = "/stop_poll 7"
    val cmd3 = "/start_poll 7"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd2)
    BotCore.doCommand(sender1, cmd3)
    val answer = BotCore.doCommand(sender1, cmd2)
    answer shouldBe "Poll #7 stopped"
  }

  "Delete Poll" should "test delete" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 8"
    val cmd4 = "/add_question (Why cats?)"
    val cmd5 = "/delete_question 1"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    val answer = BotCore.doCommand(sender1, cmd5)
    answer shouldBe "Question 1 deleted"
  }

  "Open answer" should "test open answer" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 9"
    val cmd4 = "/add_question (Why cats?)"
    val cmd5 = "/start_poll 9"
    val cmd6 = "/answer 1 cause"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    BotCore.doCommand(sender1, cmd5)
    val answer = BotCore.doCommand(sender1, cmd6)
    answer shouldBe "Answer added"
  }

  "Delete Poll 2 error" should "wait for error msg" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 10"
    val cmd4 = """/add_question (Why cats?) choice
        |(cute)
        |(pretty)"""
    val cmd5 = "/start_poll 10"
    val cmd6 = "/answer 1 They are cute"
    val cmd7 = "/result 2"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    BotCore.doCommand(sender1, cmd5)
    BotCore.doCommand(sender1, cmd6)
    val answer = BotCore.doCommand(sender1, cmd7)
    answer shouldBe "Error: poll 2 not found"
  }

  "Add multi question" should "add question" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 11"
    val cmd4 = "/add_question (Why cats?) multi (pretty) (cute)"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    val answer = BotCore.doCommand(sender1, cmd4)
    answer shouldBe "1"
  }

  "Unselect poll" should "test reselect" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 12"
    val cmd4 = "/end 12"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    val answer = BotCore.doCommand(sender1, cmd4)
    answer shouldBe "Poll unselected"
  }

  "check visibility" should "v" in {
    val cmd = "/create_poll (testPoll) (continuous)"
    val cmd3 = "/begin 13"
    val cmd4 = """/add_question (Why cats?) multi
                 |(cute)
                 |(pretty)
                 |(slozno)"""
    val cmd5 = "/start_poll 13"
    val cmd6 = "/answer 1 2 3"
    val cmd7 = "/result 13"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    BotCore.doCommand(sender1, cmd5)
    BotCore.doCommand(sender1, cmd6)
    val answer = BotCore.doCommand(sender1, cmd7)
    answer shouldBe "Poll #13 results:\n1) Why cats?"
  }

  "check visibility afterstop" should "v aftersop" in {
    val cmd = "/create_poll (testPoll) (afterstop)"
    val cmd3 = "/begin 14"
    val cmd4 = """/add_question (Why cats?) multi
                 |(cute)
                 |(pretty)
                 |(slozno)"""
    val cmd5 = "/start_poll 14"
    val cmd6 = "/answer 1 2 3"
    val cmd7 = "/result 14"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    BotCore.doCommand(sender1, cmd5)
    BotCore.doCommand(sender1, cmd6)
    val answer = BotCore.doCommand(sender1, cmd7)
    answer shouldBe "Error: cannot modify poll (It's running)"
  }

  "check answer" should "answer" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 15"
    val cmd4 = "/add_question (Why cats?)"
    val cmd5 = "/start_poll 15"
    val cmd6 = "/answer 1 lala"
    val cmd7 = "/stop_poll 15"
    val cmd8 = "/result 15"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    BotCore.doCommand(sender1, cmd5)
    BotCore.doCommand(sender1, cmd6)
    BotCore.doCommand(sender1, cmd7)
    val answer = BotCore.doCommand(sender1, cmd8)
    answer shouldBe "Poll #15 results:\n1) Why cats?\n\t-  lala"
  }

  "view poll" should "view" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 16"
    val cmd4 = "/add_question (Why cats?)"
    val cmd5 = "/view"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    val answer = BotCore.doCommand(sender1, cmd5)
    answer shouldBe "Poll #16:\n\tQuestion #1: Why cats?"
  }

  "check choise answer" should "choise answer" in {
    val cmd = "/create_poll (testPoll)"
    val cmd3 = "/begin 17"
    val cmd4 = "/add_question (Why cats?) choice (lala) (mymy)"
    val cmd5 = "/start_poll 17"
    val cmd6 = "/answer 1 1"
    val cmd7 = "/stop_poll 17"
    val cmd8 = "/result 17"
    BotCore.doCommand(sender1, cmd)
    BotCore.doCommand(sender1, cmd3)
    BotCore.doCommand(sender1, cmd4)
    BotCore.doCommand(sender1, cmd5)
    BotCore.doCommand(sender1, cmd6)
    BotCore.doCommand(sender1, cmd7)
    val answer = BotCore.doCommand(sender1, cmd8)
    answer shouldBe "Poll #17 results:\n1) Why cats?\n\t1) lala - 100%\n\t2) mymy - 0%"
  }
}