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

}