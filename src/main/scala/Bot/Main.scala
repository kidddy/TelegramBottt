package Bot


import scala.io.Source

object Main {
  def split_commands(input: String): List[(String, String)] = {
    val splited = input.split("#").toList.map(msg => msg.trim)
    splited.map(msg => {
      val res = msg.split(":", 2).toList
      val sender = res(0)
      val cmd = res(1)
      sender -> cmd
    })
  }

  def readFromFile(filename: String): Unit = {
    val input_file = Source.fromFile(filename)

    val input = input_file.getLines().map(line => line.trim).mkString(" ")

    val smt = split_commands(input)
    smt.foreach(sender_cmd => {
      val sender = sender_cmd._1
      val cmdString = sender_cmd._2
      val answer = BotCore.doCommand(sender, cmdString)
      println(s"$sender <- $answer")
    })
  }

  def main(args: Array[String]): Unit = {
    readFromFile("input.txt")
  }
}

