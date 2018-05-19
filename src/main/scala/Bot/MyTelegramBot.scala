package Bot

import info.mukel.telegrambot4s._
import api._
import methods._
import models._
import declarative._

import scala.io.Source


object MyTelegramBot extends TelegramBot with Polling with Commands {

  override def token: String = Source.fromFile("bot.token").getLines().mkString("")

  override def receiveMessage(msg: Message): Unit = {
    val text = msg.text.getOrElse("")
    val sender = msg.from.map(user => user.username.getOrElse("unknown_sender")).getOrElse("unknown_sender")
    val answer = BotCore.doCommand(sender, text)
    request(SendMessage(msg.source, answer))
  }

  def main(args: Array[String]): Unit = {
    run()
  }
}
