//object HelloWorld{
//  def main(args: Array[String]) {
//    println("Hello, world!")
//  }
//}

import java.text.SimpleDateFormat

import scala.util.parsing.combinator.RegexParsers

val formatter = new SimpleDateFormat("hh:m:ss yy:MM:dd")
object CommandParser extends RegexParsers{
  def commandName: Parser[String] = """([a-zA-Z_])""".r ^^ {_.toString}
  def longCommandname: Parser[String] = "(" ~> commandName <~ ")" ^^ {_.toString}
  def anonimity: Parser[Boolean] = ("yes" | "no") ^^ (_ == "yes")
  def visibility: Parser[String] = "afterstop" | "continuous" ^^ {_.toString}
  def date: Parser[String] = """(\d\d:\d\d:\d\d:\d\d \d\d:\d\d:\d\d)""" ^^
    {new SimpleDateFormat("hh:mm:ss yy:MM:dd").parse(_)}
  def id: Parser[String] = """\d+""".r ^^ {_.toInt}
}