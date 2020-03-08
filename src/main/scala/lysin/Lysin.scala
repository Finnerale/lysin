package lysin

import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.Success
import scala.util.Failure
import scala.io.Source
import java.io.PrintWriter
import java.io.File
import sys.process._
import unindent._
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

sealed trait LysinToken
case object Equals extends LysinToken
case object BracketOpen extends LysinToken
case object BracketClose extends LysinToken
case class Identifier(value: String) extends LysinToken

sealed trait Literal extends LysinToken
case class LiteralInteger(value: Int) extends Literal

case class Program(statements: List[Statement])
sealed trait Statement
case class Declaration(name: Identifier, dtype: Identifier) extends Statement
case class Assignment(name: Identifier, literal: Literal) extends Statement
case class Invocation(function: Identifier, param: Identifier) extends Statement

object LysinLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f\n]+".r

  def equals = "=" ^^ (_ => Equals)
  def bracket_open = "(" ^^ (_ => BracketOpen)
  def bracket_close = ")" ^^ (_ => BracketClose)

  def identifier: Parser[Identifier] =
    "[a-zA-Z]+[a-zA-Z0-9]*".r ^^ { str => Identifier(str) }

  def literalInteger: Parser[LiteralInteger] =
    "[0-9]+".r ^^ { str => LiteralInteger(str.toInt) }

  def literal: Parser[Literal] = literalInteger

  def tokens: Parser[List[LysinToken]] =
    phrase(rep1(equals | bracket_open | bracket_close | identifier | literal))
}

class LysinTokenReader(tokens: Seq[LysinToken]) extends Reader[LysinToken] {
  override def first: LysinToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[LysinToken] = new LysinTokenReader(tokens.tail)
}

object LysinParser extends Parsers {
  override type Elem = LysinToken

  def apply(tokens: Seq[LysinToken]): Program = {
    val reader = new LysinTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Program(List())
      case Success(result, next) => result
    }
  }

  private def identifier: Parser[Identifier] =
    accept("identifier", { case id @ Identifier(_) => id })

  private def literal: Parser[Literal] =
    accept("literal", { case id @ LiteralInteger(_) => id })

  def declaration: Parser[Declaration] =
    identifier ~ identifier ^^ {
      case dtype ~ name => Declaration(name, dtype)
    }

  def assignment: Parser[Assignment] =
    identifier ~ Equals ~ literal ^^ {
      case name ~ _ ~ value => Assignment(name, value)
    }

  def invocation: Parser[Invocation] =
    identifier ~ BracketOpen ~ identifier ~ BracketClose ^^ {
      case name ~ _ ~ param ~ _ => Invocation(name, param)
    }

  def program: Parser[Program] =
    phrase(rep1(declaration | assignment | invocation)) ^^ {
      rawTokens => Program(rawTokens)
    }
}

case class StackEntry(name: String, size: Int, offset: Int)

object LysinCompiler {
  def program(program: Program) = {
    val prolog = i"""
    push rbp
    mov rbp, rsp
    """
    val epilog = i"""
    mov rsp, rbp
    pop rbp
    """
    val stack = List(StackEntry("", 0, 0))
    program.statements
      .foldLeft((stack, new StringBuilder(prolog)))((sum, statement) => {
        val next = this.statement(sum._1, statement)
        (next._1, sum._2.append('\n').append(next._2))
      })._2.append('\n').append(epilog).toString
  }

  def statement(stack: List[StackEntry], statement: Statement) = {
    statement match {
      case id @ Declaration(_, _) => declaration(stack, id)
      case id @ Assignment(_, _) => (stack, assignment(stack, id))
      case id @ Invocation(_, _) => (stack, invocation(stack, id))
    }
  }

  def declaration(stack: List[StackEntry], declaration: Declaration) = {
    val size = 8
    val asm = s"sub rsp, $size"
    val offset = stack.head.offset + size
    val nstack = StackEntry(declaration.name.value, size, offset) :: stack
    (nstack, asm)
  }

  def assignment(stack: List[StackEntry], assignment: Assignment) = {
    val offset = stack
      .find(entry => entry.name == assignment.name.value).get.offset
    val value: Int = assignment.literal.asInstanceOf[LiteralInteger].value
    val asm = s"mov qword [rbp - $offset], $value"
    asm
  }

  def invocation(stack: List[StackEntry], invocation: Invocation) = {
    val param = stack
      .find(entry => entry.name == invocation.param.value).get
    val paramOffset = param.offset
    val paramSize = param.size
    val fn_name = invocation.function.value
    val asm = i"""
    push qword [rbp - $paramOffset]
    call $fn_name
    add rsp, $paramSize
    mov [rbp - $paramOffset], rax
    """
    asm
  }
}

object LysinCli extends App {
  val file_extension = ".lyn"

  if (args.length != 1) {
    println("Expected input file as argument")
    System.exit(1)
  }

  val srcFile = args(0)

  if (!srcFile.endsWith(file_extension)) {
    println("Expected .lyn file as input")
    System.exit(1)
  }

  val source = Source.fromFile(srcFile)
  val append = (builder: StringBuilder, line: Char) => builder.append(line)
  val input = source.foldLeft(new StringBuilder)(append)

  val tokens: List[LysinToken] = LysinLexer.parse(LysinLexer.tokens, input) match {
    case LysinLexer.Success(matched, _) => matched
    case LysinLexer.Failure(msg, _) => {
      println("Failure: " + msg)
      System.exit(1)
      List()
    }
    case LysinLexer.Error(msg, _) => {
      println("Error: " + msg)
      System.exit(1)
      List()
    }
  }

  val program: Program = LysinParser(tokens)

  val assembly = LysinCompiler.program(program)

  val header = i"""
  section .text

  global _start

  double:
      push rbp
      mov rbp, rsp

      mov rax, [rbp + 16]
      add rax, rax

      mov rsp, rbp
      pop rbp
      ret

  _start:

  """

  val indented = assembly.lines().map("    " + _).reduce(_ + "\n" + _).get

  val asmFile = srcFile.replace(file_extension, ".asm")
  val writer = new PrintWriter(new File(asmFile))
  writer.write(header)
  writer.write(indented)
  writer.close()

  val objFile = srcFile.replace(file_extension, ".o")
  val objStatus = s"nasm -f elf64 -F dwarf -g -o $objFile $asmFile" !

  if (objStatus != 0) {
    println("Failed to build object file")
    System.exit(1)
  }

  val binFile = srcFile.replace(file_extension, "")
  val binStatus = s"ld -o $binFile $objFile" !

  if (binStatus != 0) {
    println("Failed to build binary file")
    System.exit(1)
  }
}
