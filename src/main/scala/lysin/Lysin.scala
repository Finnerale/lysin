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

sealed trait Token
case object Equals extends Token
case object BracketOpen extends Token
case object BracketClose extends Token
case class DataType(name: String) extends Token
case class Identifier(value: String) extends Token
case class Literal(value: Int) extends Token

case class Program(statements: List[Statement])

sealed trait Statement

case class Declaration(name: Identifier) extends Statement

case class Assignment(name: Identifier, literal: Literal) extends Statement

case class Invocation(function: Identifier, param: Identifier) extends Statement

class LysinParser extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f\n]+".r

  def equals = "=" ^^ (_ => Equals)
  def bracket_open = "(" ^^ (_ => BracketOpen)
  def bracket_close = ")" ^^ (_ => BracketClose)

  def dataType: Parser[DataType] =
    "int".r ^^ { str => DataType(str) }

  def identifier: Parser[Identifier] =
    "[a-zA-Z]+[a-zA-Z0-9]*".r ^^ { str => Identifier(str) }

  def literal: Parser[Literal] =
    "[0-9]+".r ^^ { str => Literal(str.toInt) }

  def declaration: Parser[Declaration] =
    dataType ~ identifier ^^ {
      case _ ~ name => Declaration(name)
    }

  def assignment: Parser[Assignment] =
    identifier ~ equals ~ literal ^^ {
      case name ~ _ ~ value => Assignment(name, value)
    }

  def invocation: Parser[Invocation] =
    identifier ~ bracket_open ~ identifier ~ bracket_close ^^ {
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
      case decl @ Declaration(_) => declaration(stack, decl)
      case assi @ Assignment(_, _) => (stack, assignment(stack, assi))
      case invo @ Invocation(_, _) => (stack, invocation(stack, invo))
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
    val value: Int = assignment.literal.value
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

object LysinCli extends LysinParser {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Expected input file as argument")
      return
    }

    val srcFile = args(0)
    val source = Source.fromFile(srcFile)
    val append = (builder: StringBuilder, line: Char) => builder.append(line)
    val input = source.foldLeft(new StringBuilder)(append)

    val tree: Program = parse(this.program, input) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => {
        println("Failure: " + msg)
        Program(List());
      }
      case Error(msg, _) => {
        println("Error: " + msg)
        Program(List());
      }
    }

    val program = LysinCompiler.program(tree)

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

    val indented = program.lines().map("    " + _).reduce(_ + "\n" + _).get

    val asmFile = srcFile + ".asm"
    val writer = new PrintWriter(new File(asmFile))
    writer.write(header)
    writer.write(indented)
    writer.close()

    val objFile = srcFile + ".o"
    val objStatus = s"nasm -f elf64 -F dwarf -g -o $objFile $asmFile" !

    if (objStatus != 0) {
      println("Failed to build object file")
      return
    }

    val binFile = srcFile.replace(".ly", "")
    val binStatus = s"ld -o $binFile $objFile" !

    if (binStatus != 0) {
      println("Failed to build binary file")
      return
    }
  }
}

