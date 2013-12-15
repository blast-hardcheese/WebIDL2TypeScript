import util.parsing.combinator.RegexParsers

object Types {
  type JSType = String
  type TizenType = String
}

import Types._

sealed trait Token

case class MethodArgument(name: String, t: JSType) extends Token
case class Method(name: String, args: List[MethodArgument], t: JSType) extends PackageElements

case class Module(name: String, lines: List[ModuleElements]) extends Token

sealed trait ModuleElements extends Token
case class Typedef(t1: JSType, t2: TizenType) extends ModuleElements
case class Package(name: String, lines: List[PackageElements], extend: Option[String]) extends ModuleElements
case class Implementation(name: String, t: JSType) extends ModuleElements
case class Enum(name: String, defs: List[String]) extends ModuleElements

sealed trait PackageElements extends Token

case class PackageProperty(name: String, t: JSType) extends PackageElements
case class Const(name: String, t: JSType, value: String) extends PackageElements

object DocParser extends RegexParsers {
  val identifier = "[a-zA-Z][a-zA-Z0-9_]*".r
  val number = "[0-9]+(\\.[0-9]+)?".r

  val jsString = "DOMString" ^^ { _ => "String" }
  val jsNumber = (opt("unsigned") ~ ("short" | "double" | "long" | "int")) ^^ { _ => "Number" }
  val jsPassthrough = "void" | "boolean" | "Date"

  val builtinType = jsString | jsNumber | jsPassthrough

  val tizenType =
    "any" |
    "AbstractFilter" |
    "AttributeFilter" |
    "AttributeRangeFilter" |
    "ApplicationContextArraySuccessCallback" |
    "ApplicationContextId" |
    "ApplicationContext" |
    "ApplicationControlDataArrayReplyCallback" |
    "ApplicationControlData" |
    "ApplicationControl" |
    "ApplicationCertificate" |
    "ApplicationInformationEventCallback" |
    "ApplicationInformationArraySuccessCallback" |
    "ApplicationInformation" |
    "ApplicationId" |
    "ApplicationMetaData" |
    "ApplicationManagerObject" |
    "ApplicationManager" |
    "Application" |
    "Event" |
    "CompositeFilterType" |
    "CompositeFilter" |
    "SortModeOrder" |
    "SortMode" |
    "SimpleCoordinates" |
    "PackageId" |
    "PackageManagerObject" |
    "PackageManager" |
    "RequestedApplicationControl" |
    "PackageProgressCallback" |
    "PackageInformationArraySuccessCallback" |
    "PackageInformationEventCallback" |
    "PackageInformation" |
    "ErrorCallback" |
    "FindAppControlSuccessCallback" |
    "FilterMatchFlag" |
    "SuccessCallback" |
    "WebAPIException" |
    "WebAPIError" |
    "TizenObject" |
    "Tizen"

  val jsType = (builtinType | tizenType) ~ opt("[]") <~ opt("?") ^^ {
    case t ~ None => t
    case t ~ Some(a) => t + a
  }
  val optionalType = "optional" ~> jsType

  val enumString = "\"[A-Z_]+\"".r
  val enum = "enum" ~> identifier ~ "{" ~ repsep(enumString, ",") ~ "}" ^^ { case name ~ "{" ~ defs ~ "}" => Enum(name, defs) }
  val const = "const" ~> jsNumber ~ identifier ~ "=" ~ number ^^ { case t ~ name ~ "=" ~ num => Const(name, t, num) }
  val typedef = "typedef" ~> builtinType ~ tizenType ^^ { case t ~ i => Typedef(t, i) }

  val attribute = "readonly"
  val attributes = opt(attribute) ~ "attribute"
  val implementation = identifier ~ "implements" ~ identifier ^^ { case i ~ "implements" ~ t => Implementation(i, t) }


  val packageProperty = attributes ~> jsType ~ identifier ^^ { case t ~ i => PackageProperty(i, t) }
  val packageMethodArg = (jsType | optionalType) ~ identifier ^^ { case t ~ n => MethodArgument(n, t) }
  val packageMethodArgs = repsep(packageMethodArg, ",")
  val packageMethod = jsType ~ identifier ~ "(" ~ packageMethodArgs ~ ")" ^^ {
    case t ~ name ~ "(" ~ args ~ ")" => Method(name, args, t)
  }

  val module = "module" ~> identifier ~ ("{" ~> rep((enum | typedef | interface | implementation) <~ ";") <~ "};") ^^ {
    case i ~ lines => Module(i, lines)
  }
  val interfaceExtends = ":" ~> jsType
  val interface = "[" ~ interfaceBracket ~ "]" ~ "interface" ~> jsType ~ opt(interfaceExtends) ~ "{" ~ rep((const | packageMethod | packageProperty) <~ ";") ~ "}" ^^ {
    case name ~ ie ~ "{" ~ lines ~ "}" => Package(name, lines, ie)
  }
  val interfaceBracketTokens = "NoInterfaceObject"
  val interfaceBracketConstructor = "Constructor(" ~ packageMethodArgs ~ ")"
  val interfaceCallback = "Callback" ~ opt("=FunctionOnly")
  val interfaceBracket = repsep(interfaceBracketTokens | interfaceBracketConstructor | interfaceCallback, ",")

  def parseLine[T](parser: Parser[T], input: String) = parseAll(parser, input)
  def rawApply(input: String) = parseAll(module, input)
  def apply(input: String) = parseAll(module, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}


object App extends App {
  implicit class RichFile( file: java.io.File ) {
    def text = io.Source.fromFile( file )(io.Codec.UTF8).mkString

    def text_=( s: String ) {
      val out = new java.io.PrintWriter( file , "UTF-8")
      try{ out.print( s ) }
      finally{ out.close }
    }
  }

  type IndentLevel = Int

  def transformLines(lines: List[Token])(implicit level: IndentLevel): String = {
    lines.map(line => transform(line)(level + 1)).mkString("\n")
  }

  def indent()(implicit level: IndentLevel) = "  " * level

  def transform(token: Token)(implicit level: IndentLevel = 0): String = token match {
    case Module(name: String, lines: List[ModuleElements]) => transformLines(lines)
    case Typedef(t1: JSType, t2: TizenType) => s"""${indent}interface $t2 extends $t1 {}"""
    case Implementation(name: String, t: JSType) => s"""${indent}interface $name extends $t {}"""
    case Package(name, lines, None) => s"""interface $name {\n${transformLines(lines)}\n}\n"""
    case Package(name, lines, Some(t)) => s"""interface $name extends $t {\n${transformLines(lines)}\n}\n"""
    case Enum(name: String, enums: List[String]) => s"""declare enum $name { ${enums.mkString(", ")} }""" // TODO: Figure out a way to infer the type here
    case Const(name: String, t: JSType, value: String) => s"""$indent$name: $t; // $value"""
    case Method(name: String, args: List[MethodArgument], t: JSType) => s"""$indent$name(${args.map(transform).mkString(", ")}): $t;"""
    case MethodArgument(name: String, t: JSType) => s"$name: $t"
    case PackageProperty(name: String, t: JSType) => s"$indent$name: $t;"
  }

  for(arg <- args) {
    val s = io.Source.fromFile(new java.io.File(arg)).getLines.mkString("\n")

    println(DocParser.rawApply(s))

    val parsed = DocParser(s)

    println(parsed)
    println

    val out = transform(parsed)

    println(out)

    (new java.io.File(arg + ".d.ts")).text = out
  }
}
