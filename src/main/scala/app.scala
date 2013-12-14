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
case class Package(name: String, lines: List[PackageElements]) extends ModuleElements
case class Implementation(name: String, t: JSType) extends ModuleElements

sealed trait PackageElements extends Token

case class PackageProperty(name: String, t: JSType) extends PackageElements

object DocParser extends RegexParsers {
  val identifier = "[a-zA-Z][a-zA-Z0-9]*".r

  val jsString = "DOMString" ^^ { _ => "string" }
  val jsNumber = "long" | "int" ^^ { _ => "number" }
  val jsPassthrough = "void" | "boolean" | "Date"

  val builtinType = jsString | jsNumber | jsPassthrough

  val tizenType =
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
    "ApplicationManager" |
    "Application" |
    "PackageId" |
    "RequestedApplicationControl" |
    "ErrorCallback" |
    "FindAppControlSuccessCallback" |
    "SuccessCallback"

  val jsType = (builtinType | tizenType) ~ opt("[]") <~ opt("?") ^^ {
    case t ~ None => t
    case t ~ Some(a) => t + a
  }
  val optionalType = "optional" ~> jsType

  val typedef = "typedef" ~> builtinType ~ tizenType ^^ { case t ~ i => Typedef(t, i) }

  val attribute = "readonly"
  val attributes = opt(attribute) ~ "attribute"
  val implementation = identifier ~ "implements" ~ identifier ^^ { case i ~ "implements" ~ t => Implementation(i, t) }


  val packageLine = rep(packageMethod | packageProperty)
  val packageProperty = attributes ~> jsType ~ identifier <~ ";" ^^ { case t ~ i => PackageProperty(i, t) }
  val packageMethodArg = (jsType | optionalType) ~ identifier ^^ { case t ~ n => MethodArgument(n, t) }
  val packageMethodArgs = repsep(packageMethodArg, ",")
  val packageMethod = jsType ~ identifier ~ "(" ~ packageMethodArgs ~ ")" <~ ";" ^^ {
    case t ~ name ~ "(" ~ args ~ ")" => Method(name, args, t)
  }

  val module = "module" ~> identifier ~ ("{" ~> rep((typedef | interface | implementation) <~ ";") <~ "};") ^^ {
    case i ~ lines => Module(i, lines)
  }
  val interface = "[" ~ interfaceBracket ~ "]" ~ "interface" ~> identifier ~ "{" ~ packageLine ~ "}" ^^ {
    case name ~ "{" ~ lines ~ "}" => Package(name, lines)
  }
  val interfaceBracketTokens = "NoInterfaceObject"
  val interfaceBracketConstructor = "Constructor(" ~ packageMethodArgs ~ ")"
  val interfaceCallback = "Callback" ~ opt("=FunctionOnly")
  val interfaceBracket = repsep(interfaceBracketTokens | interfaceBracketConstructor | interfaceCallback, ",")

  def parseLine[T](parser: Parser[T], input: String) = parseAll(parser, input)
  def apply(input: String) = parseAll(module, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}

object App extends App {
  val s = io.Source.fromFile(new java.io.File(args(0))).getLines.mkString("\n")

  val parsed = DocParser(s)

  println(parsed)

  def transform(token: Token): String = token match {
    case Module(name: String, lines: List[ModuleElements]) => ""

    case MethodArgument(name: String, t: JSType) => ""
    case Method(name: String, args: List[MethodArgument], t: JSType) => ""
    case Typedef(t1: JSType, t2: TizenType) => ""
    case Package(name: String, lines: List[PackageElements]) => ""
    case Implementation(name: String, t: JSType) => ""
    case PackageProperty(name: String, t: JSType) => ""
  }

  println(transform(parsed))
}
