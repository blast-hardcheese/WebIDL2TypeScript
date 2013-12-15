import util.parsing.combinator.RegexParsers

object Types {
  type JSType = String
  type ExtendedType = String
}

import Types._

sealed trait Token

case class Type(name: String, isArray: Boolean, isOptional: Boolean) extends Token

case class MethodArgument(name: String, t: Type) extends Token
case class Method(name: String, args: List[MethodArgument], t: Type) extends PackageElements

case class Module(name: String, lines: List[ModuleElements]) extends Token

sealed trait ModuleElements extends Token
case class Typedef(t1: JSType, t2: ExtendedType) extends ModuleElements
case class Package(name: Type, lines: List[PackageElements], extend: Option[Type]) extends ModuleElements
case class Implementation(name: Type, t: Type) extends ModuleElements
case class Enum(name: String, defs: List[String]) extends ModuleElements

sealed trait PackageElements extends Token

case class PackageProperty(name: String, t: Type) extends PackageElements
case class Const(name: String, t: JSType, value: String) extends PackageElements

class DocParser(types: List[String]) extends RegexParsers {
  val identifier = "[a-zA-Z][a-zA-Z0-9_]*".r
  val number = "[0-9]+(\\.[0-9]+)?".r

  val jsString = "DOMString" ^^ { _ => "String" }
  val jsNumber = (opt("unsigned") ~ ("short" | "double" | "long" | "int")) ^^ { _ => "Number" }
  val jsPassthrough = "void" | "boolean" | "Date" | "Window"

  val builtinType = jsString | jsNumber | jsPassthrough

  val extendedType = types.tail.foldLeft[Parser[String]](types.head)({ case (a, next) => a | next })

  val jsType = (builtinType | extendedType) ~ opt("[]") ~ opt("?") ^^ {
    case t ~ a ~ opt => Type(t, ! a.isEmpty, ! opt.isEmpty)
  }
  val optionalType = "optional" ~> jsType ^^ {
    case t: Type => t.copy(isOptional = true)
  }

  val enumString = "\"[A-Z_]+\"".r
  val enum = "enum" ~> identifier ~ "{" ~ repsep(enumString, ",") ~ "}" ^^ { case name ~ "{" ~ defs ~ "}" => Enum(name, defs) }
  val const = "const" ~> jsNumber ~ identifier ~ "=" ~ number ^^ { case t ~ name ~ "=" ~ num => Const(name, t, num) }
  val typedef = "typedef" ~> builtinType ~ extendedType ^^ { case t ~ i => Typedef(t, i) }

  val attribute = "readonly"
  val attributes = opt(attribute) ~ "attribute"
  val implementation = jsType ~ "implements" ~ jsType ^^ { case i ~ "implements" ~ t => Implementation(i, t) }


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
    case failure : NoSuccess => {
      println(failure)
      scala.sys.error(failure.msg)
    }
  }
}

object Implicits {
  implicit class RichFile( file: java.io.File ) {
    def text = io.Source.fromFile( file )(io.Codec.UTF8).mkString

    def text_=( s: String ) {
      val out = new java.io.PrintWriter( file , "UTF-8")
      try{ out.print( s ) }
      finally{ out.close }
    }
  }
}

object WebIDLConverter {
  type IndentLevel = Int

  def transformLines(lines: List[Token])(implicit level: IndentLevel): String = {
    lines.map(line => transform(line)(level + 1)).mkString("\n")
  }

  def indent()(implicit level: IndentLevel) = "  " * level

  def transform(token: Token)(implicit level: IndentLevel = 0): String = token match {
    case Type(name: String, isArray@true, _) => s"$name[]"

    case Type(name: String, isArray@false, _) => name

    case Module(name: String, lines: List[ModuleElements]) => transformLines(lines)(-1)
    // -1 here is a workaround to fix the indentation of our collapsed "module" structure. Modules are not
    // supported by this converter, and were removed in later versions of the WebIDL spec, so this seems
    // reasonable enough.

    case Typedef(t1: JSType, t2: ExtendedType) => s"""${indent}interface $t2 extends $t1 {}"""

    case Implementation(name: Type, t: Type) => s"""${indent}interface ${transform(name)} extends ${transform(t)} {}"""

    case Package(name: Type, lines: List[PackageElements], None) => s"""interface ${transform(name)} {\n${transformLines(lines)}\n}\n"""

    case Package(name: Type, lines: List[PackageElements], Some(t: Type)) => s"""interface ${transform(name)} extends ${transform(t)} {\n${transformLines(lines)}\n}\n"""

    case Enum(name: String, enums: List[String]) => s"""declare enum $name { ${enums.mkString(", ")} }"""

    case Const(name: String, t: JSType, value: String) => s"""$indent$name: $t; // $value"""

    case Method(name: String, args: List[MethodArgument], t: Type) => s"""$indent$name(${args.map(transform).mkString(", ")}): ${transform(t)};"""

    case MethodArgument(name: String, t@Type(_, _, true)) => s"""$name?: ${transform(t)}"""

    case MethodArgument(name: String, t@Type(_, _, false)) => s"""$name: ${transform(t)}"""

    case PackageProperty(name: String, t: Type) => s"$indent$name: ${transform(t)};"
  }
}

object App extends App {
  import Implicits._

  val extendedTypes = io.Source.fromFile(new java.io.File(args.head)).getLines.toList
  val parser = new DocParser(extendedTypes)

  for(arg <- args.tail) {
    val s = io.Source.fromFile(new java.io.File(arg)).getLines.mkString("\n")

    val parsed = parser(s)
    val out = WebIDLConverter.transform(parsed)

    println(out)

    (new java.io.File(arg + ".d.ts")).text = out
  }
}
