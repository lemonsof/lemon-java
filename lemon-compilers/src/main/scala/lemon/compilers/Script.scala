package lemon.compilers

import java.io.File
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.io.{BufferedSource, Source}
import org.slf4j.LoggerFactory

class Script(file:File) extends RegexParsers with PackratParsers {
  private val logger = LoggerFactory.getLogger(this.getClass)

  val root = compile(Source.fromFile(file))

  private def compile(source: BufferedSource) : ScriptILL = {
    parseAll(script, source.bufferedReader()) match {
      case Success(r, _) => r
      case Failure(msg, input) => throw new Exception(msg + " (" + input.pos + ")")
      case Error(msg, input) => throw new Exception(msg + " (" + input.pos + ")")
    }
  }

  private lazy val script = namespace ~ usingList ~ (message ||| service ||| enum).* ^^ {
    case n ~ i ~ c => ScriptILL(n,i,c)
  }

  /**
   * the lemon script's identifier regex expression
   */
  private lazy val id = """[_a-zA-Z][a-zA-Z0-9]*""".r

  private lazy val fullName = id ~ (("." ~ id) ^^ {"." + _._2}).* ^^ { case h ~ t => h + t.mkString }

  private lazy val usingList = positioned(("using" ~> fullName) ^^ { UsingILL }).*

  private lazy val namespace = positioned("namespace" ~> fullName ^^ { NamespaceILL })

  private lazy val attribute = ("[" ~> opt(id <~ ":")) ~ (messageLiteral <~ "]") ^^ {
    case t ~ m => AttributeILL(m,t)
  }

  private lazy val message = positioned(attribute.* ~ ("message" ~> id) ~  opt(":" ~> fullName ^^ { ReferenceILL }) ~ ("{" ~> field.* <~"}") ^^ {
    case a ~ n ~ e ~ f => MessageILL(n,f,e,a)
  })

  private lazy val field = attribute.* ~ opt("required") ~ any ~ id <~ ";" ^^ {
    case a ~ Some(_) ~ t ~ n => FieldILL(n,t,required = true,a)
    case a ~ None ~ t ~ n => FieldILL(n,t,required = false,a)
  }

  private lazy val enum = (attribute.* <~ "enum") ~ id ~ ("{" ~> enumFields <~ "}") ^^ {
    case a ~ n ~ e => EnumILL(n,e,a)
  }

  private lazy val enumFields  = {
    enumField ~ ("," ~> enumField).*  ^^ {
      case f ~ t =>
        var value = -1L
        (List(f) ::: t).map({
          field => {
            if(field._1 == -1){
              value += 1
              (value,field._2)
            } else {
              value = field._1
              field
            }
          }
        })
    }
  }

  private lazy val enumField =  id ~ opt("=" ~> decimalLiteral) ^^ {
    case n ~ None => (-1L,n)
    case n ~ Some(i) => (i,n)
  }

  private lazy val service = positioned((attribute.* <~ "service") ~ id ~ opt(":" ~> fullName) ~ ("{" ~> methods <~ "}") ^^ {
    case a ~ n ~ e ~ m => ServiceILL(n,m,e.map(ReferenceILL),a)
  })

  private lazy val methods = method.*

  private lazy val params = {
    param ~ ("," ~> param).*  ^^ {
      case f ~ t => List(f) ::: t
    }
  }

  private lazy val param = attribute.* ~ opt("required") ~ any ~ id ^^ {
    case a ~ Some(_) ~ t ~ n => ParamILL(n,t,required = true,a)
    case a ~ None ~ t ~ n => ParamILL(n,t,required = false,a)
  }

  private lazy val method = attribute.* ~ any ~ id ~ ("(" ~> opt(params) <~ ")") ~ opt("throws" ~ "(" ~> exceptions <~")") ^^ {
    case a ~ r ~ n ~ Some(p) ~ Some(e) => MethodILL(n,List(ParamILL("return",r,required = false,List())):::p,e,a)
    case a ~ r ~ n ~ Some(p) ~ None => MethodILL(n,List(ParamILL("return",r,required = false,List())):::p,List(),a)
    case a ~ r ~ n ~ None ~ Some(e) => MethodILL(n,List(ParamILL("return",r,required = false,List())),e,a)
    case a ~ r ~ n ~ None ~ None => MethodILL(n,List(ParamILL("return",r,required = false,List())),List(),a)
  }

  private lazy val exceptions = id ~ ("," ~> id).* ^^ {
    case f ~ t => (List(f) ::: t).map(ReferenceILL)
  }

  private lazy val map : PackratParser[BuiltinTypeILL] = ("map" ~ "<" ~> any) ~ ("," ~> any <~ ">") ^^ { case k ~ v => MapILL(k,v) }

  private lazy val list : PackratParser[BuiltinTypeILL] = ("list" ~ "<") ~> any <~ ">" ^^ ListILL

  private lazy val set : PackratParser[BuiltinTypeILL] = ("set" ~ "<") ~> any <~ ">" ^^ SetILL

  private lazy val array : PackratParser[BuiltinTypeILL] = any ~ ("[" ~> decimalLiteral <~ "]") ^^ {case t ~ l => ArrayILL(t,l.asInstanceOf[Int])}

  private lazy val any = map ||| list ||| set ||| array ||| simpleTypes | fullName ^^ { ReferenceILL }

  private lazy val simpleTypes =  positioned("sbyte" ^^^ {VarILL(1,signed = true)} ||| "int16" ^^^ {VarILL(2,signed = true)} ||| "int32" ^^^ {VarILL(4,signed = true)} ||| "int64" ^^^ {VarILL(8,signed = true)} |||
    "byte" ^^^ {VarILL(1,signed = false)} ||| "uint16" ^^^ {VarILL(2,signed = false)} ||| "uint32" ^^^ {VarILL(4,signed = false)} ||| "uint64" ^^^ {VarILL(8,signed = false)} |||
    "sbyte_f" ^^^ {FixedILL(1,signed = true)} ||| "int16_f" ^^^ {FixedILL(2,signed = true)} ||| "int32_f" ^^^ {FixedILL(4,signed = true)} ||| "int64_f" ^^^ {FixedILL(8,signed = true)} |||
    "byte_f" ^^^ {FixedILL(1,signed = false)} ||| "uint16_f" ^^^ {FixedILL(2,signed = false)} ||| "uint32_f" ^^^ {FixedILL(4,signed = false)} ||| "uint64_f" ^^^ {FixedILL(8,signed = false)} |||
    "bool" ^^^ {new BooleanILL()} ||| "string" ^^^ new StringILL ||| "double" ^^^ FloatILL(8) ||| "float" ^^^ FloatILL(4) ||| "void" ^^^ new VoidILL )

  private lazy val literal : PackratParser[LiteralILL] = booleanLiteral | stringLiteral | hexDigit | numericLiteral | messageLiteral | fullName ^^ EnumLiteralILL

  private lazy val booleanLiteral = {
    "true" ^^^ BooleanLiteralILL(value = true) | "false" ^^^ BooleanLiteralILL(value = false)
  }

  private lazy val stringLiteral = '"' ~> """[^"]*""".r <~ '"' ^^ StringLiteralILL

  private lazy val hexDigit  =  positioned("""0[xX][0-9a-fA-F]+""".r ^^ {
    s => IntegerLiteralILL(java.lang.Long.parseLong(s.substring(2),16))
  })

  private lazy val decimalLiteral = """[0-9]*""".r ^^ { _.toLong }

  private lazy val exponentPart = ("E" | "e")  ~> ("+" | "-") ~ decimalLiteral ^^ {
    case "+" ~ i => Math.pow(10.0,i)
    case "-" ~ i => Math.pow(10.0,-i)
  }

  private lazy val numericLiteral = ("+" | "-") ~ decimalLiteral ~ opt("." ~ decimalLiteral) ~ opt(exponentPart) ^^ {
    case "+" ~ r ~ Some(f) ~ Some(p) =>  FloatLiteralILL((r.toString + "." + f.toString).toDouble * p)
    case "-" ~ r ~ Some(f) ~ Some(p) =>  FloatLiteralILL(-(r.toString + "." + f.toString).toDouble * p)
    case "+" ~ r ~ Some(f) ~ None =>  FloatLiteralILL((r.toString + "." + f.toString).toDouble)
    case "-" ~ r ~ Some(f) ~ None =>  FloatLiteralILL(-(r.toString + "." + f.toString).toDouble)
    case "+" ~ r ~ None ~ Some(p) =>  FloatLiteralILL(r.toDouble * p)
    case "-" ~ r ~ None ~ Some(p) =>  FloatLiteralILL(- r.toDouble * p)
    case "+" ~ r ~ None ~ None =>  IntegerLiteralILL(r)
    case "-" ~ r ~ None ~ None =>  IntegerLiteralILL(-r)
  }

  private lazy val messageLiteral:PackratParser[MessageLiteralILL] = {
    positioned(fullName ^^ { MessageLiteralILL(_,None,None) } ||| (fullName <~ "(") ~ opt(values) ~ (opt(namedValues) <~ ")") ^^ {
      case n ~ v ~ nv => MessageLiteralILL(n,v,nv)
    })
  }

  private lazy val values = literal ~ ("," ~> literal).* ^^ {
    case h ~ t => List(h) ::: t
  }

  private lazy val nameValue = id ~ "=" ~ literal ^^ {
    case i ~ _ ~ l => (i,l)
  }

  private lazy val namedValues = nameValue ~ ("," ~> nameValue).* ^^ {
    case h ~ t => t.foldLeft(Map(h)){ (m,t) => m + t }
  }
}
