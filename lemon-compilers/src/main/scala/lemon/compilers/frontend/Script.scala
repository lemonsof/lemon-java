package lemon.compilers.frontend

import java.io.File
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.io.{BufferedSource, Source}
import org.slf4j.LoggerFactory

class Script(file:File) extends RegexParsers with PackratParsers {
  private val logger = LoggerFactory.getLogger(this.getClass)

  val root = compile(Source.fromFile(file))

  private def compile(source: BufferedSource) : ScriptIL = {
    parseAll(script, source.bufferedReader()) match {
      case Success(r, _) => r
      case Failure(msg, input) => throw new Exception(msg + " (" + input.pos + ")")
      case Error(msg, input) => throw new Exception(msg + " (" + input.pos + ")")
    }
  }

  private lazy val script = namespace ~ usingList ~ (message ||| service ||| enum).* ^^ {
    case n ~ i ~ c => ScriptIL(n,i,c)
  }

  /**
   * the lemon script's identifier regex expression
   */
  private lazy val id = """[_a-zA-Z][a-zA-Z0-9]*""".r

  private lazy val fullName = id ~ (("." ~ id) ^^ {"." + _._2}).* ^^ { case h ~ t => h + t.mkString }

  private lazy val usingList = positioned(("using" ~> fullName) ^^ { UsingIL }).*

  private lazy val namespace = positioned("namespace" ~> fullName ^^ { NamespaceIL })

  private lazy val attribute = ("[" ~> opt(id <~ ":")) ~ (messageLiteral <~ "]") ^^ {
    case t ~ m => AttributeIL(m,t)
  }

  private lazy val message = positioned(attribute.* ~ ("message" ~> id) ~  opt(":" ~> fullName ^^ { ReferenceIL }) ~ ("{" ~> field.* <~"}") ^^ {
    case a ~ n ~ e ~ f => MessageIL(n,f,e,a)
  })

  private lazy val field = attribute.* ~ opt("required") ~ any ~ id ^^ {
    case a ~ Some(_) ~ t ~ n => FieldIL(n,t,required = true,a)
    case a ~ None ~ t ~ n => FieldIL(n,t,required = false,a)
  }

  private lazy val enum = (attribute.* <~ "enum") ~ id ~ ("{" ~> enumFields <~ "}") ^^ {
    case a ~ n ~ e => EnumIL(n,e,a)
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
    case a ~ n ~ e ~ m => ServiceIL(n,m,e.map(ReferenceIL),a)
  })

  private lazy val methods = method.*

  private lazy val params = {
    param ~ ("," ~> param).*  ^^ {
      case f ~ t => List(f) ::: t
    }
  }

  private lazy val param = attribute.* ~ opt("required") ~ any ~ id ^^ {
    case a ~ Some(_) ~ t ~ n => ParamIL(n,t,required = true,a)
    case a ~ None ~ t ~ n => ParamIL(n,t,required = false,a)
  }

  private lazy val method = attribute.* ~ any ~ id ~ ("(" ~> opt(params) <~ ")") ~ opt("throws" ~ "(" ~> exceptions <~")") ^^ {
    case a ~ r ~ n ~ Some(p) ~ Some(e) => MethodIL(n,List(ParamIL("return",r,required = false,List())):::p,e,a)
    case a ~ r ~ n ~ Some(p) ~ None => MethodIL(n,List(ParamIL("return",r,required = false,List())):::p,List(),a)
    case a ~ r ~ n ~ None ~ Some(e) => MethodIL(n,List(ParamIL("return",r,required = false,List())),e,a)
    case a ~ r ~ n ~ None ~ None => MethodIL(n,List(ParamIL("return",r,required = false,List())),List(),a)
  }

  private lazy val exceptions = id ~ ("," ~> id).* ^^ {
    case f ~ t => (List(f) ::: t).map(ReferenceIL)
  }

  private lazy val map : PackratParser[BuiltinTypeIL] = ("map" ~ "<" ~> any) ~ ("," ~> any <~ ">") ^^ { case k ~ v => MapIL(k,v) }

  private lazy val list : PackratParser[BuiltinTypeIL] = ("list" ~ "<") ~> any <~ ">" ^^ ListIL

  private lazy val set : PackratParser[BuiltinTypeIL] = ("set" ~ "<") ~> any <~ ">" ^^ SetIL

  private lazy val array : PackratParser[BuiltinTypeIL] = any ~ ("[" ~> decimalLiteral <~ "]") ^^ {case t ~ l => ArrayIL(t,l.asInstanceOf[Int])}

  private lazy val any = map ||| list ||| set ||| array ||| simpleTypes | fullName ^^ { ReferenceIL }

  private lazy val simpleTypes =  positioned("sbyte" ^^^ {VarIL(1,signed = true)} ||| "int16" ^^^ {VarIL(2,signed = true)} ||| "int32" ^^^ {VarIL(4,signed = true)} ||| "int64" ^^^ {VarIL(8,signed = true)} |||
    "byte" ^^^ {VarIL(1,signed = false)} ||| "uint16" ^^^ {VarIL(2,signed = false)} ||| "uint32" ^^^ {VarIL(4,signed = false)} ||| "uint64" ^^^ {VarIL(8,signed = false)} |||
    "sbyte_f" ^^^ {FixedIL(1,signed = true)} ||| "int16_f" ^^^ {FixedIL(2,signed = true)} ||| "int32_f" ^^^ {FixedIL(4,signed = true)} ||| "int64_f" ^^^ {FixedIL(8,signed = true)} |||
    "byte_f" ^^^ {FixedIL(1,signed = false)} ||| "uint16_f" ^^^ {FixedIL(2,signed = false)} ||| "uint32_f" ^^^ {FixedIL(4,signed = false)} ||| "uint64_f" ^^^ {FixedIL(8,signed = false)} |||
    "bool" ^^^ {new BooleanIL()} ||| "string" ^^^ new StringIL ||| "double" ^^^ FloatIL(8) ||| "float" ^^^ FloatIL(4) ||| "void" ^^^ new VoidIL )

  private lazy val literal : PackratParser[LiteralIL] = booleanLiteral | stringLiteral | hexDigit | numericLiteral | messageLiteral | fullName ^^ EnumLiteralIL

  private lazy val booleanLiteral = {
    "true" ^^^ BooleanLiteralIL(value = true) | "false" ^^^ BooleanLiteralIL(value = false)
  }

  private lazy val stringLiteral = '"' ~> """[^"]*""".r <~ '"' ^^ StringLiteralIL

  private lazy val hexDigit  =  positioned("""0[xX][0-9a-fA-F]+""".r ^^ {
    s => IntegerLiteralIL(java.lang.Long.parseLong(s.substring(2),16))
  })

  private lazy val decimalLiteral = """[0-9]*""".r ^^ { _.toLong }

  private lazy val exponentPart = ("E" | "e")  ~> ("+" | "-") ~ decimalLiteral ^^ {
    case "+" ~ i => Math.pow(10.0,i)
    case "-" ~ i => Math.pow(10.0,-i)
  }

  private lazy val numericLiteral = ("+" | "-") ~ decimalLiteral ~ opt("." ~ decimalLiteral) ~ opt(exponentPart) ^^ {
    case "+" ~ r ~ Some(f) ~ Some(p) =>  FloatLiteralIL((r.toString + "." + f.toString).toDouble * p)
    case "-" ~ r ~ Some(f) ~ Some(p) =>  FloatLiteralIL(-(r.toString + "." + f.toString).toDouble * p)
    case "+" ~ r ~ Some(f) ~ None =>  FloatLiteralIL((r.toString + "." + f.toString).toDouble)
    case "-" ~ r ~ Some(f) ~ None =>  FloatLiteralIL(-(r.toString + "." + f.toString).toDouble)
    case "+" ~ r ~ None ~ Some(p) =>  FloatLiteralIL(r.toDouble * p)
    case "-" ~ r ~ None ~ Some(p) =>  FloatLiteralIL(- r.toDouble * p)
    case "+" ~ r ~ None ~ None =>  IntegerLiteralIL(r)
    case "-" ~ r ~ None ~ None =>  IntegerLiteralIL(-r)
  }

  private lazy val messageLiteral:PackratParser[MessageLiteralIL] = {
    positioned(fullName ^^ { MessageLiteralIL(_,None,None) } ||| (fullName <~ "(") ~ opt(values) ~ (opt(namedValues) <~ ")") ^^ {
      case n ~ v ~ nv => MessageLiteralIL(n,v,nv)
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
