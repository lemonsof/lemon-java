package lemon.compilers.frontend

import java.io.File
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.io.{BufferedSource, Source}
import org.slf4j.LoggerFactory
import lemon.messages.reflect._
import scala.Some

class Script(file:File) extends RegexParsers with PackratParsers {
  private val logger = LoggerFactory.getLogger(this.getClass)

  val root = compile(Source.fromFile(file))

  private def compile(source: BufferedSource) : Script_ = {
    parseAll(script, source.bufferedReader()) match {
      case Success(r, _) => r
      case Failure(msg, input) => throw new Exception(file+":"+ msg + " (" + input.pos + ")")
      case Error(msg, input) => throw new Exception(file+":"+msg + " (" + input.pos + ")")
    }
  }

  private lazy val script = namespace ~ usingList ~ (message ||| service ||| enum).* ^^ {
    case n ~ i ~ c => Script_(n,i,c)
  }

  /**
   * the lemon script's identifier regex expression
   */
  private lazy val id = """[_a-zA-Z][a-zA-Z0-9]*""".r

  private lazy val fullName = id ~ (("." ~ id) ^^ {"." + _._2}).* ^^ { case h ~ t => h + t.mkString }

  private lazy val usingList = positioned(("using" ~> fullName) ^^ { Using_ }).*

  private lazy val namespace = positioned("namespace" ~> fullName ^^ { Namespace_ })

  private lazy val attributeTarget = "return" ||| "script"

  private lazy val attribute = ("[" ~> opt(attributeTarget <~ ":")) ~ (messageLiteral <~ "]") ^^ {
    case t ~ m => Attribute_(m,t)
  }

  private lazy val message = positioned(attribute.* ~ ("message" ~> id) ~  opt(":" ~> fullName ^^ { Ref_ }) ~ ("{" ~> field.* <~"}") ^^ {
    case a ~ n ~ e ~ f => Message_(n,f,e,a)
  })

  private lazy val field = attribute.* ~ opt("required") ~ any ~ id <~ ";" ^^ {
    case a ~ Some(_) ~ t ~ n => Field_(n,t,true,a)
    case a ~ None ~ t ~ n => Field_(n,t,false,a)
  }

  private lazy val enumLength = "1" ^^^ 1 | "2" ^^^ 2 | "4" ^^^ 4 | "8" ^^^ 8

  private lazy val enum = (attribute.* <~ "enum") ~ ("(" ~> enumLength  <~ ")") ~ id ~ ("{" ~> enumFields <~ "}") ^^ {
    case a ~ l ~ n ~ e => Enum_(n,l,e,a)
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
    case a ~ n ~ e ~ m => Service_(n,m,e.map(Ref_),a)
  })

  private lazy val methods = method.*

  private lazy val params = {
    param ~ ("," ~> param).*  ^^ {
      case f ~ t => List(f) ::: t
    }
  }

  private lazy val param = attribute.* ~ opt("required") ~ any ~ id ^^ {
    case a ~ Some(_) ~ t ~ n => Param_(n,t,required = true,a)
    case a ~ None ~ t ~ n => Param_(n,t,required = false,a)
  }

  private lazy val method = attribute.* ~ any ~ id ~ ("(" ~> opt(params) <~ ")") ~ opt("throws" ~ "(" ~> exceptions <~")") ^^ {
    case a ~ r ~ n ~ Some(p) ~ Some(e) => Method_(n,Param_("return",r,required = false,List()),p,e,a)
    case a ~ r ~ n ~ Some(p) ~ None => Method_(n,Param_("return",r,required = false,List()),p,List(),a)
    case a ~ r ~ n ~ None ~ Some(e) => Method_(n,Param_("return",r,required = false,List()),List(),e,a)
    case a ~ r ~ n ~ None ~ None => Method_(n,Param_("return",r,required = false,List()),List(),List(),a)
  }

  private lazy val exceptions = id ~ ("," ~> id).* ^^ {
    case f ~ t => (List(f) ::: t).map(Ref_)
  }

  private lazy val map : PackratParser[BuiltinType_] = ("map" ~ "<" ~> any) ~ ("," ~> any <~ ">") ^^ { case k ~ v => Map_(k,v) }

  private lazy val list : PackratParser[BuiltinType_] = ("list" ~ "<") ~> any <~ ">" ^^ List_

  private lazy val set : PackratParser[BuiltinType_] = ("set" ~ "<") ~> any <~ ">" ^^ Set_

  private lazy val array : PackratParser[BuiltinType_] = any ~ ("[" ~> decimalLiteral <~ "]") ^^ {case t ~ l => Array_(t,l.asInstanceOf[Int])}

  private lazy val any = map ||| list ||| set ||| array ||| simpleTypes | fullName ^^ { Ref_ }

  private lazy val simpleTypes =  positioned("sbyte" ^^^ {Var_(1,signed = true)} ||| "int16" ^^^ {Var_(2,signed = true)} ||| "int32" ^^^ {Var_(4,signed = true)} ||| "int64" ^^^ {Var_(8,signed = true)} |||
    "byte" ^^^ {Var_(1,signed = false)} ||| "uint16" ^^^ {Var_(2,signed = false)} ||| "uint32" ^^^ {Var_(4,signed = false)} ||| "uint64" ^^^ {Var_(8,signed = false)} |||
    "sbyte_f" ^^^ {Fixed_(1,signed = true)} ||| "int16_f" ^^^ {Fixed_(2,signed = true)} ||| "int32_f" ^^^ {Fixed_(4,signed = true)} ||| "int64_f" ^^^ {Fixed_(8,signed = true)} |||
    "byte_f" ^^^ {Fixed_(1,signed = false)} ||| "uint16_f" ^^^ {Fixed_(2,signed = false)} ||| "uint32_f" ^^^ {Fixed_(4,signed = false)} ||| "uint64_f" ^^^ {Fixed_(8,signed = false)} |||
    "bool" ^^^ {new Boolean_()} ||| "string" ^^^ new String_ ||| "double" ^^^ Float_(8) ||| "float" ^^^ Float_(4) ||| "void" ^^^ new Void_ )

  private lazy val literal : PackratParser[Literal_] = booleanLiteral | stringLiteral | hexDigit | numericLiteral | messageLiteral | fullName ^^ EnumLiteral_

  private lazy val booleanLiteral = {
    "true" ^^^ BooleanLiteral_(value = true) | "false" ^^^ BooleanLiteral_(value = false)
  }

  private lazy val stringLiteral = '"' ~> """[^"]*""".r <~ '"' ^^ StringLiteral_

  private lazy val hexDigit  =  positioned("""0[xX][0-9a-fA-F]+""".r ^^ {
    s => IntegerLiteral_(java.lang.Long.parseLong(s.substring(2),16))
  })

  private lazy val decimalLiteral = """[0-9]*""".r ^^ { _.toLong }

  private lazy val exponentPart = ("E" | "e")  ~> ("+" | "-") ~ decimalLiteral ^^ {
    case "+" ~ i => Math.pow(10.0,i)
    case "-" ~ i => Math.pow(10.0,-i)
  }

  private lazy val numericLiteral = ("+" | "-") ~ decimalLiteral ~ opt("." ~ decimalLiteral) ~ opt(exponentPart) ^^ {
    case "+" ~ r ~ Some(f) ~ Some(p) =>  FloatLiteral_((r.toString + "." + f.toString).toDouble * p)
    case "-" ~ r ~ Some(f) ~ Some(p) =>  FloatLiteral_(-(r.toString + "." + f.toString).toDouble * p)
    case "+" ~ r ~ Some(f) ~ None =>  FloatLiteral_((r.toString + "." + f.toString).toDouble)
    case "-" ~ r ~ Some(f) ~ None =>  FloatLiteral_(-(r.toString + "." + f.toString).toDouble)
    case "+" ~ r ~ None ~ Some(p) =>  FloatLiteral_(r.toDouble * p)
    case "-" ~ r ~ None ~ Some(p) =>  FloatLiteral_(- r.toDouble * p)
    case "+" ~ r ~ None ~ None =>  IntegerLiteral_(r)
    case "-" ~ r ~ None ~ None =>  IntegerLiteral_(-r)
  }

  private lazy val messageLiteral:PackratParser[MessageLiteral_] = {
    positioned(fullName ^^ { MessageLiteral_(_,None,None) } ||| (fullName <~ "(") ~ opt(values) ~ (opt(namedValues) <~ ")") ^^ {
      case n ~ v ~ nv => MessageLiteral_(n,v,nv)
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
