package datalog
import scala.math._

sealed abstract class Expr {
  def _type: Type
  def getParameters(): Set[Parameter]
}

sealed abstract class Arithmetic extends Expr {
  def _paren(e: Arithmetic): String = e match {
    case _:Param|_:One|_:Zero => s"$e"
    case _ => s"($e)"
  }
}
case class Zero(_type: Type) extends Arithmetic {
  override def toString: String = _type match {
    case _:UnitType|_:AnyType|_:NumberType => s"0"
    case SymbolType(name) => s"$name(0)"
    case BooleanType() => s"false"
    case compoundType: CompoundType => throw new Exception(s"illegal type $compoundType for Zero.")
  }
  def getParameters(): Set[Parameter] = Set()
}
case class One(_type: Type) extends Arithmetic {
  override def toString: String = _type match {
    case _:UnitType|_:AnyType|_:NumberType => s"1"
    case SymbolType(name) => s"$name(1)"
    case BooleanType() => s"true"
    case compoundType: CompoundType => throw new Exception(s"illegal type $compoundType for Zero.")
  }
  def getParameters(): Set[Parameter] = Set()
}
case class Param(p: Parameter) extends Arithmetic {
  val _type = p._type
  override def toString: String = p.toString
  def getParameters(): Set[Parameter] = Set(p)
}
case class Negative(e: Arithmetic) extends Arithmetic {
  val _type = e._type
  override def toString: String = s"-${_paren(e)}"
  def getParameters(): Set[Parameter] = e.getParameters()
}
sealed abstract class UnaryOperator extends Arithmetic {
  def e: Arithmetic
  def getParameters(): Set[Parameter] = e.getParameters()
}
case class Sqrt(e: Arithmetic) extends UnaryOperator {
  val _type = e._type
  override def toString: String = s"Math.sqrt(${e})"
}
case class Log10(e: Arithmetic) extends UnaryOperator {
  val _type = e._type
  override def toString: String = s"Math.log10(${e})"
}

sealed abstract class BinaryOperator extends Arithmetic {
  def a: Arithmetic
  def b: Arithmetic
  def getParameters(): Set[Parameter] = a.getParameters() ++ b.getParameters()
}
case class Add(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type == b._type)
  val _type = a._type
  override def toString: String = s"${_paren(a)}+${_paren(b)}"
}
case class Sub(a: Arithmetic, b:Arithmetic) extends BinaryOperator {
  require(a._type == b._type)
  val _type = a._type
  override def toString: String = s"${_paren(a)}-${_paren(b)}"
}
case class Mul(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type == b._type, s"$a,$b")
  val _type = a._type
  override def toString: String = s"${_paren(a)}*${_paren(b)}"
}
case class Div(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type == b._type, s"$a,$b")
  val _type = a._type
  override def toString: String = s"${_paren(a)}/${_paren(b)}"
}
case class Min(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type==b._type)
  def _type: Type = a._type
  override def toString: String = s"$a < $b ? $a : $b"
}
case class Pow(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type==b._type)
  def _type: Type = a._type
  override def toString: String = s"DSMath.rpow(${_paren(a)},${_paren(b)})"
}
object Arithmetic {
  // chain rule implemented in JoinView when this function is used
  def derivativeOf(e: Arithmetic, x: Param): Arithmetic = e match {
    case Zero(t) => Zero(t)
    case One(t) => Zero(t)
    case Param(p) => if (p.name==x.p.name) One(p._type) else Zero(p._type)
    case Negative(e2) => Negative(derivativeOf(e2,x))
    case Add(a,b) => Add(derivativeOf(a,x), derivativeOf(b,x))
    case Sub(a,b) => Sub(derivativeOf(a,x), derivativeOf(b,x))
    // case Mul(a,b) => Mul(derivativeOf(a,x), derivativeOf(b,x))
    case Mul(a,b) => Add(Mul(derivativeOf(a,x), b), Mul(a, derivativeOf(b,x)))
    // case Div(a,b) => ???
    case Div(a,b) => Div(Sub(Mul(derivativeOf(a,x), b), Mul(a, derivativeOf(b,x))), Mul(b,b))
    case Min(_,_) => ???
    case Sqrt(a) => Div(derivativeOf(a,x), Mul(Param(Constant(a._type, "2")), Sqrt(a)))
    // case Log10(a) => Div(One(a._type), Mul(a, Ln(10))) 
    case Log10(_) => ??? // Could not find math library to support natural log
    case Pow(a,b) => Mul(b, Pow(a, Sub(b, One(a._type))))
  }
  def simplify(expr: Arithmetic): Arithmetic = {
    def _simplify(expr: Arithmetic): Arithmetic = expr match {
      case a @ (_:Zero|_:One|_:Param) => a
      case Negative(Zero(_t)) => Zero(_t)
      case Negative(Negative(a)) => _simplify(a)
      case Negative(a) => Negative(_simplify(a))
      case Add(Zero(_),b) => _simplify(b)
      case Add(a,Zero(_)) => _simplify(a)
      case Add(a,b) => Add(_simplify(a),_simplify(b))
      case Sub(Zero(_),b) => Negative(_simplify(b))
      case Sub(a, Zero(_)) => _simplify(a)
      case Sub(a,b) => Sub(_simplify(a),_simplify(b))

      case Mul(One(_),b) => _simplify(b)
      case Mul(Negative(One(_)),b) => Negative(_simplify(b))
      case Mul(a,One(_)) => _simplify(a)
      case Mul(_,Zero(t)) => Zero(t)
      case Mul(Zero(t),_) => Zero(t)
      case Mul(_,Negative(Zero(t))) => Zero(t)
      case Mul(Negative(Zero(t)),_) => Zero(t)
      case Mul(a, Negative(One(_))) => Negative(_simplify(a))
      case Mul(a,b) => Mul(_simplify(a),_simplify(b))

      case Div(a, One(_)) => _simplify(a)
      case Div(a, Negative(One(_))) => Negative(_simplify(a))
      case Div(Zero(t), _) => Zero(t)
      case Div(Negative(Zero(t)), _) => Zero(t)
      case Div(a,b) => Div(_simplify(a),_simplify(b))

      case Min(a,b) => Min(_simplify(a),_simplify(b))

      
      case Sqrt(One(a)) => One(a)
      case Sqrt(Zero(a)) => Zero(a)
      case Sqrt(a) => Sqrt(_simplify(a))

      case Log10(One(a)) => Zero(a)
      case Log10(a) => Log10(_simplify(a))

      case Pow(One(t), b) => One(t)
      case Pow(Zero(t), b) => Zero(t)
      case Pow(a, Zero(t)) => One(t)
      case Pow(a, One(_)) => _simplify(a)
      case Pow(a, b) => Pow(_simplify(a), _simplify(b))
    }

    var e1 = expr
    var e2 = expr
    do {
      e1 = e2
      e2 = _simplify(e1)
    } while (e2!=e1)
    e2
  }
  def updateArithmeticType(expr: Expr, newType: Type): Arithmetic = expr match {
    case arith: Arithmetic => arith match {
      case Zero(_type) => Zero(newType)
      case One(_type) => One(newType)
      case Param(p) => Param(p.setType(newType))
      case Negative(e) => Negative(updateArithmeticType(e,newType))
      case binOp: BinaryOperator => binOp match {
        case Add(a, b) => Add(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
        case Sub(a, b) => Sub(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
        case Mul(a, b) => Mul(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
        case Div(a, b) => Div(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
        case Min(a, b) => Min(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
        case Pow(a, b) => Pow(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
      }
      case opun: UnaryOperator => opun match {
        case Sqrt(a) => Sqrt(updateArithmeticType(a,newType))
        case Log10(a) => Log10(updateArithmeticType(a,newType))
      }
    }
  }

  def rename(arith: Arithmetic, mapping: Map[Parameter,Parameter]): Arithmetic = arith match {
    case Zero(_type) => arith
    case One(_type) => arith
    case Param(p) => Param(mapping.getOrElse(p,p))
    case Negative(e) => Negative(rename(e, mapping))
    case opbi: BinaryOperator => opbi match {
      case Add(a, b) => Add(rename(a, mapping), rename(b, mapping))
      case Sub(a, b) => Sub(rename(a, mapping), rename(b, mapping))
      case Mul(a, b) => Mul(rename(a, mapping), rename(b, mapping))
      case Div(a, b) => Div(rename(a, mapping), rename(b, mapping))
      case Min(a, b) => Min(rename(a, mapping), rename(b, mapping))
      case Pow(a, b) => Pow(rename(a, mapping), rename(b, mapping))
    }
    case opun: UnaryOperator => opun match {
      case Sqrt(a) => Sqrt(rename(a, mapping))
      case Log10(a) => Log10(rename(a, mapping))
    }
  }

  def replace(arith: Arithmetic, mapping: Map[Param,Arithmetic]): Arithmetic = arith match {
    case _: Zero | _:One => arith
    case p: Param => mapping.getOrElse(p,p)
    case Negative(e) => Negative(replace(e, mapping))
    case opbi: BinaryOperator => opbi match {
      case Add(a, b) => Add(replace(a, mapping), replace(b,mapping))
      case Sub(a, b) => Sub(replace(a, mapping), replace(b,mapping))
      case Mul(a, b) => Mul(replace(a, mapping), replace(b,mapping))
      case Div(a, b) => Div(replace(a, mapping), replace(b,mapping))
      case Min(a, b) => Min(replace(a, mapping), replace(b,mapping))
      case Pow(a, b) => Pow(replace(a, mapping), replace(b,mapping))
    }
    case opun: UnaryOperator => opun match {
      case Sqrt(a) => Sqrt(replace(a, mapping))
      case Log10(a) => Log10(replace(a, mapping))
    }
  }

  def rename(expr: Expr, mapping: Map[Parameter,Parameter]): Expr = expr match {
    case arithmetic: Arithmetic => rename(arithmetic, mapping)
  }
}


sealed abstract class Functor {
  def args: Array[Expr]
}

trait BinOp {
  def a: Expr
  def b: Expr
  def args: Array[Expr] = Array(a,b)
}

sealed abstract class ArithOperator extends Functor with BinOp {
  def a: Arithmetic
  def b: Arithmetic
}
case class Greater(a: Arithmetic, b: Arithmetic) extends ArithOperator {
  override def toString: String = s"$a>$b"
}
case class Lesser(a: Arithmetic, b: Arithmetic) extends ArithOperator {
  override def toString: String = s"$a<$b"
}
case class Geq(a: Arithmetic, b: Arithmetic) extends ArithOperator {
  override def toString: String = s"$a>=$b"
}
case class Leq(a: Arithmetic, b: Arithmetic) extends ArithOperator {
  override def toString: String = s"$a<=$b"
}

case class Unequal(a: Expr, b: Expr) extends Functor with BinOp {
  override def toString: String = s"$a!=$b"
}
case class Equal(a: Expr, b: Expr) extends Functor with BinOp {
  override def toString: String = s"$a==$b"
}
case class Assign(a: Param, b: Expr) extends Functor with BinOp {
  override def toString: String = s"$a := $b"
  def updateOutputType(outputType: Type): Assign = {
    val newP = a.p.setType(outputType)
    val newA = Param(newP)
    this.copy(a=newA)
  }
}

