/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package ast

import reflect.generic.HasFlags
import symtab.Flags._
import symtab.SymbolTable
import scala.collection.{ mutable, immutable, generic }

trait RepTypes {
  self: AutoTraverser =>

  val global: SymbolTable
  import global._

  type Modifiers = global.Modifiers
  type Name      = global.Name
  type Symbol    = global.Symbol
  type Tree      = global.Tree
  type Type      = global.Type
  val NoSymbol   = global.NoSymbol
  
  // temporary
  lazy val SerialVersionUIDAttr: Symbol = definitions.getClass("scala.SerialVersionUID")
}

trait RepUtil {
  self: AutoTraverser =>
  
  def clazzName(clazz: Class[_]): String = clazz.getName split '.' last  
  def isDebug                            = System.getenv("PPDEBUG") != null
  def dbg(msg: String)                   = if (isDebug) println(msg)  
  def noEmpties(xs:Rep*): List[Rep]      = xs.toList filterNot (_.isEmpty)
}

trait RepImplicits {
  self: AutoTraverser =>

  implicit def stringToRepresentation(str: String): Rep        = PureStringRep(str)
  implicit def symToRepresentation(sym: Symbol): Rep           = fromSymbol(sym)
  implicit def tpeToRepresentation(tpe: Type): Rep             = fromType(tpe)
  implicit def nameToRepresentation(name: Name): Rep           = fromName(name)
  implicit def modifiersToRepresentation(mods: Modifiers): Rep = fromModifiers(mods)
  implicit def repListToRepresentation(reps: List[Rep]): Rep   = join(reps: _*)  
  implicit def nameToString(n: Name): String                   = n.toString
}

trait RepObjects {
  self: AutoTraverser =>

  abstract class ConstantRep(value: String, dvalue: String) extends Rep {
    def this(value: String) = this(value, value)
    def translate = if (isDebug) dvalue else value
  }

  case object Indentation extends Rep {
    def translate = context.spaces
  }
  case object IndentPlus extends Rep {
    def translate = {
      context.indent
      ""
    }
    def apply(num: Int): Rep = List.fill(num)(this: Rep) reduceLeft (_ % _)
  }
  case object IndentMinus extends Rep {
    def translate = {
      context.undent
      ""
    }
    def apply(num: Int): Rep = List.fill(num)(this: Rep) reduceLeft (_ % _)
  }
  case object Empty extends ConstantRep("", "<>") { override def isEmpty = true }
  case object Newline extends ConstantRep("\n", "NL\n")
  case object DoubleNewline extends ConstantRep("\n\n", "2NL\n\n")
  case object Space extends ConstantRep(" ", ".")
  case object LogicalAnd extends ConstantRep("&&")
  case object LogicalOr extends ConstantRep("||")
  case object LogicalNot extends ConstantRep("!")
  case object Wild extends ConstantRep("_")
  case object NoTouch extends ConstantRep("", "NoTouch")
  case object Semi extends ConstantRep(";", "Semi")
  
  object EmptyRep {
    def unapply(x: Any): Boolean = x match {
      case x: Rep   => x.isEmpty
      case _        => false
    }
  }
  
  object formatter {
    type T = Rep

    def join(sep: T, xs: List[T]): T = noEmpties(xs: _*) match {
      case Nil  => Empty
      case xs   => if (sep.isEmpty) xs.reduceLeft(_ % _) else xs.reduceLeft(_ % sep % _)
    }
    def words(xs: T*): T       = join(Space, xs.toList)
    def dotted(xs: T*): T      = join(".", xs.toList)
    def smash(xs: List[T]): T  = join(Empty, xs)
    def pipes(xs: List[T]): T  = join(Space % "|" % Space, xs)
    def stmts(xs: List[T]): T  = join(Newline, xs map stmt)
    def commas(xs: List[T]): T = join("," % Space, xs)

    def alternatives(xs: List[T]): T    = pipes(xs).inParens
    def block(xs: List[T]): T           = if (xs.isEmpty) "{" % "}" else "{" % Newline % indent(xs) % Newline % Indentation % "}"
    def indent(xs: List[T]): T          = IndentPlus % stmts(xs) % IndentMinus
    def indentBy(num: Int, xs: List[T]) = IndentPlus(num) % stmts(xs) % IndentMinus(num)
    def inset(num: Int, xs: List[T])    = indentBy(num, xs)
    def defn(x: T): T                   = Indentation % x % Newline
    def stmt(x: T): T                   = Indentation % x % Semi
  }
}

trait RepClasses {
  self: AutoTraverser =>
  
  import global._
  
  trait HasFlagsRep extends Rep {
    def flagCarrier: HasFlags {
      type FlagsType = Long
      def flags: Long
    }
    private def isCarrier = flagCarrier != null
    
    override def flags: Long = flagMask & {
      if (isCarrier) flagCarrier.flags
      else super.flags
    }
    override def privateWithin: Name =
      if (isCarrier && flagCarrier.hasAccessBoundary) flagCarrier.privateWithin.toString
      else super.privateWithin
  }

  trait SymRep extends HasFlagsRep {
    def symbol: Symbol
    def flagCarrier = symbol
    def translate: String = if (isEmpty) "" else symbol.nameString
    override def isEmpty = !hasSymbol
    override def repName: String = if (hasSymbol) symbol.nameString else super.repName
    // override def flags = symbol.flags
    // override def hasSymbol = symbol != null && symbol != NoSymbol
    // override def annotations =
    //   if (hasSymbol && symbol.rawAnnotations.nonEmpty) symbol.annotations map annotationRep
    //   else super.annotations
    // override def privateWithin =
    //   if (hasSymbol && symbol.privateWithin != NoSymbol) symbol.privateWithin.name
    //   else super.privateWithin
  }

  case class SymbolRep(symbol: Symbol) extends SymRep { }
  case class TypeRep[T <: Type](tpe: T) extends SymRep {
    def symbol = tpe.typeSymbol
    def typeString =
      if (hasSymbol && symbol.isAnonymousClass) fromSymbol(symbol).translate
      else tpe.toString
    override def translate = typeString
  }
  // case class TreeRep[T <: Tree](tree: T) extends SymRep {
  //   lazy val symbol = tree.symbol
  //   override def repName: String = 
  //     if (hasSymbol) super.repName
  //     else tree match {
  //       case t: RefTree   => t.name.repName
  //       case t: DefTree   => t.name.repName
  //       case _            => ""
  //     }
  //   private def optMods = tree match {
  //     case x: MemberDef => Some(x.mods)
  //     case _            => None
  //   }    
  //   override def annotations: List[Rep] = super.annotations match {
  //     case Nil  => if (optMods.isEmpty) Nil else optMods.get.annotations map fromTree
  //     case xs   => xs
  //   }    
  //   override def isEmpty = tree.isEmpty
  //   override def privateWithin = optMods match {
  //     case Some(m)  => m.privateWithin
  //     case _        => super.privateWithin
  //   }
  // }
  case class DecisionRep(cond: FoldContext => Boolean, ifTrue: Rep, ifFalse: Rep) extends Rep {
    override def flatten = List(DecisionRep(cond, ifTrue.flattened, ifFalse.flattened))
    private def choose   = if (cond(foldContext)) ifTrue else ifFalse

    def translate = choose.translate
  }
  
  case class GroupedRep(left: Rep, op: Rep, right: Rep) extends Rep {
    override def flatten = List(GroupedRep(left.flattened, op.flattened, right.flattened))
    def translate        = (left % op % right).translate
  }
  case class ExpressionRep(left: Rep, op: Rep, right: Rep) extends Rep {
    override def isEmpty = left.isEmpty && right.isEmpty
    override def flatten = List(ExpressionRep(left.flattened, op.flattened, right.flattened))
    def translate        = (left ~ op ~ right).translate
  }  
  case class NameRep(name: Name) extends Rep {
    override def repName = translate
    override def isEmpty = translate == ""
    def translate        = name.decode    
  }
  case class MultiRep(reps: List[Rep]) extends Rep {
    override def isEmpty = reps forall (_.isEmpty)
    override def flatten = reps flatMap (_.flatten)
    def translate = {
      val sb = new StringBuilder
      reps foreach (sb append _.translate)
      sb.toString
    }
  }
  case class ModifiersRep(mods: Modifiers) extends HasFlagsRep {
    def flagCarrier = mods
    override def isEmpty       = flags == 0L && privateWithin == tpnme.EMPTY
    // override def flags         = mods.flags & flagMask
    // override def privateWithin = mods.privateWithin
    def translate              = flagsToString(flags, privateWithin.toString)
  }
  case class ClassRep(label: String, clazz: Class[_]) extends Rep {
    def shortName = clazzName(clazz)
    def translate = "<No Rep: '" + label + "' is a " + shortName + ">"
  }
  case class ErrorRep[T <: AnyRef](cause: T, message: String) extends Rep {
    def shortName = clazzName(cause.getClass)
    def translate = "<Error: '" + message + "' caused by " + shortName + ">"
  }
  case class PureStringRep(value: String) extends Rep {
    override def isEmpty = translate == ""
    def translate        = value
  }
}

trait TreeReps extends AnyRef
        with RepTypes
        with RepClasses
        with RepObjects
        with RepUtil
        with RepImplicits {

  self: AutoTraverser =>

  import global._
  
  type FoldContext <: AbsFoldContext
  def foldContext: FoldContext
  def prevReps = foldContext.prevReps
  def nextReps = foldContext.nextReps
  
  trait RepOps {
    self: Rep =>
    
    /** Join directly. */
    def %(other: Rep): Rep = join(this, other)
    
    /** Join with a space. */
    def ~(other: Rep): Rep = (this, other) match {
      case (Space, Space) => Space
      case (Space, x)     => Space % x
      case (x, Space)     => x % Space
      case (x, y)         => join(x, Space, y)
    }
    
    /** If either operand is empty, empty: otherwise, join with a space. */
    def ?(other: Rep): Rep = {
      if (this.isEmpty || other.isEmpty) Empty
      else join(this, Space, other)
    }
    
    /** If this rep is empty, empty: otherwise, apply function to this. */
    def \(f: Rep => Rep): Rep =
      if (this.isEmpty) Empty
      else f(this)
    
    /***/
    def flatten: List[Rep] = List(this)
    def flattened: Rep     = flatten match {
      case Nil      => Empty
      case x :: Nil => x
      case xs       => MultiRep(noEmpties(xs map (_.flattened)))
    }
  }

  abstract class AbsFoldContext(reps: IndexedSeq[Rep]) {
    type FoldResult
    def zero: FoldResult
    def foldFn(acc: FoldResult, rep: Rep): FoldResult
    def indentSize: Int  = 2
    
    private def clear() {
      _idx = 0
      _indentLevel = 0
      _currentResult = zero
    }
    
    def fold(): FoldResult = {
      clear()
      0 until reps.size foreach { i =>
        _currentResult = foldFn(currentResult, reps(i))
        _idx = i + 1
      }
      currentResult
    }
    
    private var _idx: Int                  = _
    private var _currentResult: FoldResult = _
    private var _indentLevel: Int          = _
    
    val size          = reps.size
    def idx           = _idx
    def currentResult = _currentResult
    def indentLevel   = _indentLevel
    def prevReps      = ((idx - 1) to 0 by -1).iterator map reps
    def nextReps      = ((idx + 1) until size).iterator map reps
    def spaces        = " " * indentSize * indentLevel
    def indent        = _indentLevel += 1
    def undent        = _indentLevel -= 1
  }
  
  /** The base class for Reps.  Has one abstract method, which
   *  is used when it's time to become a String.
   */
  trait Rep extends RepOps {
    def translate: String
    
    private def allSpaces    = translate forall (_.isWhitespace)
    def context: FoldContext = foldContext
    def isEmpty              = false
    def isWhitespace         = !isEmpty && allSpaces
    def length               = translate.length
  
    /** Conveniences */
    def inParens: Rep   = GroupedRep("(", this, ")")
    def inBrackets: Rep = GroupedRep("[", this, "]")
    def inBraces: Rep   = GroupedRep("{" % NoTouch, this, NoTouch % "}")

    def optParens: Rep     = \ (_ inParens)
    def optBraces: Rep     = \ (_ inBraces)
    def optBrackets: Rep   = \ (_ inBrackets)
    def optAscription: Rep = \ (_ asAscription)
    
    def asAnnotation  = "@" % this
    def asAscription  = ":" ~ this
    def asWith: Rep   = "with" ~ this
    
    // def SEMI: Rep     = this % Semi % Newline
    // def NL: Rep       = this % Newline
    // def NLTAB: Rep    = this % Newline % Indentation
    // def NLPREFIX: Rep = Newline % Indentation % this
    // def NLPLUS: Rep   = this % Newline % IndentPlus % Indentation
    // def NLMINUS: Rep  = this % Newline % IndentMinus % Indentation

    def hasSymbol               = false
    // def annotations: List[Rep]  = Nil
    def flags: Long             = 0L
    def repName: String         = ""
    def flagMask                = if (settings.debug.value) -1L else PrintableFlags
    def privateWithin: Name     = tpnme.EMPTY
    final override def toString = translate
  }  
}
