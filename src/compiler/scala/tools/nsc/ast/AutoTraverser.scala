/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package ast

import util.returning
import scala.collection.{ mutable, immutable, generic }
import symtab.Flags._
import symtab.SymbolTable
import scala.util.DynamicVariable

class Peeper[T, CC[T] <: Traversable[T]](xs: CC[T])

// Precedence:
// (all letters)
// |
// ^
// &
// < >
// = !
// : 
// + -
// * / %
// (all other special characters)

trait WhatNextTraversers {
  self: AutoTraverser =>
  import global._

  trait WhatNextTraverser extends Traverser {
    case class WhatNext(value: Option[Rep], recurse: Boolean) { }
    private val collector = new mutable.ListBuffer[Rep]
  
    // def reduce(xs: List[Rep]): String
    def collect(tree: Tree): List[Rep] = {
      // dbg("collect called with: " + clazzName(tree.getClass) + ": " + asCompactString(tree))
      // (new Exception).printStackTrace
      collector.clear()
      traverse(tree)
      collector.toList take 1
      // collector.toList
    }
    // def fold(tree: Tree): String = reduce(collect(tree))
    def produce(value: Rep): Unit = collector += value
  
    def whatNext(tree: Tree): WhatNext = {
      val noRecurse = tree.isInstanceOf[PackageDef]
      // val noRecurse = true
      WhatNext(action(tree), noRecurse)
    }
  
    def defTree(t: DefTree): Option[Rep]
    def refTree(t: RefTree): Option[Rep]
    def termTree(t: TermTree): Option[Rep]
    def typTree(t: TypTree): Option[Rep]
    def otherTree(t: Tree): Option[Rep]
    def nonTree(x: AnyRef): Option[Rep]

    final override def traverse(tree: Tree): Unit = {
      val WhatNext(value, recurse) = whatNext(tree)
      // value match {
      //   case Some(t)  => dbg("Some(" + clazzName(tree.getClass) +  ")")
      //   case _        => dbg("Not Some(" + clazzName(tree.getClass) +  ")")
      // }
      
      value foreach produce
    
      if (recurse) {
        // dbg("traverse recursing on a " + clazzName(tree.getClass))
        super.traverse(tree)
      }
    }
    final def action(x: AnyRef): Option[Rep] = x match {
      case t: Tree            => action(t)
      case _                  => nonTree(x)
    }
    final def action(t: Tree): Option[Rep] = t match {
      case x: TypTree       => typTree(x)
      case x: DefTree       => defTree(x)
      case x: RefTree       => refTree(x)
      case x: TermTree      => termTree(x)
      case x: Tree          => otherTree(x)
    }
    implicit def automagicRecurser(s: Rep): Option[Rep] = Some(s)
  }
}

trait AutoTraverser extends WhatNextTraversers with TreeReps {
  val global: SymbolTable
  
  import global.{ indent => _, _ }
  import definitions.{ ObjectClass, ScalaObjectClass, SerializableAttr }
  import treeInfo.{ IsTrue, IsFalse, isEarlyDef }
  import formatter._  
  
  def fromModifiers(mods: Modifiers): Rep = ModifiersRep(mods)
  def fromName(name: Name): Rep           = if (name == null) "<null>" else NameRep(name)
  def fromSymbol(sym: Symbol): Rep        = SymbolRep(sym)
  def fromType(tpe: Type): Rep            = TypeRep(tpe)
  def stringRep(s: String): Rep           = PureStringRep(s)
  
  // class RepContext extends AbsRepContext {
  //   def emptyPackage = PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), Nil)
  //   def emptyClass   = ClassDef(NoMods, nme.EMPTY.toTypeName, Nil, Template(Nil, emptyValDef, Nil))
  //   def emptyMembers = Nil
  //   
  //   val currentImpl    = new DynamicVariable[ImplDef](emptyClass)
  //   val currentMembers = new DynamicVariable[List[MemberDef]](emptyMembers)
  //   val currentPackage = new DynamicVariable[PackageDef](emptyPackage)
  //   
  //   def withCurrentImpl(md: ImplDef)(body:       => T): T = currentImpl.withValue(md)(body)
  //   def withCurrentMember(md: MemberDef)(body:   => T): T = currentMembers.withValue(md :: currentMembers.value)(body)
  //   def withCurrentPackage(pd: PackageDef)(body: => T): T = currentPackage.withValue(pd)(body)
  // }
  // 
  // private var _context: RepContext = new RepContext
  // def withContext(ctx: RepContext)(tree: Tree): String = {
  //   val saved = _context
  //   try {
  //     _context = ctx
  //     new AutoTreeTraverser fold tree
  //   }
  //   finally _context = saved
  // }

  // override def traverserContext = _context
  // def currentImpl               = traverserContext.currentImpl.value
  // def currentMembers            = traverserContext.currentMembers.value
  // def currentPackage            = traverserContext.currentPackage.value.pid
  // def currentMember             = currentMembers.headOption
  // 
  // def stringify(tree: Tree): String = withContext(new RepContext)(tree)
  
  type FoldContext = StringFoldContext
  private var currentContext: StringFoldContext = new StringFoldContext(Nil)
  def foldContext: StringFoldContext = currentContext
  def stringify(tree: Tree): String = {
    val traverser = new AutoTreeTraverser
    val reps = traverser collect tree
    val peeped = {
      dbg("Collected " + reps.size + " reps.")
      // dbg("And they are: " + reps.map("'" + _ + "'").mkString(" "))
      
      val flattened = reps flatMap (_.flatten)
      dbg("Flattened to " + flattened.size + " reps.")

      // XXX perhaps sliding should be changed not to ever return too small
      val peepholed = {
        val lb = new mutable.ListBuffer[Rep]
        val it = Iterator(Empty, Empty) ++ (flattened.iterator filterNot (_.isEmpty)) ++ Iterator(Empty, Empty)
        it sliding 3 foreach {
          case List(Newline, Semi, Newline) =>
          case List(Space | Indentation, r2 @ (Space | Indentation), _) => dbg("Discarding: '" + r2 + "'")
          case List(r1, NoTouch, r3) =>
            if (r1.isWhitespace || r3.isWhitespace) Empty
            else Space
          case List(Newline, Newline, _) =>
          case List(r1, r2, r3) => lb += r2
        }
        lb.toList
      }
      dbg("Peeped to " + peepholed.size + " reps.")
      peepholed
    }
    
    currentContext = new StringFoldContext(peeped)
    currentContext.fold().toString
  }
  
  class StringFoldContext(xs: List[Rep]) extends AbsFoldContext(xs.toIndexedSeq) {
    def foldFn(acc: StringBuilder, rep: Rep): StringBuilder = {
      acc append rep.translate
    }

    type FoldResult       = StringBuilder
    def zero              = new StringBuilder("")    
    def currentLine       = currentResult.reverse takeWhile (_ != '\n') reverse
    def currentLineLength = currentLine.length
  }

  class AutoTreeTraverser extends WhatNextTraverser with AutoTreeTraverserUtility {
    implicit def fromTree(tree: Tree): Rep            = action(tree) getOrElse Empty
    implicit def fromTrees(xs: List[Tree]): List[Rep] = xs map fromTree
    private def isRedundant(x: Any) = false

    // private def isRedundant(qual: Name): Boolean = {
    //   qual == currentPackage.name || qual == currentImpl.name
    // }
    // private def isRedundant(qual: Tree): Boolean = {
    //   def bySym = hasSym(qual) && {
    //     qual.symbol == currentPackage.symbol ||
    //     qual.symbol == currentImpl.symbol
    //   }
    //   def byTree = qual match {
    //     case This(id) => isRedundant(id)
    //     case _        => false
    //   }
    //   
    //   bySym || byTree
    // }
    
    def bracketList(args: List[Tree]): Rep        = commas(args).optBrackets
    def paramList(vps: List[ValDef]): Rep         = commas(vps map defTreeRep).inParens
    def paramLists(vpss: List[List[ValDef]]): Rep = smash(vpss map paramList)
    def parenList(args: List[Tree]): Rep          = commas(args).inParens
    def tparamList(tps: List[TypeDef]): Rep       = commas(tps map tparamRep).optBrackets

    private def tparamRep(tp: TypeDef): Rep = {
      val TypeDef(mods, name, tparams, rhs) = tp
      
      getName(tp) % tparamList(tp.tparams) ~ tp.rhs      
    }
    private def typeDefRep(tp: TypeDef): Rep = {
      val TypeDef(mods, name, tparams, rhs) = tp
      
      getAnnotations(tp) ~ mods ~ "type" ~ getName(tp) % tparamList(tparams) ~ {
        if (mods.isDeferred || mods.isParameter) rhs
        else ("=" ? rhs)
      }
    }
    def clazzRep(msg: String, x: AnyRef): T = ClassRep(msg, x.getClass)    
    def debugRep(msg: String, t: Tree): Rep  = {
      dbg("What the hell is this? " + msg + " " + clazzName(t.getClass) + " " + asStandardString(t))
      clazzRep(msg, t)
    }

    def selectorRep(sel: ImportSelector): Rep =
      if (isSimpleSelector(sel)) sel.name
      else sel.name ~ "=>" ~ sel.rename
    
    def importRep(tree: Import): Rep = {
      val selectorPart: Rep = tree.selectors match {
        case Nil                                  => Empty
        case sel :: Nil if isSimpleSelector(sel)  => selectorRep(sel)
        case xs                                   => commas(xs map selectorRep).inBraces
      }
      
      "import" ~ dotted(tree.expr, selectorPart)
    }
    
    def declareToken(t: Tree): Rep = t match {
      case md: MemberDef if !md.mods.isParameter  => md.keyword
      case _                                      => Empty
    }
    // def extendToken(t: Tree): Rep = if (isDeferredTree(t)) "<:" else "extends"
    // def defineToken(t: Tree): Rep = t match {
    //   case TypeDef(mods, _, _, _) if mods.isDeferred || mods.isParameter => "<:"
    //   case _                                                             => "="
    // }

    // type ThisPlatform = Platform[_$1] forSome { 
    //   <synthetic> type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
    // } {
    //   val global: Global.this.type
    // };
    // type ThisPlatform = {
    //   extends   {
    //     ;
    //     val global: Global.this.type ;
    //   };
    // };
    // 

    def templateRep(extend: Rep, t: Template): Rep = {
      val Template(parents, self, body) = t
      val (preSuper, postSuper)         = body partition isEarlyDef
      val templBody                     = block(selfRep(self) :: (postSuper: List[Rep]))
      val mixins                        = parents filterNot (x => isScalaObject(x) || isJavaLangObject(x)) map getSuperclassName
      
      val (templHeader, templMixins) = mixins match {
        case _ if preSuper.nonEmpty   => (extend ~ block(preSuper), mixins)
        case Nil                      => (Empty, Nil)
        case x :: Nil                 => (extend ~ x, Nil)
        case x :: xs                  => (extend ~ x % Newline, xs)
      }
      val templWiths = templMixins map ("with" ~ _)
      val templMixinRep =
        if (templWiths.isEmpty) Empty
        else if (preSuper.nonEmpty || templWiths.tail.isEmpty) templWiths reduceLeft (_ ~ _)
        else inset(4, templWiths)
      
      templHeader ~ templMixinRep ~ templBody
    }
    
    def selfRep(vd: ValDef): Rep =
      if (vd.isEmpty) Empty
      else vd.name % vd.tpt.optAscription ~ "=>"

    def defStart(md: MemberDef): Rep =
      words(getAnnotations(md), md.mods, declareToken(md), getName(md))

    def defMiddle(x: MemberDef): Rep = x match {
      case DefDef(_, _, tparams, vparamss, _, _)  => tparamList(tparams) % paramLists(vparamss)
      case ClassDef(_, _, tparams, _)             => tparamList(tparams)
      case TypeDef(_, _, tparams, _)              => tparamList(tparams)
      case _                                      => Empty
    }
    def defEnd(md: MemberDef): Rep = md match {
      case x: ValOrDefDef => x.tpt \ (":" ~ _)
      case _              => Empty
    }
    def defBody(md: DefTree): Rep = {
      val rhs = md match {
        case x: ValOrDefDef if !x.mods.isDeferred => x.rhs
        case x: LabelDef                          => x.rhs
        case x: TypeDef                           => x.rhs
        case _                                    => EmptyTree
      }
      val assign: Rep = "="
      val rhsRep: Rep = rhs
      val MAX_LINE = 80
      
      if (rhs.isEmpty) assign ~ Wild
      else if (printWithoutNewline(rhs)) assign ~ rhs
      else assign ~ DecisionRep(
        ctx => ctx.currentLineLength + rhsRep.length < MAX_LINE,
        rhs,
        block(List(rhs))
      )
    }
    
    private def defTreeRep(t: DefTree): Rep = t match {
      case x: PackageDef                      => x.keyword ~ x.pid ~ block(x.stats)
      case x: DefDef if isEmptyConstructor(x) => "/* <init> */"
      case x: DefDef                          => defStart(x) % defMiddle(x) % defEnd(x) ~ defBody(x)
      case x: ValDef if x.mods.isParameter    => defStart(x) % defEnd(x) % ((Space % "=") ? x.rhs)
      case x: ValDef                          => defStart(x) % defEnd(x) ~ defBody(x)
      case x: TypeDef if x.mods.isParameter   => tparamRep(x)
      case x: TypeDef                         => typeDefRep(x)
      case x: ImplDef                         => Newline % Indentation % defStart(x) % defMiddle(x) ~ templateRep("extends", x.impl)
      case x @ LabelDef(name, params, rhs)    => "<label> def" ~ name % parenList(params) ~ defBody(x)
      case x @ Bind(name, body)               => name ~ "@" ~ body inParens
      case x                                  => debugRep("definitionRep", x)
    }
    
    private def refTreeRep(tree: RefTree): Rep = {
      val (qual, name, op) = tree match {
        case sel @ Select(_, _)                   => return selectRep(sel)
        case Ident(name)                          => (EmptyTree, name, "")
        case SelectFromTypeTree(qual, name)       => (qual, name, "#")
        case SelectFromArray(qual, name, erasure) => (qual, name, ".<arr>")
        case x                                    => return debugRep("refTreeRep", x)
      }
      if (isRedundant(qual)) "POOPREFTREEPOOP" % name
      else qual % op % name
    }
    def caseBody(body: Tree): Rep = {
      val stmts = allStatements(body)
      stmts match {
        case Nil      => Empty
        case x :: Nil => x
        case xs       => Newline ~ indent(xs)
      }
    }
    def caseRep(tree: CaseDef): Rep = tree match {
      case CaseDef(pat, EmptyTree, body)  => "case" ~ pat ~ "=>" ~ caseBody(body)
      case CaseDef(pat, guard, body)      => "case" ~ pat ~ "if" ~ guard ~ "=>" ~ caseBody(body)
    }
    def casesRep(xs: List[CaseDef]): Rep = block(xs map caseRep)

    def defTree(t: DefTree): Option[Rep] = {
      defTreeRep(rebuildFromSymbol(t))
      // 
      // def f = defTreeRep(rebuildFromSymbol(t))
      // 
      // t match {
      //   case pd: PackageDef => traverserContext.withCurrentPackage(pd)(f)
      //   case md: MemberDef  => traverserContext.withCurrentMember(md)(f)
      //   case _              => f
      // }
    }
    def refTree(t: RefTree): Option[Rep] = refTreeRep(t)

    private def needBlockForBranch(xs: List[Tree]) = xs match {
      case Nil          => false
      case (_: If) :: _ => true
      case _ :: Nil     => false
      case _            => true
    }
    def ifRep(tree: If): Rep = tree match {
      case If( IsTrue(), x, _)    => x
      case If(IsFalse(), _, x)    => x
      case If(cond, thenp, elsep) =>
        val thenStmts  = flatBlock(thenp)
        val elseStmts  = flatBlock(elsep)
        val isNoElse   = elseStmts forall isLiteralUnit
        val thenBlock: Rep = if (needBlockForBranch(thenStmts)) block(thenStmts) else Newline % indent(thenStmts)
        val elseBlock: Rep = 
          if (elseStmts forall isLiteralUnit) Empty
          else Newline % Indentation % "else" ~ {
            if (needBlockForBranch(elseStmts)) block(elseStmts)
            else Newline % indent(elseStmts)
          }
        
        "if" ~ cond.inParens ~ thenBlock % elseBlock
    }
    
    def selectRep(tree: Select): Rep = tree match {
      case Select(target, UnaryOp(op)) => op % target
      case Select(qual, name)          => qual % "." % name
    }

    def applyRep(tree: Apply): Rep = {
      val Apply(fun, args) = tree
      def defaultRep = fun % parenList(args)
      
      tree match {
        case Apply(Select(target, method), List(arg)) =>
          if (isLogicalOr(method))
            ExpressionRep(target, LogicalOr, arg)
          else if (isLogicalAnd(method))
            ExpressionRep(target, LogicalAnd, arg)
          else (target, arg) match {
            case (_: Ident, _: Literal | _: Ident)  =>
              target ~ method ~ arg
            case _ =>
              defaultRep
          }
        case _ => defaultRep
      }
    }
    // private val CloneableAttr    = definitions.getClass("scala.cloneable")
    // def checkType(tp: Type) = tp.typeSymbol match {
    //   case SerializableAttr     => "@serializable"
    //   case CloneableAttr        => "@cloneable"
    //   case SerialVersionUIDAttr => args match {
    //     case Literal(const) :: _  => "@SerialVersionUID(" + const.longValue + ")"
    //     case _                    => null // XXX
    //   }
    //   case _ => "<annotation>"
    // }
    def annotationRep(x: AnyRef): Rep = {
      def assocRep(assoc: (Name, ClassfileAnnotArg)): Rep = assoc._1 % "=" % stringRep(assoc._2.toString)
      def annotRep(ann: Rep, args: List[Rep]): Rep        = "@" % ann % args.optParens

      x match {
        // Only used for annotated types and annotation ascriptions, eliminated by typer.
        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          val connector: Rep = if (tree.isType) ":" else Empty
          tree % connector ~ annotRep(tpt, args)        

        // assocs: Arguments to classfile annotations as name-value pairs.
        case AnnotationInfo(atp, Nil, assocs) =>
          annotRep(atp, assocs map assocRep)

        // args: Arguments to Scala annotations, represented as typed trees.
        case AnnotationInfo(atp, args, Nil) =>
          annotRep(atp, args)

        case _ =>
          ErrorRep(x, "Unknown tree arrived at AnnotationRep")
      }
    }
    
    override def termTree(t: TermTree): Option[Rep] = t match {
      case EmptyTree                      => Empty
      case Star(elem)                     => elem.inParens % "*"
      case Alternative(alts)              => alternatives(alts)
      case Match(selector, cases)         => selector ~ "match" ~ casesRep(cases)
      case Throw(expr)                    => "throw" ~ expr
      case Typed(expr, tpt)               => expr % ":" ~ tpt
      case TypeApply(fun, args)           => fun % bracketList(args)
      case t @ Apply(fun, args)           => applyRep(t)
      case UnApply(fun, args)             => "<ua>" % fun % parenList(args)      
      case Return(expr)                   => "return" ~ expr
      case Function(params, body)         => paramList(params) ~ "=>" ~ body
      case Literal(x)                     => stringRep(x.escapedStringValue)
      case Block(Nil, expr)               => fromTree(expr) % Newline
      case Block(xs, expr)                => block(xs :+ expr)
      case New(tpt)                       => "new" ~ tpt
      case Super(qual, mix)               => dotted(qual, "super", mix.optBrackets)
      case This(tpnme.EMPTY)              => stringRep("this")
      case This(qual)                     => qual % "." % "this"        
      case Try(expr, catches, finalizers) => stmts(List("try" ~ expr, "catch" ? casesRep(catches), "finally" ? finalizers))
      case t @ If(_, _, _)                => ifRep(t)
      case Assign(lhs, rhs)               => lhs ~ "=" ~ rhs
      case x                              => debugRep("termTree", x)
    }
    
    def typTree(t: TypTree): Option[Rep] = {
      def sym = t.tpe.typeSymbol
      if (t.tpe != null) {
        if (sym != null && sym.isAnonymousClass) /* fromName(sym.name) */ NameRep(sym.nameString)
        else fromType(t.tpe)
      }
      else t match {
        case tt @ TypeTree() if tt.original != null => action(tt.original)
        case tt @ TypeTree()                        => Empty
        case SingletonTypeTree(ref)                 => ref % "." % "type"
        case AppliedTypeTree(tp, args)              => tp % bracketList(args)
        case SelectFromTypeTree(qual, selector)     => qual % "#" % getName(t)
        case CompoundTypeTree(templ)                => templateRep("<:", templ)
        case TypeBoundsTree(lo, hi)                 => (">:" ? lo) ~ ("<:" ? hi)
        case ExistentialTypeTree(tpt, whereClauses) => tpt ~ "forSome" ~ block(whereClauses)
        case x                                      => debugRep("typTree", x)
      }
    }
    def otherTree(t: Tree): Option[Rep] = t match {
      case x @ Annotated(_, _)      => annotationRep(x)
      case x @ CaseDef(_, _, _)     => caseRep(x)
      case x @ Import(_, _)         => importRep(x)
      // case x @ Template(_, _, _)    => templateRep("", x)
      case x                        => debugRep("otherTree", x)
    }
    def nonTree(x: AnyRef): Option[Rep] = x match {
      case m: Modifiers       => fromModifiers(m)
      case s: ImportSelector  => selectorRep(s)
      case a: AnnotationInfo  => annotationRep(a)
    }
  }
  
  def flatten(rep: Rep): List[Rep] = rep.flatten
  def join(reps: Rep*) = {
    val lb = new mutable.ListBuffer[Rep]
    reps foreach {
      case x: MultiRep  => lb ++= x.reps
      case x            => lb += x
    }
    MultiRep(lb.toList)
  }

  trait AutoTreeTraverserUtility {
    self: AutoTreeTraverser =>

    private val ScalaObjectTree = Select(Ident(nme.scala_), tpnme.ScalaObject)
    def isSimpleSelector(sel: ImportSelector) = sel.name == sel.rename
    // def isDeferredTree(tree: Tree) = tree match {
    //   case x: MemberDef => x.mods.isDeferred
    //   case _            => existsSym(tree, _.isDeferred)
    // }
    // 
    def printWithoutNewline(tree: Tree) = tree match {
      case _: Block | _: Ident | _: Select | _: Literal | _: This | _: Super |
           _: Match => true

      case _  => false
    }
    
    def rebuildFromSymbol(tree: DefTree): DefTree = {
      val sym       = tree.symbol
      val useSymbol = hasSym(tree) && sym.isInitialized
      
      if (!useSymbol) tree
      else tree match {
        case ClassDef(_, _, _, impl @ Template(ps, emptyValDef, body)) if sym.thisSym != sym =>
          ClassDef(sym, Template(ps, ValDef(sym.thisSym), body))
        case ClassDef(_, _, _, impl)           => ClassDef(sym, impl)
        case ModuleDef(_, _, impl)             => ModuleDef(sym, impl)
        case ValDef(_, _, _, rhs)              => ValDef(sym, rhs)
        case DefDef(_, _, _, vparamss, _, rhs) => DefDef(sym, vparamss, rhs)
        case TypeDef(_, _, _, rhs)             => TypeDef(sym, rhs)
        case _                                 => tree
      }
    }

    def isJavaLangObject(tree: Tree) = existsSym(tree, _ == ObjectClass)
    def isScalaObject(tree: Tree)    = (
      if (hasSym(tree)) tree.symbol == ScalaObjectClass
      else tree match {
        case ScalaObjectTree  => true
        case _                => false
      }
    )
    
    abstract class OperatorUnapply(op: String) {
      def unapply(x: Any) = x match {
        case x: Name if x.decode.toString == op   => Some(op)
        case _                                    => None
      }
    }

    def isLogicalOr(name: Name) = name.decode.toString == "||"
    def isLogicalAnd(name: Name) = name.decode.toString == "&&"
    
    object UnaryOp {
      def unapply(x: Any) = x match {
        case name: Name => unaryOp(name)
        case _          => None
      }
    }
    def unaryOp(name: Name): Option[Rep] = name.decode.toString match {
      case "unary_!"  => Some("!")
      case "unary_~"  => Some("~")
      case "unary_-"  => Some("-")
      case "unary_+"  => Some("+")
      case _          => None
    }
    
    def isIdentOrLiteral(tree: Tree) = tree match {
      case _: Ident | _: Literal    => true
      case _                        => false
    }
    def isLiteralUnit(tree: Tree) = tree match {
      case Literal(Constant(()))  => true
      case _                      => false
    }
    def isConstructor(tree: Tree)    = existsSym(tree, _.isConstructor)
    def isEmptyConstructor(dd: DefDef) = dd match {
      case DefDef(_, nme.CONSTRUCTOR, Nil, List(Nil), _, Block(_ :: _, Literal(Constant(())))) => true
      case _ => false
    }
    def getAnnotations(tree: Tree): List[Rep] = tree match {
      case t if t hasSymbolWhich (_.hasAssignedAnnotations) => t.symbol.annotations map annotationRep
      case t: MemberDef                                     => t.mods.annotations map annotationRep
      case _                                                => Nil
    }
    
    // drill down through Blocks and pull out the real statements.
    def allStatements(t: Tree): List[Tree] = t match {
      case Literal(Constant(()))  => Nil
      case Block(stmts, expr)     => stmts :+ expr flatMap allStatements
      case _                      => List(t)
    }
    def flatBlock(t: Tree): List[Tree] = t match {
      case Block(stmts, expr)     => stmts :+ expr
      case x                      => List(x)
    }

    def bodyCount(x: ValOrDefDef) = allStatements(x.rhs).size

    // symbols and types
    def hasType(tree: Tree) = tree.tpe != null && tree.tpe != NoType
    def hasSym(tree: Tree)  = tree.symbol != null && tree.symbol != NoSymbol
    
    def existsSym(tree: Tree, p: Symbol => Boolean) = hasSym(tree) && p(tree.symbol)
    def existsType(tree: Tree, p: Type => Boolean) = hasType(tree) && p(tree.tpe)
    
    def getSuperclassName(tree: Tree): Rep = tree match {
      case _: RefTree | _: DefTree  => getName(tree)
      case _ if hasSym(tree)        => getName(tree)
      case _ =>
        tree match {
          case ExistentialTypeTree(tpt, whereClauses) => getSuperclassName(tpt) ~ block(whereClauses)
          case AppliedTypeTree(tp, args)              => getSuperclassName(tp) % bracketList(args)
          case _                                      => debugRep("getSuperclassName", tree)
        }
    }
    
    // def getFullName(tree: Tree): Rep = getCompilerName(tree, x => x.fullName: Rep)
    def getName(tree: Tree): Rep = getNamePrefix(tree) % getCompilerName(tree, x => x.nameString: Rep)
    private def getNamePrefix(tree: Tree): Rep = {
      if (hasSym(tree) && tree.symbol.isMixinConstructor) "/*" ~ tree.symbol.owner.name ~ "*/" % Space
      else Empty
    }
    private def getCompilerName(tree: Tree, f: Symbol => Rep): Rep = tree match {
      case t if hasSym(t)   => f(t.symbol)
      case t: RefTree       => t.name
      case t: DefTree       => t.name
      case _                => debugRep("getCompilerName", tree)
    }
  }
}
