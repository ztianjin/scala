/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.jvm

import scala.collection.{ mutable, immutable }
import scala.tools.funnel.{ Memo, Caller }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.symtab._
import scala.tools.nsc.symtab.classfile.ClassfileConstants._
import ch.epfl.lamp.fjbg._
import JAccessFlags._

trait GenJVMUtil {
  self: GenJVM =>

  import global._
  import icodes._
  import icodes.opcodes._
  import definitions._

  /** Map from type kinds to the Java reference types. It is used for
   *  loading class constants. @see Predef.classOf.
   */
  val classLiteral = immutable.Map[TypeKind, JObjectType](
    UNIT   -> new JObjectType("java.lang.Void"),
    BOOL   -> new JObjectType("java.lang.Boolean"),
    BYTE   -> new JObjectType("java.lang.Byte"),
    SHORT  -> new JObjectType("java.lang.Short"),
    CHAR   -> new JObjectType("java.lang.Character"),
    INT    -> new JObjectType("java.lang.Integer"),
    LONG   -> new JObjectType("java.lang.Long"),
    FLOAT  -> new JObjectType("java.lang.Float"),
    DOUBLE -> new JObjectType("java.lang.Double")
  )
  val javaTypeFixed = immutable.Map[TypeKind, JType](
    UNIT   -> JType.VOID,
    BOOL   -> JType.BOOLEAN,
    BYTE   -> JType.BYTE,
    SHORT  -> JType.SHORT,
    CHAR   -> JType.CHAR,
    INT    -> JType.INT,
    LONG   -> JType.LONG,
    FLOAT  -> JType.FLOAT,
    DOUBLE -> JType.DOUBLE
  )
  // XXX this was made weak
  val javaNameMemo: Symbol => String = {
    implicit val caller = implicitly[Caller] drop 1
    memoize((sym: Symbol) =>
      if (sym.isClass || (sym.isModule && !sym.isMethod))
        sym.fullName('/') + moduleSuffix(sym)
      else
        sym.simpleName.toString.trim() + moduleSuffix(sym)
    )
  }
  // 
  // private val javaNameCache = {
  //   val map = new mutable.WeakHashMap[Symbol, String]()
  //   map ++= List(
  //     NothingClass        -> RuntimeNothingClass.fullName('/'),
  //     RuntimeNothingClass -> RuntimeNothingClass.fullName('/'),
  //     NullClass           -> RuntimeNullClass.fullName('/'),
  //     RuntimeNullClass    -> RuntimeNullClass.fullName('/')    
  //   )
  // }

  /** Return the a name of this symbol that can be used on the Java
   *  platform.  It removes spaces from names.
   *
   *  Special handling:
   *    scala.Nothing erases to scala.runtime.Nothing$
   *       scala.Null erases to scala.runtime.Null$
   *
   *  This is needed because they are not real classes, and they mean
   *  'abrupt termination upon evaluation of that expression' or null respectively.
   *  This handling is done already in GenICode, but here we need to remove
   *  references from method signatures to these types, because such classes can
   *  not exist in the classpath: the type checker will be very confused.
   */
  def javaNameLogic(sym: Symbol): String = sym match {
    case NothingClass | RuntimeNothingClass => "scala/runtime/Nothing$"
    case NullClass | RuntimeNullClass       => "scala/runtime/Null$"
    case _                                  =>
      if (sym.isClassConstructor) "<init>"
      else if (sym.isTerm && !sym.isModule) sym.simpleName.toString.trim() + moduleSuffix(sym)
      else if (primitiveCompanions(sym)) "scala/runtime/" + sym.name + "$"
      else javaNameMemo(sym)
  }

  /** This trait may be used by tools who need access to 
   *  utility methods like javaName and javaType. (for instance,
   *  the Eclipse plugin uses it).
   */
  trait BytecodeUtil {
    def javaName(sym: Symbol): String    
    def javaType(s: Symbol): JType   = javaTypeMemo(s)
    def javaType(t: Type): JType     = javaType(toTypeKind(t))
    def javaType(t: TypeKind): JType =
      if (javaTypeFixed contains t) javaTypeFixed(t)
      else t match {
        case REFERENCE(cls)  => new JObjectType(javaName(cls))
        case ARRAY(elem)     => new JArrayType(javaType(elem))
      }
    
    val javaTypeMemo: Symbol => JType = (sym: Symbol) =>
      if (sym.isMethod)
        new JMethodType(
          if (sym.isClassConstructor) JType.VOID else javaType(sym.tpe.resultType),
          sym.tpe.paramTypes map javaType toArray
        )
      else
        javaType(sym.tpe)
    
    val conds = immutable.Map[TestOp, Int](
      EQ -> JExtendedCode.COND_EQ,
      NE -> JExtendedCode.COND_NE,
      LT -> JExtendedCode.COND_LT,
      GT -> JExtendedCode.COND_GT,
      LE -> JExtendedCode.COND_LE,
      GE -> JExtendedCode.COND_GE
    )
    val negate = immutable.Map[TestOp, TestOp](
      EQ -> NE,
      NE -> EQ,
      LT -> GE,
      GT -> LE,
      LE -> GT,
      GE -> LT
    )

    protected def genConstant(jcode: JExtendedCode, const: Constant) {
      const.tag match {
        case UnitTag    => ()
        case BooleanTag => jcode emitPUSH const.booleanValue
        case ByteTag    => jcode emitPUSH const.byteValue
        case ShortTag   => jcode emitPUSH const.shortValue
        case CharTag    => jcode emitPUSH const.charValue
        case IntTag     => jcode emitPUSH const.intValue
        case LongTag    => jcode emitPUSH const.longValue
        case FloatTag   => jcode emitPUSH const.floatValue
        case DoubleTag  => jcode emitPUSH const.doubleValue
        case StringTag  => jcode emitPUSH const.stringValue
        case NullTag    => jcode.emitACONST_NULL()
        case ClassTag   =>
          val kind = toTypeKind(const.typeValue)
          val toPush =
            if (kind.isValueType) classLiteral(kind)
            else javaType(kind).asInstanceOf[JReferenceType]

          jcode emitPUSH toPush

        case EnumTag   =>
          val sym = const.symbolValue
          jcode.emitGETSTATIC(javaName(sym.owner),
                              javaName(sym),
                              javaType(sym.tpe.underlying))
        case _         =>
          abort("Unknown constant value: " + const)
      }
    }
  }
}
