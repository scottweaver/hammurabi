package hammurabi.util


/**
 * Created by scott on 12/6/14.
 */
object Reflect {
  import scala.reflect.runtime.{currentMirror => cm, universe => ru}
  import ru._


  def typeToClass[A](implicit ttag: TypeTag[A]) : Class[A] = cm.runtimeClass(typeOf[A].typeSymbol.asClass).asInstanceOf[Class[A]]

  implicit def typeTagToClazz[A](ttag: TypeTag[A]): Class[A] = typeToClass[A](ttag)

}
