package typeparams
import scala.reflect.ClassTag

class Economy

class UpgradedEconomy extends Economy

class Special1b extends UpgradedEconomy

class ExtendedEconomy extends Economy

class Business extends ExtendedEconomy

class Elite extends Business

class Platinum extends Business

class ServiceLevelAdvance[A <: Economy](implicit private val level: ClassTag[A]) {
  def advance[B <: A: ClassTag]: ServiceLevelAdvance[B] =
    new ServiceLevelAdvance[B]
  def getLevel: ClassTag[A] = level
}
