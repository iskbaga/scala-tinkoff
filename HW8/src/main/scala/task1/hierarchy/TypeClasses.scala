package task1.hierarchy
import task1.{Branch, Leaf, Tree}
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}

trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object TypeClasses {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
  implicit val treeApply: Apply[Tree] = new Apply[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)

    def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = (ff, fa) match {
      case (Leaf(f), Leaf(a))               => Leaf(f(a))
      case (Leaf(f), Branch(la, ra))        => Branch(ap(Leaf(f))(la), ap(Leaf(f))(ra))
      case (Branch(lf, rf), Leaf(a))        => Branch(ap(lf)(Leaf(a)), ap(rf)(Leaf(a)))
      case (Branch(lf, rf), Branch(la, ra)) => Branch(ap(lf)(la), ap(rf)(ra))
    }
  }

  implicit val treeApplicative: Applicative[Tree] = new Applicative[Tree] {
    def pure[A](a: A): Tree[A] = Leaf(a)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApply.ap(ff)(fa)
  }

  implicit val treeFlatMap: FlatMap[Tree] = new FlatMap[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)

    def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApply.ap(ff)(fa)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def pure[A](a: A): Tree[A] = treeApplicative.pure(a)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = treeFlatMap.flatMap(fa)(f)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)
  }
}
