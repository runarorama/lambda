object Naive {

  sealed trait Type

  // The integer type
  case object I extends Type
  // Function types
  case class Fun(t1: Type, t2: Type) extends Type

  sealed trait Term

  // Variables
  case class Var(name: String) extends Term

  // Literals
  case class Lit(value: Int) extends Term

  // Function application
  case class App(e1: Term, e2: Term) extends Term

  // Lambda abstraction
  case class Lam(name: String, typ: Type, body: Term) extends Term

  // Evaluate a term `t` given an environment `env`
  def eval(env: String => Option[Term], t: Term): Term = t match {
    case Var(s) => eval(env, env(s).getOrElse(sys.error(s"Unbound variable: $s")))
    case App(e1, e2) => eval(env, e1) match {
      case Lam(x, _, e) => eval(env, subst(x, e2, e))
      case _         => sys.error("Not a function: $e1")
    }
    case l@Lam(_, _, _) => repaint(allNames, l)
    case x => x
  }

  // A source of fresh names
  lazy val allNames: Stream[String] =
    Stream.from(1).map("t" + _)

  // Substitute `t1` for variables names `v` in `t2`
  def subst(v: String, t1: Term, t2: Term): Term =  t2 match {
    case x@Var(s) => if (v == s) t1 else t2
    case App(e1, e2) => App(subst(v, t1, e1), subst(v, t1, e2))
    case f@Lam(x, t, e) => if (x == v) f else Lam(x, t, subst(v, t1, e))
    case x => x
  }

  def freeVars(t: Term): List[String] = {
    def go(free: List[String], bound: List[String], term: Term): List[String] =
      term match {
        case Var(s) => if (bound contains s) s :: free else free
        case App(e1, e2) => go(free, bound, e1) ++ go(free, bound, e2)
        case Lam(s, _, e) => go(free, s :: bound, e)
        case x => free
      }
    go(List(), List(), t)
  }

  def repaint(bucket: Stream[String], t: Term): Term = t match {
    case Lam(v, tp, e) =>
      val fresh = bucket.head
      lazy val freshBucket = bucket.filterNot(freeVars(e) contains _)
      val e2 = subst(v, Var(fresh), e)
      Lam(fresh, tp, repaint(freshBucket.tail, e2))
    case App(e1, e2) => App(repaint(bucket, e1), repaint(bucket, e2))
    case _ => t
  }

  val i = Lam("x", I, Var("x"))
  val k = Lam("x", I, Lam("y", I, Var("x")))

  val capturing = App(App(k, Var("y")), Lit(10))
  val noncapturing = App(App(Lam("a", I, Lam("b", I, Var("a"))), Var("y")), Lit(10))

  val empty: String => Option[Term] = _ => None
  val wy: String => Option[Term] = y => if (y == "y") Some(Lit(42)) else None

  def typer(env: Map[String, Type], exp: Term): Type = exp match {
    case Lit(_) => I
    case Var(v) => env.get(v).getOrElse(sys.error(s"Unbound variable $v"))
    case App(e1, e2) =>
      val t1 = typer(env, e1)
      t1 match {
        case Fun(ta, tr) =>
          val t2 = typer(env, e2)
          if (ta == t2) tr
          else sys.error(s"Type mismatch. Expected $ta, found $t2")
        case _ => sys.error(s"Not a function type: $t1")
      }
    case Lam(x, t, e) => Fun(t, typer(env + (x -> t), e))
  }
}
