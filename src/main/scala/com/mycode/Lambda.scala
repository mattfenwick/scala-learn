// package com.mycode


// things to calculate:
//   1. set of all variables
//   2. count of use of all variables
//   3. resolve all variables (maybe generate a guaranteed unique name)
//   4. find set of free variables
//   5. find set of bound variables (plus where they're bound)

sealed trait Term { 
// trait vs. abstract class?
// why can't I define a method in here?
}

// here's what it could look like with ordinary classes: note the use of `val`
//   class A(val op: String, val arg: String)
case class A(op: Term, arg: Term) extends Term
case class F(param: String, body: Term) extends Term
case class S(name: String) extends Term
case class As(op: Term, args: List[Term]) extends Term
case class Fs(params: List[String], body: Term) extends Term {
    val ps = params.toSet
    if ( params.size > ps.size ) throw new Exception("duplicate symbol in Fs constructor")
}

val eg1 = F("z", A(S("xyz"), F("q", S("abc"))))
val eg2 = F("a", F("b", F("a", A(S("c"), F("b", S("a"))))))
val eg3 = Fs(List("a", "b", "c"), As(S("f"), List(eg1, eg2, S("z"))))
val eg4 = A(S("f"), F("f", S("f"))) // both a bound and a free "f"
val eg5 = F("x", As(S("f"), List(S("x"), S("y"))))
val eg6 = F("x", S("y"))
val eg7 = F("x", S("x"))
val eg8 = F("x", F("y", S("y")))

def print(term: Term): String = {
    term match {
        case A(a,b)   => "(" + print(a) + " " + print(b) + ")"
        case S(s)     => s
        case F(o,a)   => "\\" + o + ". " + print(a)
        case As(o,as) => "(" + print(o) + " " + as.map(print).mkString(" ") + ")"
        case Fs(ps,b) => "\\" + ps.mkString(" ") + ". " + print(b)
    }
}

object Reducer {

    def vars(term: Term): List[String] = {
        term match {
            case A(f,a)   => vars(f) ++ vars(a)
            case S(s)     => List(s)
            case F(p,b)   => List(p) ++ vars(b)
            case As(o,as) => vars(o) ++ as.flatMap(vars)
            case Fs(ps,b) => ps      ++ vars(b)
        }
    }
    
    def bound(term: Term): List[String] = {
        term match {
            case A(f,a)   => bound(f) ++ bound(a)
            case S(s)     => List()
            case F(p,b)   => List(p)  ++ bound(b)
            case As(o,as) => bound(o) ++ as.flatMap(bound)
            case Fs(ps,b) => ps       ++ bound(b)
        }
    }
    
    def free(term: Term): Set[String] = {
        term match {
            case A(f,a)   => free(f) ++ free(a)
            case S(s)     => Set(s)
            case F(p,b)   => free(b) - p
            case As(o,as) => free(o) ++ as.flatMap(free)
            case Fs(ps,b) => free(b) -- ps.toSet
        }
    }

    def used(term: Term): List[String] = {
        term match {
            case A(f,a)   => used(f) ++ used(a)
            case S(s)     => List(s)
            case F(p,b)   => List()  ++ used(b)
            case As(o,as) => used(o) ++ as.flatMap(used)
            case Fs(ps,b) => used(b)
        }
    }

    def f_list(ps: List[String], vars: List[String]) = {
      ps.map((p) => ((if (vars.contains(p)) 
                        "shadowing: " + p
                      else 
                        "not shadowing: " + p),
                     vars))
    }
    
    def shadowing_help(term: Term, vars: List[String]): List[(String, List[String])] = {
        term match {
            case A(f,a) => shadowing_help(f, vars) ++ shadowing_help(a, vars)
            case S(s)   => List(("symbol: " + s, vars))
            case F(p,b) => f_list(List(p), vars) ++ shadowing_help(b, p :: vars)
            case As(o,as) => (o :: as).flatMap((t) => shadowing_help(t, vars))
            case Fs(ps,b) => List()
        }
    }
    
    def shadowing(term: Term) = {
        shadowing_help(term, List())
    }
    
    def all(term: Term) = {
        println(print(term))
        println("vars: " + vars(term))
        println("bound: " + bound(term))
        println("free: " + free(term))
        println("used: " + used(term))
        println("shadowing: " + shadowing(term))
    }
}


def f_bound(b_pair: (Int, Map[String, String]), name: String) = {
  val ix = b_pair._1
  val map = b_pair._2
  val keyval = (name, "b" + ix)
  (ix + 1, map + keyval)
}

class Scope(val parent: Option[Scope], val vars: Map[String, String]) {

    def nested(new_vars: List[String], b: Int): (Scope, Int) = {
        val base = (b, Map()): (Int, Map[String, String])
        val (new_b, new_translations) = new_vars.foldLeft(base)(f_bound)
        (new Scope(Some(this), new_translations), new_b)
    }
    
    override def toString(): String = {
        val par = parent match {
            case None    => "()"
            case Some(p) => p.toString()
        }
        val m = Map("type"   -> "Scope", 
                    "parent" -> par,
                    "vars"   -> vars.toString)
        m.toString
    }
    
    def lookup(name: String): Option[String] = {
        if (vars.contains(name))
            vars.get(name)
        else 
            parent match {
                case None    => None
                case Some(p) => p.lookup(name)
            }
    }
}

val root = new Scope(None, Map() : Map[String, String])

def resolve(term: Term, scope: Scope, b: Int): (Term, Int) = {
    term match {
        case S(s)   => scope.lookup(s) match {
            case None   => (S("f" + s), b) // TODO free variables -- consistent names
            case Some(n) => (S(n), b)
        }
        case A(f,a)    => {
            val (f_n, b_2) = resolve(f, scope, b)
            val (a_n, b_3) = resolve(a, scope, b_2)
            (A(f_n, a_n), b_3)
        }
        case F(p,body) => {
            val (scp, b_2) = scope.nested(List(p), b)
            val (new_body, b_3) = resolve(body, scp, b_2)
            (F(scp.lookup(p).get, new_body), b_3)
        }
        case As(o,as) => {
            val (o_n, b_2) = resolve(o, scope, b)
            val base = (List(), b_2): (List[Term], Int)
            def f_um(curr: (List[Term], Int), term: Term): (List[Term], Int) = {
                val (the_as, b_n) = curr
                val (term_n, b_out) = resolve(term, scope, b_n)
                (term_n :: the_as, b_out)
            }
            val (as_n, b_final) = as.foldLeft(base)(f_um)
            (As(o_n, as_n.reverse), b_final)
        }
        case Fs(ps,body) => {
            val (scp, b_2) = scope.nested(ps, b)
            val (new_body, b_3) = resolve(body, scp, b_2)
            (Fs(ps.map((p) => scp.lookup(p).get), new_body), b_3)
        }
    }
}
/**/

