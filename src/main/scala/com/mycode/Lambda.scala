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

case class S(name: String) extends Term
case class A(op: Term, args: List[Term]) extends Term
case class F(params: List[String], body: Term) extends Term {
    val ps = params.toSet
    if ( params.size > ps.size ) throw new Exception("duplicate symbol in F constructor")
}
def F_(p: String, term: Term): F = { F(List(p), term) }
def A_(op: Term, arg: Term) : A = { A(op, List(arg)) }

val eg1 = F_("z", A_(S("xyz"), F_("q", S("abc"))))
val eg2 = F_("a", F_("b", F_("a", A_(S("c"), F_("b", S("a"))))))
val eg3 = F(List("a", "b", "c"), A(S("f"), List(eg1, eg2, S("z"))))
val eg4 = A_(S("f"), F_("f", S("f"))) // both a bound and a free "f"
val eg5 = F_("x", A(S("f"), List(S("x"), S("y"))))
val eg6 = F_("x", S("y"))
val eg7 = F_("x", S("x"))
val eg8 = F_("x", F_("y", S("y")))

def print(term: Term): String = {
    term match {
        case S(s)     => s
        case A(o,as) => "(" + print(o) + " " + as.map(print).mkString(" ") + ")"
        case F(ps,b) => "\\" + ps.mkString(" ") + ". " + print(b)
    }
}

def vars(term: Term): List[String] = {
    term match {
        case S(s)     => List(s)
        case A(o,as) => vars(o) ++ as.flatMap(vars)
        case F(ps,b) => ps      ++ vars(b)
    }
}

def bound(term: Term): List[String] = {
    term match {
        case S(s)     => List()
        case A(o,as) => bound(o) ++ as.flatMap(bound)
        case F(ps,b) => ps       ++ bound(b)
    }
}

def free(term: Term): Set[String] = {
    term match {
        case S(s)     => Set(s)
        case A(o,as) => free(o) ++ as.flatMap(free)
        case F(ps,b) => free(b) -- ps.toSet
    }
}

def used(term: Term): List[String] = {
    term match {
        case S(s)     => List(s)
        case A(o,as) => used(o) ++ as.flatMap(used)
        case F(ps,b) => used(b)
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
        case S(s)   => List(("symbol: " + s, vars))
        case A(o,as) => (o :: as).flatMap((t) => shadowing_help(t, vars))
        case F(ps,b) => f_list(ps, vars) ++ shadowing_help(b, ps ++ vars)
    }
}

def shadowing(term: Term) = {
    // TODO this needs to realize that it's shadowing free variables ... maybe ?
    shadowing_help(term, List())
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
        case A(o,as) => {
            val (o_n, b_2) = resolve(o, scope, b)
            val base = (List(), b_2): (List[Term], Int)
            def f_um(curr: (List[Term], Int), term: Term): (List[Term], Int) = {
                val (the_as, b_n) = curr
                val (term_n, b_out) = resolve(term, scope, b_n)
                (term_n :: the_as, b_out)
            }
            val (as_n, b_final) = as.foldLeft(base)(f_um)
            (A(o_n, as_n.reverse), b_final)
        }
        case F(ps,body) => {
            val (scp, b_2) = scope.nested(ps, b)
            val (new_body, b_3) = resolve(body, scp, b_2)
            (F(ps.map((p) => scp.lookup(p).get), new_body), b_3)
        }
    }
}
/**/

def all_traversals(term: Term) = {
    println(print(term))
    println("vars: " + vars(term))
    println("bound: " + bound(term))
    println("free: " + free(term))
    println("used: " + used(term))
    println("shadowing: " + shadowing(term))
    val (new_term, b) = resolve(term, root, 1)
    println("alpha-substituted: " + print(new_term))
    println
}

def all() = {
    val egs = List(eg1, eg2, eg3, eg4, eg5, eg6, eg7, eg8)
    egs.map(all_traversals)
}

