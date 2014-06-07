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
// (+ x y z) and \x y z -> z (x y)
case class As(op: Term, args: List[Term]) extends Term
case class Fs(params: List[String], body: Term) extends Term {
    val ps = params.toSet
    if ( params.size > ps.size ) throw new Exception("duplicate symbol in Fs constructor")
}

val eg1 = F("z", A(S("xyz"), F("q", S("abc"))))
val eg2 = F("a", F("b", F("a", A(S("c"), F("b", S("a"))))))
val eg3 = Fs(List("a", "b", "c"), A(eg1, eg2))

def print(term: Term): String = {
    term match {
        case A(a,b)   => "(" + print(a) + " " + print(b) + ")"
        case S(s)     => s
        case F(o,a)   => "\\" + o + "." + print(a)
        case As(o,as) => "(" + print(o) + as.map(print).mkString(" ") + ")"
        case Fs(ps,b) => "\\" + ps.mkString(" ") + " " + print(b)
    }
}

object Reducer {

    def vars(term: Term): List[String] = {
        term match {
            case A(f,a) => vars(f) ++ vars(a)
            case S(s)   => List(s)
            case F(p,b) => List(p) ++ vars(b)
        }
    }
    
    def bound(term: Term): List[String] = {
        term match {
            case A(f,a) => bound(f) ++ bound(a)
            case S(s)   => List()
            case F(p,b) => List(p) ++ bound(b)
        }
    }

    def used(term: Term): List[String] = {
        term match {
            case A(f,a) => used(f) ++ used(a)
            case S(s)   => List(s)
            case F(p,b) => List() ++ used(b)
        }
    }
    /*
    def lookup(var: String, vars: List[String])
    
    def scopes_help(term: Term, vars: List[String]) = {
        term match {
            case A(f,a) => 
            case S(s)   => 
            case F(p,b) => 
        }
    }

    def scopes(term: Term): ??? = {
        scopes_help(term, List())
    }
    */
    def shadowing_help(term: Term, vars: List[String]): List[(String, List[String])] = {
        term match {
            case A(f,a) => shadowing_help(f, vars) ++ shadowing_help(a, vars)
            case S(s)   => List(("symbol: " + s, vars))
            case F(p,b) => List((if (vars.contains(p)) ("shadowing: " + p) else ("not shadowing: " + p), vars)) ++ shadowing_help(b, p :: vars)
        }
    }
    
    def shadowing(term: Term) = {
        shadowing_help(term, List())
    }
    
    def all(term: Term) = {
        println(print(term))
        println("vars: " + vars(term))
        println("bound: " + bound(term))
        println("used: " + used(term))
        println("shadowing: " + shadowing(term))
    }
}

