// package com.mycode


// things to calculate:
//   1. set of all variables
//   2. count of use of all variables
//   3. resolve all variables (maybe generate a guaranteed unique name)
//   4. find set of free variables
//   5. find set of bound variables (plus where they're bound)

sealed trait Term { // trait vs. abstract class?
// why can't I define a method in here?
//    def print(): String = {
//        this match {
//            case B() => ""
//            case A(op, arg) => "a" + op + "b" + arg
//            case F(a,b) => ""
//            case S(x) => x
//        }
//    }
}

case class A(op: Term, arg: Term) extends Term
case class F(param: String, body: Term) extends Term
case class S(name: String) extends Term
// here's what it could look like with ordinary classes: note the use of `val`
// class A(val op: String, val arg: String)

val eg1 = F("z", A(S("xyz"), F("q", S("abc"))))

object Print {
    def print(term: Term): String = {
        term match {
            case A(a,b) => "(" + print(a) + " " + print(b) + ")"
            case S(s) => s
            case F(o,a) => "\\" + o + "." + print(a)
        }
    }
}

object Reducer {
    def vars(term: Term) = println(Print.print(term))
//        val q = Print.print(term)
        //println(Print.print(term))
//    }
}

