package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        //TODO: Handle a line object
        case Line(l) => {
            val v = evalExpr(l, env)
            v match {
                case NumValue(d) => {
                    val poly = new Polygon(List((0.0,0.0), (d,0.0)))
                    val canv = new MyCanvas(List(poly))
                    FigValue(canv)
                }
                case _ => throw new IllegalArgumentException("Line did not produce NumValue")
            }
        }
        // TODO: Handle Equilateral Triangle
        case EquiTriangle(sideLength) => {
            val v = evalExpr(sideLength, env)
            v match {
                case NumValue(d) =>  {
                    val poly = new Polygon(List((0,0), (0,d), (d*0.5, math.sqrt(3*d)*0.5)))
                    val canv = new MyCanvas(List(poly))
                    FigValue(canv)
                }
                case _ => throw new IllegalArgumentException("EquiTriangle did not produce NumValue")
            }
           
        }
        // TODO: Handle square given the side length
        case Rectangle(sideLength) => {
            val v = evalExpr(sideLength, env)
            v match {
                case NumValue(d) => {
                    val poly = new Polygon(List((0,0), (0,d), (d,d), (d,0)))
                    val canv = new MyCanvas(List(poly))
                    FigValue(canv)
                }
                case _ => throw new IllegalArgumentException("Rectangle did not produce NumValue")
            }
        }
        //TODO: Handle circle
        case Circle(rad) => {
            val v = evalExpr(rad, env)
            v match {
                case NumValue(d) => {
                    val cir = new MyCircle((d,d), d) 
                    val canv = new MyCanvas(List(cir))
                    FigValue(canv)
                }
                case _ => throw new IllegalArgumentException("Circle did not produce NumValue")
            }
        }
        // TODO: Handle addition of numbers or figures
        case Plus (e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match {
                case (NumValue(f1), NumValue(f2)) => NumValue(f1 + f2)
                case (FigValue(f1), FigValue(f2)) => FigValue(f1 overlap f2)
                case _ => throw new IllegalArgumentException("Plus did not produce NumValue or FigValue")
            }
        }
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        // TODO: Handle multiplication of numbers or figures
        case Mult(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match {
                case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
                case (FigValue(f1), FigValue(f2)) => FigValue(f1 placeRight f2)
                case _ => throw new IllegalArgumentException("Minus did not produce NumValue or FigValue")
            }
        }
        // TODO: Handle division
        case Div(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match {
                case (NumValue(f1), NumValue(f2)) => NumValue(f1 / f2)
                case (FigValue(f1), FigValue(f2)) => FigValue(f1 placeTop f2)
                case (FigValue(f1), NumValue(f2)) => FigValue(f1 rotate f2) 
                case _ => throw new IllegalArgumentException("Division did not produce NumValue or FigValue")
            }
        }
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        //TODO: Handle function definitions
        case FunDef(x, e) => {
            Closure(x, e, env) 
        }
        // TODO: Handle recursive functions -- look at Environment.scala
        case LetRec(f, x, e1, e2) => {
            val newEnv = ExtendREC(f, x, e1, env)
            evalExpr(e2, newEnv)
        }
        // TODO: Handle function calls
        case FunCall(fCallExpr, arg) => {
            val v1 = evalExpr(fCallExpr, env)
            val v2 = evalExpr(arg, env)
            v1 match {
                case Closure(x, closureEx, closedEnv) => {
                    val newEnv = Extend(x, v2, closedEnv)
                    evalExpr(closureEx, newEnv)
                }
                case _ => throw new IllegalArgumentException(s"Expression does not evaluate to closure")
            }
        }
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
