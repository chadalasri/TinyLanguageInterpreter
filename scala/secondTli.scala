import scala.collection.mutable.Map
import scala.io.StdIn.{readLine,readInt,readDouble}
import scala.math._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import java.io.FileNotFoundException
import scala.io.Source

abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: List[Expr]) extends Stmt

object TLI {
	//added each binop expression
	def handle_input(sentence: Array[String], symtable: Map[String, Double]): Unit ={
		if(sentence.length != 2){
                        throw new Exception("[SYNTAX ERROR]: improper usage of input, proper usage: input variableName")
                }
                else{
                        val prompt = sentence(1).concat(" = ")
                        print(prompt)
                        val result = readDouble()
			symtable += (sentence(1) -> result)
                }
	}
	def handle_let(sentence: Array[String], symtable: Map[String, Double]): Unit ={
		if(sentence.length < 4||sentence(2)!= "="){
			throw new Exception("[SYNTAX ERROR]: improper usage of let, proper usage: let variableName = expression")
                }
                else{
			if(sentence.length == 4){
                        	if(sentence(3).forall(_.isDigit)){
                                	val temp = new Constant(sentence(3).toDouble)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
                                else{
                                        val temp = new Var(sentence(3))
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
                        }
                        else{
                        	if(sentence(3).forall(_.isDigit) && sentence(5).forall(_.isDigit)){
                                	val temp0 = new Constant(sentence(3).toDouble)
                                        val temp1 = new Constant(sentence(5).toDouble)
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
                                else if(sentence(3).forall(_.isDigit)){
                                        val temp0 = new Constant(sentence(3).toDouble)
                                        val temp1 = new Var(sentence(5))
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
                                else if(sentence(5).forall(_.isDigit)){
                                        val temp0 = new Var(sentence(3))
                                        val temp1 = new Constant(sentence(5).toDouble)
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
                                else{
                                        val temp0 = new Var(sentence(3))
                                        val temp1 = new Var(sentence(5))
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
                        }
		}
	}

	def handle_print(sentence: Array[String], symtable: Map[String, Double]): Unit ={
		if(sentence.length < 2){
			throw new Exception("[ERROR]: Improper usage of Print. Proper usage: print expression1, expression2, ...")
		}
		var openquotes = false
		var index = -1
		for(word <- sentence){
			index += 1
		        if(word == "print" && openquotes == false){
				openquotes = false
		        }
		        else if(word.charAt(0) == '"'){
		                openquotes = true
		                print(word.substring(1) + " ")
		        }
		        else if(openquotes == true || word.charAt(word.length-1) == ','){
		        	if(word.charAt(word.length-1) == ','){
		                	if(word.length >= 2){
		                        	if(word.charAt(word.length - 2) == '"'){
		                                	openquotes = false
		                                        print(word.substring(0, word.length - 2) + " ")
		                                }
		                                else{
		                                        print(word + " ")
		                                }
		                        }
		                        else{
		                                if(word.forall(_.isDigit)){
		                                        val temp = new Constant(word.toDouble)
		                                        print(eval(temp, symtable))
		                                }
		                                else if(openquotes == false){
		                                        val temp = new Var(word.substring(0, word.length-1))
		                                        print(eval(temp, symtable))
		                                }
						else{
		                                        print(word + " ")
		                                }
		                        }
				}
				else if(word.charAt(word.length - 1) == '"'){
					openquotes = false
					print(word.substring(0, word.length-1) + " ")
				}
				else{
					if(word.forall(_.isDigit)){
						val temp = new Constant(word.toDouble)
						print(eval(temp, symtable))
					}
					else{
						val temp = new Var(word)
						print(eval(temp, symtable))
					}
				}
			}
		        else{
				if(sentence.length - index == 1){
					if(word.forall(_.isDigit)){
						val temp = new Constant(word.toDouble)
						print(eval(temp, symtable))
					}
					else{
						val temp = new Var(word)
						print(eval(temp, symtable))
					}
		       		}
				else{
					if(sentence.length - index > 2){
						if(sentence(index + 1) == "+" || sentence(index + 1) == "-" ||sentence(index + 1) == "*" || sentence(index + 1) == "/" || sentence(index + 1) == "==" || sentence(index + 1) == "!=" || sentence(index + 1) == "<" || sentence(index + 1) == ">" || sentence(index + 1) == ">=" || sentence(index + 1) == "<="){
							if(sentence(index).forall(_.isDigit) && sentence(index + 2).forall(_.isDigit)){
                                        			val temp0 = new Constant(sentence(index).toDouble)
                                        			val temp1 = new Constant(sentence(index+2).toDouble)
                                       				val temp = new BinOp(sentence(index+1), temp0, temp1)
								println(eval(temp, symtable))
                                			}
                                			else if(sentence(index).forall(_.isDigit)){
                                			        val temp0 = new Constant(sentence(index).toDouble)
                                			        val temp1 = new Var(sentence(index+2))
                                			        val temp = new BinOp(sentence(index+1), temp0, temp1)
								println(eval(temp, symtable))
                                			}
                                			else if(sentence(5).forall(_.isDigit)){
                                			        val temp0 = new Var(sentence(index))
                                			        val temp1 = new Constant(sentence(index+2).toDouble)
                                			        val temp = new BinOp(sentence(index+1), temp0, temp1)
								println(eval(temp, symtable))
                               				}
                                			else{
                                        			val temp0 = new Var(sentence(index))
                                        			val temp1 = new Var(sentence(index+2))
                                        			val temp = new BinOp(sentence(index+1), temp0, temp1)
                                				print(eval(temp, symtable))
							}
													
						}
					}
					else{
						println("herex")
					}
				}
			}
		}	
	}
	def eval(expr: Expr, symTab: Map[String, Double]): Double = expr match {
		case BinOp("+",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case BinOp("-",e1,e2) => eval(e1,symTab) - eval(e2,symTab) 
		case BinOp("*",e1,e2) => eval(e1,symTab) * eval(e2,symTab) 
		case BinOp("/",e1,e2) => if(eval(e2, symTab) == 0){throw new ArithmeticException("[ERROR]: Division by 0")}else{eval(e1,symTab) / eval(e2,symTab)} 
		case BinOp("==",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case BinOp("!=",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case BinOp(">",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case BinOp("<",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case BinOp("<=",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case BinOp(">=",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case Var(name) => symTab(name)
		case Constant(num) => num
		case _ => throw new Exception("[Error]: Unknown Expression.") // should really throw an error
		
	}

	def main(args: Array[String]) : Unit= {
		if (args.length != 1)
			throw new Exception("[ERROR]: Correct Usage - scala TLI filename.")
		var linenum = 0
		try{
			val lines = Source.fromFile(args(0)).getLines 
			var symtable:Map[String, Double] = Map()
			for(line <- lines){
				linenum += 1
				var sentence = line.replaceAll("^\\s+", "").replaceAll("\\s+$", "").split(" ")
				
				if(sentence(0).charAt(sentence(0).length()-1) == ':'){
					val label = sentence(0).dropRight(1)
					symtable += (label -> linenum)
					sentence = sentence.drop(1)
				}

				if(sentence(0) == "input"){
					handle_input(sentence, symtable)
				}
				else if(sentence(0) == "let") {
					handle_let(sentence, symtable)
				}
				else if(sentence(0) == "if"){
					println("IF")	
				}
				else if(sentence(0) == "print"){
					handle_print(sentence, symtable)
					println()
				}
				else{
					println("UNKNOWN STATMENT")
				}
			}
		}
		catch{
			case x: FileNotFoundException => throw new FileNotFoundException("[ERROR]: File not found.")
			case x: NumberFormatException => throw new NumberFormatException("[ERROR]: Must enter a number.")
			case x: NoSuchElementException => throw new NoSuchElementException("[ERROR]: Undefined Variable at line, " + linenum) 
		}
	
	}
}
