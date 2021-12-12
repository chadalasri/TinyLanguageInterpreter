//imports
import scala.collection.mutable.Map
import scala.io.StdIn.{readLine,readInt,readDouble}
import scala.math._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import java.io.FileNotFoundException
import scala.io.Source


//Given Code
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

//Given TLI object
object TLI {
	//added each binop expression
	def handle_input(sentence: Array[String], symtable: Map[String, Double]): Unit ={
		//checking for syntax errors
		if(sentence.length != 2){
                        throw new Exception("[SYNTAX ERROR]: improper usage of input, proper usage: input variableName")
                }
		//if none then proceed to prompt the user for a value
                else{
                        val prompt = sentence(1).concat(" = ")
                        print(prompt)
                        val result = readDouble()
			symtable += (sentence(1) -> result)
                }
	}

	//handling let
	def handle_let(sentence: Array[String], symtable: Map[String, Double]): Unit ={
		//Checking for syntax
		if(sentence.length < 4||sentence(2)!= "="){
			throw new Exception("[SYNTAX ERROR]: improper usage of let, proper usage: let variableName = expression")
                }
		//if syntax is good
                else{
			//if there is a single term in the expressions
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
			//assuming there is a binop expression
                        //since this is LET we will be inserting result into the symbol table using sentence(1) as key and eval(binop, symtable) as value
			else{
                        	// is binop two digits?
				if(sentence(3).forall(_.isDigit) && sentence(5).forall(_.isDigit)){
                                	val temp0 = new Constant(sentence(3).toDouble)
                                        val temp1 = new Constant(sentence(5).toDouble)
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
				// is first term in binop digit?
                                else if(sentence(3).forall(_.isDigit)){
                                        val temp0 = new Constant(sentence(3).toDouble)
                                        val temp1 = new Var(sentence(5))
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
				// is second term in binop digit?
                                else if(sentence(5).forall(_.isDigit)){
                                        val temp0 = new Var(sentence(3))
                                        val temp1 = new Constant(sentence(5).toDouble)
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
				//if not they both must be evaluated as variables
                                else{
                                        val temp0 = new Var(sentence(3))
                                        val temp1 = new Var(sentence(5))
                                        val temp = new BinOp(sentence(4), temp0, temp1)
                                        symtable += (sentence(1)->eval(temp, symtable))
                                }
                        }
		}
	}
	// Handle print, currently is glitchy with: Binops with commas at the end, AND Variables with commas at the end
	def handle_print(sentence: Array[String], symtable: Map[String, Double]): Unit ={
		//syntax checkin
		if(sentence.length < 2){
			throw new Exception("[ERROR]: Improper usage of Print. Proper usage: print expression1, expression2, ...")
		}
		//checking for openquotes to distinguish strings from variables
		var openquotes = false
		var index = -1
		//for each word in each expression
		for(word <- sentence){
			index += 1
		        //do nothing if this is the case (openquotes = false im using as a null expression)
			if(word == "print" && openquotes == false){
				openquotes = false
		        }
			//if it has a quote at the beginning print it
		        else if(word.charAt(0) == '"'){
		                openquotes = true
		                print(word.substring(1) + " ")
		        }
			//if it has open quotes or a comma filter it through here
		        else if(openquotes == true || word.charAt(word.length-1) == ','){
		        	//words with comma at the end go through here
				if(word.charAt(word.length-1) == ','){
		                	//checking for if this is the end of quotes
					//if the word has 2 or more characters
					if(word.length >= 2){
		                        	if(word.charAt(word.length - 2) == '"'){
		                                	openquotes = false
		                                        print(word.substring(0, word.length - 2) + " ")
		                                }
						//if there are no quotes but there is a comma, here is where i need to add code to deal with vars and binops
		                                else{
		                                        print(word + " ")
		                                }
		                        }
					//if its a short word, its either a digit or a variable
		                        else{
		                                //digit evaluation
						if(word.forall(_.isDigit)){
		                                        val temp = new Constant(word.toDouble)
		                                        print(eval(temp, symtable))
		                                }
						//variable evaluation
		                                else if(openquotes == false){
		                                        val temp = new Var(word.substring(0, word.length-1))
		                                        print(eval(temp, symtable))
		                                }
						//else statement to just catch and print any other possibilities
						else{
		                                        print(word + " ")
		                                }
		                        }
				}
				//if the last character is a quote, close the quotes
				else if(word.charAt(word.length - 1) == '"'){
					openquotes = false
					print(word.substring(0, word.length-1) + " ")
				}
				// if its just a digit or a variable or anything else really this else chunk might be unnecessary
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
			//if it has no quotes or commas, then it is nearing the end
		        else{
				//if there is only one term left
				if(sentence.length - index == 1){
					//check if that term is a digit
					if(word.forall(_.isDigit)){
						//checking for this so the next if doesn't give an index out of bounds error
						if(index > 1){
							//this filters out the binop operator if there is one
							if(sentence(index - 1) == "+" || sentence(index - 1) == "-" ||sentence(index - 1) == "*" || sentence(index - 1) == "/" || sentence(index - 1) == "==" || sentence(index - 1) == "!=" || sentence(index - 1) == "<" || sentence(index - 1) == ">" || sentence(index - 1) == ">=" || sentence(index - 1) == "<="){
								openquotes = false
							}
							//if it isnt a binop operator then do this. honestly this else also might be unnecessary
							else{
								val temp = new Constant(word.toDouble)
                                               			print(eval(temp, symtable))
							}
						}
						//if it is not greater than 1, do this. This also might be unnecessary
						else{
							val temp = new Constant(word.toDouble)
							print(eval(temp, symtable))
						}
					}
					//else its a variable
					else{
						val temp = new Var(word)
						print(eval(temp, symtable))
					}
		       		}
				//last else
				else{
					//if there are more terms left
					if(sentence.length - index > 2){
						//and if one of them is an operator
						if(sentence(index + 1) == "+" || sentence(index + 1) == "-" ||sentence(index + 1) == "*" || sentence(index + 1) == "/" || sentence(index + 1) == "==" || sentence(index + 1) == "!=" || sentence(index + 1) == "<" || sentence(index + 1) == ">" || sentence(index + 1) == ">=" || sentence(index + 1) == "<="){
							//this is where the binop gets evaluated
							if(sentence(index).forall(_.isDigit) && sentence(index + 2).forall(_.isDigit)){
                                        			val temp0 = new Constant(sentence(index).toDouble)
                                        			val temp1 = new Constant(sentence(index+2).toDouble)
                                       				val temp = new BinOp(sentence(index+1), temp0, temp1)
								print(eval(temp, symtable))
                                			}
							//same steps as in let
                                			else if(sentence(index).forall(_.isDigit)){
                                			        val temp0 = new Constant(sentence(index).toDouble)
                                			        val temp1 = new Var(sentence(index+2))
                                			        val temp = new BinOp(sentence(index+1), temp0, temp1)
								print(eval(temp, symtable))
                                			}
							//make a binop
                                			else if(sentence(5).forall(_.isDigit)){
                                			        val temp0 = new Var(sentence(index))
                                			        val temp1 = new Constant(sentence(index+2).toDouble)
                                			        val temp = new BinOp(sentence(index+1), temp0, temp1)
								print(eval(temp, symtable))
                               				}
							//but instead of inserting it into the symtable, print it
                                			else{
                                        			val temp0 = new Var(sentence(index))
                                        			val temp1 = new Var(sentence(index+2))
                                        			val temp = new BinOp(sentence(index+1), temp0, temp1)
                                				print(eval(temp, symtable))
							}
													
						}
					}
					//more filtering out of the operators, this also might be unnecessary. 
					//noticing that some of these elses overlap or just are unnecessary. Should go back and fix
					else{
						if(sentence(index) == "+" || sentence(index) == "-" ||sentence(index) == "*" || sentence(index) == "/" || sentence(index) == "==" || sentence(index) == "!=" || sentence(index) == "<" || sentence(index) == ">" || sentence(index) == ">=" || sentence(index) == "<="){
							openquotes = false
						}
					}
				}
			}
		}	
	}
	//handing if
	def handle_if(sentence: Array[String], symtable: Map[String, Double], lines: ArrayBuffer[String], expr: Expr): Unit ={
		//made a simple loop related to the index to easily iterate through the given list
		var index = 0
		while(index < lines.length){
			//was hard to passed in an already parsed list, so will parse here
			var temp = lines(index).replaceAll("\\s+$", "").split(" ")	
			if(temp(0).charAt(temp(0).length - 1) == ':'){
				temp = temp.drop(1)
			}
			//same as main, look at each input and pass it to the appropriate function
			if(temp(0) == "input"){
                	        handle_input(temp, symtable)
                        }
                        else if(temp(0) == "let") {
                                handle_let(temp, symtable)
                        }
                        else if(temp(0) == "if"){
                        	
				if(eval(expr, symtable) == 1){
					index = -1
				}
			}
                        else if(temp(0) == "print"){
                                handle_print(temp, symtable)
                                println()
                        }
                        else{
                        	println("UNKNOWN STATMENT")
                        }
			//temp.foreach(println)
			index += 1
		}
	}
	//given code, inserted different operators and their evaluations. Comparisons, true == 1, false == 0
	def eval(expr: Expr, symTab: Map[String, Double]): Double = expr match {
		case BinOp("+",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
		case BinOp("-",e1,e2) => eval(e1,symTab) - eval(e2,symTab) 
		case BinOp("*",e1,e2) => eval(e1,symTab) * eval(e2,symTab) 
		case BinOp("/",e1,e2) => if(eval(e2, symTab) == 0){throw new ArithmeticException("[ERROR]: Division by 0")}else{eval(e1,symTab) / eval(e2,symTab)} 
		case BinOp("==",e1,e2) => if(eval(e1,symTab) == eval(e2,symTab)){1.0}else{0.0} 
		case BinOp("!=",e1,e2) => if(eval(e1,symTab) != eval(e2,symTab)){1.0}else{0.0} 
		case BinOp(">",e1,e2) => if(eval(e1,symTab) > eval(e2,symTab)){1.0}else{0.0}
		case BinOp("<",e1,e2) => if(eval(e1,symTab) < eval(e2,symTab)){1.0}else{0.0} 
		case BinOp("<=",e1,e2) => if(eval(e1,symTab) <= eval(e2,symTab)){1.0}else{0.0} 
		case BinOp(">=",e1,e2) => if(eval(e1,symTab) >= eval(e2,symTab)){1.0}else{0.0}
		case Var(name) => symTab(name)
		case Constant(num) => num
		case _ => throw new Exception("[Error]: Unknown Expression.") // should really throw an error
		
	}
	//main of tli object
	def main(args: Array[String]) : Unit= {
		//syntax for the function call
		if (args.length != 1)
			throw new Exception("[ERROR]: Correct Usage - scala TLI filename.")
		var linenum = 0
		//everything in a try catch block, i like this part of scala
		try{
			//getting lines from the file
			val lines = Source.fromFile(args(0)).getLines 
			var symtable:Map[String, Double] = Map()
			var listOfStatements:ArrayBuffer[String] = ArrayBuffer()
			
			//iterating through the lines
			for(line <- lines){
				linenum += 1
				//parsing the lines
				var sentence = line.replaceAll("^\\s+", "").replaceAll("\\s+$", "").split(" ")
				var tempstring = ""
				//making a tempstring to make adding to listOfStatements easier
				for(item <- sentence){
					tempstring += item + " "
				}
				//makes a sentence out of items and puts them in list of statements
				listOfStatements += tempstring 
				//checking for labels
				if(sentence(0).charAt(sentence(0).length()-1) == ':'){
					val label = sentence(0).dropRight(1)
					symtable += (label -> linenum)
					sentence = sentence.drop(1)
				}
				
				//checks for each of the statements, by evaluating the first word and passing it to its appropriate function
				if(sentence(0) == "input"){
					handle_input(sentence, symtable)
				}
				else if(sentence(0) == "let") {
					handle_let(sentence, symtable)
				}
				else if(sentence(0) == "if"){
					//here it filters out the binop expression to pass it to the handle_if function to make its job easier
					//this if specifically is checking for syntax
					if(sentence.contains("goto") && sentence.length > 3){
						if(sentence(1).forall(_.isDigit) && sentence(3).forall(_.isDigit)){
                                        		val temp0 = new Constant(sentence(1).toDouble)
                                        		val temp1 = new Constant(sentence(3).toDouble)
                                        		val temp = new BinOp(sentence(2), temp0, temp1)
                                        		val evalu = eval(temp, symtable)
                               				if(evalu == 1){
                                                        	handle_if(sentence, symtable, listOfStatements.drop(symtable(sentence(sentence.length - 1)).toInt-1), temp)
                                                	}
						}
                                		else if(sentence(1).forall(_.isDigit)){
                                        		val temp0 = new Constant(sentence(1).toDouble)
                                        		val temp1 = new Var(sentence(3))
                                        		val temp = new BinOp(sentence(2), temp0, temp1)
                                        		val evalu = eval(temp, symtable)
                                			if(evalu == 1){
                                                        	handle_if(sentence, symtable, listOfStatements.drop(symtable(sentence(sentence.length - 1)).toInt-1), temp)
                                                	}
						}
                                		else if(sentence(3).forall(_.isDigit)){
                                        		val temp0 = new Var(sentence(1))
                                        		val temp1 = new Constant(sentence(3).toDouble)
                                        		val temp = new BinOp(sentence(2), temp0, temp1)
                                        		val evalu = eval(temp, symtable)
                                			if(evalu == 1){
                                                        	handle_if(sentence, symtable, listOfStatements.drop(symtable(sentence(sentence.length - 1)).toInt-1), temp)
                                                	}
						}
                                		else{
                                        		val temp0 = new Var(sentence(1))
                                        		val temp1 = new Var(sentence(3))
                                        		val temp = new BinOp(sentence(2), temp0, temp1)
                                        		val evalu = eval(temp, symtable)
							if(evalu == 1){
                                                        	handle_if(sentence, symtable, listOfStatements.drop(symtable(sentence(sentence.length - 1)).toInt-1), temp)
                                                	}
                                		}
						
					}
					//if not the above branch syntax is wrong
					else{
						throw new Exception("[SYNTAX ERROR]: Improper usage of if. Proper usage: if expression goto label.")
					}
				}
				//pass to print
				else if(sentence(0) == "print"){
					handle_print(sentence, symtable)
					println()
				}
				//If there still is something that means its an unknown statement
				else{
					throw new Exception("[SYNTAX ERROR]: UNKNOWN STATMENT")
				}
			}
		}
		//only needed to catch a few exceptions
		catch{
			case x: FileNotFoundException => throw new FileNotFoundException("[ERROR]: File not found.")
			case x: NumberFormatException => throw new NumberFormatException("[ERROR]: Must enter a number.")
			case x: NoSuchElementException => throw new NoSuchElementException("[ERROR]: Undefined Variable or Label at line, " + linenum) 
		}
	
	}
}//end
