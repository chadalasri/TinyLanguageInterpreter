#! /usr/bin/env python3
import fileinput
import sys

#check cmd line args
if len(sys.argv) < 2:
    print("Please include a file to run")
    exit()

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
	def __init__(self,op1,operator,op2=None):
		self.op1 = op1
		self.operator = operator
		self.op2 = op2

	def __str__(self):
		if self.op2 == None:
			return self.operator + " " + self.op1
		else:
			return self.op1 + " " + self.operator + " " +  self.op2

    # evaluate this expression given the environment of the symTable
	def eval(self, symTable):
		if self.operator == "var":
			return symTable[op1]
		elif self.operator == "+":
			try:
				return float(self.op1) + float(self.op2)
			except:
				return self.op1 + self.op2
		elif self.operator == "-":
			try:
                                return float(self.op1) - float(self.op2)
			except:
				return self.op1 - self.op2
		elif self.operator == "*":
			try:
                                return float(self.op1) * float(self.op2)
			except:
                                return self.op1 * self.op2
		elif self.operator == "/":
			if self.op2 == 0:
				print("Dividing by 0 at line, ", linenum)
				sys.exit()
			try:
                                return float(self.op1) / float(self.op2)
			except:
                                return self.op1 / self.op2
		elif self.operator == "<":
			if self.op1 < self.op2:
				return 1
			else:
				return 0
		elif self.operator == ">":
			if self.op1 < self.op2:
				return 1
			else:
				return 0
		elif self.operator == "<=":
			if self.op1 <= self.op2:
				return 1
			else:
				return 0
		elif self.operator == ">=":
			if self.op1 >= self.op2:
				return 1
			else:
				return 0
		elif self.operator == "==":
			if self.op1 == self.op2:
				return 1
			else:
				return 0
		elif self.operator == "!=":
			if self.op1 != self.op2:
				return 1
			else:
				return 0
		else:
			return 0


# used to store a parsed TL statement
class Stmt :
	def __init__(self,keyword,exprs):
		self.keyword = keyword
		self.exprs = exprs

	def __str__(self):
		others = ""
		for exp in self.exprs:
			others = others + " " + str(exp)
			return self.keyword + others

    # perform/execute this statement given the environment of the symTable
	def perform(self, symTable):
		if self.keyword == 'let':
			handle_let(self)
		elif self.keyword == 'input':
			handle_input(self)
		elif self.keyword == 'print':
			handle_print(self)
		elif self.keyword == 'if':    
			handle_if(self)
		else:
			print("[SYNTAX ERROR]: Unknown Statement at line, ", linenum)
			sys.exit()
#takes a list, makes into expressions and evaluates them
def evaluate_expr(exprs):
	#check if it is in symtable, if not check if it is a number or a quoted string. If neither, its an undefined variable 
	if(len(exprs) == 1):
		try:
			return symtable[exprs[0]]
		except:
			if exprs[0].isnumeric():
				return float(exprs[0])
			elif '"' in exprs[0]:
				return exprs[0]
			else:
				print("Undefined Variable  ", end = " ")
				print(exprs[0], end = " ")
				print("at line, ", linenum)
				sys.exit()
	#expression evaluation		 
	else:
		try:	#checking for undefined variables
			return Expr(symtable[exprs[0]], exprs[1], symtable[exprs[2]]).eval(symtable)
		except:
			try:
				return Expr(exprs[0], exprs[1], symtable[exprs[2]]).eval(symtable)
			except:
				try:
					expression = Expr(symtable[exprs[0]], exprs[1], exprs[2])
					return expression.eval(symtable)
				except:
					try:
						return Expr(exprs[0], exprs[1], exprs[2]).eval(symtable)
					except:
						print("Unknown Expression at line, ", linenum)
						sys.exit()

def handle_input(line):
	if len(line) != 2:
		print("[Syntax error]: improper usage of 'input' at line, %d. Correct Usage: input variableName. " %(linenum+1))
		sys.exit()
	else:
	#inserting inputted variable
		prompt = line[1] + " = "
		try:
			var = float(input(prompt))
			symtable[line[1]] = var
			expression = Expr(line[1], "var")
		except:
			print("Illegal or Missing input.")
			sys.exit()

def handle_let(line):
	if len(line) < 4:
		print("[Syntax Error]: improper usage of 'let' at line, %d. Correct Usage: let variableName = expression." %(linenu))
	elif line[2] != '=':
		print("[Syntax Error]: missing '=' in 'let' at line, %d." %(linenum))
	else:
		#evaluate expression and enter into the symbol table
		x = evaluate_expr(line[3:])
		symtable[line[1]] = x

def handle_print(line):
	#check if correct number of arguments
	if len(line) < 2:
		print("[Syntax Error]: improper usage of 'print' at line, %d. Correct Usage: print expression1, expression2..." %(linenum))
	#if there are the correct num of arguments
	else:
		#this branch deals with if there is only one term
		if len(line) == 2:
			#variable, constant num or constant string
			try:
				val = symtable[line[1]]
				print(val)
			except:
				if line[1].isnumeric() or '"' in line[1]:
					print(line[1])
				else:
					print("Undefined variable ", end =" ")
					print(line[1], end = " ")
					print("at line, ", linenum)
					sys.exit()
		#this branch handles expressions
		else:	
			#prints out "quoted things", variables and numbers
			line.pop(0)
			for item in line:
				#print("item: ", line)
				#handles the printing of expressions
				if item[-1] == ',':
					item = item[:-1]
				
				if len(line) > 2:
					if line[1] == '+' or line[1] == '-' or line[1] == '*' or line[1] == '/' or line[1] == '>' or line[1] == '<' or line[1] == '==' or line[1] == '!=' or line[1] == '<=' or line[1] == '>=':
						item = line[2]
						if item[-1] == ',':
							line[2] = item[:-1]
						#print("line: ", line, end = " ")
						print(evaluate_expr(line[0:3]), end=" ")
						line.pop(0)
						line.pop(0)
						if len(line) > 2:
							line.pop(0)
					elif item[0] == '"' or item[-1] == '"':
						if item[0] == '"':
							print(item[1:], end = " ")
							line = line[1:]
						else:
							print(item, end = " ")
							line = line[1:]
					elif item.isnumeric():
						print(item, end = " ")
						line.pop(0)
					else:
						try:
							val = symtable[item]
							print(val, end = " ")
							#print("Line before:", item)
							line.pop(0)
						except:
							print("Undefined variable ", end = " ")
				#prints numbers
				elif item.isnumeric():
					print(item, end=" ")
				#print quoted stuff
				elif item[0] == '"':
					print(item[1:], end=" ")
				elif item[-1] == ',' or item[-1] == '"':
					if item[-1] == '"':
						item = item[:-1]
						print(item, end = " ")
					else:
						item = item[:-1]
						if item[-1] != '"':
							print("[Syntax Error]: improper usage of 'print' at line, %d. Correct Usage: print expression1, expression2..." %(linenum))
							sys.exit()
				#print variables otherwise (if it is a string and has no quotes, its an undefined variable)
				else:
					try:
						val = symtable[item]
						print(val, end=" ")
					except:
						if item == '+' or item == '-' or item == '*' or item == '/' or item == '>' or item == '<' or item == '==' or item == '!=' or item == '<=' or item == '>=':
							try:
								line.pop(0)
								line.pop(0)
							except:
								try:
									line.pop(0)
								except:
									print()
									sys.exit()
						else: 
							print("Undefined variable ", end = "")
							print(item, end = " ")
							print("at line, ", linenum)
							sys.exit()
			#end the printed line
			print()					

def handle_if(line, stmts):
	index = line.index('goto')
	val = evaluate_expr(line[1:index])
	try:
		num = symtable[line[-1]] - 1
	except:
		print("[ERROR]: Label not found at line, ", linenum)
		sys.exit()

	i=num
	while val == 1:
		templine = stmts[i]
		if templine[0] == 'input':
			handle_input(templine)
		elif templine[0] == 'let':
			handle_let(templine)
		elif templine[0] == 'print':
			handle_print(templine)
		else:
			index = templine.index('goto')
			val = evaluate_expr(templine[1:index])
			i = num -1 
		i+=1

#
#main

#open the file
file_obj = open(sys.argv[1])

#empty dictionary 
#in case of label - label: line number
#in case of variable - variable: value
symtable = {}
stmts = []
#loop through file
linenum = 1
#print("Line: ", len(file_obj))
for line in file_obj:
	#parsing
	line = line.strip().split(" ")
	stmts.append(line)
	#check if line has a label, if so insert in symtable and increment starting index for line
	if line[0].endswith(':'):
		tmp = line.pop(0)
		tmp = tmp[:-1]
		symtable[tmp] = linenum

	#checking for syntax (when statement has less than required terms)

	#input ---> done
	if line[0] == 'input':
		handle_input(line)
	#let ---> done
	elif line[0] == 'let':
		handle_let(line)
	#if goto
	elif line[0] == 'if':
		#less than this is a syntax error
		if len(line) < 4:  
			print("[Syntax Error]: improper usage of 'if' at line, %d. Correct Usage: if expression goto label." %(linenum))
		else:
			if not 'goto' in line:                      
				print("[Syntax Error]: improper usage of 'if' at line, %d. Correct Usage: if expression goto label." %(linenum))
			#based on length filter for expressions
			#call evaluate expression
			try:
				handle_if(line, stmts)
			except:
				print('[Syntax Error]: Undefined Variable at line, ', linenum)
				sys.exit()
	#print ---> done
	elif line[0] == 'print':
		handle_print(line)
	#anything else
	else:
		print('[Syntax error]: Unknown Statement at line, ', linenum)

	linenum += 1

#close file
file_obj.close()
