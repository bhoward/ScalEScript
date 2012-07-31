package compilerV0

//Note: Because of this import all "Map"s will be mutable by default
import scala.collection.mutable.Map;

object ASTConverter {
	def apply(source: Stmt): AnnotStmt = {
		convert(source)
	}
	
	/* Convert functions */
	def convert(ast: Stmt): AnnotStmt = {
		var scopes: List[Scope] = Scope()::ScalaBase.getScope()::Nil;
		
		return convertStmt(ast, scopes);
	}
	def convertStmt(stmt: Stmt, scopes: List[Scope]): AnnotStmt = stmt match {
		case ValDefStmt(listOfValNames, valType, expr, valTypeFlag) => convertValDefStmt(listOfValNames, valType, expr, valTypeFlag, scopes);
    	case ParamDclStmt(id, varType) => convertParamDclStmt(id, varType, scopes);
    	case FunDefStmt(name, params, retType, body) => convertFunDefStmt(name, params, retType, body, scopes);
    	case ClassDefStmt(typeFlag, className, params, extendClass, withIds, body) => convertClassDefStmt(typeFlag, className, params, extendClass, withIds, body, scopes);
    	case _ => return convertExpr(stmt, scopes);
	}
	def convertExpr(stmt: Stmt, scopes: List[Scope]): AnnotExpr = stmt match {
		case BlockExpr(listofstatements) => convertBlockExpr(listofstatements, scopes);
	  	case BinOpExpr(op, l, r) => convertBinOpExpr(op, l, r, scopes);
	  	case IfThenExpr(predicate, expr) => convertIfThenExpr(predicate, expr, scopes);
  	    case IfThenElseExpr(predicate, trueValue, falseValue) => convertIfThenElseExpr(predicate, trueValue, falseValue, scopes);
  	    case WhileExpr(predicate, body, doFlag) => convertWhileExpr(predicate, body, doFlag, scopes);
	  	case VarExpr(varName) => convertVarExpr(varName, scopes);
    	case FunExpr(id, args) => convertFunExpr(id, args, scopes);
    	case StringExpr(value) => convertStringExpr(value, scopes);
    	case CharExpr(value) => convertCharExpr(value, scopes);
    	case NumExpr(value) => convertNumExpr(value, scopes);
    	case BoolExpr(value) => convertBoolExpr(value, scopes);
    	case AnonFuncExpr(args, body) => convertAnonFuncExpr(args, body, scopes);
    	case AssignExpr(lhs, rhs) => convertAssignExpr(lhs, rhs, scopes);
    	case ClassExpr(name, args) => convertClassExpr(name, args, scopes);
    	case FieldSelectionExpr(obj, field) => convertFieldSelectionExpr(obj, field, scopes);
		case _ => throw new Exception("Unknown expr: "+stmt);
	}
	def convertValDefStmt(ids: List[String], valType: Type, value: Expr, valTypeFlag: String, scopes: List[Scope]): AnnotValDefStmt = {	
		//Add the vals to the scope
		putAllVars(scopes, ids, valType); // TODO handle valTypeFlag (lazy/val/var)
		
		var annotValue: AnnotExpr = convertExpr(value, scopes);
		return AnnotValDefStmt(ids, valType, annotValue, valTypeFlag);
	}
	def convertParamDclStmt(id: String, varType: Type, scopes: List[Scope]): AnnotParamDclStmt = {
		//Add the vals to the scope
		putAllVars(scopes, List(id), varType);

		return AnnotParamDclStmt(id, varType);
	}
	def convertFunDefStmt(name: String, paramClauses: List[List[ParamDclStmt]], retType: Type, body: Expr, scopes: List[Scope]): AnnotFunDefStmt = {
		//To start with, check to see if the function needs to be curried
		if(paramClauses.length > 1){
			var newFunc: FunDefStmt = curryFunc(name, paramClauses, retType, body);
			
			//Completely ignore the old function. Just return the converted form of the curried one
			return convertFunDefStmt(newFunc.name, newFunc.paramClauses, newFunc.retType, newFunc.body, scopes);
		} else {
			//From this point on you can assume there is only one param clause (it would have been curried otherwise)
			var params: List[ParamDclStmt] = paramClauses.head;
		  
			var newScope: Scope = Scope();
			
		    var annotParams: List[AnnotParamDclStmt] = params.map(param => convertParamDclStmt(param.id, param.varType, newScope::scopes));
		    var annotBody: AnnotExpr = convertExpr(body, newScope::scopes);
		    
		    //Add the function to the scope
		    if (paramClauses.length > 0) {
				var paramTypes: List[Type] = params.map(param => param.varType)
				putFunc(scopes, name, paramTypes, retType);
			} else {
				putFunc(scopes, name, Nil, retType);
			}
		    return AnnotFunDefStmt(name, annotParams, retType, annotBody, newScope)
		}
	}
	def convertClassDefStmt(typeFlag: String, className: String, params: List[ParamDclStmt], extendClass: ClassInstance, withIds: List[Type], body: List[Stmt], scopes: List[Scope]): AnnotClassDefStmt = {
		var newScope: Scope = Scope();

		//Strip the with Ids out of the Types (this may need to be changed later to handle more complex types)
		var withIdStrings: List[String] = withIds.map(idType => idType.getType());
		
		var annotParams: List[AnnotParamDclStmt] = params.map(param => convertParamDclStmt(param.id, param.varType, newScope::scopes));
		var paramTypes: List[Type] = annotParams.map(param => param.varType);
		var annotBody: List[AnnotStmt] = body.map(stmt => convertStmt(stmt, newScope::scopes));

		var extendsClassName: String = "";
		if (extendClass != null) {
			extendsClassName = extendClass.id.getType();
		}
		var annotExtendsArgs: List[AnnotExpr] = extendClass.args.map(arg => convertExpr(arg, newScope::scopes));
		
		var withClassNames: List[String] = withIds.map(idType => idType.getType());
		
		//The initialization for this is slightly different for objects - it gets intialized in the match
		var symbolTable: ClassScope = null;
		
		//Add the class to the scope (or Object to the object scope along with its "class")
		typeFlag match {
		  	case "class" => {
		  		symbolTable = buildClassScope(scopes, className, extendsClassName, withClassNames, paramTypes, newScope);
		  		putClass(scopes, className, symbolTable);
		  	}
		  	case "case class" => {
		  		//TODO add case class stuff to symbolTable
		  		symbolTable = buildClassScope(scopes, className, extendsClassName, withClassNames, paramTypes, newScope);
		  		putClass(scopes, className, symbolTable);
		  	}
		  	case "trait" => {
		  		symbolTable = buildClassScope(scopes, className, extendsClassName, withClassNames, paramTypes, newScope);
		  		putClass(scopes, className, symbolTable);
		  	}
		  	case "object" => {
		  		symbolTable = buildObjectScope(scopes, className, extendsClassName, withClassNames, paramTypes, newScope);
		  		putObject(scopes, className, symbolTable);
		  	}
		  	case "case object" => {
		  		//TODO add case class stuff to symbolTable
		  		symbolTable = buildObjectScope(scopes, className, extendsClassName, withClassNames, paramTypes, newScope);
		  		putObject(scopes, className, symbolTable);
		  	}
		  	case _ => {throw new Exception("Unknown class typeFlag: "+typeFlag)}
		}
		return AnnotClassDefStmt(typeFlag, className, annotParams, extendsClassName, annotExtendsArgs, withIdStrings, annotBody, symbolTable)
	}
	def convertBlockExpr(stmts: List[Stmt], scopes: List[Scope]): AnnotBlockExpr = {
		var newScope: Scope = Scope();
		var annotStmts: List[AnnotStmt] = stmts.map(stmt => convertStmt(stmt, newScope::scopes));
		return AnnotBlockExpr(annotStmts, newScope);
	}
	def convertBinOpExpr(op: String, l: Expr, r: Expr, scopes: List[Scope]): AnnotBinOpExpr = {
		var annotL: AnnotExpr = convertExpr(l, scopes);
		var annotR: AnnotExpr = convertExpr(r, scopes);
		return AnnotBinOpExpr(op, annotL, annotR);
	}
	def convertIfThenExpr(predicate: Expr, expr: Expr, scopes: List[Scope]): AnnotIfThenExpr = {
		var annotPredicate: AnnotExpr = convertExpr(predicate, scopes);
		var annotExpr: AnnotExpr = convertExpr(expr, scopes);
		return AnnotIfThenExpr(annotPredicate, annotExpr);
	}
	def convertIfThenElseExpr(predicate: Expr, trueValue: Expr, falseValue: Expr, scopes: List[Scope]): AnnotIfThenElseExpr = {
		var annotPredicate: AnnotExpr = convertExpr(predicate, scopes);
		var annotTrue: AnnotExpr = convertExpr(trueValue, scopes);
		var annotFalse: AnnotExpr = convertExpr(falseValue, scopes);
		return AnnotIfThenElseExpr(annotPredicate, annotTrue, annotFalse);
	}
	def convertWhileExpr(predicate: Expr, body: Expr, doFlag: Boolean, scopes: List[Scope]): AnnotWhileExpr = {
		var annotPredicate: AnnotExpr = convertExpr(predicate, scopes);
		var annotBody: AnnotExpr = convertExpr(body, scopes);
		return AnnotWhileExpr(annotPredicate, annotBody, doFlag);
	}
	def convertVarExpr(varName: String, scopes: List[Scope]): AnnotVarExpr = {
		return AnnotVarExpr(varName);
	}
	def convertFunExpr(id: Expr, args: List[Expr], scopes: List[Scope]): AnnotFunExpr = {
		var annotId: AnnotExpr = convertExpr(id, scopes);
		var annotArgs: List[AnnotExpr] = args.map(arg => convertExpr(arg, scopes));
		return AnnotFunExpr(annotId, annotArgs);
	}
	def convertStringExpr(str: String, scopes: List[Scope]): AnnotStringExpr = {
		return AnnotStringExpr(str);
	}
	def convertCharExpr(ch: Char, scopes: List[Scope]): AnnotCharExpr = {
		return AnnotCharExpr(ch);
	}
	def convertNumExpr(num: Numeric, scopes: List[Scope]): AnnotNumExpr = {
		return AnnotNumExpr(num);
	}
	def convertBoolExpr(bool: Boolean, scopes: List[Scope]): AnnotBoolExpr = {
		return AnnotBoolExpr(bool);
	}
	def convertAnonFuncExpr(args: List[ParamDclStmt], body: Expr, scopes: List[Scope]): AnnotAnonFuncExpr = {
		var newScope: Scope = Scope();
		
		var annotArgs: List[AnnotParamDclStmt] = args.map(arg => convertParamDclStmt(arg.id, arg.varType, newScope::scopes));
		var annotBody: AnnotExpr = convertExpr(body, newScope::scopes);
		return AnnotAnonFuncExpr(annotArgs, annotBody, newScope);
	}
	def convertAssignExpr(lhs: Expr, rhs: Expr, scopes: List[Scope]): AnnotAssignExpr = {
	    var annotLhs: AnnotExpr = convertExpr(lhs, scopes);
		var annotRhs: AnnotExpr = convertExpr(rhs, scopes);
		return AnnotAssignExpr(annotLhs, annotRhs);
	}
	def convertClassExpr(name: Type, args: List[Expr], scopes: List[Scope]): AnnotClassExpr = {
		var className: String = name.getType();
		var annotArgs: List[AnnotExpr] = args.map(arg => convertExpr(arg, scopes));
		return AnnotClassExpr(className, annotArgs);
	}
	def convertFieldSelectionExpr(obj: Expr, field: String, scopes: List[Scope]): AnnotFieldSelectionExpr = {
		var annotObj: AnnotExpr = convertExpr(obj, scopes);
		return AnnotFieldSelectionExpr(annotObj, field);
	}
	/* End convert functions */
	
	/* Currying functions */
	//Returns the type of a curried version of a function with multiple parameter clauses
	def curryFuncType(retType: Type, paramClauses: List[List[Type]]): Type = {
		if (paramClauses.length == 0) {
			return retType;
		} else {
			return curryFuncType(new FuncType(retType, paramClauses.head), paramClauses.tail);
		}
	}
	//Returns a Curried version of the function specified. IE: converts a FunDefStmt with multiple parameter clauses to one with only one parameter clause (by returning an anonymous function which expects another parameter clause)
	def curryFunc(name: String, paramClauses: List[List[ParamDclStmt]], retType: Type, body: Expr) : FunDefStmt = {
		if (paramClauses.length > 1) {
			var paramClause: List[ParamDclStmt] = paramClauses.head;
			var paramRest: List[List[ParamDclStmt]] = paramClauses.tail;
			var paramRestTypes: List[List[Type]] = paramRest.map(params => params.map(param => param.varType));
			
			//Make sure to call curryFuncH with reversed parameterClauses (more efficient to work from the head than from the tail, so it pulls from the head first when nesting)
			return FunDefStmt(name, List(paramClause), curryFuncType(retType, paramRestTypes), curryFuncH(body, retType, paramRest.reverse))
		} else {
			//No need to curry the function - there is only 1 param clause
			return FunDefStmt(name, paramClauses, retType, body);
		}
	}
	//Returns a nesting of anonymous functions from a paramClauses.head to body, until all paramClauses are used. (Call this with the expected parameter clauses in reversed order. (More efficient to work from the head than from the tail, so it pulls from the head first when currying)
	def curryFuncH(body: Expr, retType: Type, paramClauses: List[List[ParamDclStmt]]): Expr = {
		if (paramClauses.length == 0) {
			return body;
		} else {
			var newType = new FuncType(retType, paramClauses.head.map(param => param.varType));
			return curryFuncH(AnonFuncExpr(paramClauses.head, body), newType, paramClauses.tail);
		}
	}
	/* End currying functions */
	
	/* Helper functions */
	//Adds a function to the objects map of the head of the scope specified
	def putFunc(scopes: List[Scope], funcName: String, params: List[Type], retType: Type): Boolean = {
		var map: Map[String, Type] = scopes.head.objects;
		if (!map.contains(funcName)) {
			var paramTypes: List[Type] = params.map(param => param);
			map.put(funcName, FuncType(retType, paramTypes)); 
			return true;
		} else {
			throw new Exception("The function "+funcName+" is already defined in the current scope.");
		}
	}
	//Adds all variables specified in varNames to the objects map of the head of the scope specified
	def putAllVars(scopes: List[Scope], varNames: List[String], varType: Type): Boolean = {
		var map: Map[String, Type] = scopes.head.objects;
		return putAllVarsH(map, varNames, varType);
	}
	//Adds all variables specified in varNames to the map specified
	def putAllVarsH(map: Map[String, Type], varNames: List[String], varType: Type): Boolean = varNames match {
	  	case Nil => return true;
	 	case x::xs => {
	 		if (!map.contains(x)) {
	 			map.put(x, varType); 
	 			return putAllVarsH(map, xs, varType);
	 		} else {
	 			throw new Exception("The variable "+x+" is already defined in the current scope.");
	 		}
	 	}
	}
	//Adds a class (or trait) to the types map of the head of the scope specified
	def putClass(scopes: List[Scope], className: String, symbolTable: ClassScope): Boolean = {
		var map: Map[String, ClassScope] = scopes.head.types;
		if (!map.contains(className)) {
			map.put(className, symbolTable); 
			return true;
		} else {
			throw new Exception("The class "+className+" is already defined in the current scope.");
		}
	}
	//Adds an object to the objects map of the head of the scope specified, as well as adding its "hidden" class type (object name followed by an underscore) to the types map
	def putObject(scopes: List[Scope], className: String, symbolTable: ClassScope): Boolean = {
		putClass(scopes, "_"+className, symbolTable);
  		putAllVars(scopes, List(className), BaseType("_"+className))
	}
	//Returns a function that, when invoked, will calculate the linearization of super-types for the class specified. 
	def getLinearization(scopes: List[Scope], className: String, extendsClass: String, withClasses: List[String]): ()=>List[String] = {
		var func: ()=>List[String] = ()=>({
			var classes: List[String] = Nil;
			if (extendsClass != "") {
				classes = getClassScope(scopes, extendsClass).superTypes;
			}
			var withs: List[String] = withClasses;
			while (withs.length > 0){
				var withSuperTypes: List[String] = getClassScope(scopes, withs.head).superTypes;
				while (withSuperTypes.length > 0){
					if (!classes.contains(withSuperTypes.head)){
						classes = withSuperTypes.head::classes;
					}
					withSuperTypes = withSuperTypes.tail;
				}
				withs = withs.tail;
			}
			classes = className::classes;
			classes
		})
		return func
	}
	//Returns the ClassScope of the class (or trait) specified, by searching through the types maps of scopes
	def getClassScope(scopes: List[Scope], className: String): ClassScope = scopes match {
	  	case Nil => throw new Exception("Unknown class "+className+".");
	  	case currentScope::rest => {
	  		var currentObjects = currentScope.types;
	  		if (currentObjects.contains(className)) {
	  			return currentObjects.get(className).get
  			} else {
	  			return getClassScope(rest, className)
	  		}
	  	}
	}
	//Returns a constructed ClassScope using the information specified
	def buildClassScope(scopes: List[Scope], className: String, extendsClassName: String, withClassNames: List[String], paramTypes: List[Type], symbolTable: Scope): ClassScope = {
		var classScope: ClassScope = ClassScope(getLinearization(scopes, className, extendsClassName, withClassNames), paramTypes);
		classScope.types = symbolTable.types;
		classScope.objects = symbolTable.objects;
		return classScope
	}
	//Returns a constructed ClassScope representing the "hidden" class type of the object, using the information specified
	def buildObjectScope(scopes: List[Scope], className: String, extendsClassName: String, withClassNames: List[String], paramTypes: List[Type], symbolTable: Scope): ClassScope = {
		return buildClassScope(scopes, "_"+className, extendsClassName, withClassNames, paramTypes, symbolTable);
	}
	//Prints a List with a comma and space separating the elements
	def prettyPrint(l: List[String]): String = {
		l.tail.fold(l.head)((result, element) => result + ", " + element);
	}
	/* End helper functions */
}