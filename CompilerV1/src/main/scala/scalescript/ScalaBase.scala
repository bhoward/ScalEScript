package scalescript

//Note: Because of this import all "Map"s will be mutable by default
import scala.collection.mutable.Map

/** ScalaBase is the object containing all the information related to the built-in Scala types, functions and objects.
 * 
 * @author Trevor Griswold
 * @author Mike Stees
 * @author Brian Howard
 */
object ScalaBase {
    //Objects and types are used in conjunction to create the base scope for the compilation process.
    var objects: Map[String, Type] = Map[String, Type]()
    var types: Map[String, ClassScope] = Map[String, ClassScope]()
    
    //Each type in views maps to a list of types that it is able to be automatically converted to in Scala. This is used when type checking.
    var views: Map[String, List[String]] = Map[String, List[String]]()

    /* Init maps */
    addType("Any", "");
    addType("AnyVal", "Any");
    addType("Double", "AnyVal");
    addType("Int", "AnyVal");
    addType("Boolean", "AnyVal");
    addType("Char", "AnyVal");
    addType("Unit", "AnyVal");
    addType("AnyRef", "Any");
    addType("String", "AnyRef");
    addType("Function", "AnyRef");

    addObject("println", FuncType(BaseType("Unit"), List(BaseType("Any"))));
    addObject("print", FuncType(BaseType("Unit"), List(BaseType("Any"))));

    // Make sure that these are called in the correct order, so they can build off eachother.
    addView("Double", "");
    addView("Float", "Double");
    addView("Long", "Float");
    addView("Int", "Long");
    addView("Char", "Int");
    addView("Short", "Int");
    addView("Byte", "Short");
    /* End init maps */

    /** Adds the specified type to the views map, with the specified convertType (as well as all of its convertTypes) */
    def addView(name: String, convertType: String): Unit = {
        if (!views.contains(name)) {
            if (convertType == "") {
                views.put(name, Nil);
            } else {
                if (views.contains(convertType)) {
                    views.put(name, convertType :: views.get(convertType).get)
                } else {
                    throw new Exception("The views for type " + convertType + " are not defined.");
                }
            }
        } else {
            throw new Exception("The views for " + name + " are already defined.");
        }
    }
    /** Adds the specified type to the types map, storing the specified superTypes in the classScope */
    def addType(name: String, superType: String): Unit = {
        if (!types.contains(name)) {
            if (name == "Any") {
                types.put(name, ClassScope(() => List[String]("Any"), Nil))
                //TODO Fill in actual symbolTable for Any
            } else {
                types.put(name, ClassScope(() => (name :: types.get(superType).get.superTypes), Nil))
                //TODO Fill in actual symbolTables for other scala types
            }
        } else {
            throw new Exception("The type " + name + " is already defined.");
        }
    }
    /** Adds the specified id to the objects map */
    def addObject(id: String, theType: Type): Unit = {
        if (!objects.contains(id)) {
            objects.put(id, theType);
        } else {
            throw new Exception("The object " + id + " is already defined.");
        }
    }
    /** Returns the Scala scope */
    def getScope(): Scope = {
        var scope: Scope = new Scope();
        scope.objects = ScalaBase.objects;
        scope.types = ScalaBase.types;
        return scope;
    }
}