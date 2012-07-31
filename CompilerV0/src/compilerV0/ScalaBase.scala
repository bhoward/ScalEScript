package compilerV0

//Note: Because of this import all "Map"s will be mutable by default
import scala.collection.mutable.Map;

object ScalaBase {
    var objects: Map[String, Type] = Map[String, Type]()
    var types: Map[String, ClassScope] = Map[String, ClassScope]()
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

    addView("Double", "");
    addView("Float", "Double");
    addView("Long", "Float");
    addView("Int", "Long");
    addView("Char", "Int");
    addView("Short", "Int");
    addView("Byte", "Short");
    /* End init maps */

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
    def addObject(id: String, theType: Type): Unit = {
        if (!objects.contains(id)) {
            objects.put(id, theType);
        } else {
            throw new Exception("The object " + id + " is already defined.");
        }
    }
    def getScope(): Scope = {
        var scope: Scope = Scope();
        scope.objects = ScalaBase.objects;
        scope.types = ScalaBase.types;
        return scope;
    }
}