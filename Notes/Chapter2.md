**Anonymous functions?**
- Reduce boiler plate
- Used with higher-order functions
- Functinal literals/lambdas/clojures etc

**Are blocks always anonymous functions**
- They are same in FP. In Scala, block is generic, not always refered to define anonymous functions.


**What is difference between methods and functions?**
- Structurally both are similar. Both take arguments and return value.
- Methods: Object specific concept. Has to be part of object. Specific to OOP.
- Functions: They are values. They are objects in Scala, because that is the only way they can be created on JVM. Because they are objects, they have/can have methods.
- Methods cannot be passed because only values can be passed. Hence, functions can be passed.


**Methods to functions**

Example -
>def increment(x: Int) = x + 1
>val f = increment _                   //(ETA expansion)
>val f1: Int => Int = increment

>increment.apply(100) -> not possible
>f.apply(100) -> possible, because function 'f' has the method 'apply' on it.

- Compiler provides default 'apply' method for functions.

**Is 'apply' method created for `def increment(x: Int) = x + 1`?**
- No. It is a method. Only in REPL you can write methods directly at top level. Internally it is inside an object.
