# Chapter 2: Getting Started

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

**Why recurssion?**
- for/while are side-effective
- functions are used to loop through in pure functional programming
- has it's own problems -> stack size

**Why/what tail recurssion?**
- non tail recurssive function:
    - return value is computation on top of result of function call
    - recurssive calls add up on the stack
    - maintainance of state of accumulator
- tail-recurssion:
	- no computation done, functions are done
	- accumulator is passed as arg to a function, state maintainance not needed
	- scala compiler tries converting recursive functions into iterative function
		
**Inner functions?**
	- used for iterating (go/loop naming convention for helper functions which are 
	recurssive in nature)		
	- helps encapsulating your implementation, user doesn't needs to be aware of accumulator and the signature of inner loop

**Head recurssion?**
	- http://stackoverflow.com/questions/21426688/the-difference-between-head-tail-recursion

**When would we use head recurssion in comparison to tail?**	
	- homework 

> h, f, g etc single letter naming convention used for naming functions used in HOF

**Polymorphic functions:**
- how/why are they polymorphic since they also take only one type at a time?
	- they take a function that decides on how the function should behave for a given type, you will have different methods for different types
- is there any naming convention for type, A/B? only capital?
	- no

**@specialized annotation?**
	- homework/Mushtaq/Bijendra		