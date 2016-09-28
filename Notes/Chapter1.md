# Chapter 1: What is functional programming ?

**What is FP?**
- functions as values => letting you pass and return functions

**Is definition of FP being able to treat functions as first class objects(values) only? Why are other languages not considered functional then like C, Java?**
-  Scala provides language level support that implicitly helps to write in functional way unlike C or Java.

**Why pure FP?**
- We don't have to think about what else might be happening/ Easier to reason about
- Order independent
- Parallelism
- Testing: At individual function level - very helpful and easy
		   Composite testing involving side-effects - difficult

**What is pure function?**
- modular
- separation of concern

**Are side-effects which are not observable from outside considered side-effects, e.g Map, flatMap implementations?**
- No, because semantics give non-side effective behaviour. Also, the non-observable side-effective usages(e.g using loops, mutable data structures in the implementations) help in performance.
> If you do not observe the side effect its not a side effect

**Is logging side-effect?**
- Not necessarily a side-effect. By definition, the function does return the same value in addition to logging into a file etc. Also, logging helps debugging.

**Are exceptions side-effects?**
- They are. Method returning exceptions do not guarantee return value of declared type. Hence, they do not conform to the definition of referential transparency.

**Links/Videos:**
Boundaries: https://www.youtube.com/watch?v=eOYal8elnZk
