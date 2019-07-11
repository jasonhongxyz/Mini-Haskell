# Project Instructions
* Milestone 1: 4/23/19
  * Join a Piazza group as organized by Prof Snyder
  * Create a group repo, and summarize your plans in the readme:
    * Which Additional Features you plan to do (tentative, can be changed)
    * Who has primary responsibility for what (should be approximately equal, and, again, can be changed)
  * Completion is worth 5pts/100.
* Milestone 2: **4/30/19**
  * Submit your tests for the project
    * Tests should be organized in a reasonable way, each test file should contain a toplevel test group and focus on testing one aspect of the project.  For example, ParserTest.hs should not use exec, it should test the parser directly.
    * Tests should be thorough, a Parser test without quickcheck should have more than 20 tests.  Every time you fix a bug, make a test.
    * Within reason, every mix-in should have its own test group in its own file.
    * While it is not necessary to fully complete all features by Milestone 2, you will need to have made substantial progress in order to create reasonable tests.
  * Completion is worth 5pts/100.
* Final Deadline: 5/3/19
  * Completion is worth 90pts/100.

## Notes
* In Check.hs, `deriving (Show,Eq)` should have been `deriving (Show,Eq,Ord)`
  
## Getting Started
* Come up with a language name/team name among your group
* Follow this [Link](https://classroom.github.com/g/potVpRHi) to create/Join a group repo.  Even if you are working alone.
* Like in [Week 1](../assignments/week1#setup-your-local-repository) you will need to `git clone` the new repository! 
  * ```cd``` into the newly created directory 
  * You always want to take advantage of the latest corrections to the assignments and shared tests so we will add the main repository as a source
    * In your terminal type ```git remote add upstream https://github.com/BU-CS320/Spring-2019.git```
    * check that it worked by typing ```git remote -v```.  You should see see the line ```upstream https://github.com/BU-CS320/Spring-2019.git (fetch)```
    * You always want to keep your assignment up to date by running ```git pull upstream master```, do that now
  * check the status of your repo: ```git status```
* Read [Before You Begin](BEFORE.md)


	
## Requirements
This will acount for 60/100 of the points.

Many of the requirements are flexible, for instance if you want to use Java-style comments instead of the Haskell's style, or use Python's `and` instead of `&&`,  talk to Mark. 

### Required ("Vanilla") Features
New features which need to be added beyond your week 10 code:
* Additional operators and functions as explained below, including:
  * Add support for the following types of data: floats, characters, strings, and lists.
  * All operators and functions should report a sensible error message when applied incorrectly during execution.  For example `[0,1] !! 10` should return an error like "Can't get element 10 from a 2 element list" and `7 !! 10` should return "7 is not a list". No input should cause your program to crash, instead, you need to check for all possible errors at execution time. 
  * Additional syntax for lists.  For example ```[1,2,3]```
  * Single and multiline comments. For example ```x+ 7 -- this is a comment``` and ```x+ {- this is a multi-line comment-} 7```
* Appropriate changes to "EnvUnsafe" monad, including support for logging:
  * a `print` keyword 
  * a sequencing infix operator `;`
* Implement a static check that takes in an `Ast` and warns when a variable is used but not declared. For instance  `\ x -> y + 10` should warn something like "y is not in scope".  This will not be part of your parser or interpreter (eval), but should be implemented in a separate `check` function which is normally executed between the parser and the evaluator. 

* You must write a suite of hunit-style tests for your code to verify its correctnes; there will be a lecture about this on Wednesday 4/24.
  * You need a test group for each of the features you implemented.
  * You need to have enough tests to convince us your code is correct by just looking at your tests, so make sure to cover all the edge cases you can think of and every possible error (the tests will be a major part of Milestone 2). 
  * You can build on the tests of week10, but you need to add more tests (not just for mix-ins, for vanilla as well).


In general, the precedence, associativity, and default meaning should be as in <a href="https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html">Haskell</a>. But there
is one important exception: the precedence for application should follow the last homework (application is lower in
precedence) rather than a very high precedence (as in Haskell). This is a matter of taste perhaps, but it makes
things a little easier in terms of implementation to make application have low precedence. Otherwise, you can follow
the Haskell rules. The operators are listed below in classes (operators of the same precedence) in increasing order
of precedence, with associativity and other characteristics noted.  

#### Precedence Classes
in increasing order, L associative except as noted, for arithmetic,relational,
    and boolean operators both operands must be same type
<pre>  
    ;     Separator                  -- lowest precedence, R associative
  <hr> 
          Application                -- function application (no operators, just a blank between expressions)
  <hr> 
    ||    Boolean Or                  
   <hr>    
    &&    Boolean And 
    <hr>   
                                     -- relational operators are non-associative (can only be one operator 
                                     -- in an expression) 
    ==    Equals                     -- equality operators are overloaded for all types except functions                  
    /=    Not-equal                                                     
    <     Less-than                  -- these four operators only need to compare integers and floats,  
    <=    Less-than-or-equal                              
    >=    Greater-than-or-equal              
    >     Greater-than 
      <hr> 	
    :     List cons                  -- R associative
    ++    List concatenation         -- R associative
    <hr>    
    +     Addition                   -- these 2 operators overloaded for integers and floats
    -     Subtraction                -- this is overloaded to also be a unary minus function (see below)
   <hr>    
    *     Multiplication             -- overloaded for integers and floats                          
    /     Floating-Point Division     
    //    Integer Division   
    %     Modulus (remainder after integer division)    -- only for integers
    <hr>   
    ^     Floating-Point Exponentiation             -- R associative
    **    Integer Exponential                       -- R associative
  <hr> 
    !!    List indexing operator     -- L associative, left operand must be list, right must be integer
  <hr> 
    -- prefix operators and functions
    not    Boolean not
    -      Unary minus
    print  Print expression
  <hr> 
    -- atomic expressions
    The highest precedence is the atoms, e.g., integers, floats, chars, lists represented by syntax [,,,], variables, let, if-then-else,lambda abstractions. 
</pre>  
#### Miscellaneous symbols
<pre>  
    \ and ->   Lambda abstraction constructors
    [ and ]    List constructors, "," as separator
    ' and '    Char constructors
    " and "    String constructors
    --    Start of comment line (ignore everything until the next newline)
    {-    Start of multi-line comment 
    -}    End of multi-line comment
</pre>  
#### Predefined Functions
as part of stdLib
<pre>  
    head
    tail
    elem                       -- only for types with equality; for instance: (elem (\y -> 0) [(\x -> x*0)] ) should return an error
    map
    filter
    foldr   (optional -- but give it a shot!)
    ord     (char -> integer)
    chr     (integer -> char)
    float   (integer -> float)
    int     (float -> integer with truncation)

</pre>

In addition, all week 10 language features must still work:
* The parser must still work on week 10 examples (unless it would conflict with a new feature)
* Ast, Val must be `Show`able
* The `showFullyParen` and `showPretty` must be consistent with your parser (By "consistent" we mean that if you take an AST expression, pretty-print it, and then parse it, you should end up with the same AST)
* There is a `run` and `exec` function that behaves as expected.
* Continue to support integers, Booleans and curried function types (functions of one argument)
* The language constructs from last HW must still work: let, if-then-else, etc.
* Dynamic type-checking for expressions for all operators and predefined functions and for all types, and reporting of appropriate errors.

We recommend:
* You can start with your implementation for the last homework and simply add these operators in their appropriate places.  
* You should modify the `EnvUnsafe` monad code to include logging (the `Writer` monad) using a `print` expression, as we did in a previous homework (because we will be adding the print and separator from [lang2](../assignments/week7/src/lang/Lang2.hs))
* You should provide an `eval` function to evaluate expressions in the `Ast` into a suitable result type analogous to `Val` from the last homework; it should use your monad as specified in the previous item.
* With project this size, good variable name and nice documentation will never be a waste of time. Write as much documentation as you can and also make your variable name as descriptive as possible.
* Start early!

### Additional ("Mix-In") Features
This will account for 30/100 of the points.  Listed point totals are approximate and may change.

You will be graded based on hunit test cases that you will provide us (unless specified otherwise)

Professor Snyder will give a lecture on types and type checking on Monday 4/22, and on testing on Wednesday 4/24, and may try to sneak in a lecture on the IO monad as well. And will sneak in a couple of videos...

"Simple" additions
* 5pt Add an infix function composition operator `(.)`.  So you may write `f . g` instead of `\x -> f (g x)`
* 5pt Make lambdas support multiple arguments.  So you may write `\x y z -> x` instead of `\x -> \ y -> \ z -> x`
* 5pt Add multiple sequential definitions to `let`.  So you may write `let x = 4, y = x + 5, z = y in z * 2` instead of `let x = 4 in (let y = x + 5 in (let z = y in z * 2))`
* 5pt Add `letrec` to make recursion more convenient. So you can write `letrec f = \ x -> if x == 0 then 1 else x * (f (x-1)) in f 5`.  Alternatively you may also add this functionality to `let`.  We will talk about this in lecture this week. 

Modules
* 15pt Top level mutually-recursive function definitions. You may want to add a top level operator `=` (without a `let`). 
* 10pt A language feature to import a "file" of definitions. [*](#medium)

Parser enhancments
* 10pt Add error reporting to the `Parser` monad, your parser should fail with clear context specific error messages.
* 5pt Parser which calculates the line and character where it failed for more precise error reporting.
* 10pt Scope based on indentations like in Haskell and Python[*](#medium)

Usability
* Pattern matching [*](#medium)
  * 5pt `case ... of` expressions for the integers and bools
  * +5pt Nested pattern matching that allows integers, bools, and lists.
  * +5pt Build pattern matching into lambda expressions for integers, bools, and lists.
* User-defined data types
  * 5pt Definitions and constructors 
  * 5pt Pattern-matching [*](#medium)
  * 10pt Typechecking
* 5pt A Read-Eval-Print loop, so that users can work interactively with your language, including preloading a 
      Prelude-like initial environment. You would need to learn about the IO monad (start with Chapter 10 in Hutton).

Static Checking
* 5pt Warn when a variable is introduced but never used
* 15pt Checking simple types, where every variable has a type annotation (lecture will be presented on this)
* 20-30pt  Advanced type checking: Bidirectional, Hindly-milner, or dependently typed [**](#difficult)

Mutable state
* 10pt Dynamically scoped mutable state[*](#medium)
* 20-30pt Lexically scoped mutable state[**](#difficult)

Misc
* 5-15 pt Overloaded operators and constants, automatic type conversion (as in Java or Python) [*](#medium)
* 10pt Add runtime warnings to the monad (as in the Ok monad presented in lecture), and flag appropriate conditions which are not errors, but cause concern (e.g., integers go outside the range -2<sup>29</sup> to 2<sup>29</sup>-1, you defined a variable or function but then didn't use it). [*](#medium)

Additionally you can get points by using engineering best practices
* 5pt Writing a quickcheck generator and shrinker for your Ast and using it to test your parser
* 5pt Writing clear Haddock style comments and generating the html documentation
* 5pt Setting up Continuous Integration on your github repo[*](#medium)


You can implement as many features as you want but you cannot score above a 100

There are many other features, small and large, which could be imagined (orderings on lists? array types? n-ary tuples? conversion between prefix and infix? lazy evaluation? compilation[**](#difficult)?   The list goes on and on.....).  Please talk to Mark if you have creative ideas!  Some things are very challenging, and may change the language specification drasticly: these need to be aproved by Mark before the first milestone, and may require additional work by the 2nd milestone.

<a name="medium">*</a> Mark needs to aprove before Before Milstone 2. These problems might require some work on our end, for instance We might need to give you extra permissions in your repo for you to set up Continuous Integration.  Mark also might have some hints he would like to give you before you start.

<a name="difficult">**</a> This is challenging, Mark needs to approve before ~~Milestone 1~~ ASAP.



