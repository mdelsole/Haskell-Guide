# Haskell Guide
A friendly guide to the Haskell programming language. Written by me!

# Table of Contents

## Part 1
<!--ts-->
   * [What is Haskell?](#What-is-Haskell)
   * [Learning the Basics](#Learning-the-Basics)
   * [Making Our First Function](#Making-Our-First-Function)
   * [More Features of Functions](#More-Features-of-Functions)
   * [Monads](#Monads)
<!--te-->
## Part 2
<!--ts-->
   * [Making a Full Haskell Program](#Making-a-Full-Haskell-Program)
<!--te-->

# What is Haskell?

Haskell is one of the elusive **purely functional** programming languages. Ok, cool. What does that mean?

Your usual languages such as Java, C++, etc. are something called **imperative** languages. An imperative language is one that takes a sequence of statements to reach its goal. You're telling your computer what to do.

In **functional** languages, rather than giving your computer a line-by-line sequence of instructions, you tell your computer what *is*. An imperative language executes the code line-by-line. A functional language will access the lines when it needs them. 

Let's look at an example. Let's define an integer ```c``` like this:
```
int a = 2;
int b = 3;
int c = a + b;
```
Say that after defining these, we want to change a and b:
```
int a = 2;
int b = 3;
int c = a + b;

a = 4;
b = 5;
```
In an *imperative* language, code is executed line-by-line. ```c``` will evaluate to 5, because that's what ```a``` and ```b``` were when ```c``` was defined. Changing ```a``` and ```b``` at a later point doesn't change what ```c``` is (unless you're using pointers).

In a *functional* language, this wouldn't make any sense. We can't say ```a = 4``` because we just said a *is* 2. If I told you my favorite color is blue, it *is* blue. I won't tell you later in our conversation that my favorite color is green, because I already told you it *is* blue.

In an imperative language, it sometimes makes sense to define a variable, use it in some way, and then reassign it with a different value:

```
counter = 0;
// Some code with counter
counter = 1;
```

Statments like these have no place in functional programming. We can't do ```counter = 1``` because we've already declared that counter *is* 0. Once something is defined, it is what it is. It doesn't change.

You can think of Haskell as just a way of executing a series of transformations on whatever your input is. We aren't defining steps for our program, we're defining these things that can be used.

This might seem kind of limiting, but it actually does give Haskell several very powerful advantages. The most important is probably **referential transparency**. Because we're defining what *is*, expressions don't change. This means that tracing the logic of a program is much easier, and compiler can work much faster. We'll explore the meaning and usefulness of this more as we learn Haskell.

As a result of being a functional programming language, Haskell is what we call **lazy**. This means that the statements you define, such as ```int c = a + b``` from before, won't be executed until they're needed. There's no ```c``` in memory, there's just a definition of ```c``` for when it is needed.

One final aspect of Haskell is that it is **statically typed**, meaning variable types are determined at compile-time. It'll catch all your type mismatch errors before you can run anything. 

A great feature of Haskell, enabled by its combined use of functional programming and static typing, is that it has **type inference**. You don't have to explicitly define every type; Haskell can figure things out. For example, if we say:

```
a = 4 + 5
```

Haskell can figure out that ```a``` is a number. It's a powerful language that allows for short, clean, and concise code.

## Why do we want to learn Haskell?
Speed, speed, and more speed. By the end of learning it, you'll be able to quote Sonic as you watch other programming languages, saying, "You're too slow!" In fact, I'd say Haskell's only sin is that it uses spaces instead of tabs...

# Learning the Basics

You can install the Haskell compiler **GHC** via the instructions on their website (it's not complicated, just follow the instructions). Other than that, we'll just need a text editor to start writing code. I'm using sublime.

Our first few functions won't be complex, and can be done right in the terminal. Type ```ghci``` in the terminal to turn on interactive mode for your Haskell compiler. 

## Arithmetic Expressions

Let's go ahead and just try some simple arithmetic expressions:

```
Prelude> 2 + 15
17
Prelude> 34 * 56
1904
Prelude> 345 - 5789
-5444
Prelude> 5 / 32
0.15625
```
Easy-peasy. Like most programming languages, you can do multiple operations on one line, and use parentheses.

```
Prelude> (5 * 6)/(7 - 8)
-30.0
```

One thing to note is that negative numbers *must* be enclosed by parentheses. Doing ```3 * -5``` will get you an error, but ```3 * (-5)``` works fine.

Haskell does not allow you to use any arithmetic operators with strings. When you use ```+```, it expects the operators on either side be a number.

## Boolean Expressions

Boolean expressions use the familiar ```&&``` and ```||```. "Not" is done with the word ```not```, rather than an exclamation mark. Our boolean values themselves start with capital letters: ```True``` and ```False```:

```
Prelude> True && True
True
Prelude> True || False
True
Prelude> not True
False
```
We can test for equality using ```==```. Inequality, however, uses ```/=``` rather than ```not ==``.
```
Prelude> 5 /= 6
True
```
If you try to equate something like ```5 == "Cat"```, you'll get an error because they're not the same type.

## Functions

You probably haven't realized it, but we've been writing functions. These functions have been **infix** functions. Infix functions are when the function is sandwhiched between the operands. ```5 * 6``` for example is using the infix function called ```*```. The same could be said for ```True && False```, which uses the ```&&``` infix function.

However, most functions we'll make are **prefix*** functions, where the function comes before the value it's to be used on. A simple built-in prefix function is ```max```, which takes the max of two things that can be put in an order:

```
Prelude> max 4 5
5
Prelude> max "llama" "baby"
"llama"
```
A **prefix** function can be called by writing its name followed by its parameters, putting a space in between each.

We can combine prefix and infix functions. Prefix functions have the highest precendence, so they will be calculated first, after which their results can be combined using infix functions. For example:

```
Prelude> max 4 5 * 3
15
```

If a prefix function takes in two parameters, it can be used as an infix function by surrounding the function name with back-ticks. For example:

```
Prelude> 4 `max` 5
5
```
This might not really make visual sense for a function like ```max```, but for other functions like ```div``` it can be useful:

```
-- Doesn't make visual sense
Prelude> div 10 4
2
-- Makes visual sense
Prelude> 10 `div` 4
2

```

Don't forget to use parentheses when trying to combine mutiple functions. It can get pretty difficult to tell the order of things without them. Also, you may have noticed, but comments are denoted in Haskell using ```-- Comment```.

# Making Our First Function

Now that we've got the basics, let's start making our first function. By default, all Haskell programs need to have a ```main```, just like the popular kids C++/Java. However, if we're using ```ghci```, we don't need to worry about it; we can just define functions on the fly. 

Open up that text editor and make a file with the extension ```.hs``` called ```practice.hs``` or something. We'll write our first function called ```add_three```:

```
add_three x = x + 3
```
The syntax isn't complicated. On the left side of the equals sign, we've written the function just like how we would *call* it: the name followed by its parameter(s), separated by spaces. On the right side of the equal sign, we have instructions on how to actually execute the function. 

In this case, our function's name is ```add_three``` and it has one parameter ```x```. It is evaulated by adding three to that input parameter. Simple, clean, and concise.

Let's fire that big boy up so we can see it run in ```ghci```. First, navigate to the folder where you stored your Haskell file (use ```:quit``` to exit ghci, and then use ```cd``` to navigate). Once there, run the following command:

```
:l practice
```

The ```:l``` is short for "load." It should give you a very informal "Ok, one module loaded." We can then test our function out:

```
*Main> add_three 5
8
```

It worked! You're now an expert on functions.

One thing I should mention is that the reason we don't need a ```main``` for running in ghci is because ghci makes a temporary "main" for us. That's nice, but it does mean the next program you load in with ```:l``` will overwrite the previous one. To make an actual program, you'll need to do it outside of ghci. But for now, we don't need to worry about that.

# More Features of Functions

Since functions are such a big portion of Haskell (hence why it is a *functional* programming language), it's a good idea to discuss more of the features they have to offer. While this isn't going to be an exhaustive list (we'd be here all day if it were), we'll cover some of the more important concepts.

Just to reinforce it, a function has 3 components: ```name parameters = evaluation```

## If statements

Functions can contain if statments. Haskell uses the if-then-else keywords. Opening up that ```practice.hs``` text file again, let's do an example:

```
is_even x = if (x `mod` 2) == 0
            then print "Even"
            else print "Odd"
```

Here, we determine whether x is an even number. We use the function ````mod````, with back-ticks to make it easy to read.

We can use the result of an if statment for further calculation too. Say we want our ```is_even``` function to return 100 if the number is even and 0 if it is odd:

```
is_even x = (if x `mod` 2 == 0
            then 1
            else 0) 
            * 100
```

Since we're using parentheses, the white space doesn't matter. We can make this look nicer:

```
is_even x = (if x `mod` 2 == 0 then 1 else 0) * 100
```
Just be sure to keep using those parentheses. Note that the then-else parts of the statment are mandatory. The if statement *has* to return something, because remember we're telling the computer what *is*. The if statement has to be an expression (meaning it returns a value).

## Definitions

Functions don't have to take in parameters. When they don't, we typically refer to them as **definitions**. It's here that we can begin to explain that **referential transparency** I mentioned in the beginning of this series. Let's say I make a definition like this:

```
mario = "Wa-hoo!"
```

Because this is a functional programming language, we can't change ```mario``` after defining it. This means that anywhere we see ```mario```, we can replace it with ```Wa-hoo!```. **Referential transparency** refers to the idea that we can replace expressions with other expressions that mean the same thing. As I said in the introduction of this series, this will let our compiler work much faster.

## Lists

Lists are Haskell's most used data structure. Even simple things like strings are actually lists underneath. Lists are how you combine things in Haskell. 

Lists in Haskell are **homogenous**, which means they can only contain pieces of data that are the same type. For example, you can't have a list containing both integers and characters. 

There's a new keyword we'll use here called ```let```. With ghci 8, we don't actually need ```let``` anymore. However, it's still good practice and will help you analyze what you wrote.

Lists are denoted by square brackets, with their elements separated by commas. It's identical synatax to Python:

```
Prelude> let a = [1, 2, 3, 4, 5, 6]
Prelude> a
[1,2,3,4,5,6]
```
For **character** lists, our elements will need to be enclosed in single-quotes:

```
Prelude> let b = ['L', 'u', 'i', 'g', 'i']
Prelude> b
"Luigi"
```
Notice that upon calling b, the character list has been squashed together, creating a string. Like I said before, strings *are* lists in Haskell. They just have a special print function to look nice like that.

Because strings are lists, this means we can manipulate them using list functions. To **add** two lists together, we use the ```++``` operator:

```
Prelude> [1, 2, 3, 4] ++ [3, 4, 5, 6]
[1,2,3,4,3,4,5,6]
Prelude> "Mario" ++ " and " ++ "Luigi"
"Mario and Luigi"
Prelude> ['M', 'a', 'r', 'i', 'o'] ++ " " ++ ['B', 'r', 'o', 's']
"Mario Bros"
```

We can manipulate strings exactly the same as if they were lists, because, well, they *are* lists.

We can also put lists inside of lists (and lists inside of that, and so on). However, all lists within a list must be the same depth. For example, we can't do:

```
[1, [2, 3]]
```

However, we can do:

```
[['a', 'b'], ['c', 'd']]
```

What will this return? Well, it's a list of lists of characters. A string is a list of characters, so this is really just a list of strings. Accordingly, the output is:

```
Prelude> [['a', 'b'], ['c', 'd']]
["ab","cd"]
```
## More List Operations

We can get an element by index from a list by using the ```!!``` operator. Indices start at 0, like they should (get out of here, Matlab).

```
Prelude> [3, 4, 5] !! 2
5
Prelude> "Hello" !! 4
'o'
```

How do we do this with nested lists? It should be simple:

```
Prelude> [['a', 'b'], ['c', 'd']] !! 1
"cd"
```

If we want to get the 0th element of that 1th list, we'll have to make use of parentheses:

```
Prelude> ([['a', 'b'], ['c', 'd']] !! 1) !! 0
'c'
```
Lists can also be compared using ```>```, ```<```, ```>=```, and ```<=```. This is how we compare strings too (since once again, strings *are* lists):

```
Prelude> [1, 3, 2] > [1, 2, 3] 
True
Prelude> "Hello" > "World"
False
```

Lists are compared in the traditional way strings in other languages are. The first elements are compared, and if there is a tie, the program moves to compare the next element.

Lists also have built-in functions to grab certain elements. I won't go through them all, but a few are:

```
-- Grab the first element
Prelude> head [1, 2, 3, 4]
1
-- Grab everything but the first element
Prelude> tail [1, 2, 3, 4]
[2,3,4]
-- Grab the last element
Prelude> last [1, 2, 3, 4]
4
-- Grab everything but the last element
Prelude> init [1, 2, 3, 4]
[1,2,3]
```

More importantly, lists also have operation functions. A few important ones are:
```
-- Grab the list's length
Prelude> length [1, 2, 3, 4]
4
-- Return true if the list is empty
Prelude> null [1, 2, 3]
False
-- Reverse the list
Prelude> reverse [1, 2, 3]
[3,2,1]
```

Take note that **all** of these functions return something. This is a functional programming language; a statment that does not return something has no place. In Python for example, list.sort() is an insitu method, which means it sorts the list in place and doesn't return anything. We can't do that in Haskell (nor would we want to). A call to reverse a list must return the list.

There're many more list operations (```sum```, ```maximum```, ```product```, just to name a few), but you can look them up as you need them. I can't spell them all out, or we'd never get to learn anything else.

## List Comprehension

We can construct a simple list in Haskell using the ```..``` operator. For example, if I wanted to make a list of numbers from 1 to 10, I could just do:

```
x = [1..10]
```
But what if I wanted only the even numbers from that list? Then I'd have to use something called **list comprehension**. List comprehension is a way of constructing a specific list from a more general list. The syntax will look complicated at first, but it's really not. Here's how we would get those even numbers:

```
[x | x <- [1..10], x `mod` 2 == 0]
```
The "generator" ```x <- [1..10]``` feeds the list of numbers 1-10 into x. Before returning x (left side of the ```|```), the condition ```x `mod` 2 == 0``` is checked. Only elements of the list which pass this condition will be returned. 

What does that arrow mean? How do we use it in other places? That's something we'll cover in the next section. Speaking of which...

# Monads

Monads are one of the most important concepts in Haskell. People say that monads are really complicated, but they're actually pretty simple. It's just hard to find an answer on google. You'll be hit with a wave of jargon that won't make any sense unless you already completely understand them. Just stick with me for a little bit here and I'll save you a lot of pain.

At the very beginning of this tutorial, we discussed the difference between an **imperative** language (C++/Java/Python) and a **functional** language (Haskell). The difference we decided on was that an *imperative* langauge provides the computer with a sequence of steps to execute line-by-line, while a *functional* language simply tells the computer what *is*.

Monads are *Haskell's way of implementing imperative behavior*. Fundamentally, a monad is just **a sequence of operations**. Let's look at that list comprehension example from the previous section:

```
[x | x <- [1..10], x `mod` 2 == 0]
```

This is actually a monad. What makes it so? Underneath this syntactic sugar, the expression is performing a *sequence* of operations. Here's what it looks like under the hood:

```
do
  x <- [1..10]
  guard (x `mod` 2 == 0)
  return x
```
It's really just a sequence of operations, marked by the ```do``` keyword. This is still functional programming because it's a *chain of operations*, not a line-by-line telling to our computer of what to do. The line-by-line aspect here is just to specify the *order* in which these operations are performed. Another, more complicated example would be:

```
-- The monad
main = do
  str <- getLine
  number <- stringToNum str
  result <- squareRoot number
  print result

-- Functions for above (not important)
stringToNum :: String -> IO Double
stringToNum s = return (read s)
squareRoot x = return (sqrt x)
```

Ignore the syntax we haven't covered yet. This piece of code **chains together** a sequence of operations. ```getLine``` retreives information from the command line. ```stringToNum``` converts that getLine from a string to a double. ```squareRoot``` squareroots the stringToNum. Finally, the square root is printed.

Most of our code in our program will be functional code (defining what *is*). Then, we'll actually *use* our code using monads (imperative-style code). When running a program in Haskell, the compiler will look for a monad as an entry point, by default called ```main```.

Remember that we're still in a functional langauge, so a monad *must* return something. We can define what *type* of "something" it returns, but it must return something. By default, the ```main``` monad will need to return IO, meaning we must print something. Hopefully, you're starting to see the parallels between a monad in Haskell and a method in C/C++/Java; monads really are just a way of doing imperative code in Haskell.

## The Hidden Power of Monads

Great, monads are simple after all! Not so fast. The reason monads are so fundamental, and by extension why they receive such complicated explanations, is because *Monads themselves can be treated as expressions*.

Keeping the idea of **chaining** in mind, we can monads a bit further: a monad is set of computations combined into to form a more complex computation. 

A monad is essentially a **type** (like integer or list). Being a type, monads support your standard operations like ```==``` and ```>```. However, monads have a special very important operator: ```<<=```. 

This operator, known as the "bind" or "chain" operator, is what we use to *chain* new operations to the monad. It's like appending a character to a string. With strings, you have a sequence of characters, and then you add one to the end. With monads, we have a sequence of operations, and then we add one more to the end using ```<<=```.

Using this ```<<=```, we can convert the above to special monad notation:

```
main = getLine >>= stringToNum >>= squareRoot >>= print
  where
    stringToNum :: String -> IO Double
    stringToNum s = return (read s)
    squareRoot x = return (sqrt x)
```

In summary, a monad is Haskell's way of doing imperative code. It is how we actually *use* the functions that we've defined. A monad will be our entry point to run the code. However, monads can *also* be chained together. Underneath, a monad is just a *sequence of operations*. Chaining monads just adds those new operations to the sequence. Most often, we'll "call" other monads from our ```main``` monad to run our full program.

As we learn more, we'll see that waht we've learned here are just basic monads. There are many different types with many different functionalities. But hopefully, they at least make some sense, because that's all we need to start writing our own program!

# Making a Full Haskell Program 

Now that we've covered the basics, I'm going to start picking up the pace a little for this tutorial. Simple syntax stuff is easily google-able. More complex stuff is not. 

The best way to learn is to build something. For the second half of this tutorial, we're going to build an actual large-scale program in Haskell. 

There's an old language called "Forth" that no one really uses. We will be building a miniature version of it in Haskell. That's right, we'll be making our own programming language! Why? It will force us to cover most fundamental aspects of Haskell. That, and I couldn't think of anything better to do. 

I'll include the source code for this up top in the files, in case you want to see the full extent of it. That being said, let's go!

## Setting up

Maybe you remember how to run a program in Haskell from earlier, or maybe you don't. Either way, I'll walk through it again.

First, create a folder that will be where we'll store the program files. Navigate to the folder in terminal using ```cd```. 

Then, boot up any text editor you want. I'll be using sublime, but it really doesn't matter. If you hate yourself, you can even use nano. We're going to want to have both the terminal and the text editor open next to each other for quick and easy compilation.

In your text editor, create a new file called ```Main.hs```. Recall that ```.hs``` is the extension for haskell files. With that, we can start actually coding.

## Entry points and the main module

Starting out, Haskell is going to look for an entry point. By default, the entry point Haskell will look for is named ```main``` and is of type IO (meaning it prints something).

```
main = do
  print (8 + 9)
```

We don't *have* to name it main though. In fact, outside of this starter file, we shouldn't name it main. How do we define a different entry point? Placing this line at the top of our file: ```module MyName where```. For example:

```
module Hello where

hello = do
  print (8 + 9)
```

If you read it as if it's all one line, it should make sense. It's good practice to include this even in our primary ```Main.hs``` file, so go ahead and add ```module Main where``` to the top of your file.

Likewise, we won't always want our module to be of type IO. We can define the **type** an expression is using the ```::``` operator. Again, it's good practice to specify what our ```main``` will be, so go ahead and add ```main :: IO ()```. Our program now looks like this:

```
module Main where

main :: IO ()
main = do
  print (8 + 9)
```

If you want to compile this, you can use ```ghc Main.hs```. Then, it can be run like any other program using ```./Main``` (or clicking that .exe if you're on windows).


## Adding functionality to the main module

Right now, our main module doesn't do anything but print the sum of those two numbers. Delete that ```print (8 + 9)``` line, and let's add some functionality to it. 

The first thing we need is some way to read in Forth code. Since I don't really want to implement an interactive Forth terminal, we're just going to read in code from a file. Thus, our program should take in its arguments the file name. We can do so using the built-in ```getArgs```.

We'll be using the format ```x <- action``` a lot here, which simply executes the ```action``` and binds its result to ```x```. 

Our ```getArgs``` will be our action, and our x will look like this:

```
(fileName:tl) <- getArgs
```
To use getArgs, however, we'll need to ```import System.Environment```. We place our import statmenets between ```module Main where``` and ```main = do...``` like so:

```
module Main where

import System.Environment

main :: IO ()
main = do
  (fileName:tl) <- getArgs
```

Next, we'll want to actually read the contents of the file. We can do so with the same format, using the ```readFile``` function:

```
contents <- readFile fileName
```

Now that we've got the contents, we're probably going to want to do something with those contents. So, before finishing the Main.hs, we'll have to write the interpret module

## The Interpret module

Most programs, including the one we're building here, will be complicated enough to split across different files. Fortunately, incorporating other files is easy. 

To start, make a new file called ```Interpret.hs```, placing it in the same folder as ```Main.hs```. To link this file to our ```Main.hs```, all we need to do is add an import statment: 

```
import Interpret
```

In our ```Interpret.hs```, we'll set it up similarly to how our ```Main.hs``` is set up.


...**unfinished**, for now. I've uploaded the finished product (with comments!) to this repo though.
