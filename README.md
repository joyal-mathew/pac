This is a simple language I decided to create. It is quite minimal so you have

to create most things on your own. You can try it [online](https://pac-lang.herokuapp.com/).<br>



Here is an example of a function to convert integers to strings: <br>

```

function string to_string(var int n)

var array[str] digits

var str t

var int i l h

do

digits = {};



while n do

digits <- (n % 10 + 48) ~> str;

n = n / 10;

end



i = 0;

l = $digits;

h = l / 2;



if l == 0 then

return "0";

end



while i < h do

t = digits[i];

digits[i] = digits[l - 1 - i];

digits[l - 1 - i] = t;

i = i + 1;

end



return join(digits);

end

```



## Full Syntax Guide

1. Operators <br>

1. Basic Math <br>

1.  `+`

2.  `-` (subtraction and negation)

3.  `*`

4.  `/`

5.  `%`

2. Comparison <br>

1.  `==`

2.  `!=`

3.  `<`

4.  `>`

5.  `<=`

6.  `>=`

3. Logic <br>

1.  `!` (logical not)

2.  `&&`

3.  `||`

4. Bitwise <br>

1.  `>>`

2.  `<<`

3.  `~` (bitwise not)

4.  `&`

5.  `|`

5.  `^`

5. Other <br>

1.  `~>` (casting)

2.  `=`

3.  `(`  `)` (functions)

4.  `[`  `]` (indexing)

5.  `{`  `}` (arrays)

6.  `;`

7.  `,` (separating parameters)

8.  `` ` `` (deallocating arrays and strings)

6. Sequence

1.  `@` (pop from array)

2.  `<-` (push to array and return the array itself)

3.  `$` (length of array or string)

2. Types (all are 8 bytes in size) <br>

1. explicit (can be named in the program) <br>

1.  `int`

2.  `float`

3.  `str` (up to 8 characters)

4.  `string`

5.  `array[TYPE]`

2. implicit (exist but cannot be explicitly used) <br>

1. Function

2. Void

3. Other <br>

1.  `var TYPE IDENTIFIER, IDENTIFIER;` (delcaring variables)

2.  `EXPRESSION(PARAMETERS)` (calling a function)

3.  `EXPRESSION[INDEX]` (indexing an array or string)

4.  `{ EXPRESSION, EXPRESSION }` (creating an array)

5. Line comments starting with a `#`



#### If statements

```

if CONDITION then

EXPRESSION;

end



if CONDITION then

EXPRESSION;

else

EXPRESSION;

end



if CONDITION then

EXPRESSION;

elif CONDITION then

EXPRESSION;

else

EXPRESSION;

end

```

#### While loops

```

while CONDITION do

EXPRESSION;



while CONDITION do

break;

end



while CONDITION do

break 1; # break/continue level (default is zero) #

end



continue;

end

```

#### Functions

```

# function type can be ommited to return void #

function TYPE IDENTIFIER(var TYPE IDENTIFIER var TYPE IDENTIFIER)

var TYPE IDENTIFIER # variables must be delcared in the first section

do

EXPRESSION; # logic occurs in the second section

return EXPRESSION;

end

```

#### IO

```

print_str(STR);

println_str(STR); # prints with trailing newline and flushes stream

print_string(STRING);

println_string(STRING);

```

#### Arrays

```

join(ARRAY); # joins an array of strs and returns a string



`ARRAY; # returns the capacity of an array

ARRAY <| 50 # sets the capacity of an array and returns the array itself

```

***

The top level can only consist of functions. When the program is run the main

function is called.



## How does this work?

Step | File | Purpose

:---:|---------------------|-----

1 | `src/lexing.rs` | Scan through each character of the program and group them into *tokens*.

2 | `src/parsing.rs` | Build a tree which represents what the program is doing.

3 | `src/compiling.rs` | Convert the tree into a list of simple steps.

4 | `src/assembling.rs` | Convert the steps into bytes which can be processed easily.

5 | `vmsrc` | Execute the instructions encoded into the bytes.



*tokens* are the most simple components of the language which carry meaning on their own.

They are the building blocks. <br>

ex: identifiers, operators, keywords



#### Memory model

Both arrays and strings are stored as 8 byte pointers. The following is the data

stored at those pointers:



**Arrays**



Bytes | Name | Purpose

:----:|--------------|--------

1-4 | Ref Counter | Stores the number of references to the array for automatic memory management.

5-8 | Length | Stores the length of the array.

9-12 | Capacity | Stores the capacity of the array which can be increased.

13-16 | Data Pointer | Stores the location of the data of the array, stored contiguously.



**Strings**



Bytes | Name | Purpose

:----:|--------------|--------

1-4 | Ref Counter | Stores the number of references to the string for automatic memory management.

5-8 | Length | Stores the length of the string.

. . . | Data | The data is stored in the remaining bytes because the size cannot change.



Three stacks are used in execution.



1. Operation Stack to perform calculations

2. Variable Stack to store variables

3. Call Stack to keep track of function calls



The size of these stacks default to 1KiB but can be set with command line arguments

`pac main.pas -o 512 -v 512 -c 512`

Assembly can be printed with a flag
`pac main.pas -a`