# SML_BigIntLibrary
Biginteger library for SML (Standard ML) for doing integer operations on arbitrarily large integers. 

## Operations supported
It allows following integer operations(in the parenthesis, the corresponding function names are given)
* addition([add](https://github.com/ankitshubham97/SML_BigInteger/blob/master/examples/add.sml)),
* subtraction([sub](https://github.com/ankitshubham97/SML_BigInteger/blob/master/examples/subtract.sml)),
* multiplication([mul](https://github.com/ankitshubham97/SML_BigInteger/blob/master/examples/multiply.sml)),
* division([div4bigint](https://github.com/ankitshubham97/SML_BigInteger/blob/master/examples/divide.sml)),
* mod([mod4bigint](https://github.com/ankitshubham97/SML_BigInteger/blob/master/examples/mod.sml)),
* [comparisons](https://github.com/ankitshubham97/SML_BigInteger/blob/master/examples/comparison.sml):
  * equal to?(eq)
  * not equal to?(neq)
  * less than?(lt)
  * less than or equal to?(leq)
  * greater than?(gt)
  * greater than or equal to?(geq)

## Usage
1. Copy [biginteger.sml](https://github.com/ankitshubham97/SML_BigInteger/blob/master/biginteger.sml) to the folder where lies the test sml file (call it test.sml).
2. Include the following line at the beginning of test.sml:
  `use "biginteger.sml";`
3. This library requires input in the form of string, instead of Integer (for the obvious reason that we want to do operations on integers of arbitrary length and int type of SML has a limit on its size). The string has to be converted to the biginteger type using `str2bi` function.
```bash
val a ="15" ;
val a_big = bigstruct.str2bi(a);
```
4. Use any of the operations given in [Operations supported]() like this:
```bash
val c_big = bigstruct.add(a_big,b_big);
```
where `a_big` and `b_big` are of biginteger type.

5. Get back the string form from the biginteger form using `bi2str` function.
```bash
val c = bigstruct.bi2str(c_big);
```

## Format of the input
All the functions take two biginteger as parameters. `a $ b` is equivalent to `biginteger.operation(a,b)` where `operation` is any of the operations given in [Operations supported]() and `$` is the symbol of `operation`. Note that `a` and `b` are  of biginteger type. For example, to do `a - b`, do `biginteger.sub(a,b)`

## Examples
All the examples are [here](https://github.com/ankitshubham97/SML_BigInteger/tree/master/examples)
