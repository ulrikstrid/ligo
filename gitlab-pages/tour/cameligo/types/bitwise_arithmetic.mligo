(**

Users who are already familiar with the bitwise operators can read the code
in this section and probably get what they need without explanation. For users who
are not familiar, a significant amount of background explanation might be necessary.
Arithmetic operators such as addition, subtraction, multiplication, and division
handle numbers as a series of values 0-9. They deal with numbers like 20, 5, 18,
79, or 100. *Bitwise* operators on the other hand manipulate the individual binary
digits which computers use to represent numbers. They deal with numbers like 0100,
10100110, 10111, etc. If you already know how binary works you can skip down to the
explanation of the bitwise operators themselves below, otherwise read on.

*)

(**

At the lowest level data stored in computer programs is represented as *binary*.
Binary (also known as base-2) represents numbers as a
series of 0's and 1's, on and off, true or false, or any other kind of digit which
can only take on two values. This is in contrast to decimal (or base-10) which uses
the ten symbols 0 through 9 to represent numbers. You already know how to read a
number in base 10 such as 5050. But how do you read a number in binary like
10001110? It's a lot simpler than you might think.

We know that a base-10 digit can have one of ten values zero through nine. We
also know that a binary digit (or 'bit') can have one of two values 0 or 1. To
read a base-10 number we're probably familiar with 'places', so that 56 has
'five in the tens place' and 'six in the ones place'. But there's actually a
simpler pattern to consider here. A single base-10 digit can have one of 10
values. Two base-10 digits have one of 100 values 0-99. Three base-10 digits have
one of 1000 values 0-999. Each time you add a base-10 digit, you can represent
ten times the number of values you could previously. So it is with binary, each
digit added multiplies the possible values by two. A bit can represent 2 values.
Two bits can represent 4 values. Three bits represent 8 values, and so on.

To understand the intuition behind this, lets consider a two bit number. A two
bit number can have one of four values:

00
01
10
11

A single bit can have one of two values, a single bit and another represents all
the possible combinations of the individual bits. That is, the act of adding
another digit creates two new possibilities for each previous digit. The first
bits 0 state can now be part of 00 or 10, its 1 state either 11 or 01. But we
still don't know how to _read_ a binary number. Taking the previous example,
we map each combination of bits to a base-10 number:

00 | 0
01 | 1
10 | 2
11 | 3

Thus two bits lets us represent the four values 0-3. Three bits lets us represent
0-7, and so on. Binary numbers are read like decimal numbers, except that instead
of a "ten's place" and a "one's place" we have a "two's place" and a "one's place".
Lets consider the four bit number 0101. We know that this can represent 2^4 values,
or 16. That means we have an eight's place, a four's place, a two's place and
a one's place. There are only two symbols, so either a place has a value present
or it does not. Thus 0101 is no-eight plus four plus no-two plus one, or 5. We
can see this is the case by listing out the combinations of the first three bits:

0000 | 0
0001 | 1
0010 | 2
0011 | 3
0100 | 4
0101 | 5 <-
0110 | 6
0111 | 7

Thus by exhausting combinations we can see that for smaller series of binary digits,
we convert to the right base-10 number using this scheme.
*)

(**

Each bitwise operator takes two numbers, and then compares their binary representations
to create a new number. Bitwise OR compares each column of the numbers, and if
both bits in a column are 0 that column is 0 in the resulting number, otherwise
it is 1. For example:

10010011
10101010
--------
10111011

*)
let or_op (n : nat) : nat =
  Bitwise.lor n 4p

(**

The bitwise AND puts 1 in the resulting number if both bits in that column are
1, otherwise it puts 0.

10010011
10101010
--------
10000010

*)
let and_op (n : nat) : nat =
  Bitwise.land n 7p

(**

The bitwise XOR (excluded-or) puts 1 in the resulting number if the two bits are
different, if they're the same it puts 0.

10010011
10101010
--------
00111001

*)
let xor_op (n : nat) : nat =
  Bitwise.lxor n 7p
