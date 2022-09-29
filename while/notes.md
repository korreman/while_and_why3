# While language

What does a language draft consist of?
For now, let's go with:

- Grammar and semantics for the language.
- Grammar of meta-language.


## Basic design and features

Am I using these terms correctly?

### Object language

First, I have to decide on a direction for the language:

- A low-level C-like language with pointers.
- A higher-level While language with type system features.

Let's start by writing out some fantasy code:

    fn add(x: u32, y: u32) -> u32 {
        return x + y;
    }

    fn sum(x: arr u32) -> u32 {
        let mut sum = 0;
        for i = 0 to x.len() - 1 {
            sum = sum + x[i];
        }
    }


    fn evens(x: arr u32) -> (arr u32, u32) {
        let target = 0;
        for i = 0 to x.len() - 1 {
            if x[i] % 2 == 0 {
                x[target] = x[i];
                target = target + 1;
            }
        }
        return (x, target);
    }

Pretty Rusty, but that's fine.
Let's take inspiration from that syntax as it's easy to write parsers for.

We can start with the things that aren't hard:

- Routines
- Variables
- If/else
- Loops
- Ints and bools

After that, we can consider:

- Arrays
- Pointers/references
- Structs and enums
- Heap vs stack allocation

With that in mind, an initial formal grammar:

    language ::= construct*
    construct ::= function
    function ::= "fn" snake_label "(" argument* ")" "->" type block
    argument ::= snake_label ":" type ","
    block ::= "{" statement+ "}"
    statement ::= "let" snake_label ":" type "=" expression ";"
                | variable "<-" expression ";"
                | "if" condition block "else" block
                | "while" condition block
    type ::= int | bool

    expression ::= <c-style arithmetic expressions, no surprises>

### Meta-language (?)

We want to have:

- Pre- and post-conditions for routines.
- Loop invariants

How should the syntax for this look?
It seems like the type of condition can be positionally inferred.
As such, you could write your pre-and-postconditions as:

    fn min(x: arr u32) -> u32
        < precondition* >
        < postcondition* >
    {
        function body
        let mut i = 0;
        while i < x.len() {
            <invariant>
        }
    }

Time to write some actual proofs.
