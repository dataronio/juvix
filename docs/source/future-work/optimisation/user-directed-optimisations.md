# User Directed Optimisations

- User can prove extensional equality of functions.
- Compiler can pick which function is cheaper to reduce (& pick differently in different cases)
- Can be specialised to properties on arguments, e.g. if $f x | x < 0 = g$, if the compiler can inhabit $x < 0 = True$, it can replace $f$ with $g$.
