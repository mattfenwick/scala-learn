## Examples

### format ###

    arbitrary lambda-calculus term
    alpha-substitution with the properties that:
      - names unique (mem: define `unique`),
        such that replacement (mem: define `replacement`) is safe (mem: define `safe`)
      - first letter indicates bound/free


bound in same scope:

    \a. a
    \b1. b1

bound in enclosing scope:

    \a. (\b. a)
    \b1. (\b2. b1)

shadowing enclosing scope:

    \a. (\a. a)
    \b1. (\b2. b2)

bound in same scope + shadowing enclosing scope:

    \a. a (\a. a)
    \b1. b1 (\b2. b2)

same name, but different variables:

    (\a. a) (\a. a)
    (\b1. b1) (\b2. b2)

free:

    a
    f1

free, bound:

    \a. b
    \b1. f1

different free:

    a b
    f1 f2
    
same free:

    a a
    f1 f1

free + bound = shadow free variable:

    a (\a. a)
    f1 (\b1. b1)

