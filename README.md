# Gentzen


## Purpose

Gentzen is an attempt at writing a proof checker and basic assistant for
performing proofs within arbitrary systems of Sequent Calculi.

I made Gentzen after reading parts of
"Structural Proof Theory" (Negri, Von Plato),
my goal was to better understand how the different systems relate as well as to
have a machine check my proofs.

Gentzen supports arbitrary user-defined sequent calculi.

Gentzen is still very early in development and is missing most features that
might make it useful to anyone else.


## Example

We can look at `examples/hypothetical_syllogism.gtzn` to see a definition of
a sequent calculi system (G3ip from Structural Proof Theory),
as well as a proof within that system:

    $ cat examples/hypothetical_syllogism.gtzn 
    system G3ip
    rules
        axiom         (a)    (L |- R) = [(a in L), (a in R)].()
        left_and      (a^b)  (L |- R) = L-(a^b)+a+b |- R
        right_and     (a^b)  (L |- R) = (L+a |- R-(a^b)), (L+b |- R-(a^b))
        left_or       (a∨b)  (L |- R) = (L-(a∨b)+a |- R), (L-(a∨b)+b |- R)
        right_or1     (a∨b)  (L |- R) = L |- R-(a∨b)+a
        right_or2     (a∨b)  (L |- R) = L |- R-(a∨b)+b
        left_implies  (a->b) (L |- R) = (L |- a), (L-(a->b)+b |- R)
        right_implies (a->b) (L |- R) = L+a |- R-(a->b)+b
        left_bottom   ()     (L |- R) = [(_ in L)].()
    qed

    theorem hypothetical_syllogism
    system G3ip
    sequent |- ((a->b) ^ (b->c)) -> (a -> c)
    proof
        apply right_implies [((a->b)^(b->c))->(a->c)]
        expect (a->b) ^ (b->c) |- a->c
        apply right_implies [a->c]
        apply left_and [(a->b)^(b->c)]
        apply left_implies [a->b]
        {
            apply axiom [a]
        }
        {
            apply left_implies [b->c]
            {
                apply axiom [b]
            }
            {
                apply axiom [c]
            }
        }
    qed


We can run Gentzen over this proof to have it perform a check:

    $ make && stack exec gentzen-exe -- examples/hypothetical_syllogism.gtzn 
    stack build

    Parsing results:
    WorkUnit system 'G3ip' (Theorem {name = "hypothetical_syllogism", system = "G3ip", sequent = [] |- [a->b^b->c->a->c], steps = [Apply "right_implies" [a->b^b->c->a->c],Expect [a->b^b->c] |- [a->c],Apply "right_implies" [a->c],Apply "left_and" [a->b^b->c],Apply "left_implies" [a->b],Branch [Apply "axiom" [a]] [Apply "left_implies" [b->c],Branch [Apply "axiom" [b]] [Apply "axiom" [c]]]]})

    check successful


    Success
    output:
    proof tree:
        unproven: []
        aborted:  []
        sequents:
            0 : [] |- [a->b^b->c->a->c]
            1 : [a->b^b->c] |- [a->c]
            2 : [a,a->b^b->c] |- [c]
            3 : [b->c,a->b,a] |- [c]
            4 : [b->c,a->b,a] |- [a]
            5 : [b,b->c,a] |- [c]
            6 : [b,b->c,a] |- [b]
            7 : [c,b,a] |- [c]
        steps:
            0 : Straight 0 "right_implies" [a->b^b->c->a->c] 1
            1 : Straight 1 "right_implies" [a->c] 2
            2 : Straight 2 "left_and" [a->b^b->c] 3
            3 : Split 3 "left_implies" [a->b] 4 5
            4 : Axiom 4 "axiom" [a]
            5 : Split 5 "left_implies" [b->c] 6 7
            6 : Axiom 6 "axiom" [b]
            7 : Axiom 7 "axiom" [c]

    run successful

