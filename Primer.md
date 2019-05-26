# Gentzen Primer

This document is a work in progress.


## A minimal Gentzen proof

A Gentzen file always includes a specification of the system in use followed by
a proof of a theorem.

A Gentzen theorem includes a starting sequent (goal) which serves as our initial
proof obligation and a set of steps for our proof.
A proof is considered finished if we have no remaining proof obligation.

Consider the proof from `examples/simple_implication.gtzn`:

    system axiom_and_right_implies
    rules
        right_implies (a->b) (L |- R) = L+a |- R-(a->b)+b
        axiom (a) (L |- R) = [(a in L), (a in R)].()
    qed

    theorem simple_right_implies
    system axiom_and_right_implies
    sequent |- (a->a)
    proof
        expect |- (a->a)
        apply right_implies [(a->a)]
        expect a |- a
        apply axiom [a]
    qed



## Sequent Calculus primer

### Sequents

A sequent is a logical assertion of the form

    Γ |- Δ

where

<table>
  <tr>
    <th>Symbol</th>
    <th>Name</th>
    <th>Description</th>
  </tr>
  <tr>
    <td>|-</td>
    <td>turnstile</td>
    <td>separates the left and right hand side</td>
  </tr>
  <tr>
    <td>Γ</td>
    <td>gamma</td>
    <td>A multi-set of conjunctive (and) formulas</td>
  </tr>
  <tr>
    <td>Δ</td>
    <td>delta</td>
    <td>A multi-set of disjunctive (or) formulas</td>
  </tr>
</table>

for example the sequent

    A, B |- C, D

can be read as

    if A and B are true, then either C or D is true

each of these variables A, B, C, and D could be formulas (such as X^Y),
or represent concepts we are dealing with in our logic.


When discussing particular sequents we often only mention the variables of
interest, for example consider the axiomatic sequent

    A, Γ |- A

which can be read as

    if A and some context are true, then A is true



### Inference rules

In Sequent Calculus we have inference rules allow us to manipulate sequents in
order to prove properties.

Each inference rule has a name including either "left" or "right",
"left rules" add things to the left side of the turnstile in their conclusion,
"right rules" add tings to the right side of the turnstile in their conclusion.


Consider the "right implication rule" (I have numbered the parts 1 through 4):

        (3)
         A, Γ |- B
    (1)------------- R-> (2)
         Γ |- A->B
        (4)

which has the following 4 parts:

 1. inference line, separating the top premise(s) from the bottom conclusion(s)
 2. rule name, in this case 'R->' is short hand for 'right implication'
 3. multiset of premises, in this case we only have one.
 4. multiset of conclusions, in this case we only have one.

we can view this inference rule as saying:

    if you give me a proof of the premises (3), this rule will construct a proof
    of the conclusion (4)

we can read the premise (3):

    A, Γ |- B

    if A and some context Γ (Gamma) is true, then B is true

and we can read the premise (4):

    Γ |- A->B

    if some context Γ (Gamma) is true, then A implies B is true

so taking this rule in its entirely, looking in the downward direction from
premise (3) to conclusion(4):

    if you have a proof of
        if A and some context Γ (Gamma) is true, then B is true
    then the right implies rule allows us to generate a proof of
        if some context Γ (Gamma) is true, then A implies B is true

this 'downwards' direction corresponds to constructive proof, building up more
complex proofs from simpler ones

conversely we can consider this inference rule in the reverse downwards
direction from the conclusion (4) to the premise (3), then we can read this as:

    if you want to prove
        if some context Γ (Gamma) is true, then A implies B is true
    then you have to first prove
        if A and some context Γ (Gamma) is true, then B is true

this 'upwards' direction corresponds to a proof search, breaking a complex proof
into smaller and smaller parts which can then be more easily proven.




## From Sequent Calculus to Gentzen

### Inference rules

An inference rule in Gentzen is stated 'backwards' when compared to the normal
sequent calculus formatting, this is because a proof in Gentzen can best be
viewed as a proof search - breaking down a complex statement into smaller parts
until there is nothing left to prove.

If we consider our above right implication rule as stated in sequent calculus:

      A, Γ |- B
    ------------- R->
      Γ |- A->B

in Gentzen we would represent this as:

    right_implies (a->b) (L |- R) = L+a |- R-(a->b)+b

this likely looks quite confusing at first glance, so lets try break it down by
numbering the parts:

    (1)           (2)    (3)        (4)
    right_implies (a->b) (L |- R) = L+a |- R-(a->b)+b

where

 1. our rule name
 2. the part of our rule we are focussing on, can be seen as the 'arguments'
 3. a pattern match of the sequent, L for left side of turnstile and R for right side of turnstile
 4. a rewrite/transformation rule for our resulting sequent(s)

If we have a current proof obligation (goal) of the form:

    x |- y->z

and apply this rule using:

    apply right_implies (y->z)

this would transform our proof obligation (goal) into:

    x, y |- z


The most complex part of this inference rule is the rewrite/transformation
rule (4):

    L+a |- R-(a->b)+b

notice how this looks like a sequent `L |- R` but with some extra junk,
all names in this are bound either from the argument (2) or from the
sequent (3) pattern match

the fragment `L+a` is read as

    start with the multiset bound by `L`,
    then add the formula bound by the variable `a`

the fragment `R-(a->b)+b ` can be read as

    start with the multiset bound by `R`,
    then remove the formula (a->b),
    then add the formula bound by `b`

So if we consider the proof fragment

    expect x |- (y->z)
    apply right_implies (y->z)
    expect x, y |- z

then we are binding the rule as follows:

    right_implies (a->b) (L |- R) = ....
                   ^  ^   ^    ^
                   |  |   |    |
                   y  z  [x]  [(y->z)]

which then makes our rewrite rule:

                                    [x] + y
                                    | |
    right_implies (a->b) (L |- R) = L+a |- R-(a->b)+b
                                           |        |
                                           [(y->z)] - (y->z) + z

this results in the sequent:

    [x]+y |- [(y->z)]-(y->z)+z

simplifying this gives us our remaining proof obligation:

    x, y |- z


### rewrite rule shapes

Gentzen supports a few 'shapes' for the rewrite rules.
All examples here are taken from `examples/hypothetical_syllogism.gtzn`.

Each rewrite rule can optionally specify a list of *propositions* to check,
followed by *zero*, *one*, or *two* remaining proof obligations.

Consider our G3ip axiom rule:

    axiom         (a)    (L |- R) = [(a in L), (a in R)].()

we can see here that we have a list of propositions `[(a in L), (a in R)]`,
followed by a `.` (period) separator, then the empty sequent `()` representing
that we have no remaining proof obligations for this proof branch.

If there are no propositions then we can omit the list and period, if there is
only one sequent then we may also omit the parenthesis, so these three
rewrite rules are equivalent:

    id1 () (L |- R) = [].(L |- R)
    id2 () (L |- R) = (L |- R)
    id3 () (L |- R) = L |- R


Alternatively consider our G3ip left implies rule:

    left_implies  (a->b) (L |- R) = (L |- a), (L-(a->b)+b |- R)

we have no propositions to check, but we do have *two* remaining proof
obligations which must both be proven.





