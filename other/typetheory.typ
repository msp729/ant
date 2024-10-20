#import "@preview/curryst:0.3.0": rule, proof-tree
#import "@preview/touying:0.4.0": *

#let s = themes.metropolis.register(aspect-ratio: "16-9", footer: self => self.info.title)
#let s = (s.methods.enable-handout-mode)(self: s)
#let s = (s.methods.info)(
  self: s, title: [Type Theory], subtitle: [The Math Behind (most) Programming Languages], author: [Nate Brown], date: datetime.today(), institution: [],
)
#let (init, slides, touying-outline, alert) = utils.methods(s)
#show: init

#show strong: alert

#let (slide, empty-slide, title-slide, new-section-slide, focus-slide) = utils.slides(s)
#show: slides.with(outline-slide: false)

#let ind = "ind"
#let Type = text(font: "Atkinson Hyperlegible")[Type]
#let deduce(..x) = proof-tree(rule(..x))
#let ite(condition, iftrue, iffalse) = $mono("ite")(condition, iftrue, iffalse)$
#let pr = math.limits(sym.pi)

== So, what's the big idea?
#counter(footnote).update(0)
#let stnote = footnote[Sort of, it needs schemata (parametrized infinite families of axioms), or
  second-order logic, to be useful]

- Mathematicians love rigor, because that's how we avoid jumping to false
  conclusions.
#pause
- Starting in the 19th century, and kicking up in the 20th, there has been a
  project to "found mathematics."
#pause
- The most popular foundations are set theory

#pause
- While set theory doesn't need too many axioms#stnote, it is sort of difficult
  (strange, at any rate) to work with most things:
  #pause
  - The Von Neumann (standard) encoding of natural numbers is $0={}$ & $n^+=n union {n}$
  - Ordered pairs have a bunch of representations: $ (a,b) ≅ {{a,{}},{b}} ≅ {{a,1},{b,2}} ≅ {{a},{a,b}} $
  - A function $f(x)$ is the set of all pairs $(x,f(x))$, with a special property to
    make sure it's a function.

= The better (?) way
== The basics
#counter(footnote).update(0)

#let (zero, one, two) = ("0", "1", "2").map(math.bb)

In type theory, there are two#footnote[Dependent type theory unifies them, but that's beyond the scope of this
  presentation] "sorts of things" to be studied:
#pause
- A *type* is a kind of object #pause
  - $NN$, the type of natural numbers
  - $QQ$, the type of rationals
  - $two$, the Boolean type with 2 terms
  - $one$, the unit type with 1 term
  - $zero$, the empty type with no terms
#pause
- A *term*, on the other hand, is an object. Every term has a type:#pause
  - $0: NN$
  - $"True": two$
  - $"unit": one$
  - $not [exists x. (x:zero)]$

== The basics, pt. 2

Every type is defined by 4 kinds of deduction rules (examples given are for the
Boolean type, $two$):

#{
  set text(size: 0.9em)
  grid(
    columns: 2, rows: 2, gutter: 1em, [
      *Term Introduction* rules tell you how to make terms of a type.
      #grid(columns: 2, gutter: 1em, deduce($"True": two$), deduce($"False": two$))
    ], [
      *Type Formation* rules tell you when types exist.
      #proof-tree(rule($two Type$))
    ], [
      *Term Elimination* rules tell you what to do with terms.
      #deduce($(ite(c, a, b)): A$, $A Type$, $a: A$, $b: A$, $c: two$)
    ], [
      *Computation* rules tell you how the other term rules interact.
      #deduce($ite("True", a, b) = a$, $A Type$, $a: A$, $b: A$)
      #deduce($ite("False", a, b) = b$, $A Type$, $a: A$, $b: A$)
    ],
  )
}

== A strange problem

We know that there are two values in #two: $"True" & "False"$.

To reiterate, $ite("True", a, b)=a$ and $ite("False", a, b)=b$.

Example function: $f = x |->^two ite(x, "True", "False")$, $f(x)=x$

#pause
Now, given Boolean variables $x: two$ and $y: two$,
#pause
+ How do we compute $not x$ ($x'$, $overline(x)$, $(mono("not") x)$)?
#pause
+ How about $x and y$ ($x y$, $x mono("and") y$)?
#pause
+ Maybe even $x or y$ ($x+y$, $x mono("or") y$)?
#pause
+ Can we prove that $x or not x$? (probably!)

== Something a little bit funnier
An ordered pair (a.k.a. a *product type*, named after category theory):

#grid(columns: 3, gutter: 5em, [
  #deduce($(A times B) Type$, $A Type$, $B Type$)
  #deduce($(a,b): A times B$, $a: A$, $b: B$)
], [
  #deduce($x_1: A$, $x: A times B$)
  #deduce($x_2: B$, $x: A times B$)
], [
  #deduce($(a,b)_1 = a$, $a: A$, $b: B$)
  #deduce($(a,b)_2 = b$, $a: A$, $b: B$)
])

== Something a little less funny
A *sum type* (also named after category theory):

#grid(
  columns: 3, gutter: 5em, [
    #deduce($(A + B) Type$, $A Type$, $B Type$)
    #deduce($pr_(f,g)(x): C$, $f: A -> C$, $g: B -> C$, $x: A + B$)
    #deduce($pr_(f,g)(iota_1(x)) = f(x)$, $x: A$, $f: A -> C$, $g: B -> C$)
  ], [
    #deduce($iota_1(x): A+B$, $x: A$)
    #deduce($iota_2(x): A+B$, $x:B$)
    #deduce($pr_(f,g)(iota_2(x)) = g(x)$, $x: B$, $f: A -> C$, $g: B -> C$)
  ],
)

== Something very funny to finish it off

#let (u1, u2) = ($iota_1 ("unit")$, $iota_2 ("unit")$)
#let (Tr, Fa) = ("True", "False")

Prove (!) that $one + one ≅ two$.

Specifically, give two functions $f: one + one -> two$ and $g: two -> one + one$,
and prove that $f compose g$ and $g compose f$ are both identity functions.

Important equations: $ite(Tr, a, b)=a$, $ite(Fa, a, b)=b$, $pr_(f,g)(iota_1(x))=f(x)$,
and $pr_(f,g)(iota_2(x))=g(x)$

Hints:
+ #pause They're both inductive types with two constructors (${Tr,Fa}$ vs ${u1,u2}$
+ #pause A useful function (really $K_x$ is shorthand for $y|->x$): $K_x (y)= x$
+ #pause My $f$ was $pr_(K_x,K_y)$, for a certain choice of $x$ and $y$.

== My answer (there is one other answer, but it's very similar)

#[
  #set text(size: 0.8em)
  $ f = pr_(K_Tr,K_Fa) " and " g = x |->^two ite(x, u1, u2) $

  #set text(size: 0.9em)
  $ f(g(Fa)) = f(ite(Fa, u1, u2)) = f(u2) = pr_(K_Tr,K_Fa)(u2) = K_Fa ("unit") = Fa $
  $ f(g(Tr)) = f(ite(Tr, u1, u2)) = f(u1) = pr_(K_Tr,K_Fa)(u1) = K_Tr ("unit") = Tr $
  $ g(f(u1)) = g(pr_(K_Tr,K_Fa)(u1)) = g(K_Tr (u1)) = g(Tr) = ite(Tr, u1, u2) = u1 $
  $ g(f(u2)) = g(pr_(K_Tr,K_Fa)(u2)) = g(K_Fa (u2)) = g(Fa) = ite(Fa, u1, u2) = u2 $
]

== And a final thing:

I just wanted to point out that you *can* prove all of the following:
- $X times Y tilde.equiv Y times X$
- $X + Y tilde.equiv Y + X$
- $one times X tilde.equiv X$
- $two times X tilde.equiv X + X$
- $(X + Y) times Z tilde.equiv X times Z + Y times Z$

- $X -> (Y -> Z) tilde.equiv (X times Y) -> Z$ (hence why $X -> Y$ is sometimes
  written $Y^X$, so that this becomes $(Z^Y)^X tilde.equiv Z^(X times Y)$)
- $zero + X tilde.equiv X$

= #box(image("pitbull.jpeg"), height: 1em) Thanks for your time!
