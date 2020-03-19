#lang racket


;; idea for the game:

#|
You exist in the space of all models (of some form) and can travel between models.
Your goal is to answer questions about proposed propositions:
    -- is this prop valid/invalid/satisfiable?
    -- are these certain props equisatisfiable?
(on a given level you'll have a handful of props to work on and they might depend on each other)

Along the way you have the means to change which model you're in at the moment (what records are in the store, go to the store that sounds good), in order to find counterexamples etc.
They shape the game and even determine the final proof!
You're also finding money and trying to build a dank record collection.

The Model is a Record Store!

Facts/Concepts to arise in rough order of discovery.
  C: means concept (introduced by game)
  A: means axiom (also from game)
  L,T: means lemma or theorem (player finds)
  Task: something the player needs to do.

Players get money(or records) from lemmas. First artist bought: all other records by them are always free.


Existential quantifiers and predicates

Player

C: There are albums, artists, and genres.
C: Albums are organized by genre, alphabetically by artist. within an artist the order is random.
L: There is a Miles Davis record!
C: Facts can be learned from albums.
L: Kind of Blue is a Miles Davis record!
A: We learn the personnel and the instruments they play.
L: John Coltrane played with McCoy Tyner.
T: There are jazz records, hip-hop records, and rock records

Task: put 5 records in your collection

C: Hip-hop records sample other records
L: Madlib samples jazz
Task: Buy a record sampled in a hip-hop record (put both records in your collection)

C: Sometimes artists go by multiple names. If two artists are on the same record, you can choose that they are
the same person. You might be wrong though and it may make certain tasks impossible at which point progress will become invalid.
 You can use any falsities that you can hang onto, however, to your advantage.


By now we have all connectives, and PEM. quantifiers are implicitly in play but not explicitly explained

Level 1:
Here are a bunch of facts to prove true or false. You may need to go to other stores to prove certain facts.
Before you get to start, you need to decide if Madlib and Quasimoto are the same person.
You can re-start this phase (and change your initial decision) as many times you want.

Here are some axioms: 
-- A: If two artists never work together on an album, then they are different artists
-- A: If two artists are the same artist and one artist is on a record, then both artists are on the record.

Here are some things to decide:
-- L: King Geedorah and MF DOOM are the same
-- 

Final theorem to prove:

-- Wayne Shorter never collaborates with Quasimoto



Along the way you learn new proof tactics/facts, here's a tentative order.
each group of things is a level
doubly indented things are propositions that can come up




Ideas for what the model should be:
  -- RECORD STORE

   -- Simple predicates aka what's in the model. more predicates and objects come up along the way
         -- Record(x), CD(x)
   -- disjunction
         --
   -- implication
   -- conjunction
   -- negation
  
   -- PEM!

   -- implication
   -- contradiction!

   -- existential quantifiers (other people)

   -- universal quantifiers

   -- entailment

   -- isomorphic models

   -- ... new models?
|#

;; Definitions of Models and Propositions.
;; Propositions are valid, invalid, or satisfiable.
;; Propositions are either true or false in a model.