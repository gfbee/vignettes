#lang racket #| Learning from History |#

#| Understanding of programming languages --- among programmers, educators, and researchers
    [even some in programming language theory!] --- declined in the mid-1990s to a low point
    in the early 2010s.

 I grew weary of arguments about programming languages, because of how willing many people
  were to argue about them without knowing much about any of them, let alone the history.

 But the situation appears to be changing, rapidly. Let's hasten that change, now that many
  of the attitudes and forces leading to the myths and misuderstandings are fading. |#

#| Why the change?

 Growth in machine power [memory, speed, affordability, availability, connectivity]
  expands the complexity and diversity of problems we solve by machine.

 The complexity and diversity of solutions exposes weaknesses in programming languages,
  especially weaknesses due to any biases towards particular domains or styles of expression.
  Many programmers, and programming language designers, fixate on “natural” modes of expression
  that don't generalize well. They make the mistake of thinking that a language being somewhat
  “unnatural” at the small scale [putting aside for the moment whether they're even correct
  about that judgement] will accrue that unnaturalness at larger scales. |#

#| Do language designers know what they're doing?

 Here's one language designer on why they initially omitted a construct [replace with “Y” here],
  partly based on their attitude towards another language [replaced with “X” here]:

  “I can't call it a purposeful omission since I didn't even know what they were (having
   always abhorred everything that even remotely smelled of [X] :-) but I don't think
   they would fit in well with the current language -- nor that they are necessary.
   In a recent post of Dan Connolly on www-talk he mentioned that [Ys] were rather
   complicated beasts for non-expert users (or something to that effect).”

 How confident that a decision made for those “reasons” was a good decision, let alone a good
  decision for how you want to write your programs?

 The quote is from: http://legacy.python.org/search/hypermail/python-1994q3/0645.html

 By the way, when you hear him, and others, judge something “Pythonic”, remember the above quote
  for what that means, according to the Python creator himself: based on lack of knowledge,
  prejudice, and hearsay. |#

#| Why “natural” syntax isn't.

 1. Constructs that a language designer wanted to add, but couldn't because of syntactic decisions
  made long before the language was used for:
   • significantly complex problems,
   • from a non-trivial range of application domains,
   • requiring a multitude of programming styles

   https://stackoverflow.com/questions/1233448/no-multiline-lambda-in-python-why-not
   http://python-history.blogspot.ca/2010/06/from-list-comprehensions-to-generator.html

 That illustrates a more reasonable interpretation of “Pythonic”: fits with earlier design
  decisions and their consequences --- nothing more, nothing less. Unfortunately, many
  programmers think it means “good”.


 2. A dozen groups can't be right.

  If they all disagree! If a dozen variants of a syntax each have a group arguing its naturalness,
   then we must conclude that most groups making claims of naturalness are wrong.

  ToDo: gather, some thankfully deleted now, various “front page descriptions” of the naturalness
         of various languages' syntaxes. |#
