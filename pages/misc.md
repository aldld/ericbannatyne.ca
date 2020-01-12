---
title: Miscellaneous
---

Things I'm Learning
-------------------

- **Programming Languages:** In a broad sense, I'm currently interested in
  languages that are good for systems programming, while providing strong type
  safety guarantees. To that end, I've started learning about Rust, and will
  likely explore using the language to implement a few projects. Otherwise, I'm
  interested in going deeper into the world of functional programming, including
  areas of computer science that make use of applied category theory to solve
  real-world problems. For that, I'm learning some more about Haskell, in
  addition to the more functional side of Scala.
- **Software Design and Architecture:** This is a pretty broad area. I'm not
  looking to learn basic patterns, principles, and techniques, which I'm already
  quite familiar with. Rather, I'm interested in the slightly more
  human-oriented aspects of software engineering involved in defining high-level
  architectural goals for a team to work towards, along with the process of
  improving existing codebases to be conducive to writing robust, modular,
  performant, and scalable code.
- **Distributed Computing:** During my undergrad, some of my favourite courses
  have been about distributed computing. However, these courses have tended to
  be more theoretical in nature. This includes algorithms in various synchronous
  and asynchronous models of distributed computation, along with lots of really
  cool lower bounds and impossibility results (e.g. variations on Consensus
  problems). A lot of my current work involves a fair amount of solving problems
  in a distributed setting, and I'm looking to take a more focused approach to
  learning the details of solving real-world problems in large-scale distributed
  systems. This can involve both applying some of the algorithms I've studied
  previously in practice, as well as learning techniques in new problem domains,
  which relax some of the constraints of theoretical models (e.g. CRDTs).
- **Bike Maintenance:** This one should be relatively straightforward. Right now
  I'm pretty clueless about anything related to bicycles beyond simply riding
  them. I figure if I can keep my bike in good condition, and take care of basic
  repairs on my own, I'll be much more inclined to get more use out of it.

A Few Things I've Worked On (So Far)
------------------------------------

Outside of my current job, I've also worked on several other things, especially
while I was a student at the University of Toronto. Here's a few of them:

- [auction-algos](https://github.com/aldld/auction-algos): Implementation and
  visualization for an
  [auction algorithm](https://en.wikipedia.org/wiki/Auction_algorithm) for
  assignment and network flow problems, initially proposed by Dimitri Bertsekas.
  The algorithm is implemented in Python, with step-by-step visualizations
  implemented using D3.js in a Jupyter notebook.
- [The Tree Evaluation Problem](/posts/tree-evaluation-problem/): While I was at
  UofT, I dabbled in a bit of undergraduate research, including some research in
  the field of computational complexity theory. My research focused on studying
  the complexity of the Tree Evaluation Problem, a computational problem that
  tells us about the relationship between time complexity (the number of steps
  required to solve a problem) and space complexity (the number of bits of
  memory needed to solve a problem), by studying the branching program
  complexity of a class of closely-related problems.
- [hasp](https://github.com/aldld/hasp): An interpreter for a variant of Lisp
  written in Haskell. This includes a custom implementation of a tokenizer and
  parser, in addition to logic for evaluating parsed lisp expressions. I wrote
  [a blog post about it](/posts/hasp-lisp-interpreter/), which goes into a bit
  more detail about the design and implementation of hasp.
- [Paper2LaTeX](https://github.com/aliang8/Paper2LaTeX): A PenApps hackathon
  project, applying computer vision models to transcribe hand-drawn graph and
  diagram images and generate output in TikZ format for use in LaTeX documents.
- [lip-reading](https://github.com/aldld/lip-reading): Models for performing
  visual speech recognition, i.e. lip reading from video. This was built as a
  final project for the course CSC412: Probabilistic Learning and Reasoning at
  the University of Toronto. Our implementation combines computer vision
  techniques with hidden semi-Markov models for predicting sequences of words
  using only video.
- [SyncOnSave](https://github.com/aldld/SyncOnSave): Sublime Text 2 plugin that
  automatically syncs directories with a remote server when saving. This is a
  small but useful wrapper around `rsync` I had put together at UofT, for when I
  wanted to edit code for assignments locally on my laptop, but needed to build
  and run them on the school's remote CDF servers.

Places to Find Good Coffee
--------------------------

### San Francisco & Bay Area

- **Verve:** Excellent coffee, great food, and aesthetic cafes. While I tend to
  prefer single origin coffees, Verve is also able to produce a lot of really
  interesting blends to brew at home.
- **Backyard Brew:** A really cool and super friendly outdoor cafe on Cal Ave in
  Palo Alto. It's been great seeing them expand their cafe and roasting
  operation over the past few years.
- **Equator Coffees:** A great source for light roasted single origin coffee
  from around the world. There's always a lot of coffees to choose from to brew
  at home, including the option to splurge on fancier Gesha coffee every once
  in a while.
- **Cat & Cloud:** I've never actually been to their cafe in Santa Cruz, however
  their beans always feature prominently in my regular rotation.
- **Saint Frank:** A nice place to sip a tasty espresso while waiting for the
  train, at least on days when I wake up early enough.
- **Hole in the Wall:** A coffee window in north beach, good for cold brew on a
  warm summer day.

### Vancouver

Vancouver is a great city for specialty coffee, especially if you're not too far
from downtown. If you're looking for good coffee in Vancouver, check out
[Vancouver Coffee Snob](https://www.vancouvercoffeesnob.com/). That site is one
of the main ways I find out about new coffee places to check out whenever I'm in
town.

- **Aubade Coffee:** I'm sad to hear that the Aubade cafe in Chinatown (inside
  of an antique store on Pender street) is closed, and that I only got to go
  there once. It's quite a spectacle to watch the owner meticulously prepare his
  championship Aeropress recipe, in which he presses coffee into three different
  containers at different points in the extraction, and mixes them to a
  particular ratio.
- **Iktsuarpok:** Not really a coffee shop, but more of a window. Great place to
  grab an espresso from some really friendly folks.
- **Nemesis:** Great cafe that makes an excellent pour over.
- **Prototype:** Really cool roaster on East Hastings, with lots of fascinating
  equipment. Their steamed cold brew is unlike any other coffee I've had.
- **Revolver:** Super popular cafe when I went there, with beans from a rotating
  (or, *revolving*) set of roasters.
- **49th Parallel:** A very well-known roaster and cafe chain in Vancouver,
  which is also known for Lucky's Doughnuts. I'm a fan of the Peanut Butter and
  Jelly doughnut.
- **Matchstick Coffee:** They have a few locations around the city, which are
  great for doing some work while sipping a nice pour over.

### Seattle

- **Anchorhead:** Great for both espresso and pour over. I'm a big fan of their
  Burundi Muruta coffee.
- **Espresso Vivace:** A classic Seattle coffee roaster, whose owner---[
  according to Wikipedia](https://en.wikipedia.org/wiki/Espresso_Vivace)---is
  supposedly credited with popularizing latte art in the US.

### Toronto

- **Pilot Coffee Roasters:** I first tried coffee roasted by Pilot at Wisey's
  Pies, when they were on Eglinton. They also have a shop in Union Station.
- **Hot Black Coffee:** A great shop for espresso on Queen & University. This
  was my top go-to cafe when I lived downtown.
- **Coffee Lab:** This used to be a tiny coffee shop that was really easy to
  miss: it was basically just a counter tucked away inside a used bookstore on
  Bloor street, on the edge of UofT's campus. That bookstore no longer exists,
  but I suspect that Coffee Lab may have moved elsewhere (though I haven't had
  a chance to check it out yet).
- **Sam James Coffee Bar:** They have a bunch of locations around the city; I
  remember going to one somewhere in the PATH, under King Street, where I got
  what the barista called a "perfect" espresso. (Perfect shot time?)

### Los Angeles County

- **Menotti's:** Get up early, grab a sandwich from Eggslut, a latte from
  Menotti's, and enjoy your coffee and breakfast on Venice Beach.

### Ottawa

- **Happy Goat Coffee**

### New York

- **Parlor Coffee Roasters:** Great coffee roaster based in Brooklyn. While they
  don't seem to have an actual cafe anymore, they do have a tasting room that's
  open for a few hours every Sunday. It's a great place to enjoy delicious
  coffee while being surrounded by roasting equipment and giant stacks of coffee
  bags.
- **Hi-Collar:** A tiny Japanese cafe by day (and bar by night) that specializes
  in pour over, Aeropress, and siphon coffee, with delicious omurice, katsu, and
  mentai cream pasta. This place is worth waiting in line (out in the cold) for.
