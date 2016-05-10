---
title: Hakyll is Fun!
date: 2016-05-06
---

Now that the semester is over, and I can finally take a break from the constant barrage of coding
assignments and problems sets that any CS student knows all too well, I decided that I'd take a bit
of time to update and add more content to my personal website. I figured that it'd be nice to have
some place where I could write things and put them online once in a while, so a blog seemed
appropriate.

When I first decided to actually go ahead with this, my first instinct was to simply go through my
old routine that I had developed many years ago: Download WordPress, install it on a local server,
make a WordPress theme, install plugins, push it to a live server, write things, then spend the
next year dealing with security updates and broken plugins.

This time, however, I decided to do
something different, and use [Hakyll](https://jaspervdj.be/hakyll/), a Haskell library for
generating static sites. Despite having done WordPress theme and plugin development for many, many
years, I decided to give Hakyll a try instead for a number of reasons. The main reason is that
WordPress really is overkill for a simple site like this. WordPress makes sense for a lot of larger
sites that actually use most of the features and plugins that WordPress has to offer (I don't see 
myself needing an ecommerce platform anytime soon).

After spending a lot of time developing themes
and plugins for WordPress, I've also decided that I'd rather avoid using PHP and MySQL as much as
possible, which I don't think requires any further comment.

My main reason *for* actually using Hakyll is simplicity: All I need (and all that most websites
need, really) is a way to edit content and display it online, and so a static site generator just
makes sense. No need to deal with database errors at inopportune moments. But I'm pretty
late to the static site generation game anyway, so I'm sure you already know why it's great.

The other reason is that it's an excuse to do a bit of coding in Haskell. I've used
functional programming before, whether it's in lisp, somewhat-functional features of Python, or
trying to emulate functional programming in C for an OS assignment on file systems. Taking the
Programming Languages course at UofT has opened my eyes to the ways of statically-typed
pure functional programming. I don't think I could pass up any opportunity to learn more Haskell,
and actually do something fun with it. So far, I've made some customizations like [cleaning up the
URLs](https://www.rohanjain.in/hakyll-clean-urls/) that Hakyll generates to remove the `.html`
extension, and [using LaTeX to render math](http://travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html)
using MathJax. So now I can typeset nice-looking equations like this:

$$\int_{-\infty}^{\infty} e^{-x^2}\, dx = \sqrt{\pi}.$$

Over the next while, this site will probably be somewhat of a playground for experimenting with
Hakyll. As I fiddle around with Hakyll some more, I may write more posts going into more detail
about my Hakyll setup.

One consequence of using a static site generator is the lack of a comments section, which I don't
particularly mind. I could always add one later if it ever feels necessary, using something like
Disqus. Of course, I am happy to receive any comments; if you would like to contact me, my email
is on [my home page](/). If you have any suggestions related to this website, the source code, along
with all of my posts, are [hosted on GitHub](https://github.com/aldld/ericbannatyne.ca), where you
can submit issues and pull requests and all that, if you'd like.




