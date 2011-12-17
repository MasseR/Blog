---
title: Software testing course - Part II
tags: testing,ruby,school
---

So we had our second dojo. This time the focus was on UI testing and the tool
we used was [Watir](http://Watir.com/). Watir is one of the web ui testing
tools along with [Selenium](http://seleniumhq.org/)

# UI testing

Code is easy to test. We can reason it, and observe the effects. We can test
that `reverse(reverse(x)) == x`, but it is much more difficult to test 'the
page should contain a welcome message'. For humans the test would be easy as we
can reason, but it is much more difficult for a computer.

UI testing is clearly different from traditional unit testing etc. and one
might even argue that the benefits aren't big enough. Software rigidity can be
thought as layers, where in the bottom layer is the framework/library code
which after done, rarely gets modified. Above it is the business logic which
evolves as the software evolves. On top of that is the UI layer which is so
volatile that it might change at a moments notice. This is of course over
generalization and crude estimation, but by my experience it seems to hold
true.

With Watir and selenium the basic idea is that we have a web browser driver
that responds to our commands, can navigate to addresses, can click things,
type things etc. After simulating what the user would do, we can assert that
some elements are present, or some other conditions are met. Usually the moving
tasks are also assertions, failing when the task can't be finished.

~~~{.haskell}
main = withSelenium (mkSeleniumRCSession "localhost" Firefox duckduckgo) $ do
  open "/"
  typeText (Id "hfih") "selenium"
  submit (Id "hfh")
~~~

This doesn't solve the problem of finding your elements perfectly.  For example
in selenium you can use your normal id, name, css and link selectors as well as
xpath, but the only really reliable is the id. With xpath you don't necessarily
need to have some certain identifier for an element that wouldn't otherwise
need it, but then again, moving the element would break the test.

Add to this the ambiguous nature of user stories and we find ourselves in a big
mess. This is exactly the situation that wound up in the dojo where the
scenario described that the front page should display a welcome message. For a
human this test would be easy, we can recognize various kinds of welcome
messages without trouble but with a computer, we need some help. Can we somehow
scrape the welcome from the page? Does it always contain the word "welcome"?
What if it's translated? The easiest solution would be to ask the developers
put a descriptive id for the element and it still wouldn't be a perfect
solution. Imagine that we found the element. What should we assert? Should we
assert the text "welcome"? What if the welcome message is "Nice to see you
back"? Should we check that the element is not empty? What if the developers
accidentally put the player data in there instead of the welcome message?

Also the html might not be easy to navigate even with proper identifiers. In
the dojo we were supposed to test that the players page had a list of players.
The html itself was difficult to read and reason (mix of tables and lists
IIRC) and we couldn't figure out a sure way of checking that the element had
players in it and not just empty rows and headers. Granted this was mostly
because of our ineptitude with Watir and ruby.

The rest of these musings aren't necessarily from the dojo, but more from my
experiences.

When writing automated tests, we want to have full control of the environment.
This way we can replicate the behaviour over and over again. Another benefit of
it is that we don't contaminate the data. This is equally important with UI
testing, but it hinders our testing procedure. With unit tests we can test the
system in small isolated parts, but with UI testing we are testing the system
as a whole as the user would see it.

This brings another annoyance with UI testing. Ideally after each test we would
clear the system and rebuild it. Imagine we're testing a system that's mostly
behind authentication. After each and every test we would need to relogin and
navigate to the page we were trying to test. Even if we were to refactor some
common actions, it would still take a lot of time. This takes me to my second
pain point.

Two of my greatest benefits of writing automated tests is the fact they provide
a safety-net with refactorings, especially with dynamic languages (I'm looking
at you PHP!), but that they also provide rapid feedback. I can press <F6> and
see the tests running with either 'E', 'F' or '.' indicating whether it works
or not. The alternative is that I code, navigate to the browser, navigate to
the form I'm making and fill the form and click send. Repeat for different
kinds of input. It gets even worse when I need to try some behaviour that the
browser doesn't directly allow, prompting me to start curl or netcat.

UI tests give me only the first. The browser is slow to start, page loads
aren't insignificant and there is always the overhead of navigating to the
correct place. Optimistically this would probably take each test a second a
minimum. Compare this to unit tests where running hundreds of tests takes only
seconds.
