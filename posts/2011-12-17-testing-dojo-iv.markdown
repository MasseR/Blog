---
title: Software testing course - Part IV
tags: testing,android,school
---

Friday the 9th marked the fourth and last installment of the testing
dojos. I said dojo, but this time the testing wasn't held like a dojo,
but instead like a regular group assignment. The class was divided
into two groups, the other continuing where we left the last time, and
the second half testing an android app whose name I can't remember.

### Recap to history

I was in the group that continued from the previous dojo, so I got a
good look on how the theory from this time related to what we did last
time. [The last time](/posts/2011-12-02-testing-dojo-iii.html) we
looked at exploratory testing. As a recap, it's a style of testing
where you don't necessarily know beforehand what you are testing. You
have an idea and you pursue it.

Exploratory testing probably feels familiar to every programmer, but
it has its downsides. Stemming from the fact that you don't have
certain goals, reporting gets difficult. Testing is difficult to sell
as is, but trying to sell testing to managers with reports such as "I
did some tests and found errors here and here" is even more difficult.
I'd like to say 'impossible' but as I have no real world experience,
I'll leave it out.

_Session based testing_ tries to remedy that. A quote from
[wikipedia](http://en.wikipedia.org/wiki/Session-based_testing) says
"Session based testing is a software testing method that aims to
combine accountability and exploratory testing to provide rapid defect
discovery, creative on-the-fly test design, management controls and
metrics reporting".

This is accomplished with strict roles and actions. Managers, who must
be well versed in what they do, assign tests for testers, who then do
the tests in a certain time period, recording what they do, what they
find and what they have spent their time on. These results are then
taken to debriefing with their manager who can then provide metrics
from the data.

We have now the metrics, but all is not well in the kingdom. Metrics
are notoriously difficult to get right, and even more notorious for
their ability to be misused. Don't get me wrong, the higher management
will rejoice when the metrics show that suddenly there are 90% less
errors, even if those metrics are produced during holiday seasons.

Another issue that came up during the class was that session based
testing is a beast. It brings us back to the early 21st century with
all the bureaucracy and slow moving cogs. There were some "solutions"
thrown into the air during the class, but to my ear they sounded
overly optimistic and unrealistic. "We should take the good parts of
it and make it more agile". But can we just take the best parts of
SBT? As I see it, SBT is already an answer to an extremely agile
method with bad parts, by bringing some rigidity and control. It's
probably not as easy as just "taking the best parts". But yet again, I
have no real world experience, so who knows.

### Back to testing

So the class was divided into two, other half testing [our
university](utu.fi) website, and the other half testing some android
software.
