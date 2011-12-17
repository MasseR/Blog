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
software. The groups were then divided into pairs, where the other
takes notes and the other tests. I was in the half that tested the
website. This time around we made some clear goals (charters) and kept
with it. Taking record of what we do and time spent with it. I admit
that I'm a total liar at this point. Me and my partner had a clear
goal yes, but we didn't record everything we did. We recorded when we
started, when we stopped and every issue and/or bug we found. Although
to be honest, there were so much issues that we might have as well
been recording what we do with the issues. There wasn't a page where
something wasn't amiss.

Our charters were divided for the subgroups evenly. Or rather we
created a couple of charters with different areas, with the subgroups
taking a different area each. Some of the charters were the
differences between Finnish / English sites and their content and
testing the forms.

From our experience from the last dojo, we knew that the translations
were a bigger problem than the forms, so we started with it. We
decided to keep fifteen minutes intervals and then switch, with a
total of three switches and a total duration of one hour and fifteen
minutes. We were just able to finish our third turn when lunch break
started.

After coming back, a surprise awaited us. We were supposed to switch
the groups, those who tested the website were supposed to test the
android software and vice versa. This surprisingly was tricker than we
thought. We had no idea what the software was, what the charters were,
and how well the previous sessions had been done. So we did as we best
saw fit, continued the charters that the previous group had started,
but found nothing.

On the other hand, we were able to find issues outside of the scope of
the charter. Actually the problem we found, was a tricky one, or
rather, a tricky to find. If there is a problem with the network and
the software tries to update itself, it will not timeout. After
thinking it through, there might be more to the issue than that. I
believe the problem was caused because the device was still connected,
meaning that it had connection to the access point, but the network
there uses web authentication, and that authentication had expired.
Every http request is redirected to the login page, but what happens
to other requests is uncertain. I've noticed that if I try to connect
to ssh without logging in, it will try to initiate the connection for
a long time.

Me and my pair found out during the debriefing that while we had
finished one charter (and continued the first android one), the other
groups had finished nearly all of their charters. Other thing our
group missed was the times, but thankfully we knew when we started and
remembered when we went to eat so all was not lost. One more thing to
note is that like I mused in the last post that having a clear goal
would help, this time with a clear goal, I can say that it _will_
help.

As the course comes to an end, I must bid farewell for the lecturers,
it was great. I had much fun, and learned a lot, even if I might never
be able to put it into proper use.
