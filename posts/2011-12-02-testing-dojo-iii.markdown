---
title: Software testing course - Part III
tags: testing,ruby,school
---

Today was the third dojo. Unlike the last time when we relaxed the rules and
the rest of the group was also actively advising the driver and co-pilot, this
time the rules were enforced. The subject this time however was extremely
interesting and close to my heart. This in turn helped to keep me active in my
own way, keeping an eye of what they're doing and taking notes. I was last to
go and thanks to that I was able to get a couple of ideas that I could do. I
believe we got to the heart of dojos this time.

The topic this time was exploratory testing, and at some level it's something
that every developer has done at some point when hunting down a bug.
Exploratory testing and the verbs associated to it are just a nice cover for "I
want to see how this works and break it". You have an idea what might go wrong
and you try to make it go wrong.

The task was presented to us as a challenge, to find bugs in our university web
site. It was worded something along the like "you will not find anything, as
there aren't any bugs". I don't know about the others, but it got me all riled
up, thinking of ways to break it.

I try to be mindful of the possible problems and security risks that are
involved in my web development job. I'm not the smartest man in the world but I
try to keep up with the risks involved and this challenge provided me means to
test my knowledge. I was expecting that the most touted security risks were
taken care of by now, but little did I know of the multitude of problems that
might exist elsewhere.

Our group had a similar mindset to mine, or I influenced their mindset to be
similar to mine. Either way, we put our force into finding concrete bugs and
errors. This led us to find any kind of forms and test their input. We tested
for 'random data', wrong data types, manufactured data, data with html, and
most interestingly numeric data in different formats. Most of our tests were
negative, but there were some points that really showed up.

The one error/bug that in my mind was more prominent was with the donations.
The help told that "0 or above" is allowed, so we went to test with characters
which failed the validation. Then we tried with insanely large numbers and
tried to use the checkout which are directly linked to Finnish banks. They
behaved inconsistently, but it was out of our scope. We tried negative sums
which failed the validator, and then came the interesting. We tested to input
'0', which failed the validator. Then we tried to input '0.5' which succeeded.
Then we went on to try '0.000001', which also succeeded but was rounded to
'0.00'. In effect 0 was forbidden, but 0.00 was allowed.

Another failing form was feedback which didn't really surprise me. They're
usually an afterthought and behave as such. The feedback form required the
feedback to filled, and raised an javascript `alert` if it wasn't. Javascript
disabled sent the empty feedback flying through the tubes.

Our mindset was completely different from the other group. They tried to put
themselves in the shoes of new students and exchange students and tried to find
information from the pages. This was another look at the matter, and like the
teachers said, the "right" viewpoint depends on the situation.

What really separates exploratory testing from just having fun trying to break
the system, is that you should have a clear goal of what you're trying to test
and spend your resources (time, money) on it. Our goal was checking the
application logic, and their was checking the consistency and usability. You
should be able to answer the question "has x been tested?" with a clear yes or
no. If you approach the problem with no goal, you can always think of something
else to test and never finish.
