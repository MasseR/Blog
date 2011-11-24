---
title: Software testing course - Part I
tags: testing,ruby,school
---

Turku university is having a software testing course (ohjelmistotestaus) for
computer science students. This year the course is held by two employees of
Lindorff Oy, and as far as I understood this is the first time the course is
held like this. Their plan was to have two lectures, and four
[dojos](http://codingdojo.org/). Aside for those, we are supposed to keep a
learning diary or a blog, do a lecture on a subject for our groups and write a
proper text from our subject. As you might have guessed, I chose my format to
be a blog post.

There is no much point to say anything overtly detailed about the lectures, as
they were just some general information about testing in general. Their point
seems to be to take our mind of programmer-centric testing mindset to a proper
testing-centric mindset. I believe this is why emphasized that unit testing is
not testing (at least for this course).

## Dojo

My presentation is available in [here](/static/data/cucumber.markdown) and the
feature file is in [here](/static/data/player_add.feature)

I haven't been in any other dojos before so I'm not sure how they usually go. I
believe that as this was first and foremost a learning event, we had a minor
twist to it. Every single one of us had an expert subject we were supposed to
'master' and present and teach it to our home group. Two subjects are to be
taught per dojo, where the first two were
[ATDD](https://en.wikipedia.org/wiki/Behavior_Driven_Development) and
[Cucumber](http://cukes.info). Cucumber was also my subject. I don't mind
reading a lot of theory and abstract stuff, but I believe I had grokked BDD as
well as I could, but what I wanted was hands-on experience with it.

The core idea of ATDD is to combine developers and clients. "Developer" can be
a broader term in this case, consisting even the entire team. Developers
(coders) like definitions such as "Class named Calculator with two methods, add
and subtract. The class has one private property to keep track the value.". Or
even better 'calculator.add 1  == 2'. Clients most likely have no idea what the
earlier definitions mean. What they understand instead is "Given that I have a
calculator and I add together 1 and 1 then the result should be 2".

If we can bring those client-level definitions to developers, developers can
use them as design aide (especially with agile development) or even possibly
parse them into real tests. Then to increase the level of granularity we can
increase the detail of the definitions and even go as deep as unit tests. The
benefit is then that both the developers and clients speak the same language.

The client-level definitions are already a step forward, but Cucumber and
Gherkin brings it home. Gherkin is the language that is parsed and ran with
Cucumber. Gherkin is formatted text, but it is as easy to read as for example
markdown.

~~~
Feature: Addition
    In order to avoid silly mistakes
    as a math idiot
    I want to be told the sum of two numbers
~~~

The `Feature` keyword is basically white space, just providing something easy
for the eye to focus. The text after it gives a short description of the
feature. This section is not parsed and is only for visual reference.

~~~
Scenario: Add two numbers
    Given I have entered 1 into the calculator
    And I have entered 2 into the calculator
    When I press add
    Then the result should be 3 on the screen
~~~

This is an example of a scenario, exactly the thing that closes the bridge
between developer and a client. Client gets a detailed story of what should
happen given that something is done. The `Scenario` keyword starts a scenario,
but as with feature it's still basically white space. The same goes for the
small description after it. After that, on the next indent level comes a couple
of keywords, which do have a purpose. The `Given` keyword is meant to
initialize the state of the system, `When` is meant to describe the actions and
`THen` is supposed to show the expected result. There was also a line starting
with `And`, but it basically just continues the previous keyword. In this case
it's parsed as `Given`.

~~~
Given /I have entered (\d+) into the calculator/ do |n|
    @calc.push n.to_i
end
~~~

The scenario is parsed and then executed with cucumber DSL. The DSL is closer
to the developer and clients should probably never see it. The tests are then
ran with the cucumber binary.

In the previous example we gave the values manually, but we can use a table of
values. This allows us to have plenty of test cases, while still being easy to
read. And thanks to Gherkin and Cucumber, we don't need to worry about how and
where the data is coming from.


~~~
Scenario Outline: Add two numbers
    Given I have entered <input_1> into the calculator
    And I have entered <input_2> into the calculator
    When I press <button>
    Then the result should be <output> on the screen

Examples:
    | input_1 | input_2 | button | output |
    | 20      | 30      | add    | 50     |
    | 2       | 5       | add    | 7      |
    | 0       | 40      | add    | 40     |

Scenarios: Adding zero keeps the same value
    | input_1 | input_2 | button | output |
    | 20      | 0       | add    | 20     |
    | 0       | 20      | add    | 20     |
    | 0       | 0       | add    | 0      |
~~~

There is about 20 of us and we are separated into two home groups. One of us,
the one who had studied the tool, is the technical expert. Other than that I
believe it's normal dojo, one sensei, two people, driver and copilot, on the
computer and rest are discussing on the side, without interfering with the two
people on the computer. The driver is 5 minutes on the computer, after which
the copilot takes the seat and the next person from the "buffer" becomes the
copilot.

# The task

As for the task for this dojo, our "client" wanted to create a bowling system.
We only got one feature, which was initially described as "we want the user to
be able to create a new player with firstname, surname and a picture. The
firstname and surname are mandatory but the picture is optional. If a picture
isn't given a default picture should be used."

I'd like to go through the session part by part, but as I only have the final
feature file, I'm not going to do that. Instead I'm just going to link it
[here](/static/data/player_add.feature) and parrot a few comments from the
teachers.

The first comment was of the format. Usually the feature is described as "In
order to <x>" and the scenarios as "Given I <y>". I trust their word on this,
altough I can't see why the format would be that important.

I don't agree with the second one, mostly because it's different from the
[references](https://github.com/cucumber/cucumber/blob/master/examples/i18n/en/features/addition.feature)
I used. Notice how the state is initialized in the `Given` part and the action
is described in the `When` part. They would have preferred to set the initial
state in the `When` section. Of course I could have misunderstood what they said.
