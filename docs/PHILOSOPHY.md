> **⚠️ ARCHIVED - This documentation is from an archived repository.**
> **For current documentation, visit:** https://github.com/forthix/forthic

---

# Philosophy of Forthic

Every language has a perspective -- some want to guarantee correctness, some want to guarantee safety, some are focused on conciseness, some on readability. **Forthic is focused on tweakability at the conceptual level of an app.**

Forthic wants to be written as high and as close to the user as possible. It's not good at dealing with the details. In a Forthic application, the details -- the twisty, jagged logic -- is handled by its "host language" (the language that the Forthic interpreter is built in).

Ideally a word should be defined by one line of Forthic. Each word in that definition will itself be defined in terms of other words (some defined in the host language), but there should be a single line that pulls everything together. Most importantly, the correctness of that line should be self-evident.

## Run in straight lines
Since Forthic programs are composed of words defined by single lines of code, code tends to flow in straight lines. There's a sequence of things that need to be done, and each thing that needs to be done should be expressed as a word. For example, this pulls data for the specified date and renders a report for it:
```
2021-03-05 CONTENT-FOR-DATE RENDER
```
There aren't any branches here. It flows in a straight line. If different cases need to be handled (missing data for a date or show confidential data only for certain users, for instance), this should be done by the component words. For the example above, `CONTENT-FOR-DATE` would be a good place to do this:
```
: CONTENT-FOR-DATE   (date !) GENERATE-CONTENT "Can't find content for date" DEFAULT;
```
Note that even though we're handling the case where the content is empty, the definition still runs in a straight line. In Forthic we treat special cases as temporary deviations from the line. Deviations should be handled as quickly as possible so control returns back to straight line flow. 


## Create styles to suit situations
Forthic's focus on flexibility comes not from providing you with lots of features but by removing lots of restrictions. That means that Forthic is intended to be written by people who are experienced writing software in many different styles and using many different techniques. If you know what you're doing, Forthic allows a surprisingly wide range of expression.

You can write functional code:
```
USERNAMES "USERNAMES>TICKETS" MAP
```

You can write object-oriented code:
```
REPORT <ADD-SUMMARY-SECTION <ADD-DETAIL-TABLE <ADD-CONTACT-INFO PUBLISH
```

You can write asynchronous code:
```
EXPENSIVE-PROMISE  "REPORT" <THEN   "FAILED-REPORT" <CATCH   AWAIT EMAIL-REPORT
```

You can even change the semantics of words so the appear more like English than RPN: `RENDER DOCUMENT` instead of `DOCUMENT RENDER`, for instance.

You can write code that writes code, you change redefine words on the fly, dynamically manipulate modules. And if there's something you can't do in Forthic, you can hop over to the host language and define new words that add new functionality or that can change the behavior of other words or that create new literals that make sense for your situation.

Forthic isn't a beginner's language. If you don't have experience writing functional code, object-oriented code, asynchronous code, or writing code in general, then you won't have the background for writing good Forthic programs (at least from scratch).

**However**, tweaking an existing Forthic program can be (and has been) done by beginners. When Forthic code is written at a high level and close to the user, an existing Forthic program is actually easy for anyone to tweak. At LinkedIn, we've seen this happen literally hundreds of times with TPMs along the entire spectrum of software coding experience.


## Leverage the host language
Unlike most languages, Forthic isn't meant to be run on its own. It needs a host language, and it needs its host language to be close at hand -- but it needs to have a very clear dividing line between the two.

The division between Forthic and the host language leads to simpler code on both sides of that line. By keeping the details out of the application layer, the application layer stays simpler and more conceptual: words do you what you want. But the host language layer becomes simpler, too, because each chunk of code is focused on doing one, specific thing. The code becomes more composable in the host language because all of the code coordination happens in Forthic. This approach simplifies both sides because it removes whole classes of code/logic from both sides -- fewer possibilities means fewer states, which means less complexity.