# Writing Notes in a RELEASE.txt File

## Introduction

We have been challenged to write more helpful entries in our RELEASE.txt file
for new features, improvements, and bug fixes. To help users determine the
effects of our software changes on their applications, we have developed a
template that can be used to write release notes. The template is described on
the rest of this page.

Some of our users face scrutiny from regulatory agencies for their use of
software they did not develop (aka SOUP). When they use our software, they have
to be aware of every change we make. For every change we make, they need to
investigate the effect the change will have on their software. If our change
affects their application, they may have to have their software retested or
re-certified.

[SOUP](https://en.wikipedia.org/wiki/Software_of_unknown_pedigree) stands for Software Of Unknown/Uncertain Pedigree/Provenance


## When to write a RELEASE.txt note

Generally, a release note must be written for every change that is made to the
code for which users might see a change in the way the software works. In other
words, if a user might see a difference in the way the software works, a note
should be written. By code we mean the text that will be compiled into one of
the company's software products. The code includes configuration changes and
changes to tools users might work with to configure and build our software.

A release note does not need to be written for changes to the code that users
will not see.

These things DO require a release note:

* New features
* Changes in functionality/semantics
* Anything that changes functionality or symbols in a public header file
* Command-line tool option changes
* Adding or dropping support for compilers or operating systems
* Build system (Autotools, CMake) changes that users will encounter

These things do NOT require a release note:

* Internal comments
* Refactoring that does not change behavior or public headers (`H5XYpublic.h`)
* Minor build system changes (adding warning flags)

Notes should also be added for known problems. Known problems are issues that
we know about and have not yet been able to fix.

## RELEASE.txt entry format

```
    - Title

      Problem

      Solution

      Signature
```

## RELEASE.txt entry elements

### Title

The title or tag should identify one or more categories that will help readers
decide if the entry is something they need to study. Categories include problem
areas, tool names, and code file names. Two examples are "Memory Leak" and
"h5repack". A code file such as H5R.c or H5Z.c can be used as the title if the
problem is located in the code file. If both a title and one or more tags are
used, separate each with a period. For example, "Changed Autotools Build Behavior. Fortran."
adds the Fortran tag to the title Changed Autotools Build Behavior. Use
standard capitalization rules.

### Problem

Describe the problem and how users might see the problem in a paragraph.

You might also consider the following as you describe the problem:

* Under what specific conditions does this issue arise?
* Under what specific conditions are we sure this issue will not arise?
* For a performance issue, instead of saying something is a performance issue, describe what the performance impact of issue is?

### Solution

Describe the solution in another paragraph.

You might also consider the following as you describe the solution:

* What was done to resolve the issue?
* What is the functional impact?
* Is there a workaround – a way for users design their software so as not to encounter the issue? If so, what is the workaround?
* For a performance fix, how has the performance improved? Links to published documentation would be good.

### Signature

Each entry must be signed with the initials of the author, the date in
YYYY/MM/DD format, and the JIRA ticket number. The signature is enclosed in
parentheses.

Example:

```
    (ABC - 2023/02/28, JIRA-12345, GitHub #1234)
```
