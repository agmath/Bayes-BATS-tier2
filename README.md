## Bayesian Thinking, in Context

This README contains:

+ [an overview of the *Bayesian Thinking, In Context* project](#overview)
+ [instructions for how you can use our context-free activities to develop activities within your domain](#setup)

TL;DR -- If you want to contribute, or develop your own versions of these activities, there is an annotated script, `setup.r`, in the list of files above to help you.

<a id="overview"></a>

### Overview

This is a repository for activities that promote Bayesian Thinking across multiple disciplines. Initially, we created a collection of four activities that investigate concepts from a Bayesian framework. Those activities are:

i) *From the Prior to the Posterior*, how we can use observed data in conjunction with prior assumptions to obtain an updated world view.
ii) *Credible Intervals* [need better name], how we can extract an interval which is likely to contain a population parameter from a posterior distribution.
iii) *Bayesian Hypothesis Testing*, using and comparing posterior distributions to test claims.
iv) *Bayesian Hierarchical Modeling*, constructing and assessing hierarchical models to fit data and assess claims.

We then began working with domain-experts to develop these activities out into different contexts. For some collaborators this meant [I'm writing for the future here...] connecting the activities to a particularly interesting topic from a course. For others it meant an opportunity to introduce students to some of their active research.

<a id="setup"></a>

### How Can I Build My Own?

Generally speaking, you have two options for how to start building your own versions of these activities. You could reach out to Adam ([a.gilbert1@snhu.edu](mailto:a.gilbert1@snhu.edu)) or Laura (Laura's address here, with permission) to be given your own branch within this repository. Alternatively, you could *fork* this repository and develop your own independently of this collection.

Once you've cloned this repository to your local machine, you'll need to enable `{webr}` within the project. You can do this in RStudio/Positron by running `quarto add coatless/quarto-webr` from the *terminal*.
