---
layout: post
title: "Continuations in F# - from CPS transformation to continuation monad"
---

A bare-bones continuation builder can be [defined as follows](https://stackoverflow.com/questions/40052256/how-does-continuation-monad-really-work):

{% highlight fsharp %}
type ContinuationBuilder() =
    member this.Bind (ma, f) = 
        fun kb -> ma (fun a -> f a kb)
    member this.Return x = 
        fun k -> k x
{% endhighlight %}

The definition above is as as simple as it is cryptic, and yields this soup of types when evaluated:

{% highlight fsharp %}
type ContinuationBuilder =
  class
    new : unit -> ContinuationBuilder
    member Bind : ma:(('c -> 'd) -> 'e) * f:('c -> 'f -> 'd) -> ('f -> 'e)
    member Return : x:'a -> (('a -> 'b) -> 'b)
  end
{% endhighlight %}

When I first saw something along those lines a couple years back, I thought it's inconceivable that someone could write code like this starting from a blank slate, and 