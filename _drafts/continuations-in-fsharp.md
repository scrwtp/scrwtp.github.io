---
layout: post
title: "Continuations in F#"
---

https://stackoverflow.com/questions/3322540/how-and-why-does-the-haskell-cont-monad-work
https://stackoverflow.com/questions/9050725/call-cc-implementation
https://stackoverflow.com/questions/40052256/how-does-continuation-monad-really-work
https://www.youtube.com/watch?v=2GfFlfToBCo
http://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/
http://www.fssnip.net/7c/title/Continuation-Monad-with-CallCC

A continuation is a concept representing "the rest of the computation". 

A bare-bones continuation builder can be defined as follows:

{% highlight fsharp %}
type ContinuationBuilder() =
    member this.Bind (ma, f) = 
        fun kb -> ma (fun a -> f a kb)
    member this.Return x = 
        fun k -> k x
{% endhighlight %}

This deceptively simple definition evaluates as a confusing soup of types below:

{% highlight fsharp %}
type ContinuationBuilder =
  class
    new : unit -> ContinuationBuilder
    member Bind : ma:(('c -> 'd) -> 'e) * f:('c -> 'f -> 'd) -> ('f -> 'e)
    member Return : x:'a -> (('a -> 'b) -> 'b)
  end
{% endhighlight %}

I remember seeing something like this before, and I was perplexed as to how can anyone understand code like this, let alone put it together in the first place. The conventional "follow the types" advice felt quite useless to me.

It's not impossible to decipher it however. First let's take a step back to find the monadic type here. This example is particularly tricky, because unlike options or lists that have a very clear, generic wrapper type, continuation monad is often defined with a function as the monadic type. Let's give it a type alias:

{% highlight fsharp %}
type Cont<'a, 'r> = ('a -> 'r) -> 'r
{% endhighlight %}

`Cont` represents the incomplete computation that wraps a value of type `'a`, accepts a continuation `'a -> 'r`, applies it to the wrapped value and yields a result of type `'r`. 

Armed with this new type, and some understanding of monads, we can annotate the builder in a way that makes `Bind` and `Return` signatures clear:

{% highlight fsharp %}
type ContinuationBuilder() =
    member this.Bind (ma: Cont<'a, 'r>, f: 'a -> Cont<'b, 'r>) : Cont<'b, 'r> = 
        fun kb -> ma (fun a -> f a kb)
    member this.Return (x: 'a) : Cont<'a, 'r> = 
        fun k -> k x
{% endhighlight %}

Personally, I prefer to go one step further and introduce an actual wrapper type rather than the alias - this makes some patterns more explicit in the code and does a better job of pinning the type down for type inference. 

{% highlight fsharp %}
type Cont<'a, 'r> = 
    private | Comp of (('a -> 'r) -> 'r)
    member this.RunCont (k: 'a -> 'r) = 
        match this with
        | Comp f -> f k
        
type ContinuationBuilder() =
    member this.Bind (ma : Cont<'a, 'r>, f: 'a -> Cont<'b, 'r>) : Cont<'b, 'r> = 
        Comp <| fun kb -> 
            ma.RunCont (fun a -> 
                let mb = f a
                mb.RunCont kb)
    member this.Return(x : 'a) : Cont<'a, 'r> = 
        Comp <| fun k -> k x
{% endhighlight %}