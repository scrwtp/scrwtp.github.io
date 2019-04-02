---
layout: post
title: "Continuations and call/cc in F#"
---

A continuation is an abstract concept of "the rest of a computation" from a certain point in time onward. This definition sounds pretty conceited, but it's easy to get an intuitive understanding of it, given an example. 

Let's consider following expression (it's an easily approachable dialect of Lisp called [Racket](https://racket-lang.org/)):

{% highlight racket %}
(+ (* 3 4) 5) ;=> 17
{% endhighlight %}

One way to evaluate it would be to evaluate the two subexpressions, `(* 3 4)` and `5`, and then to apply the addition operation to them.

Now let's assume we're evaluating the first one, `(* 3 4)`. We get the result, `12`. Now let's consider what to do next. We have evaluated a subexpression, what remains is the "rest of a computation". We can note it down as `(+ _ 5)`, where underscore signifies the hole to "plug in" the result we just calculated. We plug int the calculated value `12` there, for `(+ 12 5)`, and we arrive at result `17`.

Conceptually, `(+ _ 5)` is a *continuation* of `(* 3 4)` in that context. Note that it's not the only way of splitting that expression into a partially done computation and its continuation. A continuation can be called "a future" of a computation - we can essentially freeze-frame a computation at any point in time, and we could refer to the yet-not-done part of it as the continuation.

Up to this point, it's purely a conceptual distinction. In some languages however, this abstract concept can be directly reified into a concrete construct, a first-class continuation.

{% highlight racket %}
(define c #f)

(+ (* 3 (call/cc
         (lambda (k)
          (set! c k)
          k 4))) 5) ;=> 17

(c 2) ;=> 11
{% endhighlight %}

What happens here is that instead of simply yielding `4`, we're capturing a continuation at that point using `call-with-current-continuation` (abbreviated `call/cc`). This makes the continuation, `(+ (* 3 _) 5)`, available as `k` in the body of the the lambda passed to `call/cc`. In a way, this turns the expression outside the `call/cc` block "inside out", and surfaces it as `k` inside the block.

We can use this reified continuation however we see fit - here we're assigning it to a reference cell `c` and then calling the continuation, providing it the value `4`. This evaluates to the same result we've seen, `17`, but we're left with the captured continuation `c`. Applying it to `2` yields `11` - which is correct as this is the result of evaluating `(+ (* 3 2) 5)`.

While F# (as well as the wider .NET framework) doesn't support first class continuations or call/cc, it does offer an elegant mechanism to model them in. Ordinarily, a practical attempt at modeling call/cc would require transforming the code into continuation-passing style by hand, which is a mechanical, but tedious process that leaves the code significantly harder to follow than the source version. In F#, much of that can be wished away by using a computation expression builder to hide the plumbing. 





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