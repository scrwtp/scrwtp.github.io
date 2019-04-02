---
layout: post
title: "amb operator in F#"
---

[SICP section 4.3](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3.1) talks about an approach to non-deterministic computation using a Scheme special form called `amb`, an idea first [described](http://www-formal.stanford.edu/jmc/basis1.pdf) by John McCarthy in 1961. 

`amb` operator, when given a list of possible values, may return one of those values such that it obeys restrictions put on it *later in the code*. The way it works is that `amb` operator will attempt to execute the code that follows it, testing one value after another from the input list, and backtracking if a value fails to meet a restriction. In Lisps, this backtracking tends to be achieved using first class continuations, and `call-with-current-continuation` operator.

While F# does not provide first class continuations that can be freely manipulated, it has a way of modelling continuations using a computation expression builder. The approach hides much of the plumbing code away from the user, making the actual solution reminiscent of the Scheme code from SICP. 

As an example of a problem that can be elegantly solved using non-deterministic computation, SICP cites the following logic puzzle:

> Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that contains only five
floors. Baker does not live on the top floor. Cooper does not live on the bottom floor. Fletcher does not live on either the top or the bottom floor. 
Miller lives on a higher floor than does Cooper. Smith does not live on a floor adjacent to Fletcher’s. Fletcher does not live on a floor adjacent to Cooper’s. Where does everyone live?

An F# solution to the puzzle is given by this code:

{% highlight fsharp %}
let puzzle = 
    cont {
        let ctx = Amb.AmbContext.Empty

        // Baker, Cooper, Fletcher, Miller, and Smith live on different floors 
        // of an apartment house that contains only five floors. 
        let! baker      = Amb.amb ctx [ 1 .. 5 ]
        let! cooper     = Amb.amb ctx [ 1 .. 5 ]
        let! fletcher   = Amb.amb ctx [ 1 .. 5 ]
        let! miller     = Amb.amb ctx [ 1 .. 5 ]
        let! smith      = Amb.amb ctx [ 1 .. 5 ]
        
        let all = [ baker; cooper; fletcher; miller; smith ]

        do! Amb.require ctx (List.distinct all = all)

        // Baker does not live on the top floor. 
        do! Amb.require ctx (baker <> 5)
        
        // Cooper does not live on the bottom floor. 
        do! Amb.require ctx (cooper <> 1)
        
        // Fletcher does not live on either the top or the bottom floor. 
        do! Amb.require ctx (fletcher <> 1 && fletcher <> 5)

        // Miller lives on a higher floor than does Cooper. 
        do! Amb.require ctx (miller > cooper)

        // Smith does not live on a floor adjacent to Fletcher’s. 
        do! Amb.require ctx (abs (smith - fletcher) > 1)

        // Fletcher does not live on a floor adjacent to Cooper’s. 
        do! Amb.require ctx (abs (fletcher - cooper) > 1)

        return 
            [
                "baker"     , baker   
                "cooper"    , cooper  
                "fletcher"  , fletcher
                "miller"    , miller  
                "smith"     , smith   
            ]
    }
{% endhighlight %}

To get it to work, we'll need a continuation expression builder: 

{% highlight fsharp %}   
type Cont<'a, 'r> = 
    private | Cont of (('a -> 'r) -> 'r)
    member this.RunCont (k: 'a -> 'r) = 
        match this with
        | Cont f -> f k
   
type ContinuationBuilder() =
    member this.Bind (ma : Cont<'a, 'r>, f: 'a -> Cont<'b, 'r>) : Cont<'b, 'r> = 
        Cont <| fun kb -> 
            ma.RunCont (fun a -> 
                let mb = f a
                mb.RunCont kb)
    member this.Return(x : 'a) : Cont<'a, 'r> = 
        Cont <| fun k -> k x
    member this.ReturnFrom(m : Cont<'a, 'r>) = m
    member this.Zero() = this.Return()
	
module Cont = 
    
    let run k (comp: Cont<'a, 'r>) = 
        comp.RunCont k

    let callCC (f: ('a -> Cont<'b, 'r>) -> Cont<'a, 'r>) : Cont<'a, 'r> = 
        Cont <| fun k ->
            let m = f (fun a ->
                Cont <| (fun _ -> k a)) 
            m.RunCont k

let cont = ContinuationBuilder()	
{% endhighlight %} 

The builder itself is given here "as is", going into the details of how and why it works is a more extensive topic worthy of a post or two by itself. Like all computation expression builders, it provides syntax sugar that hides the glue code around composing expressions of a particular type. 

The type in question here is `Cont<'a, 'r>` - it represents the incomplete computation that wraps a value of type `'a`, accepts a continuation of type `'a -> 'r`, applies it to the wrapped value and yields a result of type `'r`.

The rest of the snippet is boilerplate necessary to get the computation expression syntax to work, with an exception of `Cont.callCC` function, which implements `call-with-current-continuation` in terms of `cont` builder. 

What exactly `callCC` does might not be immediately apparent. The result of `callCC` is a `Cont<'a, 'r>`, which as we stated above means that it's an incomplete computation that wraps a value of type `'a` and feeds it into its continuation `k: 'a -> 'r`. It also takes as an argument a function `f` that yields a value of type `Cont<'a,'r>`.

Things start getting interesting when we look at the argument to `f`:

{% highlight fsharp %}
(fun a -> Cont <| (fun _ -> k a))
{% endhighlight %}

It's a function that wraps the original continuation `k` in another `Cont` wrapper, which *ignores* whatever continuation it was given and instead executes continuation `k` on argument `a`. What this means in terms of control flow, is that we hold a `Cont` that expresses a jump back to an arbitrary point *in the past*.

What this means is that `callCC` captures the rest of the computation which expects a value of type `'a` to be fed into it - and reexposes it as a function `'a -> Cont<'b, 'r>` available for use in the body of function `f`. Note the inversion of control that happens here - `callCC` turns *the code outside of it* inside out, into a function that can be called, ignored, or as we will see in a moment, passed outside of the context of function `f`.

Now we can define the `amb` operator in terms of `Cont.callCC`:

{% highlight fsharp %} 
module Amb = 

    type AmbContext<'a, 'r> = 
        private {
            mutable stack: (unit -> Cont<'a, 'r>) list 
        }
        static member Empty = { stack = List.empty<unit -> Cont<'a, 'r>> }
        member this.Fail () : Cont<'a, 'r> = 
            match this.stack with
            | [] -> failwith "Backtracking stack exhausted!"
            | x::xs -> 
                this.stack <- xs
                x ()
        member this.Push (k: unit -> Cont<'a, 'r>) = 
            this.stack <- k :: this.stack           
            
    let rec amb (ctx: AmbContext<'a,'r>) (choices: 'a list) : Cont<'a, 'r> = 
        cont {
            match choices with
            | [] -> 
                return! ctx.Fail()
            | x::xs ->
                return! Cont.callCC (fun exit ->
                    cont {
                        do! Cont.callCC (fun (k: unit -> Cont<'a, 'r>) -> 
                            cont {
                                ctx.Push(k)
                                do! exit x
                            })
                        return! amb ctx xs
                    })
        }

    let require (ctx: AmbContext<'a,'r>) (value: bool) = 
        cont {
            if not value then 
                let! _ = amb ctx []
                return ()
        }
{% endhighlight %} 

When `amb` is called with a non-empty list of choices, it will start recursively traversing through the list in a rather convoluted way. We see two calls to `Cont.callCC`. The outer one captures an escape continuation `exit` - which is the continuation at the point where it's called, the `return! _` expression in the `amb` operator's `cont` workflow. Execution then proceeds to the inner call, which captures a continuation `k`, pushes it onto a stack and immediately exits, yielding an element from the list of choices. At which point `amb`'s work is done for now.

The interesting part is what happens when `amb` is called with an empty list of choices - either directly, or through `require` function. If available, a continuation is popped off the stack and executed. This moves the control flow back to the point where that continuation was captured. In this case, we jump back to the original continuation of the inner `callCC` call, `return! amb ctx xs`, and iterate over the next element of the list of choices. This repeats until a solution satisfying all the `require` statements is found, or all choices are exhausted.

`AmbContext` type is a wrapper around the backtracking continuation stack. This is where continuations are registered when using `amb` operator, and this is where they are consumed on failure. It can only be used with a single type of continuations, meaning that users can't mix `amb` operators selecting values of different types in a single context. The example presented uses `int` for all values - it would be impossible to mix `ints` and `strings` there currently. This is however a limitation of the implementation rather than a limitation of the approach in general.

Now that we have all the pieces in place, we can run the puzzle using `puzzle |> Cont.run id`. This yields the following result when evaluated in FSI:

{% highlight fsharp %} 
val puzzle : Cont<(string * int) list,(string * int) list>
val it : (string * int) list =
  [("baker", 3); ("cooper", 2); ("fletcher", 4); ("miller", 5); ("smith", 1)]
{% endhighlight %} 

which is in line with the answer provided in SICP.

The code in its entirety can be found [here](...). 