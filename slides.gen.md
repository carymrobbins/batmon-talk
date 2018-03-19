<!--
****    THIS IS A GENERATED FILE, DO NOT EDIT!    ****
**** CHANGES SHOULD BE MADE IN slides.md INSTEAD! ****
-->
# Batching Monadic Requests

## with Free Interpreters

_A love story of free monad & free applicative_

Cary Robbins

June 6, 2019

---

## Prerequisite Concepts

* Applicative <!-- .element: class="fragment" -->
* Monad <!-- .element: class="fragment" -->
* Free (Monad and Applicative) <!-- .element: class="fragment" -->
* GADTs (data Foo a where) <!-- .element: class="fragment" -->
* Transformers (ReaderT, ExceptT) <!-- .element: class="fragment" -->
* Natural transformations (f a -> g a) <!-- .element: class="fragment" -->
* Universal quantification (forall, Data.Some) <!-- .element: class="fragment" -->

---

## Does this only work in Haskell?

---

## Using just Haskell2010!

_jk jk_ <!-- .element: class="fragment" -->

<small>
`{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, DefaultSignatures, DeriveAnyClass, DeriveGeneric, DerivingStrategies, DerivingVia, DuplicateRecordFields, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, NamedFieldPuns, OverloadedLabels, OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}`
</small> <!-- .element: class="fragment" -->

_Can you imagine a world without language extensions?_ <!-- .element: class="fragment" -->

---

## Free Library

Just using plain ol' `free`

---

## Problem

Business Inc. needs to interact with the Google Calendar API

_However..._ <!-- .element: class="fragment" -->

<b>We're getting rate limited!</b> <!-- .element: class="fragment" -->

---

## Solution

Google provides a batch API, let's just use that!

---

## A quick derail into monads

<pre><code data-noescape data-trim class="haskell">
runThings = do
  x &lt;- thingX
  y &lt;- thingY
  z &lt;- thingZ
  pure &#40;x&#44; y&#44; z&#41;
</code></pre>

<pre><code data-noescape data-trim class="haskell">
runThings =
  thingX &gt;&gt;= \x -&gt;
    thingY &gt;&gt;= \y -&gt;
      thingZ &gt;&gt;= \z -&gt;
        pure &#40;x&#44; y&#44; z&#41;
</code></pre>
<!-- .element: class="fragment" -->

---

## Applicative to the rescue?

<pre><code data-noescape data-trim class="haskell">
runThings = do
  x &lt;- thingX
  y &lt;- thingY
  z &lt;- thingZ
  pure &#40;x&#44; y&#44; z&#41;
</code></pre>

<pre><code data-noescape data-trim class="haskell">
runThings =
  &#40;&#44;&#44;&#41;
    &lt;&#36;&gt; thingX
    &lt;&#42;&gt; thingY
    &lt;&#42;&gt; thingZ
</code></pre>
<!-- .element: class="fragment" -->

---

## Business Inc. Command Requirements

* **InsertEvent**
  * _Attempt **insert**; on 409, fall back to **update**_ <!-- .element: class="fragment" -->
* **UpdateEvent**
  * _Attempt **update**; on 404, fall back to **insert**_ <!-- .element: class="fragment" -->
* **DeleteEvent**
  * _Cancel the event via a **delete**, permitting 404 and 410_ <!-- .element: class="fragment" -->
* **EventExists**
  * _Return **true** if the event exists and is not cancelled, **false** otherwise_ <!-- .element: class="fragment" -->

---

## Business Inc. Scope Creep

Mutable commands (**insert**, **update**, **delete**) should also -
* Report what action was taken (e.g. updated via insert fallback) <!-- .element: class="fragment" -->
* Return the Google calendar event id for verification <!-- .element: class="fragment" -->

---

### Insert Event Prototype

<pre><code data-noescape data-trim class="haskell">
insertEvent
  &#58;&#58; G&#46;Client         -- &#94; Google API client
  -&gt; BEvent           -- &#94; Business event
  -&gt; IO BInsertResult -- &#94; Business result
insertEvent gClient bEvent = do
  <span class="fragment">G&#46;insert gClient bEvent <span class="fragment">&gt;&gt;= \case
    Right res -&gt; pure &#36; Inserted &#36; key res</span>
    <span class="fragment">Left e0 -&gt; do
      when &#40;status e0 /= 409&#41; &#36; throwIO e0
      <span class="fragment">G&#46;update gClient bEvent &gt;&gt;= <span class="fragment">\case
        Right res -&gt; pure &#36; UpdatedViaInsert &#36; key res</span>
        <span class="fragment">Left e1 -&gt; throwIO e1</span>
</span></span></span>
</code></pre>

Can you spot the problem? <!-- .element: class="fragment" -->

---

## Taking a step back

How do we even batch anything in the first place?

---

## Google Calendar Algebra

<pre><code data-noescape data-trim class="haskell">
&#40;insertRes&#44; getRes&#41; &lt;- G&#46;runClient gClient &#36;
  &#40;&#44;&#41;
    &lt;&#36;&gt; G&#46;mkRequest &#40;G&#46;EventsInsert gTok calId event&#41;
    &lt;&#42;&gt; G&#46;mkRequest &#40;G&#46;EventsGet    gTok calId eid&#41;
</code></pre>

---

## Google Calendar Algebra

<div style='font-size:0.99999em'>
<pre><code data-noescape data-trim class="haskell">
<span class="fragment">type ActionResult a = Action &#40;Either ClientFailure a&#41;</span>

data Action a where
  <span class="fragment">CalendarsGet
    &#58;&#58; AccessToken
    -&gt; KeyOf Calendar
    -&gt; ActionResult &#40;Maybe &#40;Keyed Calendar&#41;&#41;</span>

  <span class="fragment">EventsInsert
    &#58;&#58; AccessToken
    -&gt; KeyOf Calendar
    -&gt; CalendarEvent
    -&gt; ActionResult &#40;Keyed CalendarEvent&#41;</span>

  <span class="fragment">-- and so on</span>
</code></pre>
</div>

---

## Free Applicative Refresher

<pre><code data-noescape data-trim class="haskell">
module Control&#46;Applicative&#46;Free where

data Ap f a where
  Pure &#58;&#58; a -&gt; Ap f a
  Ap   &#58;&#58; f a -&gt; Ap f &#40;a -&gt; b&#41; -&gt; Ap f b

<span class="fragment">liftAp &#58;&#58; f a -&gt; Ap f a</span>
</code></pre>

---

## Free Applicative Request

<pre><code data-noescape data-trim class="haskell">
import qualified Control&#46;Applicative&#46;Free as FreeAp

type Request = FreeAp&#46;Ap Action

<span class="fragment">mkRequest &#58;&#58; Action a -&gt; Request a
mkRequest = FreeAp&#46;liftAp</span>
</code></pre>

---

## Google Calendar Client

<pre><code data-noescape data-trim class="haskell">
newtype Client f = Client
  &#123; runClient &#58;&#58; forall a&#46; Request a -&gt; f a &#125;
</code></pre>

---

## Now just implement it!

<img src='img/owl.png'/>

---

## Making a batched client

Dependency inversion isn't just for OO

<pre><code data-noescape data-trim class="haskell">
mkBatchedClient &#58;&#58; forall m&#46; &#40;MonadIO m&#41; =&gt; HttpHandler m -&gt; Client m


&#95;
</code></pre>

---

## Making a batched client

Dependency inversion isn't just for OO

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkBatchedClient &#58;&#58; forall m&#46; &#40;MonadIO m&#41; =&gt; HttpHandler m -&gt; Client m</span>

newtype HttpHandler f = HttpHandler
  &#123; runHttpHandler &#58;&#58; RequestPayload -&gt; f BatchResponse &#125;
</code></pre>

---

## HttpHandler

<pre><code data-noescape data-trim class="haskell">
mkBatchedHttpHandler
  &#58;&#58; &#40;MonadIO m&#44; MonadThrow m&#41; =&gt; HTTPClient&#46;Manager -&gt; HttpHandler m


&#95;
</code></pre>

---

## HttpHandler

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkBatchedHttpHandler
  &#58;&#58; &#40;MonadIO m&#44; MonadThrow m&#41; =&gt; HTTPClient&#46;Manager -&gt; HttpHandler m</span>

httpHandlerWithCounter
  &#58;&#58; &#40;MonadIO m&#41; =&gt; IORef Int -&gt; HttpHandler m -&gt; HttpHandler m
</code></pre>

---

## The crucial command

<pre><code data-noescape data-trim class="haskell">
data Command a = Command &#123; <span class="fragment" data-fragment-index="2">key &#58;&#58; UUID&#44; </span><span class="fragment" data-fragment-index="1">action &#58;&#58; Action a</span> &#125;




&#95;
</code></pre>

---

## The crucial command

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">data Command a = Command &#123; key &#58;&#58; UUID&#44; action &#58;&#58; Action a &#125;</span>

data RequestPayload = RequestPayload
  &#123; boundary &#58;&#58; ByteString
  &#44; commands &#58;&#58; [Some Command]
  &#125;
</code></pre>

---

## The client for real

<pre><code data-noescape data-trim class="haskell">
<span class="fragment">mkBatchedClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41; =&gt; HttpHandler m -&gt; Client m</span>
<span class="fragment">mkBatchedClient httpHandler = Client &#123;runClient&#125;</span>
  <span class="fragment">where
  runClient &#58;&#58; forall a&#46; Request a -&gt; m a
  runClient req = do</span>
    <span class="fragment">cmd &lt;- runApComposed genUUID req</span>
</code></pre>

---

## Backtrack to runAp

<pre><code data-noescape data-trim class="haskell">
runAp &#58;&#58; Applicative g =&gt; &#40;forall x&#46; f x -&gt; g x&#41; -&gt; Ap f a -&gt; g a
</code></pre>

<pre><code data-noescape data-trim class="haskell">
runApComposed
  &#58;&#58; &#40;Applicative g&#44; Applicative h&#41;
  =&gt; &#40;forall x&#46; f x -&gt; g &#40;h x&#41;&#41; -&gt; Ap f a -&gt; g &#40;h a&#41;
</code></pre>
<!-- .element: class="fragment" -->

<pre><code data-noescape data-trim class="haskell">
-- Create a unique UUID for each request
genUUID &#58;&#58; Action a -&gt; m &#40;FreeAp&#46;Ap Command a&#41;
genUUID a = do
  u &lt;- liftIO UUID&#46;nextRandom
  pure &#36; FreeAp&#46;liftAp &#36; Command u a
</code></pre>
<!-- .element: class="fragment" -->

---

## The Client for real

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkBatchedClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41; =&gt; HttpHandler m -&gt; Client m
mkBatchedClient httpHandler logger = Client &#123;runClient&#125;
  where
  runClient &#58;&#58; forall a&#46; Request a -&gt; m a
  runClient req = do
    cmd &lt;- runApComposed genUUID req</span>
    <span class="fragment">-- Accumulate all the commands into a list
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd</span>
</code></pre>

---

## Backtrack to runAp_

<pre><code data-noescape data-trim class="haskell">
runAp&#95; &#58;&#58; Monoid m =&gt; &#40;forall a&#46; f a -&gt; m&#41; -&gt; Ap f b -&gt; m

<span class="fragment">-- What were we doing again&#63;
runAp&#95; &#40;\c -&gt; [This c]&#41; cmd</span>

<span class="fragment">-- Specialized
runAp&#95; &#58;&#58; &#40;forall a&#46; Command a -&gt; [Some Command]&#41;
       -&gt; <span class="fragment">Ap Command b</span>
       -&gt; <span class="fragment">[Some Command]</span></span>
</code></pre>

---

## The client for real

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkBatchedClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41; =&gt; HttpHandler m -&gt; Client m
mkBatchedClient httpHandler logger = Client &#123;runClient&#125;
  where
  runClient &#58;&#58; forall a&#46; Request a -&gt; m a
  runClient req = do
    cmd &lt;- runApComposed genUUID req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd</span>
    <span class="fragment">let reader = FreeAp&#46;runAp mkReader cmd</span>
</code></pre>

---

## Batched reader

Build reader functions which can decode from our response map

<pre><code data-noescape data-trim class="haskell">
mkReader
  &#58;&#58; Command a
  -&gt; ReaderT &#40;Map UUID EncodedResponse&#41; DecodeResult a
<span class="fragment">mkReader &#40;Command uuid action&#41; = ReaderT &#36; \env -&gt;</span>
  <span class="fragment">decodeResponse action &#36; env &#33; uuid</span>
</code></pre>

---

## How does decodeResponse work?

<pre><code data-noescape data-trim class="haskell">
class Decoder a where
  decode &#58;&#58; EncodedResponse -&gt; DecodeResult a
</code></pre>

<pre><code data-noescape data-trim class="haskell">
decodeResponse &#58;&#58; Action a -&gt; EncodedResponse -&gt; Response&#46;DecodeResult a
decodeResponse action r = case action of
  <span class="fragment">CalendarsGet        &#123;&#125; -&gt; Decoder&#46;decode r</span>
  <span class="fragment">EventsInsert        &#123;&#125; -&gt; Decoder&#46;decode r</span>
  <span class="fragment">EventsInsertWithKey &#123;&#125; -&gt; Decoder&#46;decode r
  EventsUpdate        &#123;&#125; -&gt; Decoder&#46;decode r
  EventsDelete        &#123;&#125; -&gt; Decoder&#46;decode r
  EventsGet           &#123;&#125; -&gt; Decoder&#46;decode r</span>
</code></pre>
<!-- .element: class="fragment" -->

---

## The client for real

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkBatchedClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41; =&gt; HttpHandler m -&gt; Client m
mkBatchedClient httpHandler logger = Client &#123;runClient&#125;
  where
  runClient &#58;&#58; forall a&#46; Request a -&gt; m a
  runClient req = do
    cmd &lt;- runApComposed genUUID req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd
    let reader = FreeAp&#46;runAp mkReader cmd</span>
    <span class="fragment">env &lt;- runCommands cmds</span>
</code></pre>

---

## runCommands

<pre><code data-noescape data-trim class="haskell">
runCommands &#58;&#58; [Some Command] -&gt; m &#40;Map UUID EncodedResponse&#41;
runCommands cmds = do
  <span class="fragment" data-fragment-index="40">mconcat &lt;&#36;&gt;</span> <span class="fragment" data-fragment-index="20">traverse runChunk</span> <span class="fragment" data-fragment-index="10">&#40;List&#46;chunksOf batchSize cmds&#41;</span>
  <span class="fragment" data-fragment-index="15">where
  batchSize = 50 -- Google&#x27;s maximum batch size</span>

  <span class="fragment" data-fragment-index="30">runChunk &#58;&#58; [Some Command] -&gt; m &#40;Map UUID EncodedResponse&#41;</span>
</code></pre>

---

## The Client for real

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkBatchedClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41; =&gt; HttpHandler m -&gt; Client m
mkBatchedClient httpHandler logger = Client &#123;runClient&#125;
  where
  runClient &#58;&#58; forall a&#46; Request a -&gt; m a
  runClient req = do
    cmd &lt;- runApComposed genUUID req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd
    let reader = FreeAp&#46;runAp mkReader cmd
    env &lt;- runCommands cmds</span>
    <span class="fragment" data-fragment-index="15">case</span> <span class="fragment" data-fragment-index="10">Response&#46;unDecodeResult &#36;</span> <span class="fragment" data-fragment-index="5">runReaderT reader env</span> <span class="fragment" data-fragment-index="15">of</span>
      <span class="fragment" data-fragment-index="15">Right x -&gt; pure x</span>
      <span class="fragment" data-fragment-index="25">Left e -&gt; throwIO e</span>
</code></pre>

---

## Yay, we can batch!

_...Applicatives?_ <!-- .element: class="fragment" -->

---

I thought we were batching monads?

<img src="img/godzilla.png"/>

---

## Free monads aren't the solution

<pre><code data-noescape data-trim class="haskell">
module Control&#46;Monad&#46;Free where

data Free f a =
    Pure a
  &#124; Free &#40;f &#40;Free f a&#41;&#41;
</code></pre>

But they could be part of the solution! <!-- .element: class="fragment" -->

---

## What if...

<pre><code data-noescape data-trim class=haskell>
<span class=fragment data-fragment-index="1"> BInsertEvent BEventExists BUpdateEvent</span>
<span class=fragment data-fragment-index="2">      ▼           ▼            ▼     </span>
<span class=fragment data-fragment-index="2">    insert      exists       update  </span><span class=fragment data-fragment-index="4">▶ [insert, exists, update]</span>
<span class=fragment data-fragment-index="5">                                     <!--                                             -->             ▼            </span>
<span class=fragment data-fragment-index="5">                                     <!--                                             -->          Request         </span>
<span class=fragment data-fragment-index="5">                                     <!--                                             -->             ▼             </span>
<span class=fragment data-fragment-index="9">    Conflict    Pure(true)  Not Found ◀</span><span class=fragment data-fragment-index="5">        Response</span>
<span class=fragment data-fragment-index="10">      ▼                       ▼      </span>
<span class=fragment data-fragment-index="10">     >>=                     >>=     </span>
<span class=fragment data-fragment-index="10">    update                  insert   </span><span class=fragment data-fragment-index="14">▶    [update, insert]</span>
<span class=fragment data-fragment-index="15">                                     <!--                                             -->             ▼             </span>
<span class=fragment data-fragment-index="15">                                     <!--                                             -->          Request         </span>
<span class=fragment data-fragment-index="15">                                     <!--                                             -->             ▼             </span>
<span class=fragment data-fragment-index="19">   Pure(ok)                Pure(ok)  ◀</span><span class=fragment data-fragment-index="15">        Response</span>
</code></pre>

---

## Business Inc Algebra

<pre><code data-noescape data-trim class="haskell">
<span class="fragment">type BActionResult a = BAction &#40;Either BFailure a&#41;</span>

data BAction a where
  <span class="fragment">BInsertEvent &#58;&#58; UserId -&gt; BEvent   -&gt; BActionResult BInsertResult</span>
  <span class="fragment">BDeleteEvent &#58;&#58; UserId -&gt; BEventId -&gt; BActionResult BDeleteResult</span>
  <span class="fragment">BEventExists &#58;&#58; UserId -&gt; BEventId -&gt; BActionResult Bool</span>
  <span class="fragment">BUpdateEvent &#58;&#58; UserId -&gt; BEvent   -&gt; BActionResult BUpdateResult</span>

<span class="fragment">type BRequest = FreeAp&#46;Ap BAction</span>

<span class="fragment">mkBRequest &#58;&#58; BAction a -&gt; BRequest a
mkBRequest = FreeAp&#46;liftAp</span>
</code></pre>

---

## Business Inc Client

<pre><code data-noescape data-trim class="haskell">
newtype BClient f = BClient
  &#123; runBClient &#58;&#58; forall a&#46; BRequest a -&gt; f a &#125;

<span class="fragment">mkGoogleBusinessClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41;
  =&gt; TokenProvider m -&gt; G&#46;Client m -&gt; BClient m</span>
<span class="fragment">mkGoogleBusinessClient tokenProvider gClient = BClient &#123;runBClient&#125;
  where
  runBClient &#58;&#58; BRequest a -&gt; m a
  runBClient req = do
    <span class="fragment">cmd &lt;- runApComposed genUUID req</span>
    <span class="fragment">let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd</span>
    </span>

&#95;
</code></pre>

---

## Business Inc Client

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">newtype BClient f = BClient
  &#123; runBClient &#58;&#58; forall a&#46; BRequest a -&gt; f a &#125;

mkGoogleBusinessClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41;
  =&gt; TokenProvider m -&gt; G&#46;Client m -&gt; BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient &#123;runBClient&#125;
  where
  runBClient &#58;&#58; BRequest a -&gt; m a
  runBClient req = do
    cmd &lt;- runApComposed genUUID req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd</span>
    let userIds = cmds &lt;&amp;&gt; \&#40;This &#40;BCommand &#95; a&#41;&#41; -&gt; getUserIdFromBAction a
    <span class="fragment">tokens &lt;- runTokenProvider tokenProvider userIds</span>
    <span class="fragment">let inputs = mkInputs tokens cmds</span>
</code></pre>

---

## Type aliases for sanity

<pre><code data-noescape data-trim class="haskell">
<span class="fragment">-- &#124; Alias for a Google Request; represents a single &quot;step&quot; in the interpreter&#46;
type GStep = G&#46;Request</span>

<span class="fragment">-- &#124; Represents a monadic &quot;program&quot; containing several GSteps&#46;
-- The interpreter will batch intermediate GSteps of multiple GLogic programs&#46;
type GLogic = FreeMonad&#46;Free GStep</span>

<span class="fragment">-- &#124; GLogic lifted to deal with failures&#46;
type GAttempt = ExceptT BFailure GLogic</span>

<span class="fragment">-- &#124; Interpreter result of a completed GLogic program&#46;
data IResult a = IResult &#123; action &#58;&#58; BAction a&#44; value &#58;&#58; a &#125;</span>

<span class="fragment">-- &#124; Inputs into the interpreter
type Inputs = Map UUID &#40;GLogic &#40;Some IResult&#41;&#41;</span>
</code></pre>

---

## Initializing the inputs

<pre><code data-noescape data-trim class="haskell">
<span class="fragment" data-fragment-index="10">type Inputs = Map UUID &#40;GLogic &#40;Some IResult&#41;&#41;</span>

mkInputs &#58;&#58; Map UserId G&#46;AccessToken -&gt; [Some BCommand] -&gt; Inputs
<span class="fragment" data-fragment-index="20">mkInputs tokens cmds =</span>
  <span class="fragment" data-fragment-index="55">Map&#46;fromList &#36;</span>
    <span class="fragment" data-fragment-index="20">cmds</span> <span class="fragment" data-fragment-index="30">&lt;&amp;&gt; \&#40;This &#40;BCommand cmdId a&#41;&#41; -&gt;
      <span class="fragment" data-fragment-index="40">&#40;cmdId&#44;</span> <span class="fragment" data-fragment-index="50">This &lt;&#36;&gt; mkGLogic tokens a&#41;</span></span>
</code></pre>

---

## Initializing the inputs

<pre><code data-noescape data-trim class="haskell">
mkGLogic &#58;&#58; Map UserId G&#46;AccessToken -&gt; BAction a -&gt; GLogic &#40;IResult a&#41;
mkGLogic tokens = \case
  <span class="fragment" data-fragment-index="70">action&#64;&#40;BInsertEvent uid event&#41; -&gt; go action insertEvent uid event</span>
  <span class="fragment" data-fragment-index="90">action&#64;&#40;BDeleteEvent uid eid&#41;   -&gt; go action deleteEvent uid eid</span>
  <span class="fragment" data-fragment-index="95">action&#64;&#40;BEventExists uid eid&#41;   -&gt; go action eventExists uid eid</span>
  <span class="fragment" data-fragment-index="100">action&#64;&#40;BUpdateEvent uid event&#41; -&gt; go action updateEvent uid event</span>
  <span class="fragment" data-fragment-index="75">where
  go &#58;&#58; BActionResult b</span>
     <span class="fragment" data-fragment-index="76">-&gt; &#40;G&#46;AccessToken -&gt; c -&gt; GAttempt b&#41;</span>
     <span class="fragment" data-fragment-index="77">-&gt; UserId</span>
     <span class="fragment" data-fragment-index="78">-&gt; c</span>
     <span class="fragment" data-fragment-index="79">-&gt; GLogic &#40;IResult &#40;Either BFailure b&#41;&#41;</span>
  <span class="fragment" data-fragment-index="80">go action f uid c =</span> <span class="fragment" data-fragment-index="82">fmap &#40;IResult action&#41; &#46;</span> <span class="fragment" data-fragment-index="81">runExceptT &#36;</span> <span class="fragment" data-fragment-index="80">f &#40;tok &#33; uid&#41; c</span>
</code></pre>

---

## Inserting an event with update fallback

<pre><code data-noescape data-trim class="haskell">
insertEvent &#58;&#58; G&#46;AccessToken -&gt; BEvent -&gt; GAttempt BInsertResult
insertEvent tok bEvent = <span class="fragment">do
  let gEvent = fromBEvent bEvent</span>
  <span class="fragment">gAttempt &#40;G&#46;EventsInsertWithKey tok calId gEvent&#41;</span> <span class="fragment">&gt;&gt;= \case
    Right &#40;G&#46;Keyed k &#95;&#41; -&gt; pure &#36; Inserted k</span>
    <span class="fragment">Left e0 -&gt; do
      when &#40;&#40;status e0&#41; /= 409&#41; &#36; throwError &#36; toBFailure e0</span>
      <span class="fragment">gAttempt &#40;G&#46;EventsUpdate tok calId gEvent&#41;</span> <span class="fragment">&gt;&gt;= \case
        Right &#40;G&#46;Keyed k &#95;&#41; -&gt; pure &#36; UpdatedViaInsert k</span>
        <span class="fragment">Left e1 -&gt; throwError &#36; toBFailure e1</span>
</code></pre>

---

## Making a Business Client

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkGoogleBusinessClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41;
  =&gt; TokenProvider m -&gt; G&#46;Client m -&gt; BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient &#123;runBClient&#125;
  where
  runBClient &#58;&#58; BRequest a -&gt; m a
  runBClient req = do
    cmd &lt;- runApComposed mkCmd req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd
    let userIds = cmds &lt;&amp;&gt; \&#40;This &#40;BCommand &#95; a&#41;&#41; -&gt; getUserIdFromBAction a
    tokens &lt;- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds</span>
    <span class="fragment">results &lt;- runInterpreter inputs</span>
</code></pre>

---

## The Interpreter

<pre><code data-noescape data-trim class="haskell">
runInterpreter &#58;&#58; Inputs -&gt; m &#40;Map UUID &#40;Some IResult&#41;&#41;
runInterpreter inputs =
  <span class="fragment">case allComplete inputs of
    Just results -&gt; pure results</span>
</code></pre>

<pre><code data-noescape data-trim class="haskell">
<span class="fragment">-- Returns Nothing if there are any Incomplete values; otherwise returns
-- the final result map&#46;
allComplete &#58;&#58; Inputs -&gt; Maybe &#40;Map UUID &#40;Some IResult&#41;&#41;
allComplete =</span> <span class="fragment">traverse &#36; \case</span>
  <span class="fragment">FreeMonad&#46;Pure v -&gt; Just v</span>
  <span class="fragment">FreeMonad&#46;Free &#95; -&gt; Nothing</span>
</code></pre>

---

<pre><code data-noescape data-trim class="haskell">
runInterpreter &#58;&#58; Inputs -&gt; m &#40;Map UUID &#40;Some IResult&#41;&#41;
runInterpreter inputs =
  case allComplete inputs of
    Just results -&gt; pure results
    <span class="fragment">Nothing -&gt; do</span>
      <span class="fragment">-- Collect the next round of steps to be executed&#46;
      let steps = collectSteps inputs</span>

<span class="fragment">collectSteps &#58;&#58; Inputs -&gt; Map UUID &#40;GStep &#40;GLogic &#40;Some IResult&#41;&#41;&#41;
collectSteps =</span> <span class="fragment">Map&#46;mapMaybe &#36; \case</span>
  <span class="fragment">FreeMonad&#46;Free x -&gt; Just x</span>
  <span class="fragment">FreeMonad&#46;Pure &#95; -&gt; Nothing</span>
</code></pre>

---

## The interpreter loop <span class="fragment" data-fragment-index="200">🎉</span>

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">runInterpreter &#58;&#58; Inputs -&gt; m &#40;Map UUID &#40;Some IResult&#41;&#41;
runInterpreter inputs =
  case allComplete inputs of
    Just results -&gt; pure results
    Nothing -&gt; do</span>
      let steps <span class="fragment" data-fragment-index="10">&#58;&#58; Map UUID &#40;GStep &#40;GLogic &#40;Some IResult&#41;&#41;&#41;</span> = collectSteps inputs
      <span class="fragment" data-fragment-index="20">let step  <span class="fragment" data-fragment-index="30">&#58;&#58; GStep &#40;Map UUID &#40;GLogic &#40;Some IResult&#41;&#41;&#41;</span> = sequenceA steps</span>
      <span class="fragment" data-fragment-index="40">       -- &#58;&#58; GStep Inputs</span>
      <span class="fragment" data-fragment-index="60">response &#58;&#58; Inputs &lt;- G&#46;runClient gClient step</span>
      <span class="fragment" data-fragment-index="90">runInterpreter &#36;</span> <span class="fragment" data-fragment-index="80">response &lt;&gt; inputs</span>
</code></pre>

---

## Making a Business Client

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkGoogleBusinessClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41;
  =&gt; TokenProvider m -&gt; G&#46;Client m -&gt; BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient &#123;runBClient&#125;
  where
  runBClient &#58;&#58; BRequest a -&gt; m a
  runBClient req = do
    cmd &lt;- runApComposed mkCmd req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd
    let userIds = cmds &lt;&amp;&gt; \&#40;This &#40;BCommand &#95; a&#41;&#41; -&gt; getUserIdFromBAction a
    tokens &lt;- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds</span>
    results &lt;- runInterpreter inputs
    <span class="fragment">FreeAp&#46;runAp &#40;extractor results&#41; cmd</span>
</code></pre>

---

## The extractor

<pre><code data-noescape data-trim class="haskell">
extractor &#58;&#58; Map UUID &#40;Some IResult&#41; -&gt; BCommand a -&gt; m a
<span class="fragment">extractor results &#40;BCommand k action&#41; = case action of</span>
  <span class="fragment">BInsertEvent &#123;&#125; -&gt; case results &#33; k of
    This &#40;IResult &#40;BInsertEvent &#123;&#125;&#41; x&#41; -&gt; pure x
    This &#40;IResult other &#95;&#41; -&gt; throwTypeMismatch action other</span>

  <span class="fragment">BDeleteEvent &#123;&#125; -&gt; case res of
    This &#40;IResult &#40;BDeleteEvent &#123;&#125;&#41; x&#41; -&gt; pure x
    This &#40;IResult other &#95;&#41; -&gt; throwTypeMismatch action other</span>

  <span class="fragment">-- and so on&#44; the compiler will check for exhaustiveness</span>
</code></pre>

---

## Making a Business Client

<pre><code data-noescape data-trim class="haskell">
<span class="gray-code">mkGoogleBusinessClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41;
  =&gt; TokenProvider m -&gt; G&#46;Client m -&gt; BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient &#123;runBClient&#125;
  where
  runBClient &#58;&#58; BRequest a -&gt; m a
  runBClient req = do
    cmd &lt;- runApComposed mkCmd req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd
    let userIds = cmds &lt;&amp;&gt; \&#40;This &#40;BCommand &#95; a&#41;&#41; -&gt; getUserIdFromBAction a
    tokens &lt;- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds
    results &lt;- runInterpreter inputs</span>
    FreeAp&#46;runAp &#40;extractor results&#41; cmd
</code></pre>

---

## Making a Business Client

<pre><code data-noescape data-trim class="haskell">
mkGoogleBusinessClient
  &#58;&#58; forall m&#46; &#40;MonadIO m&#41;
  =&gt; TokenProvider m -&gt; G&#46;Client m -&gt; BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient &#123;runBClient&#125;
  where
  runBClient &#58;&#58; BRequest a -&gt; m a
  runBClient req = do
    cmd &lt;- runApComposed mkCmd req
    let cmds = FreeAp&#46;runAp&#95; &#40;\c -&gt; [This c]&#41; cmd
    let userIds = cmds &lt;&amp;&gt; \&#40;This &#40;BCommand &#95; a&#41;&#41; -&gt; getUserIdFromBAction a
    tokens &lt;- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds
    results &lt;- runInterpreter inputs
    FreeAp&#46;runAp &#40;extractor results&#41; cmd
</code></pre>

---

## Recap

---

Represented our request albegras using GADTs

---

Used Free Applicative to batch groups of requests

---

Used Free Monad of Free Applicative to build batchable programs

---

Inspect the next steps of our programs with an interpreter,

batching steps together as much as possible

---

Continue this process until all programs have completed

---

Don't forget to write tests!

---

## Testing

<pre><code data-noescape data-trim class="haskell">
it &quot;should work with small inputs&quot; &#36; do
  <span class="fragment">&#40;gClient&#44; counter&#41; &lt;- newGClientWithCounter
  let bClient = mkGoogleBusinessClient defaultTokenProvider gClient</span>
  <span class="fragment">[e1&#44; e2&#44; e3] &lt;- replicateM 3 randomBEvent</span>
  <span class="fragment">res &lt;- runBClient bClient &#36;
    &#40;&#44;&#44;&#41;
      &lt;&#36;&gt; mkBRequest &#40;BInsertEvent userId e1&#41;
      &lt;&#42;&gt; mkBRequest &#40;BUpdateEvent userId e2&#41;
      &lt;&#42;&gt; mkBRequest &#40;BUpdateEvent userId e3&#41;</span>
  <span class="fragment">readIORef counter &#x60;shouldReturn&#x60; 2</span>
  <span class="fragment">res &#x60;shouldBe&#x60;
    &#40; Right &#36; Inserted          &#36; getExtId e1
    &#44; Right &#36; InsertedViaUpdate &#36; getExtId e2
    &#44; Right &#36; InsertedViaUpdate &#36; getExtId e3
    &#41;</span>
</code></pre>

---

## Testing

<pre><code data-noescape data-trim class="haskell">
it &quot;should work with large&#44; dynamic inputs&quot; &#36; do
  <span class="fragment">&#40;gClient&#44; counter&#41; &lt;- newGClientWithCounter
  let bClient = mkGoogleBusinessClient defaultTokenProvider gClient</span>
  <span class="fragment">actions &#58;&#58; [Some BAction] &lt;- replicateM 100 randomBAction</span>
  <span class="fragment">let req &#58;&#58; FreeAp&#46;Ap BAction Dynamic = sequenceA &#36; dynBRequest &lt;&#36;&gt; actions</span>
  <span class="fragment">res &#58;&#58; [Dynamic] &lt;- runBClient bClient req</span>
  <span class="fragment">length res &#x60;shouldBe&#x60; 100</span>
  <span class="fragment">readIORef counter &gt;&gt;= &#x60;shouldSatisfy&#x60; &#40;&lt;= 4&#41;</span>
  <span class="fragment">traverse&#95; validateDynResponse &#36; zip actions res</span>
</code></pre>

---

## Testing

<pre><code data-noescape data-trim class="haskell">
validateDynResponse &#58;&#58; &#40;Some BAction&#44; Dynamic&#41; -&gt; IO &#40;&#41;
validateDynResponse &#40;sa&#44; d&#41; = <span class="fragment" data-fragment-index="5">withSome sa &#36; \case</span>
  <span class="fragment" data-fragment-index="10">x&#64;&#40;BInsertEvent &#123;&#125;&#41; -&gt; expectJust &#36; proxyDynResponse x d</span>
  <span class="fragment" data-fragment-index="30">x&#64;&#40;BEventExists &#123;&#125;&#41; -&gt; expectJust &#36; proxyDynResponse x d</span>
  <span class="fragment" data-fragment-index="35">x&#64;&#40;BDeleteEvent &#123;&#125;&#41; -&gt; expectJust &#36; proxyDynResponse x d</span>
  <span class="fragment" data-fragment-index="38">x&#64;&#40;BUpdateEvent &#123;&#125;&#41; -&gt; expectJust &#36; proxyDynResponse x d</span>

<span class="fragment" data-fragment-index="20">proxyDynResponse &#58;&#58; &#40;Typeable a&#41; =&gt; BAction a -&gt; Dynamic -&gt; Maybe a
proxyDynResponse &#95; = fromDynamic</span>

<span class="fragment" data-fragment-index="25">proxyDynResponse &#58;&#58; &#40;Typeable a&#41; =&gt; proxy a -&gt; Dynamic -&gt; Maybe a
proxyDynResponse &#95; = fromDynamic</span>
</code></pre>

---

## It works!

<pre><code data-noescape data-trim class="haskell">
Batmon&#46;Google&#46;Calendar&#46;BClientIT
  BClient
    should batch efficiently
    should work with large inputs
Batmon&#46;Google&#46;Calendar&#46;GClientIT
  GClient
    should batch multiple requests
    EventsInsertWithKey should work
    EventsGet should return Nothing if not exists

Finished in 8&#46;7644 seconds
5 examples&#44; 0 failures
</code></pre>

---

## Thank you!

http://caryrobbins.com/batmon-talk

