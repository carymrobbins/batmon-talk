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

```haskell
runThings = do
  x <- thingX
  y <- thingY
  z <- thingZ
  pure (x, y, z)
```

```haskell
runThings =
  thingX >>= \x ->
    thingY >>= \y ->
      thingZ >>= \z ->
        pure (x, y, z)
```
<!-- .element: class="fragment" -->

---

## Applicative to the rescue?

```haskell
runThings = do
  x <- thingX
  y <- thingY
  z <- thingZ
  pure (x, y, z)
```

```haskell
runThings =
  (,,)
    <$> thingX
    <*> thingY
    <*> thingZ
```
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

```haskell
insertEvent
  :: G.Client         -- ^ Google API client
  -> BEvent           -- ^ Business event
  -> IO BInsertResult -- ^ Business result
insertEvent gClient bEvent = do
  <frag>G.insert gClient bEvent <frag>>>= \case
    Right res -> pure $ Inserted $ key res</frag>
    <frag>Left e0 -> do
      when (status e0 /= 409) $ throwIO e0
      <frag>G.update gClient bEvent >>= <frag>\case
        Right res -> pure $ UpdatedViaInsert $ key res</frag>
        <frag>Left e1 -> throwIO e1</frag>
</frag></frag></frag>
```

Can you spot the problem? <!-- .element: class="fragment" -->

---

## Taking a step back

How do we even batch anything in the first place?

---

## Google Calendar Algebra

```haskell
(insertRes, getRes) <- G.runClient gClient $
  (,)
    <$> G.mkRequest (G.EventsInsert gTok calId event)
    <*> G.mkRequest (G.EventsGet    gTok calId eid)
```

---

## Google Calendar Algebra

<div style='font-size:0.99999em'>
```haskell
<frag>type ActionResult a = Action (Either ClientFailure a)</frag>

data Action a where
  <frag>CalendarsGet
    :: AccessToken
    -> KeyOf Calendar
    -> ActionResult (Maybe (Keyed Calendar))</frag>

  <frag>EventsInsert
    :: AccessToken
    -> KeyOf Calendar
    -> CalendarEvent
    -> ActionResult (Keyed CalendarEvent)</frag>

  <frag>-- and so on</frag>
```
</div>

---

## Free Applicative Refresher

```haskell
module Control.Applicative.Free where

data Ap f a where
  Pure :: a -> Ap f a
  Ap   :: f a -> Ap f (a -> b) -> Ap f b

<frag>liftAp :: f a -> Ap f a</frag>
```

---

## Free Applicative Request

```haskell
import qualified Control.Applicative.Free as FreeAp

type Request = FreeAp.Ap Action

<frag>mkRequest :: Action a -> Request a
mkRequest = FreeAp.liftAp</frag>
```

---

## Google Calendar Client

```haskell
newtype Client f = Client
  { runClient :: forall a. Request a -> f a }
```

---

## Now just implement it!

<img src='img/owl.png'/>

---

## Making a batched client

Dependency inversion isn't just for OO

```haskell
mkBatchedClient :: forall m. (MonadIO m) => HttpHandler m -> Client m


_
```

---

## Making a batched client

Dependency inversion isn't just for OO

```haskell
<gray>mkBatchedClient :: forall m. (MonadIO m) => HttpHandler m -> Client m</gray>

newtype HttpHandler f = HttpHandler
  { runHttpHandler :: RequestPayload -> f BatchResponse }
```

---

## HttpHandler

```haskell
mkBatchedHttpHandler
  :: (MonadIO m, MonadThrow m) => HTTPClient.Manager -> HttpHandler m


_
```

---

## HttpHandler

```haskell
<gray>mkBatchedHttpHandler
  :: (MonadIO m, MonadThrow m) => HTTPClient.Manager -> HttpHandler m</gray>

httpHandlerWithCounter
  :: (MonadIO m) => IORef Int -> HttpHandler m -> HttpHandler m
```

---

## The crucial command

```haskell
data Command a = Command { <frag 2>key :: UUID, </frag><frag 1>action :: Action a</frag> }




_
```

---

## The crucial command

```haskell
<gray>data Command a = Command { key :: UUID, action :: Action a }</gray>

data RequestPayload = RequestPayload
  { boundary :: ByteString
  , commands :: [Some Command]
  }
```

---

## The client for real

```haskell
<frag>mkBatchedClient
  :: forall m. (MonadIO m) => HttpHandler m -> Client m</frag>
<frag>mkBatchedClient httpHandler = Client {runClient}</frag>
  <frag>where
  runClient :: forall a. Request a -> m a
  runClient req = do</frag>
    <frag>cmd <- runApComposed genUUID req</frag>
```

---

## Backtrack to runAp

```haskell
runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
```

```haskell
runApComposed
  :: (Applicative g, Applicative h)
  => (forall x. f x -> g (h x)) -> Ap f a -> g (h a)
```
<!-- .element: class="fragment" -->

```haskell
-- Create a unique UUID for each request
genUUID :: Action a -> m (FreeAp.Ap Command a)
genUUID a = do
  u <- liftIO UUID.nextRandom
  pure $ FreeAp.liftAp $ Command u a
```
<!-- .element: class="fragment" -->

---

## The Client for real

```haskell
<gray>mkBatchedClient
  :: forall m. (MonadIO m) => HttpHandler m -> Client m
mkBatchedClient httpHandler logger = Client {runClient}
  where
  runClient :: forall a. Request a -> m a
  runClient req = do
    cmd <- runApComposed genUUID req</gray>
    <frag>-- Accumulate all the commands into a list
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd</frag>
```

---

## Backtrack to runAp_

```haskell
runAp_ :: Monoid m => (forall a. f a -> m) -> Ap f b -> m

<frag>-- What were we doing again?
runAp_ (\c -> [This c]) cmd</frag>

<frag>-- Specialized
runAp_ :: (forall a. Command a -> [Some Command])
       -> <frag>Ap Command b</frag>
       -> <frag>[Some Command]</frag></frag>
```

---

## The client for real

```haskell
<gray>mkBatchedClient
  :: forall m. (MonadIO m) => HttpHandler m -> Client m
mkBatchedClient httpHandler logger = Client {runClient}
  where
  runClient :: forall a. Request a -> m a
  runClient req = do
    cmd <- runApComposed genUUID req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd</gray>
    <frag>let reader = FreeAp.runAp mkReader cmd</frag>
```

---

## Batched reader

Build reader functions which can decode from our response map

```haskell
mkReader
  :: Command a
  -> ReaderT (Map UUID EncodedResponse) DecodeResult a
<frag>mkReader (Command uuid action) = ReaderT $ \env -></frag>
  <frag>decodeResponse action $ env ! uuid</frag>
```

---

## How does decodeResponse work?

```haskell
class Decoder a where
  decode :: EncodedResponse -> DecodeResult a
```

```haskell
decodeResponse :: Action a -> EncodedResponse -> Response.DecodeResult a
decodeResponse action r = case action of
  <frag>CalendarsGet        {} -> Decoder.decode r</frag>
  <frag>EventsInsert        {} -> Decoder.decode r</frag>
  <frag>EventsInsertWithKey {} -> Decoder.decode r
  EventsUpdate        {} -> Decoder.decode r
  EventsDelete        {} -> Decoder.decode r
  EventsGet           {} -> Decoder.decode r</frag>
```
<!-- .element: class="fragment" -->

---

## The client for real

```haskell
<gray>mkBatchedClient
  :: forall m. (MonadIO m) => HttpHandler m -> Client m
mkBatchedClient httpHandler logger = Client {runClient}
  where
  runClient :: forall a. Request a -> m a
  runClient req = do
    cmd <- runApComposed genUUID req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd
    let reader = FreeAp.runAp mkReader cmd</gray>
    <frag>env <- runCommands cmds</frag>
```

---

## runCommands

```haskell
runCommands :: [Some Command] -> m (Map UUID EncodedResponse)
runCommands cmds = do
  <frag 40>mconcat <$></frag> <frag 20>traverse runChunk</frag> <frag 10>(List.chunksOf batchSize cmds)</frag>
  <frag 15>where
  batchSize = 50 -- Google's maximum batch size</frag>

  <frag 30>runChunk :: [Some Command] -> m (Map UUID EncodedResponse)</frag>
```

---

## The Client for real

```haskell
<gray>mkBatchedClient
  :: forall m. (MonadIO m) => HttpHandler m -> Client m
mkBatchedClient httpHandler logger = Client {runClient}
  where
  runClient :: forall a. Request a -> m a
  runClient req = do
    cmd <- runApComposed genUUID req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd
    let reader = FreeAp.runAp mkReader cmd
    env <- runCommands cmds</gray>
    <frag 15>case</frag> <frag 10>Response.unDecodeResult $</frag> <frag 5>runReaderT reader env</frag> <frag 15>of</frag>
      <frag 15>Right x -> pure x</frag>
      <frag 25>Left e -> throwIO e</frag>
```

---

## Yay, we can batch!

_...Applicatives?_ <!-- .element: class="fragment" -->

---

I thought we were batching monads?

<img src="img/godzilla.png"/>

---

## Free monads aren't the solution

```haskell
module Control.Monad.Free where

data Free f a =
    Pure a
  | Free (f (Free f a))
```

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

```haskell
<frag>type BActionResult a = BAction (Either BFailure a)</frag>

data BAction a where
  <frag>BInsertEvent :: UserId -> BEvent   -> BActionResult BInsertResult</frag>
  <frag>BDeleteEvent :: UserId -> BEventId -> BActionResult BDeleteResult</frag>
  <frag>BEventExists :: UserId -> BEventId -> BActionResult Bool</frag>
  <frag>BUpdateEvent :: UserId -> BEvent   -> BActionResult BUpdateResult</frag>

<frag>type BRequest = FreeAp.Ap BAction</frag>

<frag>mkBRequest :: BAction a -> BRequest a
mkBRequest = FreeAp.liftAp</frag>
```

---

## Business Inc Client

```haskell
newtype BClient f = BClient
  { runBClient :: forall a. BRequest a -> f a }

<frag>mkGoogleBusinessClient
  :: forall m. (MonadIO m)
  => TokenProvider m -> G.Client m -> BClient m</frag>
<frag>mkGoogleBusinessClient tokenProvider gClient = BClient {runBClient}
  where
  runBClient :: BRequest a -> m a
  runBClient req = do
    <frag>cmd <- runApComposed genUUID req</frag>
    <frag>let cmds = FreeAp.runAp_ (\c -> [This c]) cmd</frag>
    </frag>

_
```

---

## Business Inc Client

```haskell
<gray>newtype BClient f = BClient
  { runBClient :: forall a. BRequest a -> f a }

mkGoogleBusinessClient
  :: forall m. (MonadIO m)
  => TokenProvider m -> G.Client m -> BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient {runBClient}
  where
  runBClient :: BRequest a -> m a
  runBClient req = do
    cmd <- runApComposed genUUID req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd</gray>
    let userIds = cmds <&> \(This (BCommand _ a)) -> getUserIdFromBAction a
    <frag>tokens <- runTokenProvider tokenProvider userIds</frag>
    <frag>let inputs = mkInputs tokens cmds</frag>
```

---

## Type aliases for sanity

```haskell
<frag>-- | Alias for a Google Request; represents a single "step" in the interpreter.
type GStep = G.Request</frag>

<frag>-- | Represents a monadic "program" containing several GSteps.
-- The interpreter will batch intermediate GSteps of multiple GLogic programs.
type GLogic = FreeMonad.Free GStep</frag>

<frag>-- | GLogic lifted to deal with failures.
type GAttempt = ExceptT BFailure GLogic</frag>

<frag>-- | Interpreter result of a completed GLogic program.
data IResult a = IResult { action :: BAction a, value :: a }</frag>

<frag>-- | Inputs into the interpreter
type Inputs = Map UUID (GLogic (Some IResult))</frag>
```

---

## Initializing the inputs

```haskell
<frag 10>type Inputs = Map UUID (GLogic (Some IResult))</frag>

mkInputs :: Map UserId G.AccessToken -> [Some BCommand] -> Inputs
<frag 20>mkInputs tokens cmds =</frag>
  <frag 55>Map.fromList $</frag>
    <frag 20>cmds</frag> <frag 30><&> \(This (BCommand cmdId a)) ->
      <frag 40>(cmdId,</frag> <frag 50>This <$> mkGLogic tokens a)</frag></frag>
```

---

## Initializing the inputs

```haskell
mkGLogic :: Map UserId G.AccessToken -> BAction a -> GLogic (IResult a)
mkGLogic tokens = \case
  <frag 70>action@(BInsertEvent uid event) -> go action insertEvent uid event</frag>
  <frag 90>action@(BDeleteEvent uid eid)   -> go action deleteEvent uid eid</frag>
  <frag 95>action@(BEventExists uid eid)   -> go action eventExists uid eid</frag>
  <frag 100>action@(BUpdateEvent uid event) -> go action updateEvent uid event</frag>
  <frag 75>where
  go :: BActionResult b</frag>
     <frag 76>-> (G.AccessToken -> c -> GAttempt b)</frag>
     <frag 77>-> UserId</frag>
     <frag 78>-> c</frag>
     <frag 79>-> GLogic (IResult (Either BFailure b))</frag>
  <frag 80>go action f uid c =</frag> <frag 82>fmap (IResult action) .</frag> <frag 81>runExceptT $</frag> <frag 80>f (tok ! uid) c</frag>
```

---

## Inserting an event with update fallback

```haskell
insertEvent :: G.AccessToken -> BEvent -> GAttempt BInsertResult
insertEvent tok bEvent = <frag>do
  let gEvent = fromBEvent bEvent</frag>
  <frag>gAttempt (G.EventsInsertWithKey tok calId gEvent)</frag> <frag>>>= \case
    Right (G.Keyed k _) -> pure $ Inserted k</frag>
    <frag>Left e0 -> do
      when ((status e0) /= 409) $ throwError $ toBFailure e0</frag>
      <frag>gAttempt (G.EventsUpdate tok calId gEvent)</frag> <frag>>>= \case
        Right (G.Keyed k _) -> pure $ UpdatedViaInsert k</frag>
        <frag>Left e1 -> throwError $ toBFailure e1</frag>
```

---

## Making a Business Client

```haskell
<gray>mkGoogleBusinessClient
  :: forall m. (MonadIO m)
  => TokenProvider m -> G.Client m -> BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient {runBClient}
  where
  runBClient :: BRequest a -> m a
  runBClient req = do
    cmd <- runApComposed mkCmd req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd
    let userIds = cmds <&> \(This (BCommand _ a)) -> getUserIdFromBAction a
    tokens <- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds</gray>
    <frag>results <- runInterpreter inputs</frag>
```

---

## The Interpreter

```haskell
runInterpreter :: Inputs -> m (Map UUID (Some IResult))
runInterpreter inputs =
  <frag>case allComplete inputs of
    Just results -> pure results</frag>
```

```haskell
<frag>-- Returns Nothing if there are any Incomplete values; otherwise returns
-- the final result map.
allComplete :: Inputs -> Maybe (Map UUID (Some IResult))
allComplete =</frag> <frag>traverse $ \case</frag>
  <frag>FreeMonad.Pure v -> Just v</frag>
  <frag>FreeMonad.Free _ -> Nothing</frag>
```

---

```haskell
runInterpreter :: Inputs -> m (Map UUID (Some IResult))
runInterpreter inputs =
  case allComplete inputs of
    Just results -> pure results
    <frag>Nothing -> do</frag>
      <frag>-- Collect the next round of steps to be executed.
      let steps = collectSteps inputs</frag>

<frag>collectSteps :: Inputs -> Map UUID (GStep (GLogic (Some IResult)))
collectSteps =</frag> <frag>Map.mapMaybe $ \case</frag>
  <frag>FreeMonad.Free x -> Just x</frag>
  <frag>FreeMonad.Pure _ -> Nothing</frag>
```

---

## The interpreter loop <frag 200>🎉</frag>

```haskell
<gray>runInterpreter :: Inputs -> m (Map UUID (Some IResult))
runInterpreter inputs =
  case allComplete inputs of
    Just results -> pure results
    Nothing -> do</gray>
      let steps <frag 10>:: Map UUID (GStep (GLogic (Some IResult)))</frag> = collectSteps inputs
      <frag 20>let step  <frag 30>:: GStep (Map UUID (GLogic (Some IResult)))</frag> = sequenceA steps</frag>
      <frag 40>       -- :: GStep Inputs</frag>
      <frag 60>response :: Inputs <- G.runClient gClient step</frag>
      <frag 90>runInterpreter $</frag> <frag 80>response <> inputs</frag>
```

---

## Making a Business Client

```haskell
<gray>mkGoogleBusinessClient
  :: forall m. (MonadIO m)
  => TokenProvider m -> G.Client m -> BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient {runBClient}
  where
  runBClient :: BRequest a -> m a
  runBClient req = do
    cmd <- runApComposed mkCmd req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd
    let userIds = cmds <&> \(This (BCommand _ a)) -> getUserIdFromBAction a
    tokens <- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds</gray>
    results <- runInterpreter inputs
    <frag>FreeAp.runAp (extractor results) cmd</frag>
```

---

## The extractor

```haskell
extractor :: Map UUID (Some IResult) -> BCommand a -> m a
<frag>extractor results (BCommand k action) = case action of</frag>
  <frag>BInsertEvent {} -> case results ! k of
    This (IResult (BInsertEvent {}) x) -> pure x
    This (IResult other _) -> throwTypeMismatch action other</frag>

  <frag>BDeleteEvent {} -> case res of
    This (IResult (BDeleteEvent {}) x) -> pure x
    This (IResult other _) -> throwTypeMismatch action other</frag>

  <frag>-- and so on, the compiler will check for exhaustiveness</frag>
```

---

## Making a Business Client

```haskell
<gray>mkGoogleBusinessClient
  :: forall m. (MonadIO m)
  => TokenProvider m -> G.Client m -> BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient {runBClient}
  where
  runBClient :: BRequest a -> m a
  runBClient req = do
    cmd <- runApComposed mkCmd req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd
    let userIds = cmds <&> \(This (BCommand _ a)) -> getUserIdFromBAction a
    tokens <- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds
    results <- runInterpreter inputs</gray>
    FreeAp.runAp (extractor results) cmd
```

---

## Making a Business Client

```haskell
mkGoogleBusinessClient
  :: forall m. (MonadIO m)
  => TokenProvider m -> G.Client m -> BClient m
mkGoogleBusinessClient tokenProvider gClient = BClient {runBClient}
  where
  runBClient :: BRequest a -> m a
  runBClient req = do
    cmd <- runApComposed mkCmd req
    let cmds = FreeAp.runAp_ (\c -> [This c]) cmd
    let userIds = cmds <&> \(This (BCommand _ a)) -> getUserIdFromBAction a
    tokens <- runTokenProvider tokenProvider userIds
    let inputs = mkInputs tokens cmds
    results <- runInterpreter inputs
    FreeAp.runAp (extractor results) cmd
```

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

```haskell
it "should work with small inputs" $ do
  <frag>(gClient, counter) <- newGClientWithCounter
  let bClient = mkGoogleBusinessClient defaultTokenProvider gClient</frag>
  <frag>[e1, e2, e3] <- replicateM 3 randomBEvent</frag>
  <frag>res <- runBClient bClient $
    (,,)
      <$> mkBRequest (BInsertEvent userId e1)
      <*> mkBRequest (BUpdateEvent userId e2)
      <*> mkBRequest (BUpdateEvent userId e3)</frag>
  <frag>readIORef counter `shouldReturn` 2</frag>
  <frag>res `shouldBe`
    ( Right $ Inserted          $ getExtId e1
    , Right $ InsertedViaUpdate $ getExtId e2
    , Right $ InsertedViaUpdate $ getExtId e3
    )</frag>
```

---

## Testing

```haskell
it "should work with large, dynamic inputs" $ do
  <frag>(gClient, counter) <- newGClientWithCounter
  let bClient = mkGoogleBusinessClient defaultTokenProvider gClient</frag>
  <frag>actions :: [Some BAction] <- replicateM 100 randomBAction</frag>
  <frag>let req :: FreeAp.Ap BAction Dynamic = sequenceA $ dynBRequest <$> actions</frag>
  <frag>res :: [Dynamic] <- runBClient bClient req</frag>
  <frag>length res `shouldBe` 100</frag>
  <frag>readIORef counter >>= `shouldSatisfy` (<= 4)</frag>
  <frag>traverse_ validateDynResponse $ zip actions res</frag>
```

---

## Testing

```haskell
validateDynResponse :: (Some BAction, Dynamic) -> IO ()
validateDynResponse (sa, d) = <frag 5>withSome sa $ \case</frag>
  <frag 10>x@(BInsertEvent {}) -> expectJust $ proxyDynResponse x d</frag>
  <frag 30>x@(BEventExists {}) -> expectJust $ proxyDynResponse x d</frag>
  <frag 35>x@(BDeleteEvent {}) -> expectJust $ proxyDynResponse x d</frag>
  <frag 38>x@(BUpdateEvent {}) -> expectJust $ proxyDynResponse x d</frag>

<frag 20>proxyDynResponse :: (Typeable a) => BAction a -> Dynamic -> Maybe a
proxyDynResponse _ = fromDynamic</frag>

<frag 25>proxyDynResponse :: (Typeable a) => proxy a -> Dynamic -> Maybe a
proxyDynResponse _ = fromDynamic</frag>
```

---

## It works!

```haskell
Batmon.Google.Calendar.BClientIT
  BClient
    should batch efficiently
    should work with large inputs
Batmon.Google.Calendar.GClientIT
  GClient
    should batch multiple requests
    EventsInsertWithKey should work
    EventsGet should return Nothing if not exists

Finished in 8.7644 seconds
5 examples, 0 failures
```

---

## Thank you!

Slides: http://caryrobbins.com/batmon-talk

Code: https://github.com/carymrobbins/batmon-talk
