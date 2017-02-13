# Common Command Processor Modules for Slate

> Provides helper functions for writing Slate Entity Command Processors.

## Install

### Elm

Since the Elm Package Manager doesn't allow for Native code and most everything we write at Panoramic Software has some native code in it,
you have to install this library directly from GitHub, e.g. via [elm-github-install](https://github.com/gdotdesign/elm-github-install) or some equivalent mechanism. It's just not worth the hassle of putting libraries into the Elm package manager until it allows native code.

## Command Processor

The `Command Procesor` is the main engine for processing `Commands` in `Slate`.

A `Command` is typically the output of an API. It consists of the following:

1. A list of events in their JSON format
2. OPTIONAL list of entity ids to lock
3. OPTIONAL Validator Tagger

A single `Command` can optionally lock one or more entities, optionally perform a validation via the [Slate Query Engine](https://github.com/panosoft/slate-query-engine), and upon successful validation, write multiple events to the Slate database. These events are written contigously since **blocks of events written to the Slate database are done serially**.

Locking entities and validation are mutually dependent, i.e. they both exist or neither. Any other configuration will result in a crash.

### Crashes

There are many times that the `Command Processor` will **crash** when something goes wrong. This is to prevent mistakes in programming corrupting a WRITE-ONCE-READ-MANY database.

Deletes and Updates are not allowed in Slate's database. So we must take great care when writing to the database, because once written, it's there forever.

### Impossible States

The idea of making [Impossible States Impossible](https://www.youtube.com/watch?v=IcgmSRJHu_8) won't work everywhere in Slate. This idea is great but doesn't work when you're dealing on the borders of the Functional and the Non-functional world, e.g. Slate code and the database.

The mapping of Elm records to JSON data stored in the Slate database will eventually involve a String. This mapping can't be validated by the compiler because it's not Elm.

Code has been employed to crash BEFORE inconsistent data can be written to the Slate database or if an API is validating without locking, etc.

### Validator Tagger

Imagine a case where a `Bank Account Entity` has a Command to `withdraw` money from the account. Here's a case where you want to validate that the account has enough money in it.

But to do so would involve reading many records from the database most likely using the [Slate Query Engine](https://github.com/panosoft/slate-query-engine). Certainly, not a simple function call.

So a `Validator Tagger` is passed to delegate this complex process to the `Parent` of the `Command Processor` module. The `Parent` could and should delegate this to another module since there are many states and perhaps many asynchronous calls involved in retreiving the data necessary to validate a single Command.

When the overhead to retrieve validation data is high, caching can be employed, making a separate `Validation Module` even more attractive.

### API

#### RouteToMeTagger

Parent's Tagger that will result in calling this modules update function.

```elm
type alias RouteToMeTagger msg =
    Msg -> msg
```
#### CommandErrorTagger

Tagger for command errors.

```elm
type alias CommandErrorTagger msg =
( CommandId, String ) -> msg
```

#### CommandSuccessTagger

Tagger for command success.

```elm
type alias CommandSuccessTagger msg =
CommandId -> msg
```
#### Config

Command Processor's Config

```elm
type alias Config msg =
{ routeToMeTagger : RouteToMeTagger msg
, errorTagger : ErrorTagger ( CommandId, String ) msg
, logTagger : LogTagger ( CommandId, String ) msg
, commandErrorTagger : CommandErrorTagger msg
, commandSuccessTagger : CommandSuccessTagger msg
}
```

#### init

Initialize command helper

```elm
init : Config msg -> ( Model msg, Cmd msg )
init config
```

#### process

Process command.

```elm
process : Config msg -> DbConnectionInfo -> Maybe (ValidateTagger Msg msg) -> List String -> List EntityReference -> Model msg -> ( Model msg, Cmd msg, CommandId )
process config dbConnectionInfo maybeValidateTagger lockEntityIds events model
```

### Usage

See [slate-test-entities](https://github.com/panosoft/slate-test-entities). In particular, `Test.App`.
