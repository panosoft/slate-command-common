# Common Command Processor Modules for Slate

> Provides helper functions for writing Slate Entity APIs.

## Install

### Elm

Since the Elm Package Manager doesn't allow for Native code and most everything we write at Panoramic Software has some native code in it,
you have to install this library directly from GitHub, e.g. via [elm-github-install](https://github.com/gdotdesign/elm-github-install) or some equivalent mechanism. It's just not worth the hassle of putting libraries into the Elm package manager until it allows native code.

## API

* [initCommand](#initCommand)
* [lockEntities](#lockEntities)
* [writeEvents](#writeEvents)
* [commit](#commit)
* [rollback](#rollback)
* [createMetaData](#createMetaData)


The API is used to facilitate Entity Command Processor command validation and writing events to the events table for events created by a command.


An Entity Command Processor will use the API in the following way for each command it needs to process:

1. Call `initCommand` to begin a transaction and get a `commandId` for subsequent API calls.
2. Optionally call `lockEntities` to perform validation for Entities referenced by the command being processed.
3. Call `writeEvents` to write events to the events table for events created by the command being processed. The createMetaData API function can be used to help create part of each event written.
4. Either call `commit` to commit the events written to the events table and release any Entity locks obtained, or `rollback` to abort writing events to the events table and release any Entity locks obtained.


### initCommand

> Prepares a command used to insert events into an events table.  Causes a `commandId` to be returned to the caller.

```elm
initCommand : Config msg -> Model -> Result String ( Model, Cmd msg )
initCommand config model
```
### lockEntities

> Locks Entities using `Postgresql` advisory locks.  Used to prevent events being written to the events table for Entities during command validation.

```elm
lockEntities : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd msg )
lockEntities config model commandId entityIds
```
### writeEvents

> Write events to the events table of the target database.

```elm
writeEvents : Config msg -> Model -> CommandId -> List String -> Result String ( Model, Cmd msg )
writeEvents config model commandId events
```
### commit

> Commits the transaction started by initCommand causing written events to be committed and any Entity locks to be released.

```elm
commit : Config msg -> Model -> CommandId -> Result String ( Model, Cmd msg )
commit config model commandId
```

### rollback

> Aborts the transaction started by initCommand causing written events to be rolled back and any Entity locks to be released.

```elm
rollback : Config msg -> Model -> CommandId -> Result String ( Model, Cmd msg )
rollback config model commandId
```

### createMetaData

> Creates the record for the `metadata` field in an event.

```elm
createMetaData : String -> String -> Metadata
createMetaData initiatorId command
```
