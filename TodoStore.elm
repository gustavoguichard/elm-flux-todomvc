module TodoStore where


import String


type Action
  = Create String
  | Complete TodoId
  | Destroy TodoId
  | DestroyCompleted
  | ToggleCompleteAll
  | UndoComplete TodoId
  | UpdateText TodoId String


type alias TodoItem =
  { id        : Int
  , text      : String
  , complete  : Bool }


type alias TodoId = Int


actions =
  Signal.mergeMany
    [ Signal.map Create dispatchCreate
    , Signal.map Complete dispatchComplete
    , Signal.map Destroy dispatchDestroy
    , Signal.map (always DestroyCompleted) dispatchDestroyCompleted
    , Signal.map (always ToggleCompleteAll) dispatchToggleCompleteAll
    , Signal.map UndoComplete dispatchUndoComplete
    , Signal.map (\(id, text) -> UpdateText id text) dispatchUpdateText ]


port dispatchCreate             : Signal String
port dispatchComplete           : Signal TodoId
port dispatchDestroy            : Signal TodoId
port dispatchDestroyCompleted   : Signal ()
port dispatchToggleCompleteAll  : Signal ()
port dispatchUndoComplete       : Signal TodoId
port dispatchUpdateText         : Signal (TodoId, String)


update action model =
  case action of
    Create untrimmedText ->
      let
        text = String.trim untrimmedText
      in
        if String.isEmpty text then
          model
        else
          { model | todos <- [ TodoItem model.uid text False ] ++ model.todos
                  , uid <- model.uid + 1 }
    _ ->
      model


initialModel =
  { todos = []
  , uid   = 1 }


modelChanges =
  Signal.foldp update initialModel actions

port todoListChanges : Signal (List TodoItem)
port todoListChanges =  Signal.map .todos modelChanges
                        |> Signal.dropRepeats
