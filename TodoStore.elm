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


withComplete : Bool -> TodoId -> TodoItem -> TodoItem
withComplete complete id item =
  if item.id == id then
    { item | complete <- complete }
  else
    item


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

    Complete id ->
      { model | todos <- List.map (withComplete True id) model.todos }

    UndoComplete id ->
      { model | todos <- List.map (withComplete False id) model.todos }

    ToggleCompleteAll ->
      let
        areAllComplete = List.all .complete model.todos
        toggleComplete complete item =
          { item | complete <- complete }
        allToggled =
          if areAllComplete then
            List.map (toggleComplete False) model.todos
          else
            List.map (toggleComplete True) model.todos
      in
        { model | todos <- allToggled }

    UpdateText id text ->
      let
        withText text id item =
          if item.id == id && not (String.isEmpty text) then
            { item | text <- String.trim text }
          else item
        updatedItems = List.map (withText text id) model.todos
      in
        { model | todos <- updatedItems }

    Destroy id ->
      let
        todosWithoutId = List.filter (\item -> item.id /= id) model.todos
      in
        { model | todos <- todosWithoutId }

    DestroyCompleted ->
      { model | todos <- List.filter (\item -> not item.complete) model.todos }



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
