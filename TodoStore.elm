module TodoStore where


type Action
  = Create String
  | Complete TodoId
  | Destroy TodoId
  | DestroyCompleted
  | ToggleCompleteAll
  | UndoComplete TodoId
  | UpdateText TodoId String


type alias TodoItem =
  { id : String }


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


port dispatchCreate : Signal String
port dispatchComplete : Signal TodoId
port dispatchDestroy : Signal TodoId
port dispatchDestroyCompleted : Signal ()
port dispatchToggleCompleteAll : Signal ()
port dispatchUndoComplete : Signal TodoId
port dispatchUpdateText : Signal (TodoId, String)


update action model =
  model


initialModel =
  { todos = []
  , uid = 1 }


modelChanges =
  Signal.foldp update initialModel actions

port todoListChanges : Signal (List TodoItem)
port todoListChanges = Signal.map .todos modelChanges
                        |> Signal.dropRepeats
