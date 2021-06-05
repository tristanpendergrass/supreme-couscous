module SelectionList exposing
    ( SelectionList
    , clearSelection
    , create
    , getSelected
    , mapSelection
    , select
    )


type SelectionList t d
    = SelectionList (List (Maybe t)) (Maybe ( t, d )) (List (Maybe t))


create : SelectionList t d
create =
    SelectionList [] Nothing []


clearSelection : SelectionList t d -> SelectionList t d
clearSelection =
    Debug.todo "Implement clearSelection"


select : Int -> SelectionList t d -> Result String (SelectionList t d)
select =
    Debug.todo "Implement select"


mapSelection : (d -> d) -> SelectionList t d -> Result String (SelectionList t d)
mapSelection =
    Debug.todo "Implement mapSelection"


getSelected : SelectionList t d -> Result String ( t, d )
getSelected =
    Debug.todo "Implement getSelected"
