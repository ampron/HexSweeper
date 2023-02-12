module AppModel

open GameModel

// MODEL
//------------------------------------------------------------------------------
type Model =
  {
    game: GameState
    keyMap: KeyMap
    developerModeEnabled: bool
  }

and KeyMap = (Model -> Model) array

type Index = int

type Msg =
| Restart
| ToggleFlag of int * int
| Uncover of int * int
| ClearAround of int * int
| SingleKeyPress of Browser.Types.KeyboardEvent
