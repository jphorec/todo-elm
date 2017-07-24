import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode

main =
  Html.program
    { init = init []
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
type alias Model =
  {
    todoItems : List TodoItem
  }

init : (List TodoItem) -> ( Model, Cmd Msg )
init todoItems = (Model todoItems
    , getTodoItems todoItems)

type alias TodoItem =
  {
    id: String,
    todoNumber : Int,
    message : String,
    complete: Bool
  }
-- UPDATE

type Msg
  = AddTodo 
  | LoadTodos 
  | LoadedTodos (Result Http.Error (List TodoItem)) 
  | SetComplete String
  | SetIncomplete String
  | UpdateTodo TodoItem 
  | UpdatedTodo (Result Http.Error TodoItem)

--update : Msg -> Model ->  (Model, Cmd Msg )
update msg model =
  case msg of
    AddTodo ->
      (model, Cmd.none)
    LoadTodos ->
      (model, getTodoItems model.todoItems)
    LoadedTodos (Ok todos) ->
      (Model todos, Cmd.none)
    LoadedTodos (Err _) ->
      (model, Cmd.none)
    SetComplete todoId ->
      (model, setComplete model.todoItems todoId)
    SetIncomplete id ->
      (model, Cmd.none)
    UpdateTodo todoItem ->
      (model, updateTodoItem todoItem)
    UpdatedTodo (Ok todoResponse) ->
      (model, Cmd.none)
    UpdatedTodo (Err _) ->
      (model, Cmd.none)
    

setComplete : TodoItem -> String -> Cmd Msg
setComplete  todo todoId =
  if todo.id == todoId then
    UpdateTodo todo
  else
    AddTodo

-- HTTP

getTodoItems : (List TodoItem) -> Cmd Msg
getTodoItems todoItem =
  let
    url =
      "http://localhost:8080/todo-service/todos"
  in
    Http.send LoadedTodos (Http.get url todoItemListDecoder)

updateTodoItem : TodoItem -> Cmd Msg
updateTodoItem todoItem = Http.send UpdatedTodo (put todoItem todoItemDecoder)

put : TodoItem -> Decode.Decoder TodoItem -> Http.Request TodoItem
put todoItem todoItemDecoder = 
  Http.request
    {
      method = "PUT"
    , headers = []
    , url = "localhost:8080/todo-service/todo/" ++ todoItem.id
    , body = (jsonify todoItem)
    , expect = Http.expectJson todoItemDecoder
    , timeout = Nothing
    , withCredentials = False
    }
-- JSON

todoItemDecoder : Decode.Decoder TodoItem
todoItemDecoder = Decode.map4 TodoItem (Decode.field "id" Decode.string) (Decode.field "todoNumber" Decode.int) (Decode.field "message" Decode.string) (Decode.field "complete" Decode.bool)

todoItemListDecoder : Decode.Decoder (List TodoItem)
todoItemListDecoder = Decode.list todoItemDecoder

jsonify : TodoItem -> Http.Body
jsonify todoItem =
    Http.jsonBody <| Encode.object [("id", Encode.string todoItem.id)
                                   , ("todoNumber", Encode.int todoItem.todoNumber)
                                   , ("message", Encode.string todoItem.message)
                                   , ("complete", Encode.bool todoItem.complete)
                                   ]

-- VIEW
view : Model -> Html Msg
view model =
  div [] [
    headerView,
    div [class "container"] 
        (List.map todoView model.todoItems),
    div [class "w3-panel w3-card-4 todo-container row"] [
      Html.p [class "fa fa-plus col-md-3"] [],
      Html.p [class "col-md-7"] [text "  Add Todo"]
    ]
  ]

todoView: TodoItem -> Html Msg
todoView todo =
   div [class "todo-container"] [
            div [] [
                div [class "w3-panel w3-card-4 row"] [
                  Html.p [class "col-md-3"] [
                    input 
                      [type_ "checkbox"
                      , checked todo.complete
                      , Html.Events.onCheck 
                        (\on ->
                          if on then
                            SetComplete todo.id
                          else
                            SetIncomplete todo.id
                        )
                      ] []
                  ], 
                  Html.p [class "col-md-7"] [text todo.message]
                ]
            ]
   ]

headerView: Html Msg
headerView =
 div [class ("navbar navbar-inverse avbar-fixed-top")] [
        div [class "container"] [
            div [class "collapse navbar-collapse", id "bs-example-navbar-collapse-1"] [
                Html.ul [class "nav navbar-brand"] [
                    Html.li [] [Html.a [href "#"] [text "Todo App"]]
                ]
            ]
        ]  
 ]
