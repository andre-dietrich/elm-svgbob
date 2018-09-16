module Main exposing
    ( Model
    , Msg(..)
    , arg
    , init
    , main
    , subscriptions
    , update
    , view
    )

import Html exposing (Html, button, div, pre, text, textarea)
import Html.Attributes exposing (class, contenteditable, id, style)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (string)
import SvgBob



{--code which detects lines and connections
also algorithmn to connect these lines
--}


type alias Model =
    { grid : SvgBob.Model
    }


type Msg
    = Input String
    | Convert


init : ( Model, Cmd Msg )
init =
    ( { grid = SvgBob.init arg
      }
    , Cmd.none
    )


view model =
    div
        [ style
            [ ( "display", "flex" )
            ]
        ]
        [ div []
            [ button
                [ onClick Convert
                ]
                [ text "Convert >>" ]
            ]
        , SvgBob.getSvg [ style [ ( "width", "100%" ) ] ] model.grid
        ]


update msg model =
    case msg of
        Convert ->
            ( model, Cmd.none )

        Input asciiText ->
            ( { model | grid = SvgBob.init asciiText }
            , Cmd.none
            )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


arg =
    """

+------+   +-----+   +-----+   +-----+
|      |   |     |   |     |   |     |
| Foo  +-->| Bar +---+ Baz |<--+ Moo |
|      |   |     |   |     |   |     |
+------+   +-----+   +--+--+   +-----+
              ^         |
              |         V
.-------------+-----------------------.
| Hello here and there and everywhere |
'-------------------------------------'


                        ____________
   .--------------.     \\           \\
  / a == b         \\     \\           \\     __________
 (    &&            )     ) process   )    \\         \\
  \\ 'string' ne '' /     /           /     / process /
   '--------------'     /___________/     /_________/

  User code  ^               ^ OS code
              \\             /
               \\        .--'
                \\      /
  User code  <--- Mode ----> OS code
                /      \\
            .--'        \\___
           /                \\
          v                  v
       User code            OS code

             .---.  .---. .---.  .---.    .---.  .---.
    OS API   '---'  '---' '---'  '---'    '---'  '---'
               |      |     |      |        |      |
               v      v     |      v        |      v
             .------------. | .-----------. |  .-----.
             | Filesystem | | | Scheduler | |  | MMU |
             '------------' | '-----------' |  '-----'
                    |       |      |        |
                    v       |      |        v
                 .----.     |      |    .---------.
                 | IO |<----'      |    | Network |
                 '----'            |    '---------'
                    |              |         |
                    v              v         v
             .---------------------------------------.
             |                  HAL                  |
             '---------------------------------------'


   ____[]
  | ___ |
  ||   ||  device
  ||___||  loads
  | ooo |----------------------------------------------------------.
  | ooo |    |                          |                          |
  | ooo |    |                          |                          |
  '_____'    |                          |                          |
             |                          |                          |
             v                          v                          v
   .-------------------.  .---------------------------.  .-------------------.
   | Loadable module C |  |     Loadable module A     |  | Loadable module B |
   '-------------------'  |---------------------------|  |   (instrumented)  |
             |            |         .-----.           |  '-------------------'
             '------------+-------->| A.o |           |             |
                 calls    |         '-----'           |             |
                          |    .------------------.   |             |
                          |   / A.instrumented.o /<---+-------------'
                          |  '------------------'     |    calls
                          '---------------------------'


                                        .--> Base::Class::Derived_A
                                       /
                                      .----> Base::Class::Derived_B
      Something -------.             /         \\
                        \\           /           \\---> Base::Class::Derived
      Something::else    \\         /             \\
            \\             \\       /               '--> Base::Class::Derived
             \\             \\     /
              \\             \\   .-----------> Base::Class::Derived_C
               \\             \\ /
                '------ Base::Class
                       /  \\ \\ \\
                      '    \\ \\ \\
                      |     \\ \\ \\
                      .      \\ \\ '--- The::Latest
                     /|       \\ \\      \\
 With::Some::fantasy  '        \\ \\      '---- The::Latest::Greatest
                     /|         \\ \\
         More::Stuff  '          \\ '- I::Am::Running::Out::Of::Ideas
                     /|           \\
         More::Stuff  '            \\
                     /              '--- Last::One
         More::Stuff

  Safety
    ^
    |                       *Rust
    |           *Java
    | *Python
    |                        *C++
    +-----------------------------> Control





  TODO:



        |   \\/
       -+-  /\\
        |

        |      |    |      |
        +--  --+    +--  --+   +--  --+
                    |      |   |      |

                     |    |  |     |
             .- -.   .-  -.  ._   _.
             |   |

        .-   -.  .-.
        '-   -'  | |  | |
                      '-'

      \\      |    /  |
       .     '   '   .
       |    /    |    \\

       \\
       /

       /
       \\


       /      \\
      '--    --'
     /          \\

       /   \\
    --'     '--
     /       \\

                       \\         /
       --.--  --.--   --.--   --.--
        /        \\


        |   |
        .   .
       /|   |\\

        |
        .
       / \\

       \\|/
        .
       /|\\


       \\|/
      --.--
       /|\\

       \\|/
      --+--
       /|\\

        |/  \\|
        .    .
        |    |


       -.  -.
       /     \\

        .-  .-
       /     \\


       /   /     \\    \\
      '-  '_     _'   -'


       .-.
      (   )
       '-'

       ..
      (  )
       ''


       .------.
      (        )
       '------'

        ________
       /       /
      /       /
     /_______/


        ________
        \\       \\
         \\       \\
          \\_______\\

       ________
      |________|


       ________
      |        |
      |________|

      .-.
      '-'

        ________
        \\_______\\

       /\\
      /  \\
     /____\\

       /\\
      /  \\
     /    \\
    '------'

       ___
      /   \\
      \\___/

      ______
     /      \\
    /        \\
    \\        /
     \\______/


        +---------+
        |         |                        +--------------+
        |   NFS   |--+                     |              |
        |         |  |                 +-->|   CacheFS    |
        +---------+  |   +----------+  |   |  /dev/hda5   |
                     |   |          |  |   +--------------+
        +---------+  +-->|          |  |
        |         |      |          |--+
        |   AFS   |----->| FS-Cache |
        |         |      |          |--+
        +---------+  +-->|          |  |
                     |   |          |  |   +--------------+
        +---------+  |   +----------+  |   |              |
        |         |  |                 +-->|  CacheFiles  |
        |  ISOFS  |--+                     |  /var/cache  |
        |         |                        +--------------+
        +---------+
    """
