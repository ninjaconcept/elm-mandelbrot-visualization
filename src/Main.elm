module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Time
import Color


type alias Time =
    Float


type alias Point3D =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Point2D =
    { x : Float
    , y : Float
    }


type alias GridCoordinate =
    { x : Float
    , y : Float
    }


type alias Face =
    { points : List Point3D
    , color : String
    }


type alias Complex =
    { real : Float
    , imag : Float
    }


complexAdd : Complex -> Complex -> Complex
complexAdd a b =
    { real = a.real + b.real, imag = a.imag + b.imag }


complexMultiply : Complex -> Complex -> Complex
complexMultiply a b =
    { real = a.real * b.real - a.imag * b.imag
    , imag = a.real * b.imag + a.imag * b.real
    }


complexMagnitudeSquared : Complex -> Float
complexMagnitudeSquared z =
    z.real * z.real + z.imag * z.imag


juliaIteration : Complex -> Complex -> Int -> Int -> Int
juliaIteration z c maxIter currentIter =
    if currentIter >= maxIter || complexMagnitudeSquared z > 4.0 then
        currentIter
    else
        let
            zNext = complexAdd (complexMultiply z z) c
        in
        juliaIteration zNext c maxIter (currentIter + 1)


-- Optimized Julia set calculation with early bailout
juliaIterationOptimized : Complex -> Complex -> Int
juliaIterationOptimized z c =
    let
        maxIter = 20  -- Reduced iterations for performance

        helper zCurrent iter =
            if iter >= maxIter then
                maxIter
            else
                let
                    magSq = complexMagnitudeSquared zCurrent
                in
                if magSq > 4.0 then
                    iter
                else
                    let
                        zNext = complexAdd (complexMultiply zCurrent zCurrent) c
                    in
                    helper zNext (iter + 1)
    in
    helper z 0


-- Precompute Julia set parameters for the current time
getJuliaParams : Time -> Complex
getJuliaParams time =
    let
        cReal = 0.7885 * cos (time / 2000)
        cImag = 0.7885 * sin (time / 3000)
    in
    Complex cReal cImag


-- Mandelbrot set calculation for smoother height transitions
mandelbrotIteration : Complex -> Int
mandelbrotIteration c =
    let
        maxIter = 20

        helper z iter =
            if iter >= maxIter then
                maxIter
            else
                let
                    magSq = complexMagnitudeSquared z
                in
                if magSq > 4.0 then
                    iter
                else
                    let
                        zNext = complexAdd (complexMultiply z z) c
                    in
                    helper zNext (iter + 1)
    in
    helper (Complex 0 0) 0

-- Precalculated animation parameters for performance
getAnimationParams : Time -> { offsetX : Float, offsetY : Float, scale : Float }
getAnimationParams time =
    { offsetX = -0.5 + 0.5 * sin (time / 4000)
    , offsetY = 0.25 * cos (time / 5000)
    , scale = 0.012
    }

-- Optimized Mandelbrot calculation with precalculated params
waveFunction : Int -> GridCoordinate -> Time -> Float
waveFunction variant coord time =
    let
        params = getAnimationParams time
        c = Complex (coord.x * params.scale + params.offsetX) (coord.y * params.scale + params.offsetY)
        iterations = mandelbrotIteration c

        -- Very flat height mapping to see fractal structure
        height =
            if iterations == 20 then
                0  -- Points in set are flat
            else
                toFloat iterations * 0.3  -- Very low height variation
    in
    height


-- Mandelbrot color calculation with smooth gradients
calculateMandelbrotColor : GridCoordinate -> Time -> String
calculateMandelbrotColor coord time =
    let
        -- Use precalculated params for consistency and performance
        params = getAnimationParams time
        c = Complex (coord.x * params.scale + params.offsetX) (coord.y * params.scale + params.offsetY)
        iterations = mandelbrotIteration c

        -- Classic Mandelbrot colors
        hue =
            if iterations == 20 then
                0.0  -- Black for the set
            else
                toFloat iterations / 20.0 * 0.8

        lightness =
            if iterations == 20 then
                0.0  -- Black
            else
                0.3 + (toFloat iterations / 20.0) * 0.5

        saturation = 0.9
    in
    Color.hsl hue saturation lightness
        |> Color.toCssString


gridElement : Int -> GridCoordinate -> Time -> Face
gridElement variant coord time =
    let
        height = waveFunction variant coord time
        color = calculateMandelbrotColor coord time

        -- Single point instead of 4-point polygon
        point = Point3D coord.x coord.y height
    in
    Face [point] color


grid : Int -> Time -> List Face
grid variant time =
    let
        range =
            List.range -50 50 |> List.map (toFloat >> (*) 3)

        coordinates =
            range
                |> List.concatMap
                    (\x ->
                        range |> List.map (\y -> GridCoordinate x y)
                    )
    in
    coordinates
        |> List.map (\coord -> gridElement variant coord time)


type alias Rotation =
    { x : Float
    , y : Float
    }


project3DTo2D : Point3D -> Rotation -> Point2D
project3DTo2D point rot =
    let
        -- Apply rotation around X axis
        cosX =
            cos rot.x

        sinX =
            sin rot.x

        y1 =
            point.y * cosX - point.z * sinX

        z1 =
            point.y * sinX + point.z * cosX

        -- Apply rotation around Y axis
        cosY =
            cos rot.y

        sinY =
            sin rot.y

        x2 =
            point.x * cosY + z1 * sinY

        z2 =
            -point.x * sinY + z1 * cosY

        -- Simple perspective projection
        perspective =
            300 / (300 + z2)
    in
    Point2D (x2 * perspective) (y1 * perspective)


sortByDistance : List Face -> List Face
sortByDistance faces =
    let
        averageZ face =
            face.points
                |> List.map (\p -> p.z)
                |> List.sum
                |> (\sum -> sum / toFloat (List.length face.points))
    in
    faces
        |> List.sortBy averageZ


svgProjection : Int -> Model -> List (Svg Msg)
svgProjection variant model =
    let
        -- Slow automatic camera rotation to keep Mandelbrot visible
        rotX = -0.3 + 0.1 * sin (model.time / 8000)  -- Gentle tilt variation
        rotY = 0.0001 * model.time  -- Very slow Y rotation

        rot = Rotation rotX rotY

        draw face =
            let
                -- Just use the center point of each face
                centerPoint =
                    case face.points of
                        p :: _ -> project3DTo2D p rot
                        [] -> Point2D 0 0
            in
            Svg.circle
                [ SvgAttr.cx (String.fromFloat centerPoint.x)
                , SvgAttr.cy (String.fromFloat centerPoint.y)
                , SvgAttr.r "1.5"
                , SvgAttr.fill face.color
                , SvgAttr.fillOpacity "0.9"
                ]
                []
    in
    model.time
        |> grid variant
        |> sortByDistance
        |> List.map draw


mouseDecoder : Decode.Decoder Msg
mouseDecoder =
    Decode.map2 MouseMove
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


type alias ViewBox =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    }


container : ViewBox -> List (Svg Msg) -> Html Msg
container vb svgs =
    let
        width =
            vb.maxX - vb.minX

        height =
            vb.maxY - vb.minY
    in
    Html.div []
        [ Svg.svg
            [ SvgAttr.width "100vw"
            , SvgAttr.height "100vh"
            , SvgAttr.viewBox (String.fromFloat vb.minX ++ " " ++ String.fromFloat vb.minY ++ " " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
            , style "display" "block"
            ]
            svgs
        ]


view : Model -> Html Msg
view model =
    let
        styles =
            [ style "backgroundColor" "#000000"
            , style "height" "100vh"
            , style "width" "100vw"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "overflow" "hidden"
            ]

        svgs =
            [ container (ViewBox -300 -200 300 200) (svgProjection 1 model) ]
    in
    Html.div styles svgs


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { time : Float
    , mouseX : Float
    , mouseY : Float
    , targetRotX : Float
    , targetRotY : Float
    , currentRotX : Float
    , currentRotY : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , mouseX = 0
      , mouseY = 0
      , targetRotX = 0
      , targetRotY = 0
      , currentRotX = 0
      , currentRotY = 0
      }
    , Cmd.none )


-- UPDATE


type Msg
    = Tick Time.Posix
    | MouseMove Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                newTimeValue = Time.posixToMillis newTime |> toFloat

                -- Smooth interpolation towards target rotation
                smoothing = 0.05  -- Lower = smoother, higher = more responsive

                newCurrentRotX = model.currentRotX + (model.targetRotX - model.currentRotX) * smoothing
                newCurrentRotY = model.currentRotY + (model.targetRotY - model.currentRotY) * smoothing
            in
            ( { model
              | time = newTimeValue
              , currentRotX = newCurrentRotX
              , currentRotY = newCurrentRotY
              }
            , Cmd.none )

        MouseMove x y ->
            let
                -- Calculate target rotation based on mouse position
                targetRotX = (y - 400) / 400 * 0.8
                targetRotY = (x - 400) / 400 * 0.8
            in
            ( { model
              | mouseX = x
              , mouseY = y
              , targetRotX = targetRotX
              , targetRotY = targetRotY
              }
            , Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 33 Tick