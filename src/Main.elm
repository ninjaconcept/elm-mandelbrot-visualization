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
        maxIter = 80

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

-- Advanced fractal exploration with multiple animation layers
getAnimationParams : Time -> { offsetX : Float, offsetY : Float, scale : Float }
getAnimationParams time =
    let
        -- Dramatic zoom effect for "apple man" exploration
        gentleZoom = 0.003 * sin (time / 2000)   -- Enhanced zoom-in effect
        macroZoom = 0.007 * sin (time / 6000)    -- More dramatic deep zoom
        baseScale = 0.010  -- Keep baseline, but increase range
        animatedScale = baseScale + macroZoom + gentleZoom

        -- Very fast spiral exploration pattern
        spiralAngle = -(time / 2000)  -- Much faster spiral flight
        spiralRadius = 0.3 + 0.2 * sin (time / 3000)  -- Faster radius changes
        spiralX = spiralRadius * cos spiralAngle
        spiralY = spiralRadius * sin spiralAngle

        -- Much faster figure-8 overlay pattern
        figure8Speed = time / 1500  -- Much faster figure-8
        figure8X = 0.15 * sin (figure8Speed * 2)
        figure8Y = 0.08 * sin figure8Speed

        -- Much faster breathing exploration
        breathingIntensity = 0.5 + 0.5 * sin (time / 2500)  -- Much faster intensity changes
    in
    { offsetX = -0.5 + breathingIntensity * (spiralX + figure8X)
    , offsetY = breathingIntensity * (spiralY + figure8Y)
    , scale = animatedScale
    }

-- Optimized Mandelbrot calculation with precalculated params
waveFunction : Int -> GridCoordinate -> Time -> Float
waveFunction variant coord time =
    let
        params = getAnimationParams time
        c = Complex (coord.x * params.scale + params.offsetX) (coord.y * params.scale + params.offsetY)
        iterations = mandelbrotIteration c

        -- Enhanced height mapping to show fractal detail
        height =
            if iterations == 80 then
                0  -- Points in set are flat
            else
                toFloat iterations * 0.4  -- Higher variation for detail
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

        -- Enhanced Mandelbrot colors with more detail
        hue =
            if iterations == 80 then
                0.0  -- Black for the set
            else
                -- Multiple color cycles to reveal fine structure
                let
                    normalizedIter = toFloat iterations / 80.0
                    cycles = normalizedIter * 4.0  -- 4 color cycles for detail
                in
                (cycles - toFloat (floor cycles)) * 0.9

        lightness =
            if iterations == 80 then
                0.0  -- Black
            else
                -- Brighter colors for better visibility
                0.5 + (toFloat iterations / 80.0) * 0.4

        saturation = 0.9
    in
    Color.hsl hue saturation lightness
        |> Color.toCssString


gridElement : Int -> GridCoordinate -> Time -> Face
gridElement variant coord time =
    let
        height = waveFunction variant coord time
        color = calculateMandelbrotColor coord time

        -- Flat 2D point without z-translation effects
        point = Point3D coord.x coord.y 0
    in
    Face [point] color


grid : Int -> Time -> List Face
grid variant time =
    let
        range =
            List.range -75 75 |> List.map (toFloat >> (*) 3.2)

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
    , z : Float
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

        -- Apply rotation around Z axis
        cosZ =
            cos rot.z

        sinZ =
            sin rot.z

        x3 =
            x2 * cosZ - y1 * sinZ

        y3 =
            x2 * sinZ + y1 * cosZ

        -- Simple orthographic projection without z-translation
        perspective = 1.0
    in
    Point2D (x3 * perspective) (y3 * perspective)


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
        -- Original camera animation - the best balance
        rotX = -0.3 + 0.1 * sin (model.time / 8000)  -- Gentle tilt variation
        rotY = 0.0001 * model.time  -- Very slow Y rotation
        rotZ = 0  -- No Z rotation for stability

        rot = Rotation rotX rotY rotZ

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
                , SvgAttr.r "1.0"
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
            [ container (ViewBox -400 -300 400 300) (svgProjection 1 model) ]
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