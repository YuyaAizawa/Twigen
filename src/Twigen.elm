module Twigen exposing (main)

import Base64
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Bytes.Decode as BD
import Html exposing (Html, div, h1, h3, button, ul, li, text, textarea, br, table, tbody, tr, td, select, option, input, p, a)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Random exposing (Generator, map, map2, map3, lazy)
import Task
import Url exposing (Url)



main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url
    , sentences : List String
    , tango : TangoData
    , tuikaSettei : TuikaSettei
    }

type alias TangoData =
    { meisi : List Meisi
    , keiyousi : List Keiyousi
    , dousi : List Dousi
    }

type alias TuikaSettei =
    { gokan : String
    , gyou : String
    , katuyoukei : String
    , syurui : String
    }



-- UPDATE


type Msg
    = Roll
    | NewSentences (List String)
    | TangoUpdate TangoData
    | TuikaSetteiUpdate TuikaSettei
    | LinkClicked UrlRequest
    | UrlChanged Url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                command =
                    Random.generate NewSentences
                        <| Random.list 10
                        <| sentence model.tango
            in
                ( model, command )

        NewSentences newSentences ->
            ( { model | sentences = newSentences }, Cmd.none )

        TangoUpdate tango ->
            ( { model | tango = tango }
            , Nav.pushUrl model.key <| Url.toString <| updateQuery tango model.url
            )

        TuikaSetteiUpdate settei ->
            ( { model | tuikaSettei = settei }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                model_ =
                    { model | url = url }

                maybeTango =
                    url.query
                        |> Maybe.andThen tangoFromQuery
            in
                case maybeTango of
                    Nothing ->
                        ( model_, Cmd.none )

                    Just tango ->
                        ( { model_ | tango = tango }, Cmd.none )

updateQuery : TangoData -> Url -> Url
updateQuery tango url =
    { url | query = tangoToQuery tango }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "クソツイジェネレータ"
    , body =
        [ mainView model
        , tweetButton model.url
        , p []
            [ text <| "code:"
            , a
                [ Attr.href "https://github.com/YuyaAizawa/Twigen"]
                [ text <| "github.com/YuyaAizawa/Twigen" ]
            ]
        ]
    }

mainView : Model -> Html Msg
mainView model =
    div []
        [ h1 [][ text "クソツイジェネレータ" ]
        , ul []
          (model.sentences
              |> List.map (\s ->
                  li[][text s]
              )
          )
        , button [ onClick Roll ] [ text "Roll" ]
        , div [ Attr.style "display" "flex" ]
            [ div []
                [ h3 [][ text <| "名詞" ]
                , textarea
                    [ Attr.value <| (model.tango.meisi |> String.join "\n")
                    , onInput <| meisiKousin model.tango
                    , Attr.style "resize" "none"
                    ][]
                ]
            , div []
                [ h3 [][ text <| "形容詞" ]
                , textarea
                    [ Attr.value <| (model.tango.keiyousi |> String.join "\n")
                    , onInput <| keiyousiKousin model.tango
                    , Attr.style "resize" "none"
                    ][]
                ]
            ]
        , div []
            [ h3 [][ text <| "動詞" ]
            , table []
                [ tbody
                    [ Attr.style "overflow-y" "scroll"
                    , Attr.style "display" "block"
                    , Attr.style "height" "100px"
                    ]
                    ( model.tango.dousi
                        |> List.indexedMap (\i (Dousi gokan katuyou syurui) ->
                            tr []
                                [ td [][ text <| gokan ]
                                , td [][ text <| katuyouToString katuyou ]
                                , td [][ text <| dousiSyuruiToString <| syurui ]
                                , td []
                                    [ button
                                        [ onClick <| dousiSakujo i model.tango ]
                                        [ text <| "削除" ]
                                    ]
                                ]
                        )
                    )
                ]
            , input
                [ Attr.type_ "text"
                , Attr.placeholder "動詞の語幹"
                , onInput <| gokanSettei model.tuikaSettei
                ][]
            , select
                [ onInput <| gyouSettei model.tuikaSettei ]
                [ option [ Attr.value "" ][ text <| "(なし)" ]
                , option [ Attr.value "あ" ][ text <| "あ行" ]
                , option [ Attr.value "か" ][ text <| "か行" ]
                , option [ Attr.value "が" ][ text <| "が行" ]
                , option [ Attr.value "さ" ][ text <| "さ行" ]
                , option [ Attr.value "ざ" ][ text <| "ざ行" ]
                , option [ Attr.value "た" ][ text <| "た行" ]
                , option [ Attr.value "だ" ][ text <| "だ行" ]
                , option [ Attr.value "な" ][ text <| "な行" ]
                , option [ Attr.value "ば" ][ text <| "ば行" ]
                , option [ Attr.value "ま" ][ text <| "ま行" ]
                , option [ Attr.value "ら" ][ text <| "ら行" ]
                , option [ Attr.value "わ" ][ text <| "わ行" ]
                ]
            , select
                [ onInput <| katuyoukeiSettei model.tuikaSettei ]
                [ option [ Attr.value "五段" ][ text <| "五段" ]
                , option [ Attr.value "上一" ][ text <| "上一" ]
                , option [ Attr.value "下一" ][ text <| "下一" ]
                , option [ Attr.value "さ変格" ][ text <| "変格" ]
                ]
            , select
                [ onInput <| syuruiSettei model.tuikaSettei ]
                [ option [ Attr.value "自動詞" ][ text <| "自動詞" ]
                , option [ Attr.value "他動詞" ][ text <| "他動詞" ]
                , option [ Attr.value "両方"   ][ text <| "両方" ]
                ]
            , button
                [ onClick <| dousiTuika model.tuikaSettei model.tango ]
                [ text <| "追加" ]
            ]
        ]

meisiKousin : TangoData -> String -> Msg
meisiKousin tango str =
    let
        meisi_ =
            str
                |> String.split "\n"
    in
        TangoUpdate { tango | meisi = meisi_ }

keiyousiKousin : TangoData -> String -> Msg
keiyousiKousin tango str =
    let
        keiyousi_ =
            str
                |> String.split "\n"
    in
        TangoUpdate { tango | keiyousi = keiyousi_ }

gokanSettei : TuikaSettei -> String -> Msg
gokanSettei settei gokan =
    TuikaSetteiUpdate { settei | gokan = gokan }

gyouSettei settei gyou =
    TuikaSetteiUpdate { settei | gyou = gyou }

katuyoukeiSettei settei katuyoukei =
    TuikaSetteiUpdate { settei | katuyoukei = katuyoukei }

syuruiSettei settei syurui =
    TuikaSetteiUpdate { settei | syurui = syurui }

dousiTuika settei tango =
    let
        dousi_ =
            Maybe.map2
            (\k s -> Dousi settei.gokan k s)
            (katuyouFromString settei.gyou settei.katuyoukei)
            (dousiSyuruiFromString settei.syurui)

        list_ =
            case dousi_ of
                Just d -> d :: tango.dousi
                Nothing -> tango.dousi
    in
        TangoUpdate { tango | dousi = list_ }

dousiSakujo at tango =
    let
        help at_ rest result =
            case ( at_, rest ) of
                ( 0, hd :: tl ) ->
                    help (-1) tl result

                ( n, hd :: tl ) ->
                    help (n - 1) tl (hd :: result)

                _ ->
                    result |> List.reverse
    in
        TangoUpdate { tango | dousi = help at tango.dousi [] }


tweetButton : Url -> Html Msg
tweetButton url =
    a
        [ Attr.href "https://twitter.com/share"
        , Attr.class "twitter-share-button"
        , Attr.attribute "data-text" "好きな単語でくすっとできるクソツイを生成＆シェア"
        , Attr.attribute "data-url" (Url.toString url)
        , Attr.attribute "data-hashtags" "クソツイジェネレータ"
        , Attr.attribute "data-related" "TypedTypelessTy"
        ]
        [ text <| "作成したジェネレータをTweetで共有" ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- TWEET PATTERN DIFINITIONS


sentence tango =
    choice
    [ intermittentReference tango
    , dagaomaeha tango
    , declarative tango
    , declarative tango -- weight
    , kimidakeno tango
    , haityuu tango
    , otherSentence tango
    ]

intermittentReference tango =
    choice
    [ seq [ meisiKu tango, c "，", dousiKu tango |> map dousiKuRenyou1, c "がち．" ]
    , seq [ meisiKu tango, c "，", meisiKu tango, c "みがある．" ]
    , seq [ meisiKu tango, c "，", meisi tango, c "じゃん．" ]
    ]

dagaomaeha tango =
    let
        ore =
            choice
            [ c "俺"
            , c "ワイ"
            , c "私"
            ]

        body =
            choice
            [ meisiKu tango |> map (\m -> m++"だ")
            , keiyousiGokan tango |> map keiyousiSyuusi
            , dousiKu tango |> map dousiKuSyuusi
            ]

        omae =
            choice
            [ c "お前"
            , c "きみ"
            , c "あなた"
            ]
    in
        seq [ ore, c "は", body, c "が", omae, c "は？" ]

otherSentence tango =
    choice
    [ seq [ meisiKu tango, c "で申し訳ないよ😢" ]
    , seq [ meisiKu tango, c "のNASA" ]
    , seq [ keiyousi tango |> map keiyousiSyuusi, c "いいいいいいいい✌('ω'✌ )三✌('ω')✌三( ✌'ω')✌" ]
    , seq [ c "無限に", tadousi tango |> map dousiMizen, c "れるお前の人生" ]
    , seq [ c "(", meisiKu tango, c "は", tadousi tango |> map dousiMizen, c ")ないです" ]
    , seq [ jidousi tango |> map dousiMeirei, c "！そなたは", keiyousi tango |> map keiyousiSyuusi ]
    ]

declarative tango =
    let
        body =
            choice
            [ seq [ meisiKu tango, c "は", meisiKu tango, choice [ c "", c "だ", c "である", c "でない", c "でできている" ] ]
            , seq [ meisiKu tango, c "には", meisiKu tango, c "がある" ]
            , seq [ meisiKu tango, c "は", keiyousiGokan tango |> map keiyousiSyuusi ]
            , seq [ meisiKu tango, c "は", dousiKu tango |> map dousiKuSyuusi, choice [ c "", c "ことがある"] ]
            , seq [ meisiKu tango, c "は", dousiKu tango |> map dousiKuMizen, c "ない", choice [ c "", c "ことがある"] ]
            , seq [ meisiKu tango, c "を", tadousi tango |> map dousiRenyou2, c "てはいけない" ]
            ]
    in
        choice
        [ seq [ body, c "． #宣言的知識" ]
        , seq [ body, c "（ホンマか？"]
        , seq [ body, c "．知らんけど．"]
        , seq [ body, c "って魔剤？" ]
        , seq [ c "もしかして：", body ]
        ]

kimidakeno tango =
    seq
    [ c "君だけの"
    , meisi tango
    , c "を"
    , tadousi tango |> map dousiRenyou2
    , c "て最強の"
    , meisi tango
    , c "を作り出せ！"
    ]

haityuu tango =
    Random.map2
    (\s1 ( s2, d ) ->
        s1 ++ "は" ++ s2 ++ dousiSyuusi d ++ "場合と" ++ dousiMizen d ++ "ない場合があるぞい．"
    )
    (meisiKu tango)
    (dousiKu tango)


choice : List (Generator a) -> Generator a
choice list =
    case list of
        [] -> Random.constant <| Debug.todo "Error"
        hd :: tl -> Random.uniform hd tl |> Random.andThen (\r -> r)

seq : List (Generator String) -> Generator String
seq list =
    case list of
    [] -> Random.constant ""
    hd :: tl -> Random.map2 (\hdStr tlStr -> hdStr ++ tlStr) hd (seq tl)

c = Random.constant



--- SYNTAX


type alias Meisi = String

meisi : TangoData -> Generator String
meisi tango =
    let
        sahenMeisi =
            tango.dousi
                |> List.filter (\(Dousi _ katuyoukei _) -> katuyoukei == Sahen )
                |> List.map (\(Dousi gokan _ _) -> gokan)
    in
        tango.meisi ++ sahenMeisi
            |> generatorFromList "○○"

meisiKu : TangoData -> Generator String
meisiKu tango =
    choice
    [ meisi tango
    , meisi tango -- weight
    , meisi tango -- weight
    , map2 -- 「の」
        (\m1 m2 -> m1 ++ "の" ++ m2)
        (lazy (\_ -> meisiKu tango))
        (meisi tango)
    , map2 -- 形容詞の連体形
        (\k m -> keiyousiRentai k ++ m)
        (keiyousi tango)
        (meisi tango)
    , map2 -- 用言の連体形
        (\dk m -> dousiKuRentai dk ++ m)
        (lazy (\_ -> dousiKu tango))
        (meisi tango)
    ]


type alias Keiyousi = String

keiyousi : TangoData -> Generator KeiyousiGokan
keiyousi tango =
    tango.keiyousi
        |> generatorFromList dummyKeiyousi
        |> map KeiyousiGokan

type KeiyousiGokan = KeiyousiGokan String

keiyousiGokan : TangoData -> Generator KeiyousiGokan
keiyousiGokan tango =
    let
        ppoi =
            generatorFromList ""
            [ "っぽ"
            , "らし"
            ]

        nikui =
            generatorFromList ""
            [ "難"
            , "辛"
            , "やす"
            ]
    in
        choice
        [ keiyousi tango
        , keiyousi tango -- weight
        , keiyousi tango -- weight
        , keiyousi tango -- weight
        , map3 -- 名詞 助動詞
            (\m j (KeiyousiGokan k) -> m ++ j ++ "く" ++ k |> KeiyousiGokan)
            (meisiKu tango)
            ppoi
            (keiyousi tango)
        , map3 -- 動詞の連用形＋付属語
            (\dk n (KeiyousiGokan k) -> dousiKuRenyou1 dk ++ n ++ "く" ++ k |> KeiyousiGokan)
            (dousiKu tango)
            nikui
            (keiyousi tango)
        ]

keiyousiKatuyou : Katuyoukei -> KeiyousiGokan -> String
keiyousiKatuyou katuyoukei (KeiyousiGokan gokan) =
    let
        gobi =
            case katuyoukei of
                Mizen   -> "かろ"
                Renyou1 -> "かっ"
                Renyou2 -> "く"
                Syuusi  -> "い"
                Rentai  -> "い"
                Katei   -> "けれ"
                Meirei  -> "Error"
    in
        gokan ++ gobi

keiyousiMizenn  = keiyousiKatuyou Mizen
keiyousiRenyou1 = keiyousiKatuyou Renyou1
keiyousiRenyou2 = keiyousiKatuyou Renyou2
keiyousiSyuusi  = keiyousiKatuyou Syuusi
keiyousiRentai  = keiyousiKatuyou Rentai
keiyousiKatei   = keiyousiKatuyou Katei


type Dousi =
    Dousi String Katuyou DousiSyurui

type Katuyou
    = Godan String
    | Kami String
    | Shimo String
    | Sahen

katuyouToString katuyou =
    case katuyou of
        Godan gyou -> gyou ++ "五段"
        Kami  gyou -> gyou ++ "上一"
        Shimo gyou -> gyou ++ "下一"
        Sahen      -> "さ変格"

katuyouFromString gyou katuyou =
    case katuyou of
        "五段"   -> Godan gyou |> Just
        "上一"   -> Kami  gyou |> Just
        "下一"   -> Shimo gyou |> Just
        "さ変格" -> Sahen      |> Just
        _ -> Nothing

type DousiSyurui
    = Jidousi
    | Tadousi
    | Ryouhou

dousiSyuruiToString syurui =
    case syurui of
        Jidousi -> "自動詞"
        Tadousi -> "他動詞"
        Ryouhou -> "両方"

dousiSyuruiFromString syurui =
    case syurui of
        "自動詞" -> Jidousi |> Just
        "他動詞" -> Tadousi |> Just
        "両方"   -> Ryouhou |> Just
        _ -> Nothing

jidousi : TangoData -> Generator Dousi
jidousi tango =
    tango.dousi
        |> List.filter (\(Dousi _ _ syurui) ->
            syurui == Jidousi || syurui == Ryouhou)
        |> generatorFromList dummyDousi

tadousi : TangoData -> Generator Dousi
tadousi tango =
    tango.dousi
        |> List.filter (\(Dousi _ _ syurui) ->
            syurui == Tadousi || syurui == Ryouhou)
        |> generatorFromList dummyDousi

type alias DousiKu = ( String, Dousi )

dousiKu : TangoData -> Generator DousiKu
dousiKu tango =
    let
        hukusi =
            generatorFromList ""
            [ "とても"
            , "非常に"
            , "みるからに"
            , "多少は"
            ]

        youni =
            generatorFromList ""
            [ "ように" ]

        jidou =
             jidousi tango |> map (\d -> ("", d))

        tadou =
            map2
            (\m d -> (m++"を", d))
            (lazy (\_ -> meisiKu tango))
            (tadousi tango)
    in
        choice
        [ jidou
        , jidou -- weight
        , jidou -- weight
        , tadou
        , tadou -- weight
        , tadou -- weight
        , map3 -- 動詞の連体形＋「ように」
            (\dk y d2 -> ( dousiKuRentai dk ++ y, d2 ))
            (lazy (\_ -> dousiKu tango))
            youni
            (jidousi tango)
        , map2 -- 形容詞の連用形
            (\k ( s, d ) -> ( keiyousiRenyou2 k ++ s, d ))
            (lazy (\_ -> keiyousiGokan tango))
            (lazy (\_ -> dousiKu tango))
        , map2 -- 副詞
            (\h ( s, d ) -> ( h++s, d ))
            hukusi
            (lazy (\_ -> dousiKu tango))
        ]

type Katuyoukei
    = Mizen
    | Renyou1
    | Renyou2
    | Syuusi
    | Rentai
    | Katei
    | Meirei

dousiKatuyou : Katuyoukei -> Dousi -> String
dousiKatuyou katuyoukei dousi =
    let
        (Dousi gokan katuyou _) =
            dousi
        (Katuyougobi mizen ( renyou1, renyou2 ) syuusi rentai katei meirei) =
            katuyougobi katuyou

        gobi =
            case katuyoukei of
                Mizen -> mizen
                Renyou1 -> renyou1
                Renyou2 -> renyou2
                Syuusi -> syuusi
                Rentai -> rentai
                Katei -> katei
                Meirei -> meirei
    in
        gokan ++ gobi

dousiMizen   = dousiKatuyou Mizen
dousiRenyou1 = dousiKatuyou Renyou1
dousiRenyou2 = dousiKatuyou Renyou2
dousiSyuusi  = dousiKatuyou Syuusi
dousiRentai  = dousiKatuyou Rentai
dousiKatei   = dousiKatuyou Katei
dousiMeirei  = dousiKatuyou Meirei

dousiKuKatuyou : Katuyoukei -> ( String, Dousi ) -> String
dousiKuKatuyou katuyoukei ( syuusyoku, dousi ) =
    syuusyoku ++ dousiKatuyou katuyoukei dousi

dousiKuMizen   = dousiKuKatuyou Mizen
dousiKuRenyou1 = dousiKuKatuyou Renyou1
dousiKuRenyou2 = dousiKuKatuyou Renyou2
dousiKuSyuusi  = dousiKuKatuyou Syuusi
dousiKuRentai  = dousiKuKatuyou Rentai
dousiKuKatei   = dousiKuKatuyou Katei
dousiKuMeirei  = dousiKuKatuyou Meirei

type Katuyougobi =
    Katuyougobi String ( String, String ) String String String  String

katuyougobi : Katuyou -> Katuyougobi
katuyougobi katuyou =
    case katuyou of
        Godan "か" -> Katuyougobi "か" ( "き", "い" ) "く" "く" "け" "け"
        Godan "が" -> Katuyougobi "が" ( "ぎ", "い" ) "ぐ" "ぐ" "げ" "げ"
        Godan "さ" -> Katuyougobi "さ" ( "し", "し" ) "す" "す" "せ" "せ"
        Godan "た" -> Katuyougobi "た" ( "ち", "っ" ) "つ" "つ" "て" "て"
        Godan "な" -> Katuyougobi "な" ( "に", "ん" ) "ぬ" "ぬ" "ね" "ね"
        Godan "ば" -> Katuyougobi "ば" ( "び", "ん" ) "ぶ" "ぶ" "べ" "べ"
        Godan "ま" -> Katuyougobi "ま" ( "み", "ん" ) "む" "む" "め" "め"
        Godan "ら" -> Katuyougobi "ら" ( "り", "っ" ) "る" "る" "れ" "れ"
        Godan "わ" -> Katuyougobi "わ" ( "い", "っ" ) "う" "う" "え" "え"
        Kami  ""   -> Katuyougobi "" ( "", "" ) "る" "る" "れ" "ろ"
        Kami  "あ" -> Katuyougobi "い" ( "い", "い" ) "いる" "いる" "いれ" "いろ"
        Kami  "か" -> Katuyougobi "き" ( "き", "き" ) "きる" "きる" "きれ" "きろ"
        Kami  "が" -> Katuyougobi "ぎ" ( "ぎ", "ぎ" ) "ぎる" "ぎる" "ぎれ" "ぎろ"
        Kami  "ざ" -> Katuyougobi "じ" ( "じ", "じ" ) "じる" "じる" "じれ" "じろ"
        Kami  "た" -> Katuyougobi "ち" ( "ち", "ち" ) "ちる" "ちる" "ちれ" "ちろ"
        Kami  "ば" -> Katuyougobi "び" ( "び", "び" ) "びる" "びる" "びれ" "びろ"
        Kami  "ま" -> Katuyougobi "み" ( "み", "み" ) "みる" "みる" "みれ" "みろ"
        Kami  "ら" -> Katuyougobi "り" ( "り", "り" ) "りる" "りる" "りれ" "りろ"
        Shimo ""   -> Katuyougobi "" ( "", "" ) "る" "る" "れ" "ろ"
        Shimo "あ" -> Katuyougobi "え" ( "え", "え" ) "える" "える" "えれ" "えろ"
        Shimo "か" -> Katuyougobi "け" ( "け", "け" ) "ける" "ける" "けれ" "けろ"
        Shimo "が" -> Katuyougobi "げ" ( "げ", "げ" ) "げる" "げる" "げれ" "げろ"
        Shimo "さ" -> Katuyougobi "せ" ( "せ", "せ" ) "せる" "せる" "せれ" "せろ"
        Shimo "ざ" -> Katuyougobi "ぜ" ( "ぜ", "ぜ" ) "ぜる" "ぜる" "ぜれ" "ぜろ"
        Shimo "た" -> Katuyougobi "て" ( "て", "て" ) "てる" "てる" "てれ" "てろ"
        Shimo "だ" -> Katuyougobi "で" ( "で", "で" ) "でる" "でる" "でれ" "でろ"
        Shimo "ば" -> Katuyougobi "べ" ( "べ", "べ" ) "べる" "べる" "べれ" "べろ"
        Shimo "ま" -> Katuyougobi "め" ( "め", "め" ) "める" "める" "めれ" "めろ"
        Shimo "ら" -> Katuyougobi "れ" ( "れ", "れ" ) "れる" "れる" "れれ" "れろ"
        Sahen      -> Katuyougobi "し" ( "し", "し" ) "する" "する" "すれ" "せよ"
        _          -> Katuyougobi "Error" ( "Error", "Error" ) "Error" "Error" "Error" "Error"

generatorFromList : a -> List a -> Generator a
generatorFromList dummy list =
    case list of
        [] -> Random.constant dummy
        hd :: tl -> Random.uniform hd tl

dummyDousi =
    Dousi "○○" Sahen Ryouhou

dummyKeiyousi =
    "○○し"



-- SERIALIZE


tangoToQuery : TangoData -> Maybe String
tangoToQuery tango =
    tango
        |> tangoToBytes
        |> Base64.fromBytes
        |> Maybe.map base64ToUri

tangoFromQuery : String -> Maybe TangoData
tangoFromQuery query =
    query
        |> uriToBase64
        |> Base64.toBytes
        |> Maybe.andThen tangoFromBytes

tangoToBytes : TangoData -> Bytes
tangoToBytes data =
    let
        encoder =
            BE.sequence
                [ list255Encoder stringEncoder data.meisi
                , list255Encoder stringEncoder data.keiyousi
                , list255Encoder dousiEncoder data.dousi
                ]
    in
        BE.encode encoder

tangoFromBytes : Bytes -> Maybe TangoData
tangoFromBytes bytes =
    let
        decoder =
            list255Decoder stringDecoder |> BD.andThen (\m ->
            list255Decoder stringDecoder |> BD.andThen (\k ->
            list255Decoder dousiDecoder |> BD.map (\d ->
                { meisi = m, keiyousi = k, dousi = d }
            )))
    in
        BD.decode decoder bytes

stringEncoder : String -> BE.Encoder
stringEncoder str =
    BE.sequence
    [ BE.unsignedInt8 <| BE.getStringWidth str
    , BE.string str
    ]

stringDecoder : BD.Decoder String
stringDecoder =
    BD.unsignedInt8
        |> BD.andThen BD.string

dousiEncoder : Dousi -> BE.Encoder
dousiEncoder dousi =
    let
        (Dousi gokan _ _) =
            dousi
    in
        BE.sequence
        [ BE.unsignedInt8 <| BE.getStringWidth gokan
        , BE.string gokan
        , gokanIgaiEncoder dousi
        ]

dousiDecoder : BD.Decoder Dousi
dousiDecoder =
    BD.unsignedInt8
        |> BD.andThen BD.string
        |> BD.andThen gokanIgaiDecoder

gokanIgaiEncoder (Dousi _ katuyoukei syurui) =
    let
        gyou =
            (case katuyoukei of
                Godan g -> gyouToInt g
                Kami  g -> gyouToInt g
                Shimo g -> gyouToInt g
                Sahen   -> Just 0)
            |> Maybe.withDefault 0

        katuyoukeiSyurui =
            katuyoukeiSyuruiToInt katuyoukei

        syuruiInt =
            dousiSyuruiToInt syurui
    in
        syuruiInt * 64 + katuyoukeiSyurui * 16 + gyou
            |> BE.unsignedInt8

gokanIgaiDecoder : String -> BD.Decoder Dousi
gokanIgaiDecoder gokan =
    let
        gyou i =
            gyouFromInt (modBy 16 i)

        katuyoukeiSyurui i =
            katuyoukeiSyuruiFromInt (modBy 64 i // 16)

        dousiSyurui i =
            dousiSyuruiFromInt (i // 64)

        gokanIgai =
            (\i ->
                Maybe.map3
                (\g k s -> Dousi gokan (k g) s)
                (gyou i)
                (katuyoukeiSyurui i)
                (dousiSyurui i)
            )
    in
        BD.unsignedInt8
            |> BD.andThen (
                gokanIgai
                    >> Maybe.map BD.succeed
                    >> Maybe.withDefault BD.fail
            )

list255Encoder : (a -> BE.Encoder) -> List a -> BE.Encoder
list255Encoder aEncoder list =
    BE.sequence <|
        BE.unsignedInt8 (list |> List.length)
            :: (list |> List.reverse |> List.map aEncoder)

list255Decoder : BD.Decoder a -> BD.Decoder (List a)
list255Decoder decoder =
    let
        listStep : BD.Decoder a -> (Int, List a) -> BD.Decoder (BD.Step (Int, List a) (List a))
        listStep decoder_ (n, xs) =
            if n <= 0 then
                BD.succeed (BD.Done xs)
            else
                BD.map (\x -> BD.Loop (n - 1, x :: xs)) decoder_
    in
        BD.unsignedInt8
            |> BD.andThen (\len -> BD.loop (len, []) (listStep decoder))

gyouToInt g =
    case g of
        ""   -> Just 0
        "あ" -> Just 1
        "か" -> Just 2
        "が" -> Just 3
        "さ" -> Just 4
        "ざ" -> Just 5
        "た" -> Just 6
        "だ" -> Just 7
        "な" -> Just 8
        "は" -> Just 9
        "ば" -> Just 10
        "ぱ" -> Just 11
        "ま" -> Just 12
        "や" -> Just 13
        "ら" -> Just 14
        "わ" -> Just 15
        _ -> Nothing

gyouFromInt i =
    case i of
        0  -> Just ""
        1  -> Just "あ"
        2  -> Just "か"
        3  -> Just "が"
        4  -> Just "さ"
        5  -> Just "ざ"
        6  -> Just "た"
        7  -> Just "だ"
        8  -> Just "な"
        9  -> Just "は"
        10 -> Just "ば"
        11 -> Just "ぱ"
        12 -> Just "ま"
        13 -> Just "や"
        14 -> Just "ら"
        15 -> Just "わ"
        _ -> Nothing

katuyoukeiSyuruiToInt katuyoukei =
    case katuyoukei of
        Godan _ -> 0
        Kami _  -> 1
        Shimo _ -> 2
        Sahen   -> 3

katuyoukeiSyuruiFromInt i =
    case i of
        0 -> Just Godan
        1 -> Just Kami
        2 -> Just Shimo
        3 -> Just (\_ -> Sahen)
        _ -> Nothing

dousiSyuruiToInt syurui =
    case syurui of
        Jidousi -> 0
        Tadousi -> 1
        Ryouhou -> 2

dousiSyuruiFromInt i =
    case i of
        0 -> Just Jidousi
        1 -> Just Tadousi
        2 -> Just Ryouhou
        _ -> Nothing

base64ToUri : String -> String
base64ToUri =
    String.replace "+" "*"
        >> String.replace "/" "."
        >> String.replace "=" "-"

uriToBase64 : String -> String
uriToBase64 =
    String.replace "*" "+"
        >> String.replace "." "/"
        >> String.replace "-" "="



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initModel url key
    , Task.perform UrlChanged <| Task.succeed <| url
    )

initModel : Url -> Nav.Key -> Model
initModel url key =
    { key = key
    , url = url
    , sentences = []
    , tango =
        { meisi = []
        , keiyousi = []
        , dousi = []
        }
    , tuikaSettei =
        { gokan = ""
        , gyou = ""
        , katuyoukei = "五段"
        , syurui = "自動詞"
        }
    }
