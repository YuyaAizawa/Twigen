module Twigen exposing (main)

import Browser
import Html exposing (Html, div, h1, h3, button, ul, li, text, textarea, br, table, tbody, tr, td, select, option, input)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Random exposing (Generator, map, map2, map3, lazy)



main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { sentences : List String
    , meisi : List Meisi
    , keiyousi : List Keiyousi
    , dousi : List Dousi
    , tuikaSettei : TuikaSettei
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
    | MeisiUpdate (List String)
    | KeiyousiUpdate (List String)
    | DousiUpdate (List Dousi)
    | TuikaSetteiUpdate TuikaSettei

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                command =
                    Random.generate NewSentences
                        <| Random.list 10
                        <| sentence model
            in
                ( model, command )

        NewSentences newSentences ->
            ( { model | sentences = newSentences }, Cmd.none )

        MeisiUpdate list ->
            ( { model | meisi = list }, Cmd.none )

        KeiyousiUpdate list ->
            ( { model | keiyousi = list }, Cmd.none )

        DousiUpdate list ->
            ( { model | dousi = list }, Cmd.none )

        TuikaSetteiUpdate settei ->
            ( { model | tuikaSettei = settei }, Cmd.none )




-- VIEW


view : Model -> Html Msg
view model =
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
                    [ Attr.value <| (model.meisi |> String.join "\n")
                    , onInput meisiKousin
                    , Attr.style "resize" "none"
                    ][]
                ]
            , div []
                [ h3 [][ text <| "形容詞" ]
                , textarea
                    [ Attr.value <| (model.keiyousi |> String.join "\n")
                    , onInput keiyousiKousin
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
                    ( model.dousi
                        |> List.indexedMap (\i (Dousi gokan katuyou syurui) ->
                            tr []
                                [ td [][ text <| gokan ]
                                , td [][ text <| katuyouToString katuyou ]
                                , td [][ text <| dousiSyuruiToString <| syurui ]
                                , td []
                                    [ button
                                        [ onClick <| dousiSakujo i model.dousi ]
                                        [ text <| "削除" ]
                                    ]
                                ]
                        )
                    )
                ]
            , input
                [ Attr.type_ "text"
                , onInput <| gokanSettei model.tuikaSettei
                ][]
            , select
                [ onInput <| gyouSettei model.tuikaSettei ]
                [ option [ Attr.value "あ" ][ text <| "あ行" ]
                , option [ Attr.value "か" ][ text <| "か行" ]
                , option [ Attr.value "さ" ][ text <| "さ行" ]
                , option [ Attr.value "た" ][ text <| "た行" ]
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
                [ onClick <| dousiTuika model.tuikaSettei model.dousi ]
                [ text <| "追加" ]
            ]
        ]

meisiKousin : String -> Msg
meisiKousin =
  String.split "\n"
    >> MeisiUpdate

keiyousiKousin : String -> Msg
keiyousiKousin =
  String.split "\n"
    >> KeiyousiUpdate

gokanSettei : TuikaSettei -> String -> Msg
gokanSettei settei gokan =
    TuikaSetteiUpdate { settei | gokan = gokan }

gyouSettei settei gyou =
    TuikaSetteiUpdate { settei | gyou = gyou }

katuyoukeiSettei settei katuyoukei =
    TuikaSetteiUpdate { settei | katuyoukei = katuyoukei }

syuruiSettei settei syurui =
    TuikaSetteiUpdate { settei | syurui = syurui }

dousiTuika settei list =
    let
        dousi_ =
            Maybe.map2
            (\k s -> Dousi settei.gokan k s)
            (katuyouFromString settei.gyou settei.katuyoukei)
            (dousiSyuruiFromString settei.syurui)

        list_ =
            case dousi_ of
                Just d -> d :: list
                Nothing -> list
    in
        DousiUpdate list_

dousiSakujo at list =
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
        DousiUpdate <| help at list []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- TWEET PATTERN DIFINITIONS


sentence model =
    choice
    [ intermittentReference model
    , dagaomaeha model
    , declarative model
    , declarative model -- weight
    , kimidakeno model
    , haityuu model
    , otherSentence model
    ]

intermittentReference model =
    choice
    [ seq [ meisiKu model, c "，", dousiKu model |> map dousiKuRenyou1, c "がち．" ]
    , seq [ meisiKu model, c "，", meisiKu model, c "みがある．" ]
    , seq [ meisiKu model, c "，", meisi model, c "じゃん．" ]
    ]

dagaomaeha model =
    let
        ore =
            choice
            [ c "俺"
            , c "ワイ"
            , c "私"
            ]

        body =
            choice
            [ meisiKu model |> map (\m -> m++"だ")
            , keiyousiGokan model |> map keiyousiSyuusi
            , dousiKu model |> map dousiKuSyuusi
            ]

        omae =
            choice
            [ c "お前"
            , c "きみ"
            , c "あなた"
            ]
    in
        seq [ ore, c "は", body, c "が", omae, c "は？" ]

otherSentence model =
    choice
    [ seq [ meisiKu model, c "で申し訳ないよ😢" ]
    , seq [ meisiKu model, c "のNASA" ]
    , seq [ keiyousi model |> map keiyousiSyuusi, c "いいいいいいいい✌('ω'✌ )三✌('ω')✌三( ✌'ω')✌" ]
    , seq [ c "無限に", tadousi model |> map dousiMizen, c "れるお前の人生" ]
    , seq [ c "(", meisiKu model, c "は", tadousi model |> map dousiMizen, c ")ないです" ]
    , seq [ jidousi model |> map dousiMeirei, c "！そなたは", keiyousi model |> map keiyousiSyuusi ]
    ]

declarative model =
    let
        body =
            choice
            [ seq [ meisiKu model, c "は", meisiKu model, choice [ c "", c "だ", c "である", c "でない", c "でできている" ] ]
            , seq [ meisiKu model, c "には", meisiKu model, c "がある" ]
            , seq [ meisiKu model, c "は", keiyousiGokan model |> map keiyousiSyuusi ]
            , seq [ meisiKu model, c "は", dousiKu model |> map dousiKuSyuusi, choice [ c "", c "ことがある"] ]
            , seq [ meisiKu model, c "は", dousiKu model |> map dousiKuMizen, c "ない", choice [ c "", c "ことがある"] ]
            , seq [ meisiKu model, c "を", tadousi model |> map dousiRenyou2, c "てはいけない" ]
            ]
    in
        choice
        [ seq [ body, c "． #宣言的知識" ]
        , seq [ body, c "（ホンマか？"]
        , seq [ body, c "．知らんけど．"]
        , seq [ body, c "って魔剤？" ]
        , seq [ c "もしかして：", body ]
        ]

kimidakeno model =
    seq
    [ c "君だけの"
    , meisi model
    , c "を"
    , tadousi model |> map dousiRenyou2
    , c "て最強の"
    , meisi model
    , c "を作り出せ！"
    ]

haityuu model =
    Random.map2
    (\s1 ( s2, d ) ->
        s1 ++ "は" ++ s2 ++ dousiSyuusi d ++ "場合と" ++ dousiMizen d ++ "ない場合があるぞい．"
    )
    (meisiKu model)
    (dousiKu model)


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

meisi : Model -> Generator String
meisi model =
    let
        sahenMeisi =
            model.dousi
                |> List.filter (\(Dousi _ katuyoukei _) -> katuyoukei == Sahen )
                |> List.map (\(Dousi gokan _ _) -> gokan)
    in
        model.meisi ++ sahenMeisi
            |> generatorFromList

meisiKu : Model -> Generator String
meisiKu model =
    choice
    [ meisi model
    , meisi model -- weight
    , meisi model -- weight
    , map2 -- 「の」
        (\m1 m2 -> m1 ++ "の" ++ m2)
        (lazy (\_ -> meisiKu model))
        (meisi model)
    , map2 -- 形容詞の連体形
        (\k m -> keiyousiRentai k ++ m)
        (keiyousi model)
        (meisi model)
    , map2 -- 用言の連体形
        (\dk m -> dousiKuRentai dk ++ m)
        (lazy (\_ -> dousiKu model))
        (meisi model)
    ]


type alias Keiyousi = String

keiyousi : Model -> Generator KeiyousiGokan
keiyousi model =
    generatorFromList model.keiyousi
        |> map KeiyousiGokan

type KeiyousiGokan = KeiyousiGokan String

keiyousiGokan : Model -> Generator KeiyousiGokan
keiyousiGokan model =
    let
        ppoi =
            generatorFromList
            [ "っぽ"
            , "らし"
            ]

        nikui =
            generatorFromList
            [ "難"
            , "辛"
            , "やす"
            ]
    in
        choice
        [ keiyousi model
        , keiyousi model -- weight
        , keiyousi model -- weight
        , keiyousi model -- weight
        , map3 -- 名詞 助動詞
            (\m j (KeiyousiGokan k) -> m ++ j ++ "く" ++ k |> KeiyousiGokan)
            (meisiKu model)
            ppoi
            (keiyousi model)
        , map3 -- 動詞の連用形＋付属語
            (\dk n (KeiyousiGokan k) -> dousiKuRenyou1 dk ++ n ++ "く" ++ k |> KeiyousiGokan)
            (dousiKu model)
            nikui
            (keiyousi model)
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

jidousi : Model -> Generator Dousi
jidousi model =
    model.dousi
        |> List.filter (\(Dousi _ _ syurui) ->
            syurui == Jidousi || syurui == Ryouhou)
        |> generatorFromList

tadousi : Model -> Generator Dousi
tadousi model =
    model.dousi
        |> List.filter (\(Dousi _ _ syurui) ->
            syurui == Tadousi || syurui == Ryouhou)
        |> generatorFromList

type alias DousiKu = ( String, Dousi )

dousiKu : Model -> Generator DousiKu
dousiKu model =
    let
        hukusi =
            generatorFromList
            [ "とても"
            , "非常に"
            , "みるからに"
            , "多少は"
            ]

        youni =
            generatorFromList
            [ "ように" ]

        jidou =
             jidousi model |> map (\d -> ("", d))

        tadou =
            map2
            (\m d -> (m++"を", d))
            (lazy (\_ -> meisiKu model))
            (tadousi model)
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
            (lazy (\_ -> dousiKu model))
            youni
            (jidousi model)
        , map2 -- 形容詞の連用形
            (\k ( s, d ) -> ( keiyousiRenyou2 k ++ s, d ))
            (lazy (\_ -> keiyousiGokan model))
            (lazy (\_ -> dousiKu model))
        , map2 -- 副詞
            (\h ( s, d ) -> ( h++s, d ))
            hukusi
            (lazy (\_ -> dousiKu model))
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
        Kami  "き" -> Katuyougobi "き" ( "き", "き" ) "きる" "きる" "きれ" "きろ"
        Kami  "た" -> Katuyougobi "ち" ( "ち", "ち" ) "ちる" "ちる" "ちれ" "ちろ"
        Shimo ""   -> Katuyougobi "" ( "", "" ) "る" "る" "れ" "ろ"
        Shimo "あ" -> Katuyougobi "え" ( "え", "え" ) "える" "える" "えれ" "えろ"
        Shimo "が" -> Katuyougobi "げ" ( "げ", "げ" ) "げる" "げる" "げれ" "げろ"
        Shimo "さ" -> Katuyougobi "せ" ( "せ", "せ" ) "せる" "せる" "せれ" "せろ"
        Shimo "ば" -> Katuyougobi "べ" ( "べ", "べ" ) "べる" "べる" "べれ" "べろ"
        Shimo "ま" -> Katuyougobi "め" ( "め", "め" ) "める" "める" "めれ" "めろ"
        Sahen      -> Katuyougobi "し" ( "し", "し" ) "する" "する" "すれ" "せよ"
        _          -> Katuyougobi "Error" ( "Error", "Error" ) "Error" "Error" "Error" "Error"

generatorFromList : List a -> Generator a
generatorFromList list =
    case list of
        [] -> Random.constant <| Debug.todo "Error"
        hd :: tl -> Random.uniform hd tl


-- INIT


init : ( Model, Cmd Msg )
init =
    (
        { sentences = []
        , meisi =
            [ "人"
            , "神"
            , "他人"
            , "人類"
            , "可能性"
            , "アイドル"
            , "可燃性"
            , "群馬"
            , "年収"
            , "百合"
            , "メモリ空間"
            , "流動性"
            , "ＣＰＵ"
            , "化粧品"
            , "生活リズム"
            , "バナナ"
            , "隠れマルコフモデル"
            , "猫"
            , "筑波大学"
            , "核実験"
            , "ＡＩ"
            , "薬"
            , "社会"
            , "ゴリラ"
            , "単位"
            , "人生"
            , "オタク"
            ]
        , keiyousi =
            [ "美し"
            , "優し"
            , "賢"
            , "虚し"
            , "怖"
            , "痛"
            , "悲し"
            , "美味し"
            , "醜"
            , "悔し"
            , "可愛"
            , "大き"
            , "長"
            , "若"
            , "深"
            , "遠"
            , "暗"
            , "薄"
            , "たくまし"
            , "楽し"
            , "激し"
            ]
        , dousi =
            ([ Dousi "燃" <| Shimo "あ"
            , Dousi "生" <| Kami  "き"
            , Dousi "話" <| Godan "さ"
            , Dousi "寝" <| Shimo ""
            , Dousi "光" <| Godan "ら"
            , Dousi "輝" <| Godan "か"
            , Dousi "曲が" <| Godan "ら"
            , Dousi "歩" <| Godan "か"
            , Dousi "落" <| Kami "た"
            ] |> List.map (\f -> f Jidousi))
            ++
            ([ Dousi "食" <| Shimo "ば"
            , Dousi "飲" <| Godan "ま"
            , Dousi "買" <| Godan "わ"
            , Dousi "見" <| Shimo ""
            , Dousi "見" <| Shimo "さ"
            , Dousi "書" <| Godan "か"
            , Dousi "送" <| Godan "ら"
            , Dousi "使" <| Godan "わ"
            , Dousi "話" <| Godan "さ"
            , Dousi "穿" <| Godan "た"
            , Dousi "曲" <| Shimo "が"
            , Dousi "攻" <| Shimo "ま"
            , Dousi "落と" <| Godan "さ"
            , Dousi "叩" <| Godan "か"
            ] |> List.map (\f -> f Tadousi))
            ++
            ([ "筋トレ"
            , "崩壊"
            ] |> List.map (\gokan -> Dousi gokan Sahen Jidousi))
            ++
            ([ "待望"
            , "強要"
            , "報告"
            , "実装"
            , "連想"
            ] |> List.map (\gokan -> Dousi gokan Sahen Tadousi))
            ++
            (["配信"
            , "開発"
            , "エンジョイ"
            ] |> List.map (\gokan -> Dousi gokan Sahen Ryouhou))
        , tuikaSettei =
            { gokan = ""
            , gyou = "あ"
            , katuyoukei = "五段"
            , syurui = "自動詞"
            }
        }
    , Cmd.none
    )
