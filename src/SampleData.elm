module SampleData exposing (CrimeRate, crimeRates, mistakeTypeFrequency, studentPerformanceInClass, subjectPerformance, timeSeries, uniteSuccessAndTime)

import Data.Subject exposing (Subject(..))
import Date exposing (Date)
import Dict exposing (Dict)


timeSeries : List ( Date, Float )
timeSeries =
    [ ( Date.fromTime 1448928000000, 2.5 )
    , ( Date.fromTime 1451606400000, 2 )
    , ( Date.fromTime 1452211200000, 3.5 )
    , ( Date.fromTime 1452816000000, 2 )
    , ( Date.fromTime 1453420800000, 3 )
    , ( Date.fromTime 1454284800000, 1 )
    , ( Date.fromTime 1456790400000, 1.2 )
    ]


subjectPerformance : List ( Subject, ( Float, Float ) )
subjectPerformance =
    [ ( Subject "subject1", ( 10, 5 ) )
    , ( Subject "subject2", ( 0, 5 ) )
    , ( Subject "subject3", ( 10, 1 ) )
    , ( Subject "subject4", ( 10, 0 ) )
    , ( Subject "subject5", ( 0, 10 ) )
    , ( Subject "subject6", ( 8, 8 ) )
    ]



-- performance of student name, first float = success percentage in general/in subject/whatever, second float = average time in seconds


uniteSuccessAndTime : ( Float, Float ) -> Float
uniteSuccessAndTime ( success_percentage, avg_time_in_secs ) =
    let
        func : Float
        func =
            Basics.min 1 (20 * (1 / avg_time_in_secs))
    in
    Debug.log "unite" <| 0.5 * success_percentage + 0.5 * func


studentPerformanceInClass : List ( String, ( Float, Float ) )
studentPerformanceInClass =
    List.sortBy (Tuple.second >> uniteSuccessAndTime) <|
        [ ( "rick", ( 0.8, 150.0 ) )
        , ( "yosi", ( 0.905, 600.0 ) )
        , ( "ruti", ( 0.55, 10.0 ) )
        , ( "mickie", ( 0.1, 100 ) )
        , ( "mor", ( 0.67, 200 ) )
        , ( "yuval", ( 0.3, 20 ) )
        , ( "yoram", ( 1.0, 90 ) )
        , ( "bella", ( 0.85, 300 ) )
        ]


mistakeTypeFrequency : List ( String, Float )
mistakeTypeFrequency =
    [ ( "type1", 10 )
    , ( "type2", 71.3 )
    , ( "type3", 2 )
    , ( "type4", 10 )
    ]


type alias CrimeRate =
    { year : Int
    , population : Int
    , murder : Int
    , rape : Int
    , robbery : Int
    , assault : Int
    , burglary : Int
    , larceny : Int
    , motorTheft : Int
    }


crimeRates : List CrimeRate
crimeRates =
    [ CrimeRate 1994 260327021 23326 102216 618949 1113179 2712774 7879812 1539287
    , CrimeRate 1995 262803276 21606 97470 580509 1099207 2593784 7997710 1472441
    , CrimeRate 1996 265228572 19645 96252 535594 1037049 2506400 7904685 1394238
    , CrimeRate 1997 267783607 18208 96153 498534 1023201 2460526 7743760 1354189
    , CrimeRate 1998 270248003 16974 93144 447186 976583 2332735 7376311 1242781
    , CrimeRate 1999 272690813 15522 89411 409371 911740 2100739 6955520 1152075
    , CrimeRate 2000 281421906 15586 90178 408016 911706 2050992 6971590 1160002
    , CrimeRate 2001 285317559 16037 90863 423557 909023 2116531 7092267 1228391
    , CrimeRate 2002 287973924 16229 95235 420806 891407 2151252 7057379 1246646
    , CrimeRate 2003 290788976 16528 93883 414235 859030 2154834 7026802 1261226
    , CrimeRate 2004 293656842 16148 95089 401470 847381 2144446 6937089 1237851
    , CrimeRate 2005 296507061 16740 94347 417438 862220 2155448 6783447 1235859
    , CrimeRate 2006 299398484 17309 94472 449246 874096 2194993 6626363 1198245
    , CrimeRate 2007 301621157 17128 92160 447324 866358 2190198 6591542 1100472
    , CrimeRate 2008 304059724 16465 90750 443563 843683 2228887 6586206 959059
    , CrimeRate 2009 307006550 15399 89241 408742 812514 2203313 6338095 795652
    , CrimeRate 2010 309330219 14722 85593 369089 781844 2168459 6204601 739565
    , CrimeRate 2011 311587816 14661 84175 354746 752423 2185140 6151095 716508
    , CrimeRate 2012 313873685 14856 85141 355051 762009 2109932 6168874 723186
    , CrimeRate 2013 316128839 14196 79770 345031 724149 1928465 6004453 699594
    ]
