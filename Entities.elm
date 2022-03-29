module StrategicPlanning.AssignLead.Entities exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)


type alias AssessmentLead =
    { uuid : String
    , firstName : String
    , lastName : String
    , email : String
    }


assessmentLeadDecoder : JD.Decoder AssessmentLead
assessmentLeadDecoder =
    JD.succeed AssessmentLead
        |> required "user_uuid" JD.string
        |> required "first_name" JD.string
        |> required "last_name" JD.string
        |> required "email" JD.string
