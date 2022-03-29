module StrategicPlanning.AssignLead.Model exposing (..)

import Common.OrgNodes.Nodes exposing (Node(..))
import StrategicPlanning.AssignLead.Entities exposing (AssessmentLead)
import StrategicPlanning.Entities exposing (OrgNodeProperties, emptyOrgProps)


type alias Model =
    { availableLeads : List AssessmentLead
    , existingLeads : List AssessmentLead
    , hierarchy : Node OrgNodeProperties
    , orgProps : OrgNodeProperties
    , searchInput : Maybe String
    , focusedLead : Maybe String
    , isFocusOnSearch : Bool
    , anyChangeExists : Bool
    , isCreatePlanView : Bool
    }


initialModel : Model
initialModel =
    { availableLeads = []
    , existingLeads = []
    , hierarchy = Empty
    , orgProps = emptyOrgProps
    , searchInput = Nothing
    , focusedLead = Nothing
    , isFocusOnSearch = True
    , anyChangeExists = False
    , isCreatePlanView = False
    }
