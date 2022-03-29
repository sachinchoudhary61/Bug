module StrategicPlanning.AssignLead.Msg exposing (..)

import Browser.Dom as Dom
import Http
import StrategicPlanning.AssignLead.Entities exposing (AssessmentLead)


type Msg
    = CloseAssignLeadsFlyout
    | AddLead String AssessmentLead
    | RemoveLead String String
    | AssessmentLeadsSaved (Result Http.Error ())
    | ApplyLeads (List AssessmentLead) String
    | FilterLeads String
    | FocusSearchBar
    | UnFocusSearchBar
    | FocusElement (Result Dom.Error ())
    | SetFocusedLead String
    | NoOp
    | Back
