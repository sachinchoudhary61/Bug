module StrategicPlanning.AssignLead.Helper exposing (..)

import Common.OrgNodes.Nodes as Nodes exposing (Node)
import Common.Request as Request
import Common.Utils as Utils
import Json.Decode as JD
import Json.Encode as JE
import Regex
import StrategicPlanning.AssignLead.Entities exposing (AssessmentLead)
import StrategicPlanning.AssignLead.Model exposing (Model)
import StrategicPlanning.AssignLead.Msg exposing (Msg(..))
import StrategicPlanning.Entities exposing (OrgNodeProperties, emptyOrgProps)


fullname : AssessmentLead -> String
fullname lead =
    lead.firstName ++ " " ++ lead.lastName


searchLeads : Maybe String -> List AssessmentLead -> List AssessmentLead
searchLeads searchInput leads =
    searchInput
        |> Maybe.map (\name -> List.filter (filterLead name) leads)
        |> Maybe.withDefault leads


filterLead : String -> AssessmentLead -> Bool
filterLead name lead =
    Regex.contains (Utils.regexCaseInsensitive name) (fullname lead)
        || Regex.contains (Utils.regexCaseInsensitive name) (lead.firstName ++ lead.lastName)
        || Regex.contains (Utils.regexCaseInsensitive name) lead.email


isSelected : List AssessmentLead -> String -> Bool
isSelected leads id =
    leads |> List.map .uuid |> List.member id


selectLead : OrgNodeProperties -> AssessmentLead -> Bool -> JD.Decoder Msg
selectLead { leads, nodeUuid } lead leadExist =
    if leadExist || List.length leads >= noOfLeadsAllowed then
        JD.fail ""

    else
        JD.succeed (AddLead nodeUuid lead)


initials : AssessmentLead -> String
initials lead =
    firstCharToUpper lead.firstName
        ++ firstCharToUpper lead.lastName


firstCharToUpper : String -> String
firstCharToUpper =
    String.left 1 >> String.toUpper


updateModelByAddingLead : Model -> String -> AssessmentLead -> Model
updateModelByAddingLead model nodeUuid lead =
    model
        |> updateHierarchy (addLead nodeUuid lead)
        |> updateModel (filterOrg nodeUuid)


updateModelByRemovingLead : Model -> String -> String -> Model
updateModelByRemovingLead model nodeUuid leadUuid =
    model
        |> updateHierarchy (filterLeads nodeUuid leadUuid)
        |> updateModel (filterOrg nodeUuid)


updateHierarchy : (OrgNodeProperties -> OrgNodeProperties) -> Model -> Model
updateHierarchy fun model =
    { model | hierarchy = model.hierarchy |> Nodes.map fun }


updateModel : (Node OrgNodeProperties -> OrgNodeProperties) -> Model -> Model
updateModel fun model =
    { model
        | anyChangeExists = True
        , searchInput = Nothing
        , orgProps = fun model.hierarchy
    }


filterOrg : String -> Node OrgNodeProperties -> OrgNodeProperties
filterOrg nodeUuid hierarchy =
    let
        result =
            Nodes.fold
                (\orgProps acc ->
                    if orgProps.nodeUuid == nodeUuid then
                        List.singleton orgProps

                    else
                        acc
                )
                []
                hierarchy
    in
    result
        |> List.head
        |> Maybe.withDefault emptyOrgProps


filterLeads : String -> String -> OrgNodeProperties -> OrgNodeProperties
filterLeads id leadUuid props =
    if idMatches id props then
        { props
            | leads =
                props.leads
                    |> List.filter (not << (==) leadUuid << .uuid)
        }

    else
        props


idMatches : String -> OrgNodeProperties -> Bool
idMatches id props =
    id == props.nodeUuid


addLead : String -> AssessmentLead -> OrgNodeProperties -> OrgNodeProperties
addLead id lead props =
    if idMatches id props && List.length props.leads < noOfLeadsAllowed then
        { props | leads = props.leads |> (::) lead }

    else
        props


noOfLeadsAllowed : Int
noOfLeadsAllowed =
    5


saveAssessmentLeads : String -> String -> List AssessmentLead -> List AssessmentLead -> Cmd Msg
saveAssessmentLeads url nodeId new old =
    let
        oldJson =
            old |> List.map .uuid

        newJson =
            new |> List.map .uuid

        body =
            JE.object
                [ ( "old_leads", oldJson |> JE.list JE.string )
                , ( "new_leads", newJson |> JE.list JE.string )
                ]

        route =
            url ++ "/" ++ nodeId ++ "/lead"
    in
    Request.postWithExpectWhatEver route body AssessmentLeadsSaved
