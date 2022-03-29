module StrategicPlanning.AssignLead.Update exposing (..)

import Browser.Dom as Dom
import Common.Request as Request
import StrategicPlanning.AssignLead.Helper exposing (..)
import StrategicPlanning.AssignLead.Model exposing (Model)
import StrategicPlanning.AssignLead.Msg exposing (Msg(..))
import StrategicPlanning.Ports as Ports
import Task


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model endpoint =
    case msg of
        FocusSearchBar ->
            ( { model | isFocusOnSearch = True }, Ports.handleAssessmentLeadsKeydown () )

        UnFocusSearchBar ->
            ( { model | isFocusOnSearch = False }, Ports.handleAssessmentLeadsKeydown () )

        FocusElement _ ->
            ( model, Cmd.none )

        SetFocusedLead leadId ->
            let
                focusedLead =
                    case leadId of
                        "" ->
                            Nothing

                        _ ->
                            Just leadId
            in
            ( { model
                | focusedLead = focusedLead
              }
            , Ports.handleAssessmentLeadsKeydown ()
            )

        FilterLeads search ->
            ( { model
                | searchInput = Just search
              }
            , Cmd.none
            )

        AddLead nodeUuid lead ->
            ( updateModelByAddingLead model nodeUuid lead
            , Cmd.batch
                [ Dom.focus "searchbox" |> Task.attempt FocusElement
                , Ports.createTag ("tag-" ++ lead.uuid)
                ]
            )

        RemoveLead nodeUuid leadUuid ->
            ( updateModelByRemovingLead model nodeUuid leadUuid, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.getFocusedLead SetFocusedLead
