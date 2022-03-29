module StrategicPlanning.AssignLead.View exposing (view)

import Common.Components as Components exposing (wmButton)
import Common.Markup as Markup
    exposing
        ( ariaLabel
        , ariaLive
        , ariaSelected
        , role
        )
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h4
        , input
        , li
        , main_
        , section
        , span
        , text
        , ul
        )
import Html.Attributes as Attributes
    exposing
        ( attribute
        , class
        , classList
        , disabled
        , href
        , id
        , placeholder
        , tabindex
        , value
        )
import Html.Events
    exposing
        ( on
        , onBlur
        , onClick
        , onFocus
        , onInput
        )
import StrategicPlanning.AssignLead.Entities exposing (AssessmentLead)
import StrategicPlanning.AssignLead.Helper
    exposing
        ( fullname
        , initials
        , isSelected
        , noOfLeadsAllowed
        , searchLeads
        , selectLead
        )
import StrategicPlanning.AssignLead.Model exposing (Model)
import StrategicPlanning.AssignLead.Msg exposing (Msg(..))
import StrategicPlanning.Entities
    exposing
        ( OrgNodeProperties
        , OrganizationMappingTranslations
        )
import StrategicPlanning.MapOrganization.Entities exposing (OrgSelectionView(..))


view : Model -> OrgSelectionView -> OrganizationMappingTranslations -> Html Msg
view model orgSelectionView translations =
    renderContent model orgSelectionView translations


renderContent : Model -> OrgSelectionView -> OrganizationMappingTranslations -> Html Msg
renderContent model orgSelectionView translations =
    div
        [ class "flyout-panel show"
        , role "dialog"
        , id "assign-leads-flyout"
        , ariaLabel translations.addLeads
        , tabindex -1
        ]
        [ div [ class "overlay" ] []
        , div [ class "container" ]
            [ renderHeader model orgSelectionView translations
            , renderBody model translations
            , renderFooter model translations
            ]
        ]


renderHeader : Model -> OrgSelectionView -> OrganizationMappingTranslations -> Html Msg
renderHeader { orgProps, searchInput, isFocusOnSearch, isCreatePlanView } orgSelectionView translations =
    let
        headerText =
            if isCreatePlanView then
                translations.selectOrgHeaderText

            else
                renderHeaderForOrgSelectionView orgSelectionView translations
    in
    div [ class "header-organization-flyout" ]
        [ section [ class "header flex-row header-border" ]
            [ div [ class "page-header" ]
                [ div [ class "header-info" ]
                    [ section [ class "bread-crumbs assign-lead-breadcrumb-margin", id "bread-crumbs-move-to-map-organizations" ]
                        [ a [ href "#", id "link-map-organizations", class "link", onClick Back ]
                            [ span [ class "bread-crumbs-link-text" ] [ text headerText ] ]
                        , span [ class "bread-crumbs-text" ] [ text translations.addLeads ]
                        ]
                    , h4 [ class "title" ] [ text translations.addLeads ]
                    ]
                ]
            , wmButton
                [ attribute "button-type" "navigational"
                , attribute "icon" "close"
                , id "close-org-flyout-btn"
                , attribute "tooltip" translations.closeText
                , attribute "tooltip-position" "bottom"
                , onClick CloseAssignLeadsFlyout
                ]
                []
            ]
        , div
            [ classList
                [ ( "assign-leads-selection-body", True )
                , ( "is-focused", isFocusOnSearch )
                ]
            ]
            [ div [ class "tag-list scrollsibling" ]
                [ ul [ class "createdtags tag-list", id "non-editable-leads-tag", role "application" ]
                    (orgProps.leads
                        |> List.reverse
                        |> List.indexedMap
                            (\index ->
                                tags
                                    ((index + 1 == noOfLeadsAllowed)
                                        && (List.length orgProps.leads == noOfLeadsAllowed)
                                    )
                                    orgProps.nodeUuid
                                    translations
                            )
                    )
                , renderSearchBar orgProps searchInput translations
                ]
            ]
        ]


renderHeaderForOrgSelectionView : OrgSelectionView -> OrganizationMappingTranslations -> String
renderHeaderForOrgSelectionView orgSelectionView translations =
    case orgSelectionView of
        PublishOrganizations ->
            translations.headerText

        _ ->
            translations.assignOrgHeaderText


renderSearchBar : OrgNodeProperties -> Maybe String -> OrganizationMappingTranslations -> Html Msg
renderSearchBar props searchInput translations =
    if List.length props.leads >= noOfLeadsAllowed then
        text ""

    else
        input
            [ id "searchbox"
            , class "taginput"
            , placeholder translations.addLeads
            , tabindex 0
            , ariaLabel translations.addLeads
            , attribute "aria-owns" "suggestedLeads"
            , attribute "aria-autocomplete" "list"
            , onFocus FocusSearchBar
            , onBlur UnFocusSearchBar
            , onInput FilterLeads
            , value (searchInput |> Maybe.withDefault "")
            ]
            []


tags : Bool -> String -> OrganizationMappingTranslations -> AssessmentLead -> Html Msg
tags isLastElement nodeUuid translations lead =
    let
        tabindexValue =
            if isLastElement then
                0

            else
                -1
    in
    li
        [ id ("tag-" ++ lead.uuid)
        , class "person tag -deletable"
        , tabindex tabindexValue
        ]
        [ text (fullname lead)
        , button
            [ class "button delete mdi -icononly mdi-close"
            , tabindex -1
            , ariaLabel translations.deleteAriaTag
            , onClick (RemoveLead nodeUuid lead.uuid)
            ]
            []
        ]


renderBody : Model -> OrganizationMappingTranslations -> Html Msg
renderBody model translations =
    section
        [ class "content assignLeadContent" ]
        [ div [ id "assign-leads-main-body" ]
            [ ul
                [ id "suggestedLeads"
                , role "listbox"
                , ariaLabel translations.leadsListLabel
                , class "assign-leads-ul-style people-list scrollelement -selectionlist"
                ]
                (renderLeads model translations)
            ]
        ]


renderLeads : Model -> OrganizationMappingTranslations -> List (Html Msg)
renderLeads { searchInput, availableLeads, orgProps, focusedLead } translations =
    case searchLeads searchInput availableLeads of
        [] ->
            [ div [ role "alert", ariaLive "polite" ] [ text translations.noMatches ] ]

        leads ->
            leads
                |> List.map (renderAssessmentLead orgProps focusedLead)


renderAssessmentLead : OrgNodeProperties -> Maybe String -> AssessmentLead -> Html Msg
renderAssessmentLead orgProps focusedLead lead =
    let
        isLeadSelected =
            isSelected orgProps.leads lead.uuid
    in
    li
        [ id ("user-" ++ lead.uuid)
        , classList
            [ ( "person lead-detail", True )
            , ( "selected", isLeadSelected )
            , ( "active", Just ("user-" ++ lead.uuid) == focusedLead )
            ]
        , ariaSelected isLeadSelected
        , attribute "aria-owns" "suggestedLeads"
        , role "option"
        , on "click" (selectLead orgProps lead isLeadSelected)
        , tabindex -1
        , ariaLabel (fullname lead)
        ]
        [ div [ class "name" ]
            [ span [ class "avatar icon-color-2" ] [ text <| initials lead ]
            , div [ class "full-detail" ]
                [ div [ class "fullname" ] [ text <| fullname lead ]
                , div [ class "info" ] [ text lead.email ]
                ]
            ]
        ]


renderFooter : Model -> OrganizationMappingTranslations -> Html Msg
renderFooter { orgProps, anyChangeExists } translations =
    div [ class "flyout-footer" ]
        [ div [ class "assign-leads-flyout-footer-buttons" ]
            [ div [ class "button-collection" ]
                [ wmButton
                    [ attribute "button-type" "secondary"
                    , onClick Back
                    , id "assign-leads-flyout-back-btn"
                    ]
                    [ text translations.backBtn ]
                , wmButton
                    [ attribute "button-type" "primary"
                    , id "assign-leads-flyout-apply-btn"
                    , onClick (ApplyLeads orgProps.leads orgProps.nodeUuid)

                    -- , disabled (not anyChangeExists)
                    ]
                    [ text translations.applyBtn ]
                ]
            ]
        ]
