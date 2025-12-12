module TicketPlease exposing (..)

import Set
import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))


devTeam =
    Set.fromList [ "Alice", "Bob", "Charlie" ]


emptyComment : ( User, String ) -> Bool
emptyComment ( _, comment ) =
    String.isEmpty comment


numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket { createdBy, comments }) =
    let
        ( ticketCreator, _ ) =
            createdBy

        createdByTicketCreator : ( User, String ) -> Bool
        createdByTicketCreator ( commentCreator, _ ) =
            commentCreator == ticketCreator
    in
    comments
        |> List.filter createdByTicketCreator
        |> List.length


assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket { assignedTo }) =
    case assignedTo of
        Nothing ->
            False

        Just (User name) ->
            Set.member name devTeam


assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user ((Ticket ({ status } as ticketData)) as ticket) =
    case status of
        New ->
            Ticket { ticketData | assignedTo = Just user, status = InProgress }

        Archived ->
            ticket

        _ ->
            Ticket { ticketData | assignedTo = Just user }
