module Web.Controller.Communications where

import Web.Controller.Prelude
import Web.View.Communications.Index
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple (Query)
import Text.RawString.QQ (r)

instance Controller CommunicationsController where
    action CommunicationsAction { .. } = do
        botId <- fetchBotId
        persons <- fetchPersonsExcluding botId
        toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
        communications <- fetchCommunicationsBetween botId selectedPersonId
        let newMessage = newRecord @PhoneMessage |> set #toId (get #id toPhoneNumber)
        render IndexView { .. }

    action CommunicationsCreateMessageAction = do
        let toPhoneNumberId = Id (paramUUID "toId")
        let body = paramText "body"
        botId <- fetchBotId
        toPerson <- fetchPersonFor toPhoneNumberId
        fromPhoneNumber <- fetchPhoneNumberFor botId
        now <- getCurrentTime 
        newRecord @PhoneMessage
            |> set #fromId (get #id fromPhoneNumber)
            |> set #toId toPhoneNumberId
            |> setJust #sentAt now
            |> set #body body
            |> createRecord
        redirectTo $ CommunicationsAction $ get #id toPerson
        
fetchPersonsExcluding :: (?modelContext :: ModelContext) => Id Person -> IO [Person]
fetchPersonsExcluding idToExclude = do
    persons <- query @Person |> fetch
    filter (\person -> get #id person /= idToExclude) persons |> pure

fetchPhoneNumberFor :: (?modelContext :: ModelContext) => Id Person -> IO PhoneNumber
fetchPhoneNumberFor personId = do
    phoneContact <- query @PhoneContact 
        |> filterWhere (#personId, personId)
        |> fetchOne
    fetchOne (get #phoneNumberId phoneContact)

fetchPersonFor :: (?modelContext :: ModelContext) => Id PhoneNumber -> IO Person
fetchPersonFor phoneNumberId = do
    phoneContact <- query @PhoneContact
        |> filterWhere (#phoneNumberId, phoneNumberId)
        |> fetchOne
    fetchOne (get #personId phoneContact)

fetchBotId :: (?modelContext :: ModelContext) => IO (Id Person)
fetchBotId = get #id <$> fetchBot

fetchBot :: (?modelContext :: ModelContext) => IO Person
fetchBot = query @Person |> filterWhere (#goesBy, botName) |> fetchOne

botName :: Text
botName = "Tim the Bot"

fetchCommunicationsBetween :: (?modelContext :: ModelContext) => Id Person -> Id Person -> IO [Communication]
fetchCommunicationsBetween personIdA personIdB = sqlQuery communicationsQuery (personIdA, personIdB)

communicationsQuery :: Query
communicationsQuery = [r|
select
    persons_a.goes_by name_a,
    persons_b.goes_by name_b,
    (phone_messages.from_id = phone_numbers_a.id) is_from_person_a,
    phone_messages.created_at,
    phone_messages.sent_at,
    phone_messages.body
from
    persons persons_a,
    phone_contacts phone_contacts_a,
    phone_numbers phone_numbers_a,
    persons persons_b,
    phone_contacts phone_contacts_b,
    phone_numbers phone_numbers_b,
    phone_messages
where
    persons_a.id = ?
    and phone_contacts_a.person_id = persons_a.id 
    and phone_contacts_a.phone_number_id = phone_numbers_a.id 
    and persons_b.id = ?
    and phone_contacts_b.person_id = persons_b.id 
    and phone_contacts_b.phone_number_id = phone_numbers_b.id 
    and ((phone_messages.from_id = phone_numbers_a.id 
          and phone_messages.to_id = phone_numbers_b.id)
          or
          (phone_messages.from_id = phone_numbers_b.id 
           and phone_messages.to_id = phone_numbers_a.id))
order by
    phone_messages.created_at asc;
|]

instance FromRow Communication where
    fromRow = Communication <$> field <*> field <*> field <*> field <*> field <*> field
