#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Calendar API v3}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you manipulate events and other calendar data.
@hyperlink["https://developers.google.com/google-apps/calendar/firstapp" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "calendar.v3.js")]
@section{API Parameters}
The following optional keyword arguments may be passed to @italic{all} functions for this web service:
@defproc[(_
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
@margin-note{This is not actually a function. This is just using Scribble's defproc form to list the optional keyword arguments that may be passed to @italic{all} functions for this service.}
@racket[fields]: Selector specifying which fields to include in a partial response.

@racket[key]: API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.

@racket[alt]: Data format for the response.

@racket[oauth_token]: OAuth 2.0 token for the current user.

@racket[prettyPrint]: Returns response with indentations and line breaks.

@racket[quotaUser]: Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.

@racket[userIp]: IP address of the site where the request originates. Use this if you want to enforce per-user limits.

}

@section{Resources}

@subsection{calendars}
@defproc[(calendar-calendars-get
[#:calendarId calendarId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns metadata for a calendar.

@racket[calendarId]: Calendar identifier.

}

@defproc[(calendar-calendars-insert
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:summary summary string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a secondary calendar.

@racket[id]: Identifier of the calendar.

@racket[kind]: Type of the resource ("calendar#calendar").

@racket[description]: Description of the calendar. Optional.

@racket[location]: Geographic location of the calendar as free-form text. Optional.

@racket[etag]: ETag of the resource.

@racket[summary]: Title of the calendar.

@racket[timeZone]: The time zone of the calendar. Optional.

}

@defproc[(calendar-calendars-patch
[#:calendarId calendarId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:summary summary string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates metadata for a calendar. This method supports patch semantics.

@racket[calendarId]: Calendar identifier.

@racket[id]: Identifier of the calendar.

@racket[kind]: Type of the resource ("calendar#calendar").

@racket[description]: Description of the calendar. Optional.

@racket[location]: Geographic location of the calendar as free-form text. Optional.

@racket[etag]: ETag of the resource.

@racket[summary]: Title of the calendar.

@racket[timeZone]: The time zone of the calendar. Optional.

}

@defproc[(calendar-calendars-clear
[#:calendarId calendarId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Clears a primary calendar. This operation deletes all data associated with the primary calendar of an account and cannot be undone.

@racket[calendarId]: Calendar identifier.

}

@defproc[(calendar-calendars-update
[#:calendarId calendarId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:summary summary string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates metadata for a calendar.

@racket[calendarId]: Calendar identifier.

@racket[id]: Identifier of the calendar.

@racket[kind]: Type of the resource ("calendar#calendar").

@racket[description]: Description of the calendar. Optional.

@racket[location]: Geographic location of the calendar as free-form text. Optional.

@racket[etag]: ETag of the resource.

@racket[summary]: Title of the calendar.

@racket[timeZone]: The time zone of the calendar. Optional.

}

@defproc[(calendar-calendars-delete
[#:calendarId calendarId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a secondary calendar.

@racket[calendarId]: Calendar identifier.

}

@subsection{acl}
@defproc[(calendar-acl-list
[#:calendarId calendarId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the rules in the access control list for the calendar.

@racket[calendarId]: Calendar identifier.

}

@defproc[(calendar-acl-get
[#:calendarId calendarId string?]
[#:ruleId ruleId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns an access control rule.

@racket[calendarId]: Calendar identifier.

@racket[ruleId]: ACL rule identifier.

}

@defproc[(calendar-acl-insert
[#:calendarId calendarId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:etag etag string? 'N/A]
[#:role role string? 'N/A]
[#:scope scope string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates an access control rule.

@racket[calendarId]: Calendar identifier.

@racket[id]: Identifier of the ACL rule.

@racket[kind]: Type of the resource ("calendar#aclRule").

@racket[etag]: ETag of the resource.

@racket[role]: The role assigned to the scope. Possible values are:  
- "none" - Provides no access. 
- "freeBusyReader" - Provides read access to free/busy information. 
- "reader" - Provides read access to the calendar. Private events will appear to users with reader access, but event details will be hidden. 
- "writer" - Provides read and write access to the calendar. Private events will appear to users with writer access, and event details will be visible. 
- "owner" - Provides ownership of the calendar. This role has all of the permissions of the writer role with the additional ability to see and manipulate ACLs.

@racket[scope]: The scope of the rule.

}

@defproc[(calendar-acl-patch
[#:calendarId calendarId string?]
[#:ruleId ruleId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:etag etag string? 'N/A]
[#:role role string? 'N/A]
[#:scope scope string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an access control rule. This method supports patch semantics.

@racket[calendarId]: Calendar identifier.

@racket[ruleId]: ACL rule identifier.

@racket[id]: Identifier of the ACL rule.

@racket[kind]: Type of the resource ("calendar#aclRule").

@racket[etag]: ETag of the resource.

@racket[role]: The role assigned to the scope. Possible values are:  
- "none" - Provides no access. 
- "freeBusyReader" - Provides read access to free/busy information. 
- "reader" - Provides read access to the calendar. Private events will appear to users with reader access, but event details will be hidden. 
- "writer" - Provides read and write access to the calendar. Private events will appear to users with writer access, and event details will be visible. 
- "owner" - Provides ownership of the calendar. This role has all of the permissions of the writer role with the additional ability to see and manipulate ACLs.

@racket[scope]: The scope of the rule.

}

@defproc[(calendar-acl-update
[#:calendarId calendarId string?]
[#:ruleId ruleId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:etag etag string? 'N/A]
[#:role role string? 'N/A]
[#:scope scope string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an access control rule.

@racket[calendarId]: Calendar identifier.

@racket[ruleId]: ACL rule identifier.

@racket[id]: Identifier of the ACL rule.

@racket[kind]: Type of the resource ("calendar#aclRule").

@racket[etag]: ETag of the resource.

@racket[role]: The role assigned to the scope. Possible values are:  
- "none" - Provides no access. 
- "freeBusyReader" - Provides read access to free/busy information. 
- "reader" - Provides read access to the calendar. Private events will appear to users with reader access, but event details will be hidden. 
- "writer" - Provides read and write access to the calendar. Private events will appear to users with writer access, and event details will be visible. 
- "owner" - Provides ownership of the calendar. This role has all of the permissions of the writer role with the additional ability to see and manipulate ACLs.

@racket[scope]: The scope of the rule.

}

@defproc[(calendar-acl-delete
[#:calendarId calendarId string?]
[#:ruleId ruleId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes an access control rule.

@racket[calendarId]: Calendar identifier.

@racket[ruleId]: ACL rule identifier.

}

@subsection{calendarList}
@defproc[(calendar-calendarList-list
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:minAccessRole minAccessRole string? 'N/A]
[#:showHidden showHidden string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns entries on the user's calendar list.

@racket[maxResults]: Maximum number of entries returned on one result page. Optional.

@racket[pageToken]: Token specifying which result page to return. Optional.

@racket[minAccessRole]: The minimum access role for the user in the returned entires. Optional. The default is no restriction.

@racket[showHidden]: Whether to show hidden entries. Optional. The default is False.

}

@defproc[(calendar-calendarList-get
[#:calendarId calendarId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns an entry on the user's calendar list.

@racket[calendarId]: Calendar identifier.

}

@defproc[(calendar-calendarList-insert
[#:colorRgbFormat colorRgbFormat string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:summary summary string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:accessRole accessRole string? 'N/A]
[#:backgroundColor backgroundColor string? 'N/A]
[#:colorId colorId string? 'N/A]
[#:defaultReminders defaultReminders string? 'N/A]
[#:foregroundColor foregroundColor string? 'N/A]
[#:hidden hidden string? 'N/A]
[#:selected selected string? 'N/A]
[#:summaryOverride summaryOverride string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds an entry to the user's calendar list.

@racket[colorRgbFormat]: Whether to use the 'frontendColor' and 'backgroundColor' fields to write the calendar colors (RGB). If this feature is used, the index-based 'color' field will be set to the best matching option automatically. Optional. The default is False.

@racket[id]: Identifier of the calendar.

@racket[kind]: Type of the resource ("calendar#calendarListEntry").

@racket[description]: Description of the calendar. Optional. Read-only.

@racket[location]: Geographic location of the calendar as free-form text. Optional. Read-only.

@racket[etag]: ETag of the resource.

@racket[summary]: Title of the calendar. Read-only.

@racket[timeZone]: The time zone of the calendar. Optional. Read-only.

@racket[accessRole]: The effective access role that the authenticated user has on the calendar. Read-only. Possible values are:  
- "freeBusyReader" - Provides read access to free/busy information. 
- "reader" - Provides read access to the calendar. Private events will appear to users with reader access, but event details will be hidden. 
- "writer" - Provides read and write access to the calendar. Private events will appear to users with writer access, and event details will be visible. 
- "owner" - Provides ownership of the calendar. This role has all of the permissions of the writer role with the additional ability to see and manipulate ACLs.

@racket[backgroundColor]: The main color of the calendar in the format '#0088aa'. This property supersedes the index-based colorId property. Optional.

@racket[colorId]: The color of the calendar. This is an ID referring to an entry in the "calendar" section of the colors definition (see the "colors" endpoint). Optional.

@racket[defaultReminders]: The default reminders that the authenticated user has for this calendar.

@racket[foregroundColor]: The foreground color of the calendar in the format '#ffffff'. This property supersedes the index-based colorId property. Optional.

@racket[hidden]: Whether the calendar has been hidden from the list. Optional. The default is False.

@racket[selected]: Whether the calendar content shows up in the calendar UI. Optional. The default is False.

@racket[summaryOverride]: The summary that the authenticated user has set for this calendar. Optional.

}

@defproc[(calendar-calendarList-patch
[#:calendarId calendarId string?]
[#:colorRgbFormat colorRgbFormat string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:summary summary string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:accessRole accessRole string? 'N/A]
[#:backgroundColor backgroundColor string? 'N/A]
[#:colorId colorId string? 'N/A]
[#:defaultReminders defaultReminders string? 'N/A]
[#:foregroundColor foregroundColor string? 'N/A]
[#:hidden hidden string? 'N/A]
[#:selected selected string? 'N/A]
[#:summaryOverride summaryOverride string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an entry on the user's calendar list. This method supports patch semantics.

@racket[calendarId]: Calendar identifier.

@racket[colorRgbFormat]: Whether to use the 'frontendColor' and 'backgroundColor' fields to write the calendar colors (RGB). If this feature is used, the index-based 'color' field will be set to the best matching option automatically. Optional. The default is False.

@racket[id]: Identifier of the calendar.

@racket[kind]: Type of the resource ("calendar#calendarListEntry").

@racket[description]: Description of the calendar. Optional. Read-only.

@racket[location]: Geographic location of the calendar as free-form text. Optional. Read-only.

@racket[etag]: ETag of the resource.

@racket[summary]: Title of the calendar. Read-only.

@racket[timeZone]: The time zone of the calendar. Optional. Read-only.

@racket[accessRole]: The effective access role that the authenticated user has on the calendar. Read-only. Possible values are:  
- "freeBusyReader" - Provides read access to free/busy information. 
- "reader" - Provides read access to the calendar. Private events will appear to users with reader access, but event details will be hidden. 
- "writer" - Provides read and write access to the calendar. Private events will appear to users with writer access, and event details will be visible. 
- "owner" - Provides ownership of the calendar. This role has all of the permissions of the writer role with the additional ability to see and manipulate ACLs.

@racket[backgroundColor]: The main color of the calendar in the format '#0088aa'. This property supersedes the index-based colorId property. Optional.

@racket[colorId]: The color of the calendar. This is an ID referring to an entry in the "calendar" section of the colors definition (see the "colors" endpoint). Optional.

@racket[defaultReminders]: The default reminders that the authenticated user has for this calendar.

@racket[foregroundColor]: The foreground color of the calendar in the format '#ffffff'. This property supersedes the index-based colorId property. Optional.

@racket[hidden]: Whether the calendar has been hidden from the list. Optional. The default is False.

@racket[selected]: Whether the calendar content shows up in the calendar UI. Optional. The default is False.

@racket[summaryOverride]: The summary that the authenticated user has set for this calendar. Optional.

}

@defproc[(calendar-calendarList-update
[#:calendarId calendarId string?]
[#:colorRgbFormat colorRgbFormat string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:summary summary string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:accessRole accessRole string? 'N/A]
[#:backgroundColor backgroundColor string? 'N/A]
[#:colorId colorId string? 'N/A]
[#:defaultReminders defaultReminders string? 'N/A]
[#:foregroundColor foregroundColor string? 'N/A]
[#:hidden hidden string? 'N/A]
[#:selected selected string? 'N/A]
[#:summaryOverride summaryOverride string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an entry on the user's calendar list.

@racket[calendarId]: Calendar identifier.

@racket[colorRgbFormat]: Whether to use the 'frontendColor' and 'backgroundColor' fields to write the calendar colors (RGB). If this feature is used, the index-based 'color' field will be set to the best matching option automatically. Optional. The default is False.

@racket[id]: Identifier of the calendar.

@racket[kind]: Type of the resource ("calendar#calendarListEntry").

@racket[description]: Description of the calendar. Optional. Read-only.

@racket[location]: Geographic location of the calendar as free-form text. Optional. Read-only.

@racket[etag]: ETag of the resource.

@racket[summary]: Title of the calendar. Read-only.

@racket[timeZone]: The time zone of the calendar. Optional. Read-only.

@racket[accessRole]: The effective access role that the authenticated user has on the calendar. Read-only. Possible values are:  
- "freeBusyReader" - Provides read access to free/busy information. 
- "reader" - Provides read access to the calendar. Private events will appear to users with reader access, but event details will be hidden. 
- "writer" - Provides read and write access to the calendar. Private events will appear to users with writer access, and event details will be visible. 
- "owner" - Provides ownership of the calendar. This role has all of the permissions of the writer role with the additional ability to see and manipulate ACLs.

@racket[backgroundColor]: The main color of the calendar in the format '#0088aa'. This property supersedes the index-based colorId property. Optional.

@racket[colorId]: The color of the calendar. This is an ID referring to an entry in the "calendar" section of the colors definition (see the "colors" endpoint). Optional.

@racket[defaultReminders]: The default reminders that the authenticated user has for this calendar.

@racket[foregroundColor]: The foreground color of the calendar in the format '#ffffff'. This property supersedes the index-based colorId property. Optional.

@racket[hidden]: Whether the calendar has been hidden from the list. Optional. The default is False.

@racket[selected]: Whether the calendar content shows up in the calendar UI. Optional. The default is False.

@racket[summaryOverride]: The summary that the authenticated user has set for this calendar. Optional.

}

@defproc[(calendar-calendarList-delete
[#:calendarId calendarId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes an entry on the user's calendar list.

@racket[calendarId]: Calendar identifier.

}

@subsection{colors}
@defproc[(calendar-colors-get
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the color definitions for calendars and events.

}

@subsection{events}
@defproc[(calendar-events-list
[#:calendarId calendarId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:orderBy orderBy string? 'N/A]
[#:updatedMin updatedMin string? 'N/A]
[#:showDeleted showDeleted string? 'N/A]
[#:q q string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:iCalUID iCalUID string? 'N/A]
[#:timeMax timeMax string? 'N/A]
[#:timeMin timeMin string? 'N/A]
[#:alwaysIncludeEmail alwaysIncludeEmail string? 'N/A]
[#:maxAttendees maxAttendees string? 'N/A]
[#:showHiddenInvitations showHiddenInvitations string? 'N/A]
[#:singleEvents singleEvents string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns events on the specified calendar.

@racket[calendarId]: Calendar identifier.

@racket[maxResults]: Maximum number of events returned on one result page. Optional.

@racket[pageToken]: Token specifying which result page to return. Optional.

@racket[orderBy]: The order of the events returned in the result. Optional. The default is an unspecified, stable order.

@racket[updatedMin]: Lower bound for an event's last modification time (as a RFC 3339 timestamp) to filter by. Optional. The default is not to filter by last modification time.

@racket[showDeleted]: Whether to include deleted single events (with 'status' equals 'cancelled') in the result. Cancelled instances of recurring events will still be included if 'singleEvents' is False. Optional. The default is False.

@racket[q]: Free text search terms to find events that match these terms in any field, except for extended properties. Optional.

@racket[timeZone]: Time zone used in the response. Optional. The default is the time zone of the calendar.

@racket[iCalUID]: Specifies iCalendar UID (iCalUID) of events to be included in the response. Optional.

@racket[timeMax]: Upper bound (exclusive) for an event's start time to filter by. Optional. The default is not to filter by start time.

@racket[timeMin]: Lower bound (inclusive) for an event's end time to filter by. Optional. The default is not to filter by end time.

@racket[alwaysIncludeEmail]: Whether to always include a value in the "email" field for the organizer, creator and attendees, even if no real email is available (i.e. a generated, non-working value will be provided). The use of this option is discouraged and should only be used by clients which cannot handle the absence of an email address value in the mentioned places. Optional. The default is False.

@racket[maxAttendees]: The maximum number of attendees to include in the response. If there are more than the specified number of attendees, only the participant is returned. Optional.

@racket[showHiddenInvitations]: Whether to include hidden invitations in the result. Optional. The default is False.

@racket[singleEvents]: Whether to expand recurring events into instances and only return single one-off events and instances of recurring events, but not the underlying recurring events themselves. Optional. The default is False.

}

@defproc[(calendar-events-get
[#:calendarId calendarId string?]
[#:eventId eventId string?]
[#:timeZone timeZone string? 'N/A]
[#:alwaysIncludeEmail alwaysIncludeEmail string? 'N/A]
[#:maxAttendees maxAttendees string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns an event.

@racket[calendarId]: Calendar identifier.

@racket[eventId]: Event identifier.

@racket[timeZone]: Time zone used in the response. Optional. The default is the time zone of the calendar.

@racket[alwaysIncludeEmail]: Whether to always include a value in the "email" field for the organizer, creator and attendees, even if no real email is available (i.e. a generated, non-working value will be provided). The use of this option is discouraged and should only be used by clients which cannot handle the absence of an email address value in the mentioned places. Optional. The default is False.

@racket[maxAttendees]: The maximum number of attendees to include in the response. If there are more than the specified number of attendees, only the participant is returned. Optional.

}

@defproc[(calendar-events-import
[#:calendarId calendarId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:start start string? 'N/A]
[#:created created string? 'N/A]
[#:summary summary string? 'N/A]
[#:colorId colorId string? 'N/A]
[#:anyoneCanAddSelf anyoneCanAddSelf string? 'N/A]
[#:attendees attendees string? 'N/A]
[#:attendeesOmitted attendeesOmitted string? 'N/A]
[#:creator creator string? 'N/A]
[#:end end string? 'N/A]
[#:endTimeUnspecified endTimeUnspecified string? 'N/A]
[#:extendedProperties extendedProperties string? 'N/A]
[#:gadget gadget string? 'N/A]
[#:guestsCanInviteOthers guestsCanInviteOthers string? 'N/A]
[#:guestsCanModify guestsCanModify string? 'N/A]
[#:guestsCanSeeOtherGuests guestsCanSeeOtherGuests string? 'N/A]
[#:hangoutLink hangoutLink string? 'N/A]
[#:htmlLink htmlLink string? 'N/A]
[#:iCalUID iCalUID string? 'N/A]
[#:locked locked string? 'N/A]
[#:organizer organizer string? 'N/A]
[#:originalStartTime originalStartTime string? 'N/A]
[#:privateCopy privateCopy string? 'N/A]
[#:recurrence recurrence string? 'N/A]
[#:recurringEventId recurringEventId string? 'N/A]
[#:reminders reminders string? 'N/A]
[#:sequence sequence string? 'N/A]
[#:status status string? 'N/A]
[#:transparency transparency string? 'N/A]
[#:visibility visibility string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Imports an event.

@racket[calendarId]: Calendar identifier.

@racket[id]: Identifier of the event.

@racket[kind]: Type of the resource ("calendar#event").

@racket[description]: Description of the event. Optional.

@racket[location]: Geographic location of the event as free-form text. Optional.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the event (as a RFC 3339 timestamp). Read-only.

@racket[start]: The (inclusive) start time of the event. For a recurring event, this is the start time of the first instance.

@racket[created]: Creation time of the event (as a RFC 3339 timestamp). Read-only.

@racket[summary]: Title of the event.

@racket[colorId]: The color of the event. This is an ID referring to an entry in the "event" section of the colors definition (see the "colors" endpoint). Optional.

@racket[anyoneCanAddSelf]: Whether anyone can invite themselves to the event. Optional. The default is False.

@racket[attendees]: The attendees of the event.

@racket[attendeesOmitted]: Whether attendees may have been omitted from the event's representation. When retrieving an event, this may be due to a restriction specified by the 'maxAttendee' query parameter. When updating an event, this can be used to only update the participant's response. Optional. The default is False.

@racket[creator]: The creator of the event. Read-only.

@racket[end]: The (exclusive) end time of the event. For a recurring event, this is the end time of the first instance.

@racket[endTimeUnspecified]: Whether the end time is actually unspecified. An end time is still provided for compatibility reasons, even if this attribute is set to True. The default is False.

@racket[extendedProperties]: Extended properties of the event.

@racket[gadget]: A gadget that extends this event.

@racket[guestsCanInviteOthers]: Whether attendees other than the organizer can invite others to the event. Optional. The default is False.

@racket[guestsCanModify]: Whether attendees other than the organizer can modify the event. Optional. The default is False.

@racket[guestsCanSeeOtherGuests]: Whether attendees other than the organizer can see who the event's attendees are. Optional. The default is False.

@racket[hangoutLink]: An absolute link to the Google+ hangout associated with this event. Read-only.

@racket[htmlLink]: An absolute link to this event in the Google Calendar Web UI. Read-only.

@racket[iCalUID]: Event ID in the iCalendar format.

@racket[locked]: Whether this is a locked event copy where no changes can be made to the main event fields "summary", "description", "location", "start", "end" or "recurrence". The default is False. Read-Only.

@racket[organizer]: The organizer of the event. If the organizer is also an attendee, this is indicated with a separate entry in 'attendees' with the 'organizer' field set to True. To change the organizer, use the "move" operation. Read-only, except when importing an event.

@racket[originalStartTime]: For an instance of a recurring event, this is the time at which this event would start according to the recurrence data in the recurring event identified by recurringEventId. Immutable.

@racket[privateCopy]: Whether this is a private event copy where changes are not shared with other copies on other calendars. Optional. Immutable.

@racket[recurrence]: List of RRULE, EXRULE, RDATE and EXDATE lines for a recurring event. This field is omitted for single events or instances of recurring events.

@racket[recurringEventId]: For an instance of a recurring event, this is the event ID of the recurring event itself. Immutable.

@racket[reminders]: Information about the event's reminders for the authenticated user.

@racket[sequence]: Sequence number as per iCalendar.

@racket[status]: Status of the event. Optional. Possible values are:  
- "confirmed" - The event is confirmed. This is the default status. 
- "tentative" - The event is tentatively confirmed. 
- "cancelled" - The event is cancelled.

@racket[transparency]: Whether the event blocks time on the calendar. Optional. Possible values are:  
- "opaque" - The event blocks time on the calendar. This is the default value. 
- "transparent" - The event does not block time on the calendar.

@racket[visibility]: Visibility of the event. Optional. Possible values are:  
- "default" - Uses the default visibility for events on the calendar. This is the default value. 
- "public" - The event is public and event details are visible to all readers of the calendar. 
- "private" - The event is private and only event attendees may view event details. 
- "confidential" - The event is private. This value is provided for compatibility reasons.

}

@defproc[(calendar-events-insert
[#:calendarId calendarId string?]
[#:sendNotifications sendNotifications string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:start start string? 'N/A]
[#:created created string? 'N/A]
[#:summary summary string? 'N/A]
[#:colorId colorId string? 'N/A]
[#:anyoneCanAddSelf anyoneCanAddSelf string? 'N/A]
[#:attendees attendees string? 'N/A]
[#:attendeesOmitted attendeesOmitted string? 'N/A]
[#:creator creator string? 'N/A]
[#:end end string? 'N/A]
[#:endTimeUnspecified endTimeUnspecified string? 'N/A]
[#:extendedProperties extendedProperties string? 'N/A]
[#:gadget gadget string? 'N/A]
[#:guestsCanInviteOthers guestsCanInviteOthers string? 'N/A]
[#:guestsCanModify guestsCanModify string? 'N/A]
[#:guestsCanSeeOtherGuests guestsCanSeeOtherGuests string? 'N/A]
[#:hangoutLink hangoutLink string? 'N/A]
[#:htmlLink htmlLink string? 'N/A]
[#:iCalUID iCalUID string? 'N/A]
[#:locked locked string? 'N/A]
[#:organizer organizer string? 'N/A]
[#:originalStartTime originalStartTime string? 'N/A]
[#:privateCopy privateCopy string? 'N/A]
[#:recurrence recurrence string? 'N/A]
[#:recurringEventId recurringEventId string? 'N/A]
[#:reminders reminders string? 'N/A]
[#:sequence sequence string? 'N/A]
[#:status status string? 'N/A]
[#:transparency transparency string? 'N/A]
[#:visibility visibility string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates an event.

@racket[calendarId]: Calendar identifier.

@racket[sendNotifications]: Whether to send notifications about the creation of the new event. Optional. The default is False.

@racket[id]: Identifier of the event.

@racket[kind]: Type of the resource ("calendar#event").

@racket[description]: Description of the event. Optional.

@racket[location]: Geographic location of the event as free-form text. Optional.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the event (as a RFC 3339 timestamp). Read-only.

@racket[start]: The (inclusive) start time of the event. For a recurring event, this is the start time of the first instance.

@racket[created]: Creation time of the event (as a RFC 3339 timestamp). Read-only.

@racket[summary]: Title of the event.

@racket[colorId]: The color of the event. This is an ID referring to an entry in the "event" section of the colors definition (see the "colors" endpoint). Optional.

@racket[anyoneCanAddSelf]: Whether anyone can invite themselves to the event. Optional. The default is False.

@racket[attendees]: The attendees of the event.

@racket[attendeesOmitted]: Whether attendees may have been omitted from the event's representation. When retrieving an event, this may be due to a restriction specified by the 'maxAttendee' query parameter. When updating an event, this can be used to only update the participant's response. Optional. The default is False.

@racket[creator]: The creator of the event. Read-only.

@racket[end]: The (exclusive) end time of the event. For a recurring event, this is the end time of the first instance.

@racket[endTimeUnspecified]: Whether the end time is actually unspecified. An end time is still provided for compatibility reasons, even if this attribute is set to True. The default is False.

@racket[extendedProperties]: Extended properties of the event.

@racket[gadget]: A gadget that extends this event.

@racket[guestsCanInviteOthers]: Whether attendees other than the organizer can invite others to the event. Optional. The default is False.

@racket[guestsCanModify]: Whether attendees other than the organizer can modify the event. Optional. The default is False.

@racket[guestsCanSeeOtherGuests]: Whether attendees other than the organizer can see who the event's attendees are. Optional. The default is False.

@racket[hangoutLink]: An absolute link to the Google+ hangout associated with this event. Read-only.

@racket[htmlLink]: An absolute link to this event in the Google Calendar Web UI. Read-only.

@racket[iCalUID]: Event ID in the iCalendar format.

@racket[locked]: Whether this is a locked event copy where no changes can be made to the main event fields "summary", "description", "location", "start", "end" or "recurrence". The default is False. Read-Only.

@racket[organizer]: The organizer of the event. If the organizer is also an attendee, this is indicated with a separate entry in 'attendees' with the 'organizer' field set to True. To change the organizer, use the "move" operation. Read-only, except when importing an event.

@racket[originalStartTime]: For an instance of a recurring event, this is the time at which this event would start according to the recurrence data in the recurring event identified by recurringEventId. Immutable.

@racket[privateCopy]: Whether this is a private event copy where changes are not shared with other copies on other calendars. Optional. Immutable.

@racket[recurrence]: List of RRULE, EXRULE, RDATE and EXDATE lines for a recurring event. This field is omitted for single events or instances of recurring events.

@racket[recurringEventId]: For an instance of a recurring event, this is the event ID of the recurring event itself. Immutable.

@racket[reminders]: Information about the event's reminders for the authenticated user.

@racket[sequence]: Sequence number as per iCalendar.

@racket[status]: Status of the event. Optional. Possible values are:  
- "confirmed" - The event is confirmed. This is the default status. 
- "tentative" - The event is tentatively confirmed. 
- "cancelled" - The event is cancelled.

@racket[transparency]: Whether the event blocks time on the calendar. Optional. Possible values are:  
- "opaque" - The event blocks time on the calendar. This is the default value. 
- "transparent" - The event does not block time on the calendar.

@racket[visibility]: Visibility of the event. Optional. Possible values are:  
- "default" - Uses the default visibility for events on the calendar. This is the default value. 
- "public" - The event is public and event details are visible to all readers of the calendar. 
- "private" - The event is private and only event attendees may view event details. 
- "confidential" - The event is private. This value is provided for compatibility reasons.

}

@defproc[(calendar-events-patch
[#:calendarId calendarId string?]
[#:eventId eventId string?]
[#:sendNotifications sendNotifications string? 'N/A]
[#:alwaysIncludeEmail alwaysIncludeEmail string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:start start string? 'N/A]
[#:created created string? 'N/A]
[#:summary summary string? 'N/A]
[#:colorId colorId string? 'N/A]
[#:anyoneCanAddSelf anyoneCanAddSelf string? 'N/A]
[#:attendees attendees string? 'N/A]
[#:attendeesOmitted attendeesOmitted string? 'N/A]
[#:creator creator string? 'N/A]
[#:end end string? 'N/A]
[#:endTimeUnspecified endTimeUnspecified string? 'N/A]
[#:extendedProperties extendedProperties string? 'N/A]
[#:gadget gadget string? 'N/A]
[#:guestsCanInviteOthers guestsCanInviteOthers string? 'N/A]
[#:guestsCanModify guestsCanModify string? 'N/A]
[#:guestsCanSeeOtherGuests guestsCanSeeOtherGuests string? 'N/A]
[#:hangoutLink hangoutLink string? 'N/A]
[#:htmlLink htmlLink string? 'N/A]
[#:iCalUID iCalUID string? 'N/A]
[#:locked locked string? 'N/A]
[#:organizer organizer string? 'N/A]
[#:originalStartTime originalStartTime string? 'N/A]
[#:privateCopy privateCopy string? 'N/A]
[#:recurrence recurrence string? 'N/A]
[#:recurringEventId recurringEventId string? 'N/A]
[#:reminders reminders string? 'N/A]
[#:sequence sequence string? 'N/A]
[#:status status string? 'N/A]
[#:transparency transparency string? 'N/A]
[#:visibility visibility string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an event. This method supports patch semantics.

@racket[calendarId]: Calendar identifier.

@racket[eventId]: Event identifier.

@racket[sendNotifications]: Whether to send notifications about the event update (e.g. attendee's responses, title changes, etc.). Optional. The default is False.

@racket[alwaysIncludeEmail]: Whether to always include a value in the "email" field for the organizer, creator and attendees, even if no real email is available (i.e. a generated, non-working value will be provided). The use of this option is discouraged and should only be used by clients which cannot handle the absence of an email address value in the mentioned places. Optional. The default is False.

@racket[id]: Identifier of the event.

@racket[kind]: Type of the resource ("calendar#event").

@racket[description]: Description of the event. Optional.

@racket[location]: Geographic location of the event as free-form text. Optional.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the event (as a RFC 3339 timestamp). Read-only.

@racket[start]: The (inclusive) start time of the event. For a recurring event, this is the start time of the first instance.

@racket[created]: Creation time of the event (as a RFC 3339 timestamp). Read-only.

@racket[summary]: Title of the event.

@racket[colorId]: The color of the event. This is an ID referring to an entry in the "event" section of the colors definition (see the "colors" endpoint). Optional.

@racket[anyoneCanAddSelf]: Whether anyone can invite themselves to the event. Optional. The default is False.

@racket[attendees]: The attendees of the event.

@racket[attendeesOmitted]: Whether attendees may have been omitted from the event's representation. When retrieving an event, this may be due to a restriction specified by the 'maxAttendee' query parameter. When updating an event, this can be used to only update the participant's response. Optional. The default is False.

@racket[creator]: The creator of the event. Read-only.

@racket[end]: The (exclusive) end time of the event. For a recurring event, this is the end time of the first instance.

@racket[endTimeUnspecified]: Whether the end time is actually unspecified. An end time is still provided for compatibility reasons, even if this attribute is set to True. The default is False.

@racket[extendedProperties]: Extended properties of the event.

@racket[gadget]: A gadget that extends this event.

@racket[guestsCanInviteOthers]: Whether attendees other than the organizer can invite others to the event. Optional. The default is False.

@racket[guestsCanModify]: Whether attendees other than the organizer can modify the event. Optional. The default is False.

@racket[guestsCanSeeOtherGuests]: Whether attendees other than the organizer can see who the event's attendees are. Optional. The default is False.

@racket[hangoutLink]: An absolute link to the Google+ hangout associated with this event. Read-only.

@racket[htmlLink]: An absolute link to this event in the Google Calendar Web UI. Read-only.

@racket[iCalUID]: Event ID in the iCalendar format.

@racket[locked]: Whether this is a locked event copy where no changes can be made to the main event fields "summary", "description", "location", "start", "end" or "recurrence". The default is False. Read-Only.

@racket[organizer]: The organizer of the event. If the organizer is also an attendee, this is indicated with a separate entry in 'attendees' with the 'organizer' field set to True. To change the organizer, use the "move" operation. Read-only, except when importing an event.

@racket[originalStartTime]: For an instance of a recurring event, this is the time at which this event would start according to the recurrence data in the recurring event identified by recurringEventId. Immutable.

@racket[privateCopy]: Whether this is a private event copy where changes are not shared with other copies on other calendars. Optional. Immutable.

@racket[recurrence]: List of RRULE, EXRULE, RDATE and EXDATE lines for a recurring event. This field is omitted for single events or instances of recurring events.

@racket[recurringEventId]: For an instance of a recurring event, this is the event ID of the recurring event itself. Immutable.

@racket[reminders]: Information about the event's reminders for the authenticated user.

@racket[sequence]: Sequence number as per iCalendar.

@racket[status]: Status of the event. Optional. Possible values are:  
- "confirmed" - The event is confirmed. This is the default status. 
- "tentative" - The event is tentatively confirmed. 
- "cancelled" - The event is cancelled.

@racket[transparency]: Whether the event blocks time on the calendar. Optional. Possible values are:  
- "opaque" - The event blocks time on the calendar. This is the default value. 
- "transparent" - The event does not block time on the calendar.

@racket[visibility]: Visibility of the event. Optional. Possible values are:  
- "default" - Uses the default visibility for events on the calendar. This is the default value. 
- "public" - The event is public and event details are visible to all readers of the calendar. 
- "private" - The event is private and only event attendees may view event details. 
- "confidential" - The event is private. This value is provided for compatibility reasons.

}

@defproc[(calendar-events-instances
[#:calendarId calendarId string?]
[#:eventId eventId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:showDeleted showDeleted string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:alwaysIncludeEmail alwaysIncludeEmail string? 'N/A]
[#:maxAttendees maxAttendees string? 'N/A]
[#:originalStart originalStart string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns instances of the specified recurring event.

@racket[calendarId]: Calendar identifier.

@racket[eventId]: Recurring event identifier.

@racket[maxResults]: Maximum number of events returned on one result page. Optional.

@racket[pageToken]: Token specifying which result page to return. Optional.

@racket[showDeleted]: Whether to include deleted events (with 'eventStatus' equals 'cancelled') in the result. Optional. The default is False.

@racket[timeZone]: Time zone used in the response. Optional. The default is the time zone of the calendar.

@racket[alwaysIncludeEmail]: Whether to always include a value in the "email" field for the organizer, creator and attendees, even if no real email is available (i.e. a generated, non-working value will be provided). The use of this option is discouraged and should only be used by clients which cannot handle the absence of an email address value in the mentioned places. Optional. The default is False.

@racket[maxAttendees]: The maximum number of attendees to include in the response. If there are more than the specified number of attendees, only the participant is returned. Optional.

@racket[originalStart]: The original start time of the instance in the result. Optional.

}

@defproc[(calendar-events-move
[#:calendarId calendarId string?]
[#:eventId eventId string?]
[#:destination destination string?]
[#:sendNotifications sendNotifications string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Moves an event to another calendar, i.e. changes an event's organizer.

@racket[calendarId]: Calendar identifier of the source calendar where the event currently is on.

@racket[eventId]: Event identifier.

@racket[destination]: Calendar identifier of the target calendar where the event is to be moved to.

@racket[sendNotifications]: Whether to send notifications about the change of the event's organizer. Optional. The default is False.

}

@defproc[(calendar-events-quickAdd
[#:calendarId calendarId string?]
[#:text text string?]
[#:sendNotifications sendNotifications string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates an event based on a simple text string.

@racket[calendarId]: Calendar identifier.

@racket[text]: The text describing the event to be created.

@racket[sendNotifications]: Whether to send notifications about the creation of the event. Optional. The default is False.

}

@defproc[(calendar-events-update
[#:calendarId calendarId string?]
[#:eventId eventId string?]
[#:sendNotifications sendNotifications string? 'N/A]
[#:alwaysIncludeEmail alwaysIncludeEmail string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:location location string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:start start string? 'N/A]
[#:created created string? 'N/A]
[#:summary summary string? 'N/A]
[#:colorId colorId string? 'N/A]
[#:anyoneCanAddSelf anyoneCanAddSelf string? 'N/A]
[#:attendees attendees string? 'N/A]
[#:attendeesOmitted attendeesOmitted string? 'N/A]
[#:creator creator string? 'N/A]
[#:end end string? 'N/A]
[#:endTimeUnspecified endTimeUnspecified string? 'N/A]
[#:extendedProperties extendedProperties string? 'N/A]
[#:gadget gadget string? 'N/A]
[#:guestsCanInviteOthers guestsCanInviteOthers string? 'N/A]
[#:guestsCanModify guestsCanModify string? 'N/A]
[#:guestsCanSeeOtherGuests guestsCanSeeOtherGuests string? 'N/A]
[#:hangoutLink hangoutLink string? 'N/A]
[#:htmlLink htmlLink string? 'N/A]
[#:iCalUID iCalUID string? 'N/A]
[#:locked locked string? 'N/A]
[#:organizer organizer string? 'N/A]
[#:originalStartTime originalStartTime string? 'N/A]
[#:privateCopy privateCopy string? 'N/A]
[#:recurrence recurrence string? 'N/A]
[#:recurringEventId recurringEventId string? 'N/A]
[#:reminders reminders string? 'N/A]
[#:sequence sequence string? 'N/A]
[#:status status string? 'N/A]
[#:transparency transparency string? 'N/A]
[#:visibility visibility string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an event.

@racket[calendarId]: Calendar identifier.

@racket[eventId]: Event identifier.

@racket[sendNotifications]: Whether to send notifications about the event update (e.g. attendee's responses, title changes, etc.). Optional. The default is False.

@racket[alwaysIncludeEmail]: Whether to always include a value in the "email" field for the organizer, creator and attendees, even if no real email is available (i.e. a generated, non-working value will be provided). The use of this option is discouraged and should only be used by clients which cannot handle the absence of an email address value in the mentioned places. Optional. The default is False.

@racket[id]: Identifier of the event.

@racket[kind]: Type of the resource ("calendar#event").

@racket[description]: Description of the event. Optional.

@racket[location]: Geographic location of the event as free-form text. Optional.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the event (as a RFC 3339 timestamp). Read-only.

@racket[start]: The (inclusive) start time of the event. For a recurring event, this is the start time of the first instance.

@racket[created]: Creation time of the event (as a RFC 3339 timestamp). Read-only.

@racket[summary]: Title of the event.

@racket[colorId]: The color of the event. This is an ID referring to an entry in the "event" section of the colors definition (see the "colors" endpoint). Optional.

@racket[anyoneCanAddSelf]: Whether anyone can invite themselves to the event. Optional. The default is False.

@racket[attendees]: The attendees of the event.

@racket[attendeesOmitted]: Whether attendees may have been omitted from the event's representation. When retrieving an event, this may be due to a restriction specified by the 'maxAttendee' query parameter. When updating an event, this can be used to only update the participant's response. Optional. The default is False.

@racket[creator]: The creator of the event. Read-only.

@racket[end]: The (exclusive) end time of the event. For a recurring event, this is the end time of the first instance.

@racket[endTimeUnspecified]: Whether the end time is actually unspecified. An end time is still provided for compatibility reasons, even if this attribute is set to True. The default is False.

@racket[extendedProperties]: Extended properties of the event.

@racket[gadget]: A gadget that extends this event.

@racket[guestsCanInviteOthers]: Whether attendees other than the organizer can invite others to the event. Optional. The default is False.

@racket[guestsCanModify]: Whether attendees other than the organizer can modify the event. Optional. The default is False.

@racket[guestsCanSeeOtherGuests]: Whether attendees other than the organizer can see who the event's attendees are. Optional. The default is False.

@racket[hangoutLink]: An absolute link to the Google+ hangout associated with this event. Read-only.

@racket[htmlLink]: An absolute link to this event in the Google Calendar Web UI. Read-only.

@racket[iCalUID]: Event ID in the iCalendar format.

@racket[locked]: Whether this is a locked event copy where no changes can be made to the main event fields "summary", "description", "location", "start", "end" or "recurrence". The default is False. Read-Only.

@racket[organizer]: The organizer of the event. If the organizer is also an attendee, this is indicated with a separate entry in 'attendees' with the 'organizer' field set to True. To change the organizer, use the "move" operation. Read-only, except when importing an event.

@racket[originalStartTime]: For an instance of a recurring event, this is the time at which this event would start according to the recurrence data in the recurring event identified by recurringEventId. Immutable.

@racket[privateCopy]: Whether this is a private event copy where changes are not shared with other copies on other calendars. Optional. Immutable.

@racket[recurrence]: List of RRULE, EXRULE, RDATE and EXDATE lines for a recurring event. This field is omitted for single events or instances of recurring events.

@racket[recurringEventId]: For an instance of a recurring event, this is the event ID of the recurring event itself. Immutable.

@racket[reminders]: Information about the event's reminders for the authenticated user.

@racket[sequence]: Sequence number as per iCalendar.

@racket[status]: Status of the event. Optional. Possible values are:  
- "confirmed" - The event is confirmed. This is the default status. 
- "tentative" - The event is tentatively confirmed. 
- "cancelled" - The event is cancelled.

@racket[transparency]: Whether the event blocks time on the calendar. Optional. Possible values are:  
- "opaque" - The event blocks time on the calendar. This is the default value. 
- "transparent" - The event does not block time on the calendar.

@racket[visibility]: Visibility of the event. Optional. Possible values are:  
- "default" - Uses the default visibility for events on the calendar. This is the default value. 
- "public" - The event is public and event details are visible to all readers of the calendar. 
- "private" - The event is private and only event attendees may view event details. 
- "confidential" - The event is private. This value is provided for compatibility reasons.

}

@defproc[(calendar-events-delete
[#:calendarId calendarId string?]
[#:eventId eventId string?]
[#:sendNotifications sendNotifications string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes an event.

@racket[calendarId]: Calendar identifier.

@racket[eventId]: Event identifier.

@racket[sendNotifications]: Whether to send notifications about the deletion of the event. Optional. The default is False.

}

@subsection{freebusy}
@defproc[(calendar-freebusy-query
[#:items items string? 'N/A]
[#:timeZone timeZone string? 'N/A]
[#:calendarExpansionMax calendarExpansionMax string? 'N/A]
[#:groupExpansionMax groupExpansionMax string? 'N/A]
[#:timeMax timeMax string? 'N/A]
[#:timeMin timeMin string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns free/busy information for a set of calendars.

@racket[items]: List of calendars and/or groups to query.

@racket[timeZone]: Time zone used in the response. Optional. The default is UTC.

@racket[calendarExpansionMax]: Maximal number of calendars for which FreeBusy information is to be provided. Optional.

@racket[groupExpansionMax]: Maximal number of calendar identifiers to be provided for a single group. Optional. An error will be returned for a group with more members than this value.

@racket[timeMax]: The end of the interval for the query.

@racket[timeMin]: The start of the interval for the query.

}

@subsection{settings}
@defproc[(calendar-settings-list
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns all user settings for the authenticated user.

}

@defproc[(calendar-settings-get
[#:setting setting string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns a single user setting.

@racket[setting]: Name of the user setting.

}

