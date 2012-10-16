#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Google Maps Coordinate API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you view and manage jobs in a Coordinate team.
@hyperlink["https://developers.google.com/coordinate/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "coordinate.v1.js")]
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

@subsection{customFieldDef}
@defproc[(coordinate.customFieldDef.list
[teamId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of custom field definitions for a team.

@racket[teamId]: Team ID

}

@subsection{jobs}
@defproc[(coordinate.jobs.list
[teamId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:minModifiedTimestampMs minModifiedTimestampMs string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves jobs created or modified since the given timestamp.

@racket[teamId]: Team ID

@racket[maxResults]: Maximum number of results to return in one page.

@racket[pageToken]: Continuation token

@racket[minModifiedTimestampMs]: Minimum time a job was modified in milliseconds since epoch.

}

@defproc[(coordinate.jobs.get
[teamId string?]
[jobId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a job, including all the changes made to the job.

@racket[teamId]: Team ID

@racket[jobId]: Job number

}

@defproc[(coordinate.jobs.insert
[title string?]
[address string?]
[lat string?]
[lng string?]
[teamId string?]
[#:customField customField string? 'N/A]
[#:assignee assignee string? 'N/A]
[#:customerName customerName string? 'N/A]
[#:customerPhoneNumber customerPhoneNumber string? 'N/A]
[#:note note string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:state state string? 'N/A]
[#:jobChange jobChange string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new job. Only the state field of the job should be set.

@racket[title]: Job title

@racket[address]: Job address as newline (Unix) separated string

@racket[lat]: The latitude coordinate of this job's location.

@racket[lng]: The longitude coordinate of this job's location.

@racket[teamId]: Team ID

@racket[customField]: Map from custom field id (from /team//custom_fields) to the field value. For example '123=Alice'

@racket[assignee]: Assignee email address, or empty string to unassign.

@racket[customerName]: Customer name

@racket[customerPhoneNumber]: Customer phone number

@racket[note]: Job note as newline (Unix) separated string

@racket[id]: Job id.

@racket[kind]: Identifies this object as a job.

@racket[state]: Current job state.

@racket[jobChange]: List of job changes since it was created. The first change corresponds to the state of the job when it was created.

}

@defproc[(coordinate.jobs.patch
[teamId string?]
[jobId string?]
[#:title title string? 'N/A]
[#:address address string? 'N/A]
[#:progress progress string? 'N/A]
[#:customField customField string? 'N/A]
[#:assignee assignee string? 'N/A]
[#:customerName customerName string? 'N/A]
[#:customerPhoneNumber customerPhoneNumber string? 'N/A]
[#:note note string? 'N/A]
[#:lat lat string? 'N/A]
[#:lng lng string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:state state string? 'N/A]
[#:jobChange jobChange string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a job. Fields that are set in the job state will be updated. This method supports patch semantics.

@racket[teamId]: Team ID

@racket[jobId]: Job number

@racket[title]: Job title

@racket[address]: Job address as newline (Unix) separated string

@racket[progress]: Job progress

@racket[customField]: Map from custom field id (from /team//custom_fields) to the field value. For example '123=Alice'

@racket[assignee]: Assignee email address, or empty string to unassign.

@racket[customerName]: Customer name

@racket[customerPhoneNumber]: Customer phone number

@racket[note]: Job note as newline (Unix) separated string

@racket[lat]: The latitude coordinate of this job's location.

@racket[lng]: The longitude coordinate of this job's location.

@racket[id]: Job id.

@racket[kind]: Identifies this object as a job.

@racket[state]: Current job state.

@racket[jobChange]: List of job changes since it was created. The first change corresponds to the state of the job when it was created.

}

@defproc[(coordinate.jobs.update
[teamId string?]
[jobId string?]
[#:title title string? 'N/A]
[#:address address string? 'N/A]
[#:progress progress string? 'N/A]
[#:customField customField string? 'N/A]
[#:assignee assignee string? 'N/A]
[#:customerName customerName string? 'N/A]
[#:customerPhoneNumber customerPhoneNumber string? 'N/A]
[#:note note string? 'N/A]
[#:lat lat string? 'N/A]
[#:lng lng string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:state state string? 'N/A]
[#:jobChange jobChange string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a job. Fields that are set in the job state will be updated.

@racket[teamId]: Team ID

@racket[jobId]: Job number

@racket[title]: Job title

@racket[address]: Job address as newline (Unix) separated string

@racket[progress]: Job progress

@racket[customField]: Map from custom field id (from /team//custom_fields) to the field value. For example '123=Alice'

@racket[assignee]: Assignee email address, or empty string to unassign.

@racket[customerName]: Customer name

@racket[customerPhoneNumber]: Customer phone number

@racket[note]: Job note as newline (Unix) separated string

@racket[lat]: The latitude coordinate of this job's location.

@racket[lng]: The longitude coordinate of this job's location.

@racket[id]: Job id.

@racket[kind]: Identifies this object as a job.

@racket[state]: Current job state.

@racket[jobChange]: List of job changes since it was created. The first change corresponds to the state of the job when it was created.

}

