#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Moderator API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Moderator API
@hyperlink["http://code.google.com/apis/moderator/v1/using_rest.html" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "moderator.v1.js")]
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

@subsection{tags}
@defproc[(moderator-tags-list
[seriesId string?]
[submissionId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists all tags for the specified submission within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

}

@defproc[(moderator-tags-insert
[seriesId string?]
[submissionId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:text text string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new tag for the specified submission within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[id]: 

@racket[kind]: 

@racket[text]: 

}

@defproc[(moderator-tags-delete
[seriesId string?]
[submissionId string?]
[tagId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified tag from the specified submission within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[tagId]: 

}

@subsection{submissions}
@defproc[(moderator-submissions-get
[seriesId string?]
[submissionId string?]
[#:lang lang string? 'N/A]
[#:includeVotes includeVotes string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified submission within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[lang]: The language code for the language the client prefers resuls in.

@racket[includeVotes]: Specifies whether to include the current user's vote

}

@defproc[(moderator-submissions-insert
[seriesId string?]
[topicId string?]
[#:anonymous anonymous string? 'N/A]
[#:unauthToken unauthToken string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:created created string? 'N/A]
[#:vote vote string? 'N/A]
[#:geo geo string? 'N/A]
[#:attribution attribution string? 'N/A]
[#:counters counters string? 'N/A]
[#:attachmentUrl attachmentUrl string? 'N/A]
[#:parentSubmissionId parentSubmissionId string? 'N/A]
[#:topics topics string? 'N/A]
[#:translations translations string? 'N/A]
[#:text text string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new submission in the specified topic within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[topicId]: The decimal ID of the Topic within the Series.

@racket[anonymous]: Set to true to mark the new submission as anonymous.

@racket[unauthToken]: User identifier for unauthenticated usage mode

@racket[id]: 

@racket[kind]: 

@racket[author]: 

@racket[created]: 

@racket[vote]: 

@racket[geo]: 

@racket[attribution]: 

@racket[counters]: 

@racket[attachmentUrl]: 

@racket[parentSubmissionId]: 

@racket[topics]: 

@racket[translations]: 

@racket[text]: 

}

@subsection{votes}
@defproc[(moderator-votes-list
[seriesId string?]
[#:max-results max-results string? 'N/A]
[#:start-index start-index string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists the votes by the authenticated user for the given series.

@racket[seriesId]: The decimal ID of the Series.

@racket[max-results]: Maximum number of results to return.

@racket[start-index]: Index of the first result to be retrieved.

}

@defproc[(moderator-votes-get
[seriesId string?]
[submissionId string?]
[#:userId userId string? 'N/A]
[#:unauthToken unauthToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the votes by the authenticated user for the specified submission within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[userId]: 

@racket[unauthToken]: User identifier for unauthenticated usage mode

}

@defproc[(moderator-votes-insert
[seriesId string?]
[submissionId string?]
[#:unauthToken unauthToken string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:flag flag string? 'N/A]
[#:vote vote string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new vote by the authenticated user for the specified submission within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[unauthToken]: User identifier for unauthenticated usage mode

@racket[id]: 

@racket[kind]: 

@racket[flag]: 

@racket[vote]: 

}

@defproc[(moderator-votes-patch
[seriesId string?]
[submissionId string?]
[#:userId userId string? 'N/A]
[#:unauthToken unauthToken string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:flag flag string? 'N/A]
[#:vote vote string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the votes by the authenticated user for the specified submission within the specified series. This method supports patch semantics.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[userId]: 

@racket[unauthToken]: User identifier for unauthenticated usage mode

@racket[id]: 

@racket[kind]: 

@racket[flag]: 

@racket[vote]: 

}

@defproc[(moderator-votes-update
[seriesId string?]
[submissionId string?]
[#:userId userId string? 'N/A]
[#:unauthToken unauthToken string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:flag flag string? 'N/A]
[#:vote vote string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the votes by the authenticated user for the specified submission within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[userId]: 

@racket[unauthToken]: User identifier for unauthenticated usage mode

@racket[id]: 

@racket[kind]: 

@racket[flag]: 

@racket[vote]: 

}

@subsection{topics}
@section{Resources}
@defproc[(moderator-topics-list
[seriesId string?]
[#:mode mode string? 'N/A]
[#:max-results max-results string? 'N/A]
[#:start-index start-index string? 'N/A]
[#:q q string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Searches the topics within the specified series and returns the search results.

@racket[seriesId]: The decimal ID of the Series.

@racket[mode]: 

@racket[max-results]: Maximum number of results to return.

@racket[start-index]: Index of the first result to be retrieved.

@racket[q]: Search query.

}

@defproc[(moderator-topics-get
[seriesId string?]
[topicId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified topic from the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[topicId]: The decimal ID of the Topic within the Series.

}

@defproc[(moderator-topics-insert
[seriesId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:counters counters string? 'N/A]
[#:rules rules string? 'N/A]
[#:featuredSubmission featuredSubmission string? 'N/A]
[#:presenter presenter string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new topic into the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[id]: 

@racket[kind]: 

@racket[name]: 

@racket[description]: 

@racket[counters]: 

@racket[rules]: 

@racket[featuredSubmission]: 

@racket[presenter]: 

}

@defproc[(moderator-topics-update
[seriesId string?]
[topicId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:counters counters string? 'N/A]
[#:rules rules string? 'N/A]
[#:featuredSubmission featuredSubmission string? 'N/A]
[#:presenter presenter string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the specified topic within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[topicId]: The decimal ID of the Topic within the Series.

@racket[id]: 

@racket[kind]: 

@racket[name]: 

@racket[description]: 

@racket[counters]: 

@racket[rules]: 

@racket[featuredSubmission]: 

@racket[presenter]: 

}

@subsection{series}
@section{Resources}
@defproc[(moderator-series-list
[#:max-results max-results string? 'N/A]
[#:start-index start-index string? 'N/A]
[#:q q string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Searches the series and returns the search results.

@racket[max-results]: Maximum number of results to return.

@racket[start-index]: Index of the first result to be retrieved.

@racket[q]: Search query.

}

@defproc[(moderator-series-get
[seriesId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified series.

@racket[seriesId]: The decimal ID of the Series.

}

@defproc[(moderator-series-insert
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:anonymousSubmissionAllowed anonymousSubmissionAllowed string? 'N/A]
[#:counters counters string? 'N/A]
[#:numTopics numTopics string? 'N/A]
[#:rules rules string? 'N/A]
[#:unauthSubmissionAllowed unauthSubmissionAllowed string? 'N/A]
[#:unauthVotingAllowed unauthVotingAllowed string? 'N/A]
[#:videoSubmissionAllowed videoSubmissionAllowed string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new series.

@racket[id]: 

@racket[kind]: 

@racket[name]: 

@racket[description]: 

@racket[anonymousSubmissionAllowed]: 

@racket[counters]: 

@racket[numTopics]: 

@racket[rules]: 

@racket[unauthSubmissionAllowed]: 

@racket[unauthVotingAllowed]: 

@racket[videoSubmissionAllowed]: 

}

@defproc[(moderator-series-patch
[seriesId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:anonymousSubmissionAllowed anonymousSubmissionAllowed string? 'N/A]
[#:counters counters string? 'N/A]
[#:numTopics numTopics string? 'N/A]
[#:rules rules string? 'N/A]
[#:unauthSubmissionAllowed unauthSubmissionAllowed string? 'N/A]
[#:unauthVotingAllowed unauthVotingAllowed string? 'N/A]
[#:videoSubmissionAllowed videoSubmissionAllowed string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the specified series. This method supports patch semantics.

@racket[seriesId]: The decimal ID of the Series.

@racket[id]: 

@racket[kind]: 

@racket[name]: 

@racket[description]: 

@racket[anonymousSubmissionAllowed]: 

@racket[counters]: 

@racket[numTopics]: 

@racket[rules]: 

@racket[unauthSubmissionAllowed]: 

@racket[unauthVotingAllowed]: 

@racket[videoSubmissionAllowed]: 

}

@defproc[(moderator-series-update
[seriesId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:anonymousSubmissionAllowed anonymousSubmissionAllowed string? 'N/A]
[#:counters counters string? 'N/A]
[#:numTopics numTopics string? 'N/A]
[#:rules rules string? 'N/A]
[#:unauthSubmissionAllowed unauthSubmissionAllowed string? 'N/A]
[#:unauthVotingAllowed unauthVotingAllowed string? 'N/A]
[#:videoSubmissionAllowed videoSubmissionAllowed string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[id]: 

@racket[kind]: 

@racket[name]: 

@racket[description]: 

@racket[anonymousSubmissionAllowed]: 

@racket[counters]: 

@racket[numTopics]: 

@racket[rules]: 

@racket[unauthSubmissionAllowed]: 

@racket[unauthVotingAllowed]: 

@racket[videoSubmissionAllowed]: 

}

@subsection{responses}
@defproc[(moderator-responses-list
[seriesId string?]
[submissionId string?]
[#:sort sort string? 'N/A]
[#:author author string? 'N/A]
[#:max-results max-results string? 'N/A]
[#:start-index start-index string? 'N/A]
[#:q q string? 'N/A]
[#:hasAttachedVideo hasAttachedVideo string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists or searches the responses for the specified submission within the specified series and returns the search results.

@racket[seriesId]: The decimal ID of the Series.

@racket[submissionId]: The decimal ID of the Submission within the Series.

@racket[sort]: Sort order.

@racket[author]: Restricts the results to submissions by a specific author.

@racket[max-results]: Maximum number of results to return.

@racket[start-index]: Index of the first result to be retrieved.

@racket[q]: Search query.

@racket[hasAttachedVideo]: Specifies whether to restrict to submissions that have videos attached.

}

@defproc[(moderator-responses-insert
[seriesId string?]
[topicId string?]
[parentSubmissionId string?]
[#:anonymous anonymous string? 'N/A]
[#:unauthToken unauthToken string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:created created string? 'N/A]
[#:vote vote string? 'N/A]
[#:geo geo string? 'N/A]
[#:attribution attribution string? 'N/A]
[#:counters counters string? 'N/A]
[#:attachmentUrl attachmentUrl string? 'N/A]
[#:topics topics string? 'N/A]
[#:translations translations string? 'N/A]
[#:text text string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a response for the specified submission in the specified topic within the specified series.

@racket[seriesId]: The decimal ID of the Series.

@racket[topicId]: The decimal ID of the Topic within the Series.

@racket[parentSubmissionId]: The decimal ID of the parent Submission within the Series.

@racket[anonymous]: Set to true to mark the new submission as anonymous.

@racket[unauthToken]: User identifier for unauthenticated usage mode

@racket[id]: 

@racket[kind]: 

@racket[author]: 

@racket[created]: 

@racket[vote]: 

@racket[geo]: 

@racket[attribution]: 

@racket[counters]: 

@racket[attachmentUrl]: 

@racket[topics]: 

@racket[translations]: 

@racket[text]: 

}

@subsection{featured}
@section{Resources}
@subsection{global}
@section{Resources}
@subsection{my}
@section{Resources}
@subsection{myrecent}
@section{Resources}
@subsection{profiles}
@defproc[(moderator-profiles-get
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the profile information for the authenticated user.

}

@defproc[(moderator-profiles-patch
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:attribution attribution string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the profile information for the authenticated user. This method supports patch semantics.

@racket[id]: 

@racket[kind]: 

@racket[attribution]: 

}

@defproc[(moderator-profiles-update
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:attribution attribution string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the profile information for the authenticated user.

@racket[id]: 

@racket[kind]: 

@racket[attribution]: 

}

