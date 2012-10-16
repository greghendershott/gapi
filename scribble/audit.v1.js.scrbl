#lang scribble/manual
Hi hi hi
@(require planet/scribble (for-label racket))
@title{Enterprise Audit API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you access user activities in your enterprise made through various applications.
@hyperlink["http://code.google.com/googleapps/domain/audit_admin/v1/getting_started.html" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "audit.v1.js")]
@section{API Parameters}
The following optional keyword arguments may be passed to all functions for this web service:
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

@subsection{activities}
@defproc[(audit-activities-list
[#:applicationId applicationId string?]
[#:customerId customerId string?]
[#:parameters parameters string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:actorApplicationId actorApplicationId string? 'N/A]
[#:actorEmail actorEmail string? 'N/A]
[#:actorIpAddress actorIpAddress string? 'N/A]
[#:caller caller string? 'N/A]
[#:continuationToken continuationToken string? 'N/A]
[#:endTime endTime string? 'N/A]
[#:eventName eventName string? 'N/A]
[#:startTime startTime string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of activities for a specific customer and application.

@racket[applicationId]: Application ID of the application on which the event was performed.

@racket[customerId]: Represents the customer who is the owner of target object on which action was performed.

@racket[parameters]: Event parameters in the form [parameter1 name]:[parameter1 value],[parameter2 name]:[parameter2 value],...

@racket[maxResults]: Number of activity records to be shown in each page.

@racket[actorApplicationId]: Application ID of the application which interacted on behalf of the user while performing the event.

@racket[actorEmail]: Email address of the user who performed the action.

@racket[actorIpAddress]: IP Address of host where the event was performed. Supports both IPv4 and IPv6 addresses.

@racket[caller]: Type of the caller.

@racket[continuationToken]: Next page URL.

@racket[endTime]: Return events which occured at or before this time.

@racket[eventName]: Name of the event being queried.

@racket[startTime]: Return events which occured at or after this time.

}

