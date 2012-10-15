#lang scribble/manual
@title{Enterprise Audit API v1}
Lets you access user activities in your enterprise made through various applications.
@hyperlink["http://code.google.com/googleapps/domain/audit_admin/v1/getting_started.html" "Documentation link"]
@table-of-contents{}
@section{API Parameters}
These optional keyword arguments may be passed to all functions for this API:
@defproc[(any-function
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
This is not actually a function. This is just using Scribble's
defproc form to list the optional keyword arguments that may be passed
to any function for this API.

@racket[fields]: Selector specifying which fields to include in a partial response.

@racket[key]: API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.

@racket[alt]: Data format for the response.

@racket[oauth_token]: OAuth 2.0 token for the current user.

@racket[prettyPrint]: Returns response with indentations and line breaks.

@racket[quotaUser]: Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.

@racket[userIp]: IP address of the site where the request originates. Use this if you want to enforce per-user limits.

}


@section{Functions for the `activities' resource}
@defproc[(audit.activities.list
[applicationId string?]
[customerId string?]
[#:parameters parameters string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:endTime endTime string? 'N/A]
[#:startTime startTime string? 'N/A]
[#:actorApplicationId actorApplicationId string? 'N/A]
[#:actorEmail actorEmail string? 'N/A]
[#:actorIpAddress actorIpAddress string? 'N/A]
[#:caller caller string? 'N/A]
[#:continuationToken continuationToken string? 'N/A]
[#:eventName eventName string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
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

@racket[endTime]: Return events which occured at or before this time.

@racket[startTime]: Return events which occured at or after this time.

@racket[actorApplicationId]: Application ID of the application which interacted on behalf of the user while performing the event.

@racket[actorEmail]: Email address of the user who performed the action.

@racket[actorIpAddress]: IP Address of host where the event was performed. Supports both IPv4 and IPv6 addresses.

@racket[caller]: Type of the caller.

@racket[continuationToken]: Next page URL.

@racket[eventName]: Name of the event being queried.

}
