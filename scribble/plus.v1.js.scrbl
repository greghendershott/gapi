#lang scribble/manual
Hi hi hi
@(require planet/scribble (for-label racket))
@title{Google+ API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
The Google+ API enables developers to build on top of the Google+ platform.
@hyperlink["https://developers.google.com/+/api/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "plus.v1.js")]
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
@defproc[(plus-activities-list
[#:userId userId string?]
[#:collection collection string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List all of the activities in the specified collection for a particular user.

@racket[userId]: The ID of the user to get activities for. The special value "me" can be used to indicate the authenticated user.

@racket[collection]: The collection of activities to list.

@racket[maxResults]: The maximum number of activities to include in the response, which is used for paging. For any response, the actual number returned might be less than the specified maxResults.

@racket[pageToken]: The continuation token, which is used to page through large result sets. To get the next page of results, set this parameter to the value of "nextPageToken" from the previous response.

}

@defproc[(plus-activities-get
[#:activityId activityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get an activity.

@racket[activityId]: The ID of the activity to get.

}

@defproc[(plus-activities-search
[#:query query string?]
[#:language language string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:orderBy orderBy string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Search public activities.

@racket[query]: Full-text search query string.

@racket[language]: Specify the preferred language to search with. See search language codes for available values.

@racket[maxResults]: The maximum number of activities to include in the response, which is used for paging. For any response, the actual number returned might be less than the specified maxResults.

@racket[pageToken]: The continuation token, which is used to page through large result sets. To get the next page of results, set this parameter to the value of "nextPageToken" from the previous response. This token can be of any length.

@racket[orderBy]: Specifies how to order search results.

}

@subsection{comments}
@defproc[(plus-comments-list
[#:activityId activityId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:sortOrder sortOrder string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List all of the comments for an activity.

@racket[activityId]: The ID of the activity to get comments for.

@racket[maxResults]: The maximum number of comments to include in the response, which is used for paging. For any response, the actual number returned might be less than the specified maxResults.

@racket[pageToken]: The continuation token, which is used to page through large result sets. To get the next page of results, set this parameter to the value of "nextPageToken" from the previous response.

@racket[sortOrder]: The order in which to sort the list of comments.

}

@defproc[(plus-comments-get
[#:commentId commentId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get a comment.

@racket[commentId]: The ID of the comment to get.

}

@subsection{people}
@defproc[(plus-people-get
[#:userId userId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get a person's profile.

@racket[userId]: The ID of the person to get the profile for. The special value "me" can be used to indicate the authenticated user.

}

@defproc[(plus-people-search
[#:query query string?]
[#:language language string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Search all public profiles.

@racket[query]: Specify a query string for full text search of public text in all profiles.

@racket[language]: Specify the preferred language to search with. See search language codes for available values.

@racket[maxResults]: The maximum number of people to include in the response, which is used for paging. For any response, the actual number returned might be less than the specified maxResults.

@racket[pageToken]: The continuation token, which is used to page through large result sets. To get the next page of results, set this parameter to the value of "nextPageToken" from the previous response. This token can be of any length.

}

@defproc[(plus-people-listByActivity
[#:activityId activityId string?]
[#:collection collection string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List all of the people in the specified collection for a particular activity.

@racket[activityId]: The ID of the activity to get the list of people for.

@racket[collection]: The collection of people to list.

@racket[maxResults]: The maximum number of people to include in the response, which is used for paging. For any response, the actual number returned might be less than the specified maxResults.

@racket[pageToken]: The continuation token, which is used to page through large result sets. To get the next page of results, set this parameter to the value of "nextPageToken" from the previous response.

}

