#lang scribble/manual
@title{Freebase API v1}
Lets you access the Freebase repository of open data.
@hyperlink["http://wiki.freebase.com/wiki/API" "Documentation link"]
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


@section{Functions for the `topic' resource}
@defproc[(freebase.topic.lookup
[id string?]
[#:filter filter string? 'N/A]
[#:lang lang string? 'N/A]
[#:limit limit string? 'N/A]
[#:dateline dateline string? 'N/A]
[#:raw raw string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get properties and meta-data about a topic.

@racket[id]: The id of the item that you want data about.

@racket[filter]: A frebase domain, type or property id, 'suggest', 'commons', or 'all'. Filter the results and returns only appropriate properties.

@racket[lang]: The language you 'd like the content in - a freebase /type/lang language key.

@racket[limit]: The maximum number of property values to return for each property.

@racket[dateline]: Determines how up-to-date the data returned is. A unix epoch time, a guid or a 'now'

@racket[raw]: Do not apply any constraints, or get any names.

}

@section{Functions for the `text' resource}
@defproc[(freebase.text.get
[id string?]
[#:format format string? 'N/A]
[#:maxlength maxlength string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns blob attached to node at specified id as HTML

@racket[id]: The id of the item that you want data about

@racket[format]: Sanitizing transformation.

@racket[maxlength]: The max number of characters to return. Valid only for 'plain' format.

}

