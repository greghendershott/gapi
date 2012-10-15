#lang scribble/manual
@title{URL Shortener API v1}
Lets you create, inspect, and manage goo.gl short URLs
@hyperlink["http://code.google.com/apis/urlshortener/v1/getting_started.html" "Documentation link"]
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


@section{Functions for the `url' resource}
@defproc[(urlshortener.url.list
[#:start-token start-token string? 'N/A]
[#:projection projection string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of URLs shortened by a user.

@racket[start-token]: Token for requesting successive pages of results.

@racket[projection]: Additional information to return.

}

@defproc[(urlshortener.url.get
[shortUrl string?]
[#:projection projection string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Expands a short URL or gets creation time and analytics.

@racket[shortUrl]: The short URL, including the protocol.

@racket[projection]: Additional information to return.

}

@defproc[(urlshortener.url.insert
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:analytics analytics string? 'N/A]
[#:created created string? 'N/A]
[#:longUrl longUrl string? 'N/A]
[#:status status string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new short URL.

@racket[id]: Short URL, e.g. "http://goo.gl/l6MS".

@racket[kind]: The fixed string "urlshortener#url".

@racket[analytics]: A summary of the click analytics for the short and long URL. Might not be present if not requested or currently unavailable.

@racket[created]: Time the short URL was created; ISO 8601 representation using the yyyy-MM-dd'T'HH:mm:ss.SSSZZ format, e.g. "2010-10-14T19:01:24.944+00:00".

@racket[longUrl]: Long URL, e.g. "http://www.google.com/". Might not be present if the status is "REMOVED".

@racket[status]: Status of the target URL. Possible values: "OK", "MALWARE", "PHISHING", or "REMOVED". A URL might be marked "REMOVED" if it was flagged as spam, for example.

}

