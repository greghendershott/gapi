#lang scribble/manual
(require planet/scribble)
@title {URL Shortener API v1}
Lets you create, inspect, and manage goo.gl short URLs
@hyperlink["http://code.google.com/apis/urlshortener/v1/getting_started.html" "Documentation link"]
@table-of-contents
@section{API Parameters}
These keyword arguments may be passed to all functions

#:fields

#:key

#:alt

#:oauth_token

#:prettyPrint

#:quotaUser

#:userIp

@subsection{Functions for the `url' resource:}
@defproc[(urlshortener.url.list
[#:projection projection string? 'N/A]
[#:start-token start-token string? 'N/A]
) jsexpr?]{
Retrieves a list of URLs shortened by a user.
}

@defproc[(urlshortener.url.get
[shortUrl string?]
[#:projection projection string? 'N/A]
) jsexpr?]{
Expands a short URL or gets creation time and analytics.
}

@defproc[(urlshortener.url.insert
[#:id id string? 'N/A]
[#:longUrl longUrl string? 'N/A]
[#:kind kind string? 'N/A]
[#:status status string? 'N/A]
[#:analytics analytics string? 'N/A]
[#:created created string? 'N/A]
) jsexpr?]{
Creates a new short URL.
}

