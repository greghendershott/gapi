#lang scribble/manual
@title{Translate API v2}
Lets you translate text from one language to another
@hyperlink["http://code.google.com/apis/language/translate/v2/using_rest.html" "Documentation link"]
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


@section{Functions for the `translations' resource}
@defproc[(language.translations.list
[q string?]
[target string?]
[#:format format string? 'N/A]
[#:source source string? 'N/A]
[#:cid cid string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns text translations from one language to another.

@racket[q]: The text to translate

@racket[target]: The target language into which the text should be translated

@racket[format]: The format of the text

@racket[source]: The source language of the text

@racket[cid]: The customization id for translate

}

@section{Functions for the `detections' resource}
@defproc[(language.detections.list
[q string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Detect the language of text.

@racket[q]: The text to detect

}

@section{Functions for the `languages' resource}
@defproc[(language.languages.list
[#:target target string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List the source/target languages supported by the API

@racket[target]: the language and collation in which the localized results should be returned

}

