#lang scribble/manual
@title{Books API v1}
Lets you search for books and manage your Google Books library.
@hyperlink["https://developers.google.com/books/docs/v1/getting_started" "Documentation link"]
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


@section{Functions for the `volumes' resource}
@defproc[(books.volumes.list
[q string?]
[#:filter filter string? 'N/A]
[#:projection projection string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:source source string? 'N/A]
[#:startIndex startIndex string? 'N/A]
[#:printType printType string? 'N/A]
[#:showPreorders showPreorders string? 'N/A]
[#:partner partner string? 'N/A]
[#:download download string? 'N/A]
[#:langRestrict langRestrict string? 'N/A]
[#:libraryRestrict libraryRestrict string? 'N/A]
[#:orderBy orderBy string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Performs a book search.

@racket[q]: Full-text search query string.

@racket[filter]: Filter search results.

@racket[projection]: Restrict information returned to a set of selected fields.

@racket[maxResults]: Maximum number of results to return.

@racket[source]: String to identify the originator of this request.

@racket[startIndex]: Index of the first result to return (starts at 0)

@racket[printType]: Restrict to books or magazines.

@racket[showPreorders]: Set to true to show books available for preorder. Defaults to false.

@racket[partner]: Restrict and brand results for partner ID.

@racket[download]: Restrict to volumes by download availability.

@racket[langRestrict]: Restrict results to books with this language code.

@racket[libraryRestrict]: Restrict search to this user's library.

@racket[orderBy]: Sort search results.

}

@defproc[(books.volumes.get
[volumeId string?]
[#:projection projection string? 'N/A]
[#:source source string? 'N/A]
[#:country country string? 'N/A]
[#:partner partner string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets volume information for a single volume.

@racket[volumeId]: ID of volume to retrieve.

@racket[projection]: Restrict information returned to a set of selected fields.

@racket[source]: String to identify the originator of this request.

@racket[country]: ISO-3166-1 code to override the IP-based location.

@racket[partner]: Brand results for partner ID.

}

@section{Functions for the `bookshelves' resource}
@defproc[(books.bookshelves.list
[userId string?]
[#:source source string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of public bookshelves for the specified user.

@racket[userId]: ID of user for whom to retrieve bookshelves.

@racket[source]: String to identify the originator of this request.

}

@defproc[(books.bookshelves.get
[userId string?]
[shelf string?]
[#:source source string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves metadata for a specific bookshelf for the specified user.

@racket[userId]: ID of user for whom to retrieve bookshelves.

@racket[shelf]: ID of bookshelf to retrieve.

@racket[source]: String to identify the originator of this request.

}

@section{Functions for the `layers' resource}
@defproc[(books.layers.list
[volumeId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:source source string? 'N/A]
[#:contentVersion contentVersion string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List the layer summaries for a volume.

@racket[volumeId]: The volume to retrieve layers for.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: The value of the nextToken from the previous page.

@racket[source]: String to identify the originator of this request.

@racket[contentVersion]: The content version for the requested volume.

}

@defproc[(books.layers.get
[volumeId string?]
[summaryId string?]
[#:source source string? 'N/A]
[#:contentVersion contentVersion string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets the layer summary for a volume.

@racket[volumeId]: The volume to retrieve layers for.

@racket[summaryId]: The ID for the layer to get the summary for.

@racket[source]: String to identify the originator of this request.

@racket[contentVersion]: The content version for the requested volume.

}

@section{Functions for the `myconfig' resource}
@defproc[(books.myconfig.releaseDownloadAccess
[cpksver string?]
[volumeIds string?]
[#:source source string? 'N/A]
[#:locale locale string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Release downloaded content access restriction.

@racket[cpksver]: The device/version ID from which to release the restriction.

@racket[volumeIds]: The volume(s) to release restrictions for.

@racket[source]: String to identify the originator of this request.

@racket[locale]: ISO-639-1, ISO-3166-1 codes for message localization, i.e. en_US.

}

@defproc[(books.myconfig.requestAccess
[source string?]
[volumeId string?]
[nonce string?]
[cpksver string?]
[#:locale locale string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Request concurrent and download access restrictions.

@racket[source]: String to identify the originator of this request.

@racket[volumeId]: The volume to request concurrent/download restrictions for.

@racket[nonce]: The client nonce value.

@racket[cpksver]: The device/version ID from which to request the restrictions.

@racket[locale]: ISO-639-1, ISO-3166-1 codes for message localization, i.e. en_US.

}

@defproc[(books.myconfig.syncVolumeLicenses
[source string?]
[nonce string?]
[cpksver string?]
[#:locale locale string? 'N/A]
[#:showPreorders showPreorders string? 'N/A]
[#:volumeIds volumeIds string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Request downloaded content access for specified volumes on the My eBooks shelf.

@racket[source]: String to identify the originator of this request.

@racket[nonce]: The client nonce value.

@racket[cpksver]: The device/version ID from which to release the restriction.

@racket[locale]: ISO-639-1, ISO-3166-1 codes for message localization, i.e. en_US.

@racket[showPreorders]: Set to true to show pre-ordered books. Defaults to false.

@racket[volumeIds]: The volume(s) to request download restrictions for.

}

@section{Functions for the `mylibrary' resource}
