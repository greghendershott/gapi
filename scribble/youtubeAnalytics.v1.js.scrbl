#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{YouTube Analytics API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Retrieve your YouTube Analytics reports.
@hyperlink["http://developers.google.com/youtube/analytics/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "youtubeAnalytics.v1.js")]
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

@subsection{reports}
@defproc[(youtubeAnalytics-reports-query
[ids string?]
[end-date string?]
[metrics string?]
[start-date string?]
[#:sort sort string? 'N/A]
[#:dimensions dimensions string? 'N/A]
[#:filters filters string? 'N/A]
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
Retrieve your YouTube Analytics reports.

@racket[ids]: Unique channel or content owner ID for retrieving YouTube Analytics data. Either channel==C or contentOwner==O where 'C' is the encrypted channel ID and 'O' is the content owner name.

@racket[end-date]: End date for fetching YouTube Analytics data. All requests should specify an end date formatted as YYYY-MM-DD.

@racket[metrics]: A comma-separated list of YouTube Analytics metrics. E.g., 'views' or 'likes,dislikes'

@racket[start-date]: Start date for fetching YouTube Analytics data. All requests should specify a start date formatted as YYYY-MM-DD.

@racket[sort]: A comma-separated list of dimensions or metrics that determine the sort order for YouTube Analytics data. By default the sort order is ascending, '-' prefix causes descending sort order.

@racket[dimensions]: A comma-separated list of YouTube Analytics dimensions. E.g., 'video', or 'ageGroup,gender'.

@racket[filters]: A list of dimension filters to be applied to YouTube Analytics data. Multiple filters can be joined together with the ';' character. The returned result table will satisfy both filters. E.g., video==dMH0bHeiRNg;country==IT will restrict the returned stats to the given video and the country Italy.

@racket[max-results]: The maximum number of rows to include in the response.

@racket[start-index]: An index of the first entity to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter (one-based, inclusive).

}

