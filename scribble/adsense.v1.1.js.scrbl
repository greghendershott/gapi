#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{AdSense Management API v1.1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Gives AdSense publishers access to their inventory and the ability to generate reports
@hyperlink["https://developers.google.com/adsense/management/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "adsense.v1.1.js")]
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

@subsection{accounts}
@section{Resources}
@defproc[(adsense.accounts.list
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
List all accounts available to this AdSense account.

@racket[maxResults]: The maximum number of accounts to include in the response, used for paging.

@racket[pageToken]: A continuation token, used to page through accounts. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

}

@defproc[(adsense.accounts.get
[accountId string?]
[#:tree tree string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get information about the selected AdSense account.

@racket[accountId]: Account to get information about.

@racket[tree]: Whether the tree of sub accounts should be returned.

}

@subsection{customchannels}
@section{Resources}
@defproc[(adsense.customchannels.list
[adClientId string?]
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
List all custom channels in the specified ad client for this AdSense account.

@racket[adClientId]: Ad client for which to list custom channels.

@racket[maxResults]: The maximum number of custom channels to include in the response, used for paging.

@racket[pageToken]: A continuation token, used to page through custom channels. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

}

@defproc[(adsense.customchannels.get
[adClientId string?]
[customChannelId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get the specified custom channel from the specified ad client.

@racket[adClientId]: Ad client which contains the custom channel.

@racket[customChannelId]: Custom channel to retrieve.

}

@subsection{adunits}
@section{Resources}
@defproc[(adsense.adunits.list
[adClientId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:includeInactive includeInactive string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List all ad units in the specified ad client for this AdSense account.

@racket[adClientId]: Ad client for which to list ad units.

@racket[maxResults]: The maximum number of ad units to include in the response, used for paging.

@racket[pageToken]: A continuation token, used to page through ad units. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

@racket[includeInactive]: Whether to include inactive ad units. Default: true.

}

@defproc[(adsense.adunits.get
[adClientId string?]
[adUnitId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets the specified ad unit in the specified ad client.

@racket[adClientId]: Ad client for which to get the ad unit.

@racket[adUnitId]: Ad unit to retrieve.

}

@subsection{adclients}
@defproc[(adsense.adclients.list
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
List all ad clients in this AdSense account.

@racket[maxResults]: The maximum number of ad clients to include in the response, used for paging.

@racket[pageToken]: A continuation token, used to page through ad clients. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

}

@subsection{reports}
@defproc[(adsense.reports.generate
[endDate string?]
[startDate string?]
[#:sort sort string? 'N/A]
[#:filter filter string? 'N/A]
[#:accountId accountId string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:currency currency string? 'N/A]
[#:dimension dimension string? 'N/A]
[#:locale locale string? 'N/A]
[#:metric metric string? 'N/A]
[#:startIndex startIndex string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Generate an AdSense report based on the report request sent in the query parameters. Returns the result as JSON; to retrieve output in CSV format specify "alt=csv" as a query parameter.

@racket[endDate]: End of the date range to report on in "YYYY-MM-DD" format, inclusive.

@racket[startDate]: Start of the date range to report on in "YYYY-MM-DD" format, inclusive.

@racket[sort]: The name of a dimension or metric to sort the resulting report on, optionally prefixed with "+" to sort ascending or "-" to sort descending. If no prefix is specified, the column is sorted ascending.

@racket[filter]: Filters to be run on the report.

@racket[accountId]: Accounts upon which to report.

@racket[maxResults]: The maximum number of rows of report data to return.

@racket[currency]: Optional currency to use when reporting on monetary metrics. Defaults to the account's currency if not set.

@racket[dimension]: Dimensions to base the report on.

@racket[locale]: Optional locale to use for translating report output to a local language. Defaults to "en_US" if not specified.

@racket[metric]: Numeric columns to include in the report.

@racket[startIndex]: Index of the first row of report data to return.

}

@subsection{urlchannels}
@defproc[(adsense.urlchannels.list
[adClientId string?]
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
List all URL channels in the specified ad client for this AdSense account.

@racket[adClientId]: Ad client for which to list URL channels.

@racket[maxResults]: The maximum number of URL channels to include in the response, used for paging.

@racket[pageToken]: A continuation token, used to page through URL channels. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

}

