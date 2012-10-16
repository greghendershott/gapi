#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{AdSense Host API v4.1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Gives AdSense Hosts access to report generation, ad code generation, and publisher management capabilities.
@hyperlink["https://developers.google.com/adsense/host/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "adsensehost.v4.1.js")]
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
@defproc[(adsensehost-accounts-list
[filterAdClientId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List hosted accounts associated with this AdSense account by ad client id.

@racket[filterAdClientId]: Ad clients to list accounts for.

}

@defproc[(adsensehost-accounts-get
[accountId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get information about the selected associated AdSense account.

@racket[accountId]: Account to get information about.

}

@subsection{customchannels}
@defproc[(adsensehost-customchannels-list
[adClientId string?]
[#:pageToken pageToken string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List all host custom channels in this AdSense account.

@racket[adClientId]: Ad client for which to list custom channels.

@racket[pageToken]: A continuation token, used to page through custom channels. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

@racket[maxResults]: The maximum number of custom channels to include in the response, used for paging.

}

@defproc[(adsensehost-customchannels-get
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
Get a specific custom channel from the host AdSense account.

@racket[adClientId]: Ad client from which to get the custom channel.

@racket[customChannelId]: Custom channel to get.

}

@defproc[(adsensehost-customchannels-insert
[adClientId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:code code string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Add a new custom channel to the host AdSense account.

@racket[adClientId]: Ad client to which the new custom channel will be added.

@racket[id]: Unique identifier of this custom channel. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format.

@racket[kind]: Kind of resource this is, in this case adsensehost#customChannel.

@racket[name]: Name of this custom channel.

@racket[code]: Code of this custom channel, not necessarily unique across ad clients.

}

@defproc[(adsensehost-customchannels-patch
[adClientId string?]
[customChannelId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:code code string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a custom channel in the host AdSense account. This method supports patch semantics.

@racket[adClientId]: Ad client in which the custom channel will be updated.

@racket[customChannelId]: Custom channel to get.

@racket[id]: Unique identifier of this custom channel. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format.

@racket[kind]: Kind of resource this is, in this case adsensehost#customChannel.

@racket[name]: Name of this custom channel.

@racket[code]: Code of this custom channel, not necessarily unique across ad clients.

}

@defproc[(adsensehost-customchannels-update
[adClientId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:code code string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a custom channel in the host AdSense account.

@racket[adClientId]: Ad client in which the custom channel will be updated.

@racket[id]: Unique identifier of this custom channel. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format.

@racket[kind]: Kind of resource this is, in this case adsensehost#customChannel.

@racket[name]: Name of this custom channel.

@racket[code]: Code of this custom channel, not necessarily unique across ad clients.

}

@defproc[(adsensehost-customchannels-delete
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
Delete a specific custom channel from the host AdSense account.

@racket[adClientId]: Ad client from which to delete the custom channel.

@racket[customChannelId]: Custom channel to delete.

}

@subsection{adclients}
@defproc[(adsensehost-adclients-list
[#:pageToken pageToken string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List all host ad clients in this AdSense account.

@racket[pageToken]: A continuation token, used to page through ad clients. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

@racket[maxResults]: The maximum number of ad clients to include in the response, used for paging.

}

@defproc[(adsensehost-adclients-get
[adClientId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get information about one of the ad clients in the Host AdSense account.

@racket[adClientId]: Ad client to get.

}

@subsection{reports}
@defproc[(adsensehost-reports-generate
[endDate string?]
[startDate string?]
[#:sort sort string? 'N/A]
[#:filter filter string? 'N/A]
[#:maxResults maxResults string? 'N/A]
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

@racket[maxResults]: The maximum number of rows of report data to return.

@racket[dimension]: Dimensions to base the report on.

@racket[locale]: Optional locale to use for translating report output to a local language. Defaults to "en_US" if not specified.

@racket[metric]: Numeric columns to include in the report.

@racket[startIndex]: Index of the first row of report data to return.

}

@subsection{urlchannels}
@defproc[(adsensehost-urlchannels-list
[adClientId string?]
[#:pageToken pageToken string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List all host URL channels in the host AdSense account.

@racket[adClientId]: Ad client for which to list URL channels.

@racket[pageToken]: A continuation token, used to page through URL channels. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response.

@racket[maxResults]: The maximum number of URL channels to include in the response, used for paging.

}

@defproc[(adsensehost-urlchannels-insert
[adClientId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:urlPattern urlPattern string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Add a new URL channel to the host AdSense account.

@racket[adClientId]: Ad client to which the new URL channel will be added.

@racket[id]: Unique identifier of this URL channel. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format.

@racket[kind]: Kind of resource this is, in this case adsensehost#urlChannel.

@racket[urlPattern]: URL Pattern of this URL channel. Does not include "http://" or "https://". Example: www.example.com/home

}

@defproc[(adsensehost-urlchannels-delete
[adClientId string?]
[urlChannelId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Delete a URL channel from the host AdSense account.

@racket[adClientId]: Ad client from which to delete the URL channel.

@racket[urlChannelId]: URL channel to delete.

}

@subsection{associationsessions}
@defproc[(adsensehost-associationsessions-start
[productCode string?]
[websiteUrl string?]
[#:userLocale userLocale string? 'N/A]
[#:websiteLocale websiteLocale string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Create an association session for initiating an association with an AdSense user.

@racket[productCode]: Products to associate with the user.

@racket[websiteUrl]: The URL of the user's hosted website.

@racket[userLocale]: The preferred locale of the user.

@racket[websiteLocale]: The locale of the user's hosted website.

}

@defproc[(adsensehost-associationsessions-verify
[token string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Verify an association session after the association callback returns from AdSense signup.

@racket[token]: The token returned to the association callback URL.

}

