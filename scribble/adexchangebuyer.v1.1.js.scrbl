#lang scribble/manual
@title{Ad Exchange Buyer API v1.1}
Lets you manage your Ad Exchange Buyer account.
@hyperlink["https://developers.google.com/ad-exchange/buyer-rest" "Documentation link"]
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


@section{Functions for the `accounts' resource}
@defproc[(adexchangebuyer.accounts.list
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the authenticated user's list of accounts.

}

@defproc[(adexchangebuyer.accounts.get
[id string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one account by ID.

@racket[id]: The account id

}

@defproc[(adexchangebuyer.accounts.patch
[id string?]
[#:kind kind string? 'N/A]
[#:bidderLocation bidderLocation string? 'N/A]
[#:cookieMatchingNid cookieMatchingNid string? 'N/A]
[#:cookieMatchingUrl cookieMatchingUrl string? 'N/A]
[#:maximumTotalQps maximumTotalQps string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing account. This method supports patch semantics.

@racket[id]: The account id

@racket[kind]: Resource type.

@racket[bidderLocation]: Your bidder locations that have distinct URLs.

@racket[cookieMatchingNid]: The nid parameter value used in cookie match requests. Please contact your technical account manager if you need to change this.

@racket[cookieMatchingUrl]: The base URL used in cookie match requests.

@racket[maximumTotalQps]: The sum of all bidderLocation.maximumQps values cannot exceed this. Please contact your technical account manager if you need to change this.

}

@defproc[(adexchangebuyer.accounts.update
[id string?]
[#:kind kind string? 'N/A]
[#:bidderLocation bidderLocation string? 'N/A]
[#:cookieMatchingNid cookieMatchingNid string? 'N/A]
[#:cookieMatchingUrl cookieMatchingUrl string? 'N/A]
[#:maximumTotalQps maximumTotalQps string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing account.

@racket[id]: The account id

@racket[kind]: Resource type.

@racket[bidderLocation]: Your bidder locations that have distinct URLs.

@racket[cookieMatchingNid]: The nid parameter value used in cookie match requests. Please contact your technical account manager if you need to change this.

@racket[cookieMatchingUrl]: The base URL used in cookie match requests.

@racket[maximumTotalQps]: The sum of all bidderLocation.maximumQps values cannot exceed this. Please contact your technical account manager if you need to change this.

}

@section{Functions for the `directDeals' resource}
@defproc[(adexchangebuyer.directDeals.list
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the authenticated user's list of direct deals.

}

@defproc[(adexchangebuyer.directDeals.get
[id string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one direct deal by ID.

@racket[id]: The direct deal id

}

@section{Functions for the `creatives' resource}
@defproc[(adexchangebuyer.creatives.list
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:statusFilter statusFilter string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of the authenticated user's active creatives.

@racket[maxResults]: Maximum number of entries returned on one result page. If not set, the default is 100. Optional.

@racket[pageToken]: A continuation token, used to page through ad clients. To retrieve the next page, set this parameter to the value of "nextPageToken" from the previous response. Optional.

@racket[statusFilter]: When specified, only creatives having the given status are returned.

}

@defproc[(adexchangebuyer.creatives.get
[accountId string?]
[buyerCreativeId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets the status for a single creative.

@racket[accountId]: The id for the account that will serve this creative.

@racket[buyerCreativeId]: The buyer-specific id for this creative.

}

@defproc[(adexchangebuyer.creatives.insert
[#:attribute attribute string? 'N/A]
[#:kind kind string? 'N/A]
[#:status status string? 'N/A]
[#:width width string? 'N/A]
[#:accountId accountId string? 'N/A]
[#:HTMLSnippet HTMLSnippet string? 'N/A]
[#:advertiserId advertiserId string? 'N/A]
[#:advertiserName advertiserName string? 'N/A]
[#:buyerCreativeId buyerCreativeId string? 'N/A]
[#:clickThroughUrl clickThroughUrl string? 'N/A]
[#:disapprovalReasons disapprovalReasons string? 'N/A]
[#:height height string? 'N/A]
[#:productCategories productCategories string? 'N/A]
[#:sensitiveCategories sensitiveCategories string? 'N/A]
[#:vendorType vendorType string? 'N/A]
[#:videoURL videoURL string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Submit a new creative.

@racket[attribute]: All attributes for the ads that may be shown from this snippet.

@racket[kind]: Resource type.

@racket[status]: Creative serving status. Read-only. This field should not be set in requests.

@racket[width]: Ad width.

@racket[accountId]: Account id.

@racket[HTMLSnippet]: The HTML snippet that displays the ad when inserted in the web page. If set, videoURL should not be set.

@racket[advertiserId]: Detected advertiser id, if any. Read-only. This field should not be set in requests.

@racket[advertiserName]: The name of the company being advertised in the creative.

@racket[buyerCreativeId]: A buyer-specific id identifying the creative in this ad.

@racket[clickThroughUrl]: The set of destination urls for the snippet.

@racket[disapprovalReasons]: The reason for disapproval, if any. Note that not all disapproval reasons may be categorized, so it is possible for the creative to have a status of DISAPPROVED with an empty list for disapproval_reasons. In this case, please reach out to your TAM to help debug the issue. Read-only. This field should not be set in requests.

@racket[height]: Ad height.

@racket[productCategories]: Detected product categories, if any. Read-only. This field should not be set in requests.

@racket[sensitiveCategories]: Detected sensitive categories, if any. Read-only. This field should not be set in requests.

@racket[vendorType]: All vendor types for the ads that may be shown from this snippet.

@racket[videoURL]: The url to fetch a video ad. If set, HTMLSnippet should not be set.

}

