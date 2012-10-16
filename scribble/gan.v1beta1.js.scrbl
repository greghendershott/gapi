#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Google Affiliate Network API v1beta1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you have programmatic access to your Google Affiliate Network data.
@hyperlink["https://developers.google.com/affiliate-network/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "gan.v1beta1.js")]
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

@subsection{advertisers}
@defproc[(gan-advertisers-list
[#:role role string?]
[#:roleId roleId string?]
[#:relationshipStatus relationshipStatus string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:advertiserCategory advertiserCategory string? 'N/A]
[#:minNinetyDayEpc minNinetyDayEpc string? 'N/A]
[#:minPayoutRank minPayoutRank string? 'N/A]
[#:minSevenDayEpc minSevenDayEpc string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves data about all advertisers that the requesting advertiser/publisher has access to.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[relationshipStatus]: Filters out all advertisers for which do not have the given relationship status with the requesting publisher.

@racket[maxResults]: Max number of items to return in this page. Optional. Defaults to 20.

@racket[pageToken]: The value of 'nextPageToken' from the previous page. Optional.

@racket[advertiserCategory]: Caret(^) delimted list of advertiser categories. Valid categories are defined here: http://www.google.com/support/affiliatenetwork/advertiser/bin/answer.py?hl=en&answer=107581. Filters out all advertisers not in one of the given advertiser categories. Optional.

@racket[minNinetyDayEpc]: Filters out all advertisers that have a ninety day EPC average lower than the given value (inclusive). Min value: 0.0. Optional.

@racket[minPayoutRank]: A value between 1 and 4, where 1 represents the quartile of advertisers with the lowest ranks and 4 represents the quartile of advertisers with the highest ranks. Filters out all advertisers with a lower rank than the given quartile. For example if a 2 was given only advertisers with a payout rank of 25 or higher would be included. Optional.

@racket[minSevenDayEpc]: Filters out all advertisers that have a seven day EPC average lower than the given value (inclusive). Min value: 0.0. Optional.

}

@defproc[(gan-advertisers-get
[#:role role string?]
[#:roleId roleId string?]
[#:advertiserId advertiserId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves data about a single advertiser if that the requesting advertiser/publisher has access to it. Only publishers can lookup advertisers. Advertisers can request information about themselves by omitting the advertiserId query parameter.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[advertiserId]: The ID of the advertiser to look up. Optional.

}

@subsection{ccOffers}
@defproc[(gan-ccOffers-list
[#:publisher publisher string?]
[#:projection projection string? 'N/A]
[#:advertiser advertiser string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves credit card offers for the given publisher.

@racket[publisher]: The ID of the publisher in question.

@racket[projection]: The set of fields to return.

@racket[advertiser]: The advertiser ID of a card issuer whose offers to include. Optional, may be repeated.

}

@subsection{events}
@defproc[(gan-events-list
[#:role role string?]
[#:roleId roleId string?]
[#:type type string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:status status string? 'N/A]
[#:sku sku string? 'N/A]
[#:advertiserId advertiserId string? 'N/A]
[#:chargeType chargeType string? 'N/A]
[#:memberId memberId string? 'N/A]
[#:orderId orderId string? 'N/A]
[#:publisherId publisherId string? 'N/A]
[#:eventDateMax eventDateMax string? 'N/A]
[#:eventDateMin eventDateMin string? 'N/A]
[#:linkId linkId string? 'N/A]
[#:modifyDateMax modifyDateMax string? 'N/A]
[#:modifyDateMin modifyDateMin string? 'N/A]
[#:productCategory productCategory string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves event data for a given advertiser/publisher.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[type]: Filters out all events that are not of the given type. Valid values: 'action', 'transaction', 'charge'. Optional.

@racket[maxResults]: Max number of offers to return in this page. Optional. Defaults to 20.

@racket[pageToken]: The value of 'nextPageToken' from the previous page. Optional.

@racket[status]: Filters out all events that do not have the given status. Valid values: 'active', 'canceled'. Optional.

@racket[sku]: Caret(^) delimited list of SKUs. Filters out all events that do not reference one of the given SKU. Optional.

@racket[advertiserId]: Caret(^) delimited list of advertiser IDs. Filters out all events that do not reference one of the given advertiser IDs. Only used when under publishers role. Optional.

@racket[chargeType]: Filters out all charge events that are not of the given charge type. Valid values: 'other', 'slotting_fee', 'monthly_minimum', 'tier_bonus', 'credit', 'debit'. Optional.

@racket[memberId]: Caret(^) delimited list of member IDs. Filters out all events that do not reference one of the given member IDs. Optional.

@racket[orderId]: Caret(^) delimited list of order IDs. Filters out all events that do not reference one of the given order IDs. Optional.

@racket[publisherId]: Caret(^) delimited list of publisher IDs. Filters out all events that do not reference one of the given publishers IDs. Only used when under advertiser role. Optional.

@racket[eventDateMax]: Filters out all events later than given date. Optional. Defaults to 24 hours after eventMin.

@racket[eventDateMin]: Filters out all events earlier than given date. Optional. Defaults to 24 hours from current date/time.

@racket[linkId]: Caret(^) delimited list of link IDs. Filters out all events that do not reference one of the given link IDs. Optional.

@racket[modifyDateMax]: Filters out all events modified later than given date. Optional. Defaults to 24 hours after modifyDateMin, if modifyDateMin is explicitly set.

@racket[modifyDateMin]: Filters out all events modified earlier than given date. Optional. Defaults to 24 hours before the current modifyDateMax, if modifyDateMax is explicitly set.

@racket[productCategory]: Caret(^) delimited list of product categories. Filters out all events that do not reference a product in one of the given product categories. Optional.

}

@subsection{links}
@defproc[(gan-links-list
[#:role role string?]
[#:roleId roleId string?]
[#:relationshipStatus relationshipStatus string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:advertiserId advertiserId string? 'N/A]
[#:authorship authorship string? 'N/A]
[#:linkType linkType string? 'N/A]
[#:promotionType promotionType string? 'N/A]
[#:advertiserCategory advertiserCategory string? 'N/A]
[#:assetSize assetSize string? 'N/A]
[#:startDateMax startDateMax string? 'N/A]
[#:startDateMin startDateMin string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves all links that match the query parameters.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[relationshipStatus]: The status of the relationship.

@racket[maxResults]: Max number of items to return in this page. Optional. Defaults to 20.

@racket[pageToken]: The value of 'nextPageToken' from the previous page. Optional.

@racket[advertiserId]: Limits the resulting links to the ones belonging to the listed advertisers.

@racket[authorship]: The role of the author of the link.

@racket[linkType]: The type of the link.

@racket[promotionType]: The promotion type.

@racket[advertiserCategory]: The advertiser's primary vertical.

@racket[assetSize]: The size of the given asset.

@racket[startDateMax]: The end of the start date range.

@racket[startDateMin]: The beginning of the start date range.

}

@defproc[(gan-links-get
[#:role role string?]
[#:roleId roleId string?]
[#:linkId linkId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves data about a single link if the requesting advertiser/publisher has access to it. Advertisers can look up their own links. Publishers can look up visible links or links belonging to advertisers they are in a relationship with.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[linkId]: The ID of the link to look up.

}

@defproc[(gan-links-insert
[#:role role string?]
[#:roleId roleId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:endDate endDate string? 'N/A]
[#:startDate startDate string? 'N/A]
[#:advertiserId advertiserId string? 'N/A]
[#:authorship authorship string? 'N/A]
[#:availability availability string? 'N/A]
[#:clickTrackingUrl clickTrackingUrl string? 'N/A]
[#:createDate createDate string? 'N/A]
[#:destinationUrl destinationUrl string? 'N/A]
[#:duration duration string? 'N/A]
[#:imageAltText imageAltText string? 'N/A]
[#:impressionTrackingUrl impressionTrackingUrl string? 'N/A]
[#:isActive isActive string? 'N/A]
[#:linkType linkType string? 'N/A]
[#:promotionType promotionType string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new link.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[id]: The ID of this link.

@racket[kind]: The kind for one entity.

@racket[name]: The logical name for this link.

@racket[description]: Description.

@racket[endDate]: Date that this link becomes inactive.

@racket[startDate]: Date that this link becomes active.

@racket[advertiserId]: The advertiser id for the advertiser who owns this link.

@racket[authorship]: Authorship

@racket[availability]: Availability.

@racket[clickTrackingUrl]: Tracking url for clicks.

@racket[createDate]: Date that this link was created.

@racket[destinationUrl]: The destination URL for the link.

@racket[duration]: Duration

@racket[imageAltText]: image alt text.

@racket[impressionTrackingUrl]: Tracking url for impressions.

@racket[isActive]: Flag for if this link is active.

@racket[linkType]: The link type.

@racket[promotionType]: Promotion Type

}

@subsection{publishers}
@defproc[(gan-publishers-list
[#:role role string?]
[#:roleId roleId string?]
[#:relationshipStatus relationshipStatus string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:minNinetyDayEpc minNinetyDayEpc string? 'N/A]
[#:minPayoutRank minPayoutRank string? 'N/A]
[#:minSevenDayEpc minSevenDayEpc string? 'N/A]
[#:publisherCategory publisherCategory string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves data about all publishers that the requesting advertiser/publisher has access to.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[relationshipStatus]: Filters out all publishers for which do not have the given relationship status with the requesting publisher.

@racket[maxResults]: Max number of items to return in this page. Optional. Defaults to 20.

@racket[pageToken]: The value of 'nextPageToken' from the previous page. Optional.

@racket[minNinetyDayEpc]: Filters out all publishers that have a ninety day EPC average lower than the given value (inclusive). Min value: 0.0. Optional.

@racket[minPayoutRank]: A value between 1 and 4, where 1 represents the quartile of publishers with the lowest ranks and 4 represents the quartile of publishers with the highest ranks. Filters out all publishers with a lower rank than the given quartile. For example if a 2 was given only publishers with a payout rank of 25 or higher would be included. Optional.

@racket[minSevenDayEpc]: Filters out all publishers that have a seven day EPC average lower than the given value (inclusive). Min value 0.0. Optional.

@racket[publisherCategory]: Caret(^) delimted list of publisher categories. Valid categories: (unclassified|community_and_content|shopping_and_promotion|loyalty_and_rewards|network|search_specialist|comparison_shopping|email). Filters out all publishers not in one of the given advertiser categories. Optional.

}

@defproc[(gan-publishers-get
[#:role role string?]
[#:roleId roleId string?]
[#:publisherId publisherId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves data about a single advertiser if that the requesting advertiser/publisher has access to it. Only advertisers can look up publishers. Publishers can request information about themselves by omitting the publisherId query parameter.

@racket[role]: The role of the requester. Valid values: 'advertisers' or 'publishers'.

@racket[roleId]: The ID of the requesting advertiser or publisher.

@racket[publisherId]: The ID of the publisher to look up. Optional.

}

