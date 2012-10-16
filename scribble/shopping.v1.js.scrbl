#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Search API For Shopping v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you search over product data.
@hyperlink["https://developers.google.com/shopping-search/v1/getting_started" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "shopping.v1.js")]
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

@subsection{products}
@defproc[(shopping-products-list
[#:source source string?]
[#:location location string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:language language string? 'N/A]
[#:country country string? 'N/A]
[#:q q string? 'N/A]
[#:startIndex startIndex string? 'N/A]
[#:thumbnails thumbnails string? 'N/A]
[#:availability availability string? 'N/A]
[#:currency currency string? 'N/A]
[#:attributeFilter attributeFilter string? 'N/A]
[#:categories.enabled categories.enabled string? 'N/A]
[#:categories.include categories.include string? 'N/A]
[#:categories.useGcsConfig categories.useGcsConfig string? 'N/A]
[#:plusOne.enabled plusOne.enabled string? 'N/A]
[#:plusOne.styles plusOne.styles string? 'N/A]
[#:plusOne.useGcsConfig plusOne.useGcsConfig string? 'N/A]
[#:taxonomy taxonomy string? 'N/A]
[#:boostBy boostBy string? 'N/A]
[#:categoryRecommendations.category categoryRecommendations.category string? 'N/A]
[#:categoryRecommendations.enabled categoryRecommendations.enabled string? 'N/A]
[#:categoryRecommendations.include categoryRecommendations.include string? 'N/A]
[#:categoryRecommendations.useGcsConfig categoryRecommendations.useGcsConfig string? 'N/A]
[#:channels channels string? 'N/A]
[#:clickTracking clickTracking string? 'N/A]
[#:crowdBy crowdBy string? 'N/A]
[#:facets.discover facets.discover string? 'N/A]
[#:facets.enabled facets.enabled string? 'N/A]
[#:facets.include facets.include string? 'N/A]
[#:facets.useGcsConfig facets.useGcsConfig string? 'N/A]
[#:maxVariants maxVariants string? 'N/A]
[#:promotions.enabled promotions.enabled string? 'N/A]
[#:promotions.useGcsConfig promotions.useGcsConfig string? 'N/A]
[#:rankBy rankBy string? 'N/A]
[#:redirects.enabled redirects.enabled string? 'N/A]
[#:redirects.useGcsConfig redirects.useGcsConfig string? 'N/A]
[#:relatedQueries.enabled relatedQueries.enabled string? 'N/A]
[#:relatedQueries.useGcsConfig relatedQueries.useGcsConfig string? 'N/A]
[#:restrictBy restrictBy string? 'N/A]
[#:safe safe string? 'N/A]
[#:spelling.enabled spelling.enabled string? 'N/A]
[#:spelling.useGcsConfig spelling.useGcsConfig string? 'N/A]
[#:useCase useCase string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns a list of products and content modules

@racket[source]: Query source

@racket[location]: Location used to determine tax and shipping

@racket[maxResults]: Maximum number of results to return

@racket[language]: Language restriction (BCP 47)

@racket[country]: Country restriction (ISO 3166)

@racket[q]: Search query

@racket[startIndex]: Index (1-based) of first product to return

@racket[thumbnails]: Image thumbnails specification

@racket[availability]: Comma separated list of availabilities (outOfStock, limited, inStock, backOrder, preOrder, onDisplayToOrder) to return

@racket[currency]: Currency restriction (ISO 4217)

@racket[attributeFilter]: Comma separated list of attributes to return

@racket[categories.enabled]: Whether to return category information

@racket[categories.include]: Category specification

@racket[categories.useGcsConfig]: This parameter is currently ignored

@racket[plusOne.enabled]: Whether to return +1 button code

@racket[plusOne.styles]: +1 button rendering styles

@racket[plusOne.useGcsConfig]: Whether to use +1 button styles configured in the GCS account

@racket[taxonomy]: Taxonomy name

@racket[boostBy]: Boosting specification

@racket[categoryRecommendations.category]: Category for which to retrieve recommendations

@racket[categoryRecommendations.enabled]: Whether to return category recommendation information

@racket[categoryRecommendations.include]: Category recommendation specification

@racket[categoryRecommendations.useGcsConfig]: This parameter is currently ignored

@racket[channels]: Channels specification

@racket[clickTracking]: Whether to add a click tracking parameter to offer URLs

@racket[crowdBy]: Crowding specification

@racket[facets.discover]: Facets to discover

@racket[facets.enabled]: Whether to return facet information

@racket[facets.include]: Facets to include (applies when useGcsConfig == false)

@racket[facets.useGcsConfig]: Whether to return facet information as configured in the GCS account

@racket[maxVariants]: Maximum number of variant results to return per result

@racket[promotions.enabled]: Whether to return promotion information

@racket[promotions.useGcsConfig]: Whether to return promotion information as configured in the GCS account

@racket[rankBy]: Ranking specification

@racket[redirects.enabled]: Whether to return redirect information

@racket[redirects.useGcsConfig]: Whether to return redirect information as configured in the GCS account

@racket[relatedQueries.enabled]: Whether to return related queries

@racket[relatedQueries.useGcsConfig]: This parameter is currently ignored

@racket[restrictBy]: Restriction specification

@racket[safe]: Whether safe search is enabled. Default: true

@racket[spelling.enabled]: Whether to return spelling suggestions

@racket[spelling.useGcsConfig]: This parameter is currently ignored

@racket[useCase]: One of CommerceSearchUseCase, ShoppingApiUseCase

}

@defproc[(shopping-products-get
[#:source source string?]
[#:accountId accountId string?]
[#:productId productId string?]
[#:productIdType productIdType string?]
[#:location location string? 'N/A]
[#:thumbnails thumbnails string? 'N/A]
[#:attributeFilter attributeFilter string? 'N/A]
[#:categories.enabled categories.enabled string? 'N/A]
[#:categories.include categories.include string? 'N/A]
[#:categories.useGcsConfig categories.useGcsConfig string? 'N/A]
[#:plusOne.enabled plusOne.enabled string? 'N/A]
[#:plusOne.styles plusOne.styles string? 'N/A]
[#:plusOne.useGcsConfig plusOne.useGcsConfig string? 'N/A]
[#:recommendations.enabled recommendations.enabled string? 'N/A]
[#:recommendations.include recommendations.include string? 'N/A]
[#:recommendations.useGcsConfig recommendations.useGcsConfig string? 'N/A]
[#:taxonomy taxonomy string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns a single product

@racket[source]: Query source

@racket[accountId]: Merchant center account id

@racket[productId]: Id of product

@racket[productIdType]: Type of productId

@racket[location]: Location used to determine tax and shipping

@racket[thumbnails]: Thumbnail specification

@racket[attributeFilter]: Comma separated list of attributes to return

@racket[categories.enabled]: Whether to return category information

@racket[categories.include]: Category specification

@racket[categories.useGcsConfig]: This parameter is currently ignored

@racket[plusOne.enabled]: Whether to return +1 button code

@racket[plusOne.styles]: +1 button rendering styles

@racket[plusOne.useGcsConfig]: Whether to use +1 button styles configured in the GCS account

@racket[recommendations.enabled]: Whether to return recommendation information

@racket[recommendations.include]: Recommendation specification

@racket[recommendations.useGcsConfig]: This parameter is currently ignored

@racket[taxonomy]: Merchant taxonomy

}

