#lang scribble/manual
@title{Enterprise License Manager API v1}
Licensing API to view and manage license for your domain.
@hyperlink["https://developers.google.com/google-apps/licensing/" "Documentation link"]
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


@section{Functions for the `licenseAssignments' resource}
@defproc[(licensing.licenseAssignments.get
[productId string?]
[skuId string?]
[userId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get license assignment of a particular product and sku for a user

@racket[productId]: Name for product

@racket[skuId]: Name for sku

@racket[userId]: email id or unique Id of the user

}

@defproc[(licensing.licenseAssignments.insert
[productId string?]
[skuId string?]
[#:userId userId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Assign License.

@racket[productId]: Name for product

@racket[skuId]: Name for sku

@racket[userId]: Email id of the user

}

@defproc[(licensing.licenseAssignments.patch
[productId string?]
[skuId string?]
[userId string?]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etags etags string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Assign License. This method supports patch semantics.

@racket[productId]: Name for product

@racket[skuId]: Name for sku for which license would be revoked

@racket[userId]: email id or unique Id of the user

@racket[kind]: Identifies the resource as a LicenseAssignment.

@racket[selfLink]: Link to this page.

@racket[etags]: ETag of the resource.

}

@defproc[(licensing.licenseAssignments.listForProduct
[productId string?]
[customerId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List license assignments for given product of the customer.

@racket[productId]: Name for product

@racket[customerId]: CustomerId represents the customer for whom licenseassignments are queried

@racket[maxResults]: Maximum number of campaigns to return at one time. Must be positive. Optional. Default value is 100.

@racket[pageToken]: Token to fetch the next page.Optional. By default server will return first page

}

@defproc[(licensing.licenseAssignments.listForProductAndSku
[productId string?]
[skuId string?]
[customerId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List license assignments for given product and sku of the customer.

@racket[productId]: Name for product

@racket[skuId]: Name for sku

@racket[customerId]: CustomerId represents the customer for whom licenseassignments are queried

@racket[maxResults]: Maximum number of campaigns to return at one time. Must be positive. Optional. Default value is 100.

@racket[pageToken]: Token to fetch the next page.Optional. By default server will return first page

}

@defproc[(licensing.licenseAssignments.update
[productId string?]
[skuId string?]
[userId string?]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etags etags string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Assign License.

@racket[productId]: Name for product

@racket[skuId]: Name for sku for which license would be revoked

@racket[userId]: email id or unique Id of the user

@racket[kind]: Identifies the resource as a LicenseAssignment.

@racket[selfLink]: Link to this page.

@racket[etags]: ETag of the resource.

}

@defproc[(licensing.licenseAssignments.delete
[productId string?]
[skuId string?]
[userId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Revoke License.

@racket[productId]: Name for product

@racket[skuId]: Name for sku

@racket[userId]: email id or unique Id of the user

}
