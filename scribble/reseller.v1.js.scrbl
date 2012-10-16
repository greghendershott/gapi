#lang scribble/manual
Hi hi hi
@(require planet/scribble (for-label racket))
@title{Enterprise Apps Reseller API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you create and manage your customers and their subscriptions.
@hyperlink["https://developers.google.com/google-apps/reseller/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "reseller.v1.js")]
@section{API Parameters}
The following optional keyword arguments may be passed to all functions for this web service:
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

@subsection{subscriptions}
@defproc[(reseller-subscriptions-list
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:customerNamePrefix customerNamePrefix string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists subscriptions of a reseller, optionally filtered by a customer name prefix.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Token to specify next page in the list

@racket[customerNamePrefix]: Prefix of the customer's domain name by which the subscriptions should be filtered. Optional

}

@defproc[(reseller-subscriptions-get
[#:customerId customerId string?]
[#:subscriptionId subscriptionId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a subscription of the customer.

@racket[customerId]: Id of the Customer

@racket[subscriptionId]: Id of the subscription, which is unique for a customer

}

@defproc[(reseller-subscriptions-insert
[#:customerId customerId string?]
[#:customerAuthToken customerAuthToken string? 'N/A]
[#:kind kind string? 'N/A]
[#:creationTime creationTime string? 'N/A]
[#:purchaseOrderId purchaseOrderId string? 'N/A]
[#:seats seats string? 'N/A]
[#:plan plan string? 'N/A]
[#:renewalSettings renewalSettings string? 'N/A]
[#:skuId skuId string? 'N/A]
[#:subscriptionId subscriptionId string? 'N/A]
[#:trialSettings trialSettings string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates/Transfers a subscription for the customer.

@racket[customerId]: Id of the Customer

@racket[customerAuthToken]: An auth token needed for transferring a subscription. Can be generated at https://www.google.com/a/cpanel/customer-domain/TransferToken. Optional.

@racket[kind]: Identifies the resource as a Subscription.

@racket[creationTime]: Creation time of this subscription in milliseconds since Unix epoch.

@racket[purchaseOrderId]: Purchase order id for your order tracking purposes.

@racket[seats]: Number/Limit of seats in the new plan.

@racket[plan]: Plan details of the subscription

@racket[renewalSettings]: Renewal settings of the subscription.

@racket[skuId]: Name of the sku for which this subscription is purchased.

@racket[subscriptionId]: The id of the subscription.

@racket[trialSettings]: Trial Settings of the subscription.

}

@defproc[(reseller-subscriptions-changePlan
[#:customerId customerId string?]
[#:subscriptionId subscriptionId string?]
[#:kind kind string? 'N/A]
[#:planName planName string? 'N/A]
[#:purchaseOrderId purchaseOrderId string? 'N/A]
[#:seats seats string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Changes the plan of a subscription

@racket[customerId]: Id of the Customer

@racket[subscriptionId]: Id of the subscription, which is unique for a customer

@racket[kind]: Identifies the resource as a subscription change plan request.

@racket[planName]: Name of the plan to change to.

@racket[purchaseOrderId]: Purchase order id for your order tracking purposes.

@racket[seats]: Number/Limit of seats in the new plan.

}

@defproc[(reseller-subscriptions-changeRenewalSettings
[#:customerId customerId string?]
[#:subscriptionId subscriptionId string?]
[#:kind kind string? 'N/A]
[#:renewalType renewalType string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Changes the renewal settings of a subscription

@racket[customerId]: Id of the Customer

@racket[subscriptionId]: Id of the subscription, which is unique for a customer

@racket[kind]: Identifies the resource as a subscription renewal setting.

@racket[renewalType]: Subscription renewal type.

}

@defproc[(reseller-subscriptions-changeSeats
[#:customerId customerId string?]
[#:subscriptionId subscriptionId string?]
[#:kind kind string? 'N/A]
[#:maximumNumberOfSeats maximumNumberOfSeats string? 'N/A]
[#:numberOfSeats numberOfSeats string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Changes the seats configuration of a subscription

@racket[customerId]: Id of the Customer

@racket[subscriptionId]: Id of the subscription, which is unique for a customer

@racket[kind]: Identifies the resource as a subscription change plan request.

@racket[maximumNumberOfSeats]: Maximum number of seats that can be purchased. This needs to be provided only for a non-commitment plan. For a commitment plan it is decided by the contract.

@racket[numberOfSeats]: Number of seats to purchase. This is applicable only for a commitment plan.

}

@defproc[(reseller-subscriptions-startPaidService
[#:customerId customerId string?]
[#:subscriptionId subscriptionId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Starts paid service of a trial subscription

@racket[customerId]: Id of the Customer

@racket[subscriptionId]: Id of the subscription, which is unique for a customer

}

@defproc[(reseller-subscriptions-delete
[#:customerId customerId string?]
[#:subscriptionId subscriptionId string?]
[#:deletionType deletionType string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Cancels/Downgrades a subscription.

@racket[customerId]: Id of the Customer

@racket[subscriptionId]: Id of the subscription, which is unique for a customer

@racket[deletionType]: Whether the subscription is to be fully cancelled or downgraded

}

@subsection{customers}
@defproc[(reseller-customers-get
[#:customerId customerId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a customer resource if one exists and is owned by the reseller.

@racket[customerId]: Id of the Customer

}

@defproc[(reseller-customers-insert
[#:customerAuthToken customerAuthToken string? 'N/A]
[#:kind kind string? 'N/A]
[#:alternateEmail alternateEmail string? 'N/A]
[#:customerDomain customerDomain string? 'N/A]
[#:customerId customerId string? 'N/A]
[#:phoneNumber phoneNumber string? 'N/A]
[#:postalAddress postalAddress string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a customer resource if one does not already exist.

@racket[customerAuthToken]: An auth token needed for inserting a customer for which domain already exists. Can be generated at https://www.google.com/a/cpanel//TransferToken. Optional.

@racket[kind]: Identifies the resource as a customer.

@racket[alternateEmail]: The alternate email of the customer.

@racket[customerDomain]: The domain name of the customer.

@racket[customerId]: The id of the customer.

@racket[phoneNumber]: The phone number of the customer.

@racket[postalAddress]: The postal address of the customer.

}

@defproc[(reseller-customers-patch
[#:customerId customerId string?]
[#:kind kind string? 'N/A]
[#:alternateEmail alternateEmail string? 'N/A]
[#:customerDomain customerDomain string? 'N/A]
[#:phoneNumber phoneNumber string? 'N/A]
[#:postalAddress postalAddress string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a customer resource if one it exists and is owned by the reseller. This method supports patch semantics.

@racket[customerId]: Id of the Customer

@racket[kind]: Identifies the resource as a customer.

@racket[alternateEmail]: The alternate email of the customer.

@racket[customerDomain]: The domain name of the customer.

@racket[phoneNumber]: The phone number of the customer.

@racket[postalAddress]: The postal address of the customer.

}

@defproc[(reseller-customers-update
[#:customerId customerId string?]
[#:kind kind string? 'N/A]
[#:alternateEmail alternateEmail string? 'N/A]
[#:customerDomain customerDomain string? 'N/A]
[#:phoneNumber phoneNumber string? 'N/A]
[#:postalAddress postalAddress string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a customer resource if one it exists and is owned by the reseller.

@racket[customerId]: Id of the Customer

@racket[kind]: Identifies the resource as a customer.

@racket[alternateEmail]: The alternate email of the customer.

@racket[customerDomain]: The domain name of the customer.

@racket[phoneNumber]: The phone number of the customer.

@racket[postalAddress]: The postal address of the customer.

}

