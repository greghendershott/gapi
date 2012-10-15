#lang scribble/manual
@title{Google Play Android Developer API v1}
Lets Android application developers access their Google Play accounts.
@hyperlink["https://developers.google.com/android-publisher" "Documentation link"]
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


@section{Functions for the `purchases' resource}
@defproc[(androidpublisher.purchases.get
[token string?]
[packageName string?]
[subscriptionId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Checks whether a user's subscription purchase is valid and returns its expiry time.

@racket[token]: The token provided to the user's device when the subscription was purchased.

@racket[packageName]: The package name of the application for which this subscription was purchased (for example, 'com.some.thing').

@racket[subscriptionId]: The purchased subscription ID (for example, 'monthly001').

}

@defproc[(androidpublisher.purchases.cancel
[token string?]
[packageName string?]
[subscriptionId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Cancels a user's subscription purchase. The subscription remains valid until its expiration time.

@racket[token]: The token provided to the user's device when the subscription was purchased.

@racket[packageName]: The package name of the application for which this subscription was purchased (for example, 'com.some.thing').

@racket[subscriptionId]: The purchased subscription ID (for example, 'monthly001').

}

