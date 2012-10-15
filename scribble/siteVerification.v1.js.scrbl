#lang scribble/manual
@title{Google Site Verification API v1}
Lets you programatically verify ownership of websites or domains with Google.
@hyperlink["http://code.google.com/apis/siteverification/" "Documentation link"]
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


@section{Functions for the `webResource' resource}
@defproc[(siteVerification.webResource.list
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get the list of your verified websites and domains.

}

@defproc[(siteVerification.webResource.get
[id string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get the most current data for a website or domain.

@racket[id]: The id of a verified site or domain.

}

@defproc[(siteVerification.webResource.insert
[verificationMethod string?]
[#:id id string? 'N/A]
[#:site site string? 'N/A]
[#:owners owners string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Attempt verification of a website or domain.

@racket[verificationMethod]: The method to use for verifying a site or domain.

@racket[id]: The string used to identify this site. This value should be used in the "id" portion of the REST URL for the Get, Update, and Delete operations.

@racket[site]: The address and type of a site that is verified or will be verified.

@racket[owners]: The email addresses of all verified owners.

}

@defproc[(siteVerification.webResource.patch
[id string?]
[#:site site string? 'N/A]
[#:owners owners string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Modify the list of owners for your website or domain. This method supports patch semantics.

@racket[id]: The id of a verified site or domain.

@racket[site]: The address and type of a site that is verified or will be verified.

@racket[owners]: The email addresses of all verified owners.

}

@defproc[(siteVerification.webResource.getToken
[#:site site string? 'N/A]
[#:verificationMethod verificationMethod string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get a verification token for placing on a website or domain.

@racket[site]: The site for which a verification token will be generated.

@racket[verificationMethod]: The verification method that will be used to verify this site. For sites, 'FILE' or 'META' methods may be used. For domains, only 'DNS' may be used.

}

@defproc[(siteVerification.webResource.update
[id string?]
[#:site site string? 'N/A]
[#:owners owners string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Modify the list of owners for your website or domain.

@racket[id]: The id of a verified site or domain.

@racket[site]: The address and type of a site that is verified or will be verified.

@racket[owners]: The email addresses of all verified owners.

}

@defproc[(siteVerification.webResource.delete
[id string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Relinquish ownership of a website or domain.

@racket[id]: The id of a verified site or domain.

}

