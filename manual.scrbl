#lang scribble/manual

@(require planet/scribble
          (for-label racket)
          (for-label (this-package-in main))
          (for-label (this-package-in dynamic))
          (for-label (this-package-in macro))
          (for-label (this-package-in code-generate))
          )

@title{GAPI: Google API Discovery Service}

@table-of-contents{}

@section{Introduction}


@section{Dynamic}

@defmodule/this-package[dynamic]

@defproc[(discovery-document->service [discovery-document jsexpr?]) service?]{

Create a @racket[service?] object representing the web service defined
by the JSON provided as a @racket[jsexpr?].

}


@defproc[(local-discovery-document->service
[filename path-string?]
) service?]{

Create a @racket[service?] object representing the web service defined
by the JSON file @racket[filename].

}


@defproc[(get-discovery-document->service
[name string?]
[ver string?]
) service?]{

Create a @racket[service?] object representing the web service defined
by the name and version, which is fetched via HTTP from the Google API
Discovery service.

}


@defproc[(method-proc
[service service?]
[resource-name symbol?]
[method-name symbol?]
) (dict -> jsexpr?)]{

Given a @racket[service?], @racket[resource-name], and
@racket[method-name], get a procedure that makes HTTP requests to the
service.

The parameters to be supplied to the service are supplied as a
@racket[dict?] where the key is a @racket[symbol?] for the name of the
parameter, and the value is (you guessed it) the value.

The parameter names are those specified by the discovery document.

The response from the service is JSON, and is returned by the function
as a @racket[jsexpr?]. This library doesn't try to marshal the
response into structs. Instead you will need to understand its format
and retrieve the information, for example @racket[(hash-ref js
'items)] if the response contains a list of items.

}


@section{Macro}

@defmodule/this-package[macro]

@defform[(require-gapi-doc discovery-document-name)]{

Defines functions respresenting the web service defined by the JSON
file named @racket[discovery-document-name].

Each function makes an HTTP request to the web service. The function
takes normal arguments for parameters that are defined as required,
whereas optional keyword arguments are used to represent other
parameters. These values are supplied in the HTTP request to the service.

The response from the service is JSON, and is returned by the function
as a @racket[jsexpr?]. This library doesn't try to marshal the
response into structs. Instead you will need to understand its format
and retrieve the information, for example @racket[(hash-ref js
'items)] if the response contains a list of items.

As a convenience, the @racket[#:key] argument (which corresponds to
the @tt{key} query parameter in the HTTP request) defaults to the
@racket[api-key] Racket parameter. Put your Google API key in a file
named @tt{.google-api-key} in your home directory--in other words,
@tt{~/.google-api-key}.

As another convenience, see the @racket[paged] form which simplifies
making repeated calls to a service that returns results in small
"pages" (batches of results).

@hyperlink["./adexchangebuyer.v1.1.js.html" "AdExchange"]

The currently available service documents are:

@itemize[
@item{@hyperlink["adexchangebuyer.v1.1.js.html" "adexchangebuyer.v1.1.js"] : Lets you manage your Ad Exchange Buyer account.}
@item{@hyperlink["adsense.v1.1.js.html" "adsense.v1.1.js"] : Gives AdSense publishers access to their inventory and the ability to generate reports}
@item{@hyperlink["adsensehost.v4.1.js.html" "adsensehost.v4.1.js"] : Gives AdSense Hosts access to report generation, ad code generation, and publisher management capabilities.}
@item{@hyperlink["analytics.v3.js.html" "analytics.v3.js"] : View and manage your Google Analytics data}
@item{@hyperlink["androidpublisher.v1.js.html" "androidpublisher.v1.js"] : Lets Android application developers access their Google Play accounts.}
@item{@hyperlink["audit.v1.js.html" "audit.v1.js"] : Lets you access user activities in your enterprise made through various applications.}
@item{@hyperlink["bigquery.v2.js.html" "bigquery.v2.js"] : A data platform for customers to create, manage, share and query data.}
@item{@hyperlink["blogger.v3.js.html" "blogger.v3.js"] : API for access to the data within Blogger.}
@item{@hyperlink["books.v1.js.html" "books.v1.js"] : Lets you search for books and manage your Google Books library.}
@item{@hyperlink["calendar.v3.js.html" "calendar.v3.js"] : Lets you manipulate events and other calendar data.}
@item{@hyperlink["civicinfo.us_v1.js.html" "civicinfo.us_v1.js"] : An API for accessing civic information.}
@item{@hyperlink["compute.v1beta12.js.html" "compute.v1beta12.js"] : API for the Google Compute Engine service.}
@item{@hyperlink["coordinate.v1.js.html" "coordinate.v1.js"] : Lets you view and manage jobs in a Coordinate team.}
@item{@hyperlink["customsearch.v1.js.html" "customsearch.v1.js"] : Lets you search over a website or collection of websites}
@item{@hyperlink["dfareporting.v1.1.js.html" "dfareporting.v1.1.js"] : Lets you create, run and download reports.}
@item{@hyperlink["discovery.v1.js.html" "discovery.v1.js"] : Lets you discover information about other Google APIs, such as what APIs are available, the resource and method details for each API}
@item{@hyperlink["drive.v2.js.html" "drive.v2.js"] : The API to interact with Drive.}
@item{@hyperlink["freebase.v1.js.html" "freebase.v1.js"] : Lets you access the Freebase repository of open data.}
@item{@hyperlink["fusiontables.v1.js.html" "fusiontables.v1.js"] : API for working with Fusion Tables data.}
@item{@hyperlink["gan.v1beta1.js.html" "gan.v1beta1.js"] : Lets you have programmatic access to your Google Affiliate Network data.}
@item{@hyperlink["groupssettings.v1.js.html" "groupssettings.v1.js"] : Lets you manage permission levels and related settings of a group.}
@item{@hyperlink["latitude.v1.js.html" "latitude.v1.js"] : Lets you read and update your current location and work with your location history}
@item{@hyperlink["licensing.v1.js.html" "licensing.v1.js"] : Licensing API to view and manage license for your domain.}
@item{@hyperlink["moderator.v1.js.html" "moderator.v1.js"] : Moderator API}
@item{@hyperlink["oauth2.v2.js.html" "oauth2.v2.js"] : Lets you access OAuth2 protocol related APIs.}
@item{@hyperlink["orkut.v2.js.html" "orkut.v2.js"] : Lets you manage activities, comments and badges in Orkut. More stuff coming in time.}
@item{@hyperlink["pagespeedonline.v1.js.html" "pagespeedonline.v1.js"] : Lets you analyze the performance of a web page and get tailored suggestions to make that page faster.}
@item{@hyperlink["plus.v1.js.html" "plus.v1.js"] : The Google+ API enables developers to build on top of the Google+ platform.}
@item{@hyperlink["prediction.v1.5.js.html" "prediction.v1.5.js"] : Lets you access a cloud hosted machine learning service that makes it easy to build smart apps}
@item{@hyperlink["reseller.v1.js.html" "reseller.v1.js"] : Lets you create and manage your customers and their subscriptions.}
@item{@hyperlink["shopping.v1.js.html" "shopping.v1.js"] : Lets you search over product data.}
@item{@hyperlink["siteVerification.v1.js.html" "siteVerification.v1.js"] : Lets you programatically verify ownership of websites or domains with Google.}
@item{@hyperlink["storage.v1beta1.js.html" "storage.v1beta1.js"] : Lets you store and retrieve potentially-large, immutable data objects.}
@item{@hyperlink["taskqueue.v1beta2.js.html" "taskqueue.v1beta2.js"] : Lets you access a Google App Engine Pull Task Queue over REST.}
@item{@hyperlink["tasks.v1.js.html" "tasks.v1.js"] : Lets you manage your tasks and task lists.}
@item{@hyperlink["translate.v2.js.html" "translate.v2.js"] : Lets you translate text from one language to another}
@item{@hyperlink["urlshortener.v1.js.html" "urlshortener.v1.js"] : Lets you create, inspect, and manage goo.gl short URLs}
@item{@hyperlink["webfonts.v1.js.html" "webfonts.v1.js"] : The Google Web Fonts Developer API.}
@item{@hyperlink["youtube.v3.js.html" "youtube.v3.js"] : Programmatic access to YouTube features.}
@item{@hyperlink["youtubeAnalytics.v1.js.html" "youtubeAnalytics.v1.js"] : Retrieve your YouTube Analytics reports.}
]

}


@defform[(paged (function arguments ...))]{

Makes repeated calls to @racket[function] with @racket[arguments] if
necessary to obtain additional results. If a set of results from the
web service includes a @tt{nextPageToken} value, indicating that more
results are available, then @racket[function] is called again with
@racket[arguments] plus the additional argument @racket[#:pageToken].

If a service will return a very large amount of results, you may
@italic{want} to make the calls individually and process the results
in small batches.  However if you would rather get the all of the
results in one, albeit potentially very large, list of items, use this
form.

}

@section{Generating .RKT files}

@defmodule/this-package[code-generate]
